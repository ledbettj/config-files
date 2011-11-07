;;; -*- indent-tabs-mode:nil coding:latin-1-unix -*-
;;;
;;; swank-rpc.lisp  -- Pass remote calls and responses between lisp systems.
;;;
;;; Created 2010, Terje Norderhaug <terje@in-progress.com>
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;

(defpackage #:swank-rpc
  (:use :cl)
  (:export 
   #:read-message
   #:swank-reader-error
   #:swank-reader-error.packet
   #:swank-reader-error.cause
   #:write-message))

(in-package :swank-rpc)


;;;;; Input

(define-condition swank-reader-error (reader-error)
  ((packet :type string :initarg :packet 
           :reader swank-reader-error.packet)
   (cause :type reader-error :initarg :cause 
          :reader swank-reader-error.cause)))

(defun read-message (stream package)
  (let ((packet (read-packet stream)))
    (handler-case (values (read-form packet package))
      (reader-error (c)
        (error (make-condition 'swank-reader-error 
                               :packet packet :cause c))))))

(defun read-packet (stream)
  (multiple-value-bind (byte0 length) (parse-header stream)
    (cond ((= byte0 0)
           (let ((octets (read-chunk stream length)))
             (handler-case (swank-backend:utf8-to-string octets)
               (error (c) 
                 (error (make-condition 'swank-reader-error 
                                        :packet (asciify octets)
                                        :cause c))))))
          (t
           (error "Invalid header byte0 #b~b" byte0)))))

(defun asciify (packet)
  (with-output-to-string (*standard-output*)
    (loop for code across (etypecase packet 
                            (string (map 'vector #'char-code packet))
                            (vector packet))
          do (cond ((<= code #x7f) (write-char (code-char code)))
                   (t (format t "\\x~x" code))))))

(defun parse-header (stream)
  (values (read-byte stream)
          (logior (ash (read-byte stream) 16)
                  (ash (read-byte stream) 8)
                  (read-byte stream))))
                  
(defun read-chunk (stream length)
  (let* ((buffer (make-array length :element-type '(unsigned-byte 8)))
         (count (read-sequence buffer stream)))
    (assert (= count length) () "Short read: length=~D  count=~D" length count)
    buffer))

;; FIXME: no one ever tested this and will probably not work.
(defparameter *validate-input* nil
  "Set to true to require input that strictly conforms to the protocol")

(defun read-form (string package)
  (with-standard-io-syntax
    (let ((*package* package))
      (if *validate-input*
          (validating-read string)
          (read-from-string string)))))

(defun validating-read (string)
  (with-input-from-string (*standard-input* string)
    (simple-read)))

(defun simple-read ()
   "Read a form that conforms to the protocol, otherwise signal an error."
   (let ((c (read-char)))
     (case c
       (#\" (with-output-to-string (*standard-output*)
              (loop for c = (read-char) do
                    (case c
                      (#\" (return))
                      (#\\ (write-char (read-char)))
                      (t (write-char c))))))
       (#\( (loop collect (simple-read)
                  while (ecase (read-char)
                          (#\) nil)
                          (#\space t))))
       (#\' `(quote ,(simple-read)))
       (t (let ((string (with-output-to-string (*standard-output*)
                          (loop for ch = c then (read-char nil nil) do
                                (case ch
                                  ((nil) (return))
                                  (#\\ (write-char (read-char)))
                                  ((#\space #\)) (unread-char ch)(return))
                                  (t (write-char ch)))))))
            (cond ((digit-char-p c) (parse-integer string))
                  ((intern string))))))))


;;;;; Output

(defun write-message (message package stream)
  (let* ((string (prin1-to-string-for-emacs message package))
         (octets (handler-case (swank-backend:string-to-utf8 string)
                   (error (c) (encoding-error c string))))
         (length (length octets)))
    (write-header stream 0 length)
    (write-sequence octets stream)
    (finish-output stream)))

;; FIXME: for now just tell emacs that we and an encoding problem.
(defun encoding-error (condition string)
  (swank-backend:string-to-utf8
   (prin1-to-string-for-emacs
    `(:reader-error
      ,(asciify string)
      ,(format nil "Error during string-to-utf8: ~a"
               (or (ignore-errors (asciify (princ-to-string condition)))
                   (asciify (princ-to-string (type-of condition))))))
    (find-package :cl))))

(defun write-header (stream byte0 length)
  (declare (type (unsigned-byte 8) byte0)
           (type (unsigned-byte 24) length))
  ;;(format *trace-output* "byte0: ~d length: ~d (#x~x)~%" byte0 length length)
  (write-byte byte0 stream)
  (write-byte (ldb (byte 8 16) length) stream)
  (write-byte (ldb (byte 8 8) length) stream)
  (write-byte (ldb (byte 8 0) length) stream))

(defun prin1-to-string-for-emacs (object package)
  (with-standard-io-syntax
    (let ((*print-case* :downcase)
          (*print-readably* nil)
          (*print-pretty* nil)
          (*package* package))
      (prin1-to-string object))))


#| TEST/DEMO:

(defparameter *transport*
  (with-output-to-string (out)
    (write-message '(:message (hello "world")) *package* out)
    (write-message '(:return 5) *package* out)
    (write-message '(:emacs-rex NIL) *package* out)))

*transport*
                 
(with-input-from-string (in *transport*)
  (loop while (peek-char T in NIL)
        collect (read-message in *package*)))

|#
