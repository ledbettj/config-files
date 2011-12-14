# suitable for use in .bashrc, etc

# user@host:working-dir$
PS1="\[\e[1;34m\]\u@\h:\[\e[0m\]\w\$ "

# add the specified directory to the end of $PATH if it is not already present
path_append() {
  if [ -d "$1" ] && [[ ":$PATH:" != *":$1:"* ]]; then
    export PATH="$PATH:$1"
  fi
}

# add the specified directory to the start of $PATH if it is not already present
path_prepend() {
  if [ -d "$1" ] && [[ ":$PATH:" != *":$1:"* ]]; then
    export PATH="$1:$PATH"
  fi
}

# after every 'cd', perform 'ls' if cd succeeded
cdls() {
  builtin cd "$*"
  RC=$?
  if [ $RC -eq 0 ]; then
    ls
  fi
}

alias cd="cdls"

# editor settings
export EDITOR="emacs -nw"
export VISUAL="emacs"

# if emacs is running in daemon mode
alias ec="emacsclient -c"
alias et="emacsclient -t"
