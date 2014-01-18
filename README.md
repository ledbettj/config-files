Emacs Configuration
===================

This is my [GNU Emacs](http://www.gnu.org/software/emacs/) configuration file,
which customizes Emacs to behave the way I like it.  It's been re-written
several times, hopefully getting better each time.  Currently, it bootstraps
itself using [el-get](https://github.com/dimitri/el-get) to install a bunch
of useful third-party libraries, such that there are no dependencies or
requirements for installation beyond cloning this repo and linking `.emacs.d`
into place.

`init.el` handles bootstrapping and configuring `el-get`, and then loads
customizations from the `custom/` directory.

`emacs@.service` provides a
[systemd](http://freedesktop.org/wiki/Software/systemd) unit file for starting
an emacs daemon.  Usage:

    sudo cp emacs@.service /etc/systemd/system/
    sudo systemctl enable emacs@${USER} # run this service on boot
    sudo systemctl start  emacs@${USER} # start it now

Once you have a daemon instance running, you can use `emacsclient` to attach:

    emacsclient -c # create a new graphical frame
    emacsclient -t # create a new frame in the current terminal

These commands are aliased to `ec` and `et` respectively.

Bash Snippets
=============

Just a few bash functions that I find useful across multiple machines.

InputRC
=======

Tweaks to readline.

Slate
=====

Slate (for OS X) configuration to tile windows on enormous thunderbolt monitors.
