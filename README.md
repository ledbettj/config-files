Emacs Configuration
===================

![Emacs!](https://raw.githubusercontent.com/ledbettj/config-files/master/emacs.png)

This is my [GNU Emacs](http://www.gnu.org/software/emacs/) configuration file,
which customizes Emacs to behave the way I like it.  It's been re-written
several times, hopefully getting better each time.  Currently, it bootstraps
itself using [el-get](https://github.com/dimitri/el-get) to install a bunch
of useful third-party libraries, such that there are no dependencies or
requirements for installation beyond cloning this repo and linking `.emacs.d`
into place.

`init.el` handles bootstrapping and configuring `el-get`, and then loads
customizations from the `custom/` directory.

Bash Snippets
=============

Just a few bash functions that I find useful across multiple machines.

InputRC
=======

Tweaks to readline.

Mjolnir
=======

Mjolnir (for OS X) configuration to tile windows on enormous thunderbolt monitors.
