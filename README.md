Emacs Configuration
===================

This is my [GNU Emacs](http://www.gnu.org/software/emacs/) configuration file,
which customizes Emacs to behave the way I like it.  It's been re-written
several times, hopefully getting better each time.  Currently, it bootstraps
itself using [el-get](https://github.com/dimitri/el-get) to install a bunch
of useful third-party libraries, such that there are no dependencies or
requirements for installation beyond cloning this repo and linking `.emacs.d`
into place.

My customizations live in the `customization/` directory:

* `ui.el` - tweaks to the Emacs UI, including specifying a theme.
* `behavior.el` - tweaks to the way various Emacs features or commands work.
* `ruby.el` - tweaks to the behavior of `ruby-mode`.
* `js.el` - tweaks to the behavior of `js-mode`.
* `keybinds.el` - my custom keybindings.
