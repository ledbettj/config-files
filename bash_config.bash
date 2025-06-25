cdls() {
  builtin cd "$*" && ls --color=auto
}

alias grep='grep --color=auto'
alias ls='ls --color=auto'
alias cd="cdls"

__x_exists() {
  [[ -x $(command -v "$1") ]]
}

if __x_exists "emacs" ; then
  alias ec="emacsclient -c -a 'emacs'"
  alias et="emacsclient -t -a 'emacs -nw'"
  export EDITOR="emacs -nw"
  export VISUAL="emacsclient -c -a 'emacs'"
fi

if __x_exists "starship" ; then
  eval "$(starship init bash)"
fi

if __x_exists "rbenv" ; then
  eval "$(rbenv init -)"
fi

# FZF configuration
[[ -r /usr/share/fzf/completion.bash ]] && . /usr/share/fzf/completion.bash
[[ -r /usr/share/fzf/key-bindings.bash ]] && . /usr/share/fzf/key-bindings.bash

 export FZF_DEFAULT_OPTS="--color=bg+:#303030,fg+:#f9da9d,pointer:#f4a912"
