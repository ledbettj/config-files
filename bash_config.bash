UNAME=$(uname -s)
if [[ "$UNAME" -eq "Darwin" ]]; then
  FZF_DIR=/opt/homebrew/opt/fzf/shell
  NVM_DIR=/opt/homebrew/opt/nvm
  NVM_SH=nvm.sh
else
  FZF_DIR=/usr/share/fzf
  NVM_DIR=/usr/share/nvm
  NVM_SH=init-nvm.sh
fi


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
  alias ek="emacsclient -e '(kill-emacs)'"
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
[[ -r $FZF_DIR/completion.bash ]] && . $FZF_DIR/completion.bash
[[ -r $FZF_DIR/key-bindings.bash ]] && . $FZF_DIR/key-bindings.bash

export FZF_DEFAULT_OPTS="--color=bg+:#303030,fg+:#f9da9d,pointer:#f4a912"
export PATH="$PATH:$HOME/.config/emacs/bin"
export PATH="$PATH:$HOME/.local/bin"
export GPG_TTY=$(tty)

[[ -r "$NVM_DIR/$NVM_SH" ]] && source "$NVM_DIR/$NVM_SH"

dpi() {
    docker run --rm -it \
        -v "$PWD:/workspace" \
        -v pi-agent-home:/root/.pi/agent pi-sandbox "$@"
}


dclod() {
    docker run --rm -it \
        -v "$PWD:/workspace" \
        -v claude-agent-home:/root/.claude \
        claude-sandbox "$@"
}
