# suitable for use in .bashrc, etc

# user@host:working-dir$
PS1="\[\e[1;34m\]\u@\h:\[\e[0m\]\w\$ "
# or with git integration:
GIT_BRANCH='$(__git_ps1 "|\[\e[0;32m\]%s\[\e[m\]")'
PS1="\[\e[1;34m\]\u@\h:\[\e[m\]\w"$GIT_BRANCH"\$ "


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

# touch the restart.txt file for a rails project from anywhere in the
# project tree.
repow() {
  if [ -d ./tmp ]; then
    touch ./tmp/restart.txt
    echo "Touched restart file for '$(basename $(PWD))'"
    return 0
  else
    MATCHING=$(echo $PWD | grep -oE 'Projects/[^/]+/')
    RC=$?

    if [ $RC -eq 0 ]; then
      touch "$HOME/${MATCHING}tmp/restart.txt"
      echo "Touched restart file for $(basename $MATCHING)."
      return 0
    fi

    echo "No tmp folder anywhere to be found.  Sorry."
    return 1
  fi
}

# after every 'cd', perform 'ls' if cd succeeded
cdls() {
  builtin cd "$*" && ls
}
alias cd="cdls"
alias brake="bundle exec rake"
alias be="bundle exec"
bspec() {
  if [ $# -eq 0 ]; then
    bundle exec rake spec
  elif [ "$1" == "here" ]; then
    bundle exec rake spec "SPEC=$(pwd)"
  else
    bundle exec rake spec "SPEC=$(pwd)/$(basename $1)"
  fi
}

alias uncolor="perl -pe 's/\e\[?.*?[\@-~]//g'"

# editor settings
export EDITOR="emacs -nw"
export VISUAL="emacs"

# if emacs is running in daemon mode
alias ec="emacsclient -c"
alias et="emacsclient -t"

if [[ "$OSTYPE" =~ ^darwin ]]; then
  alias ldd='otool -L'
fi
