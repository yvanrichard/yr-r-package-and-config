# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
# don't overwrite GNU Midnight Commander's setting of `ignorespace'.
HISTCONTROL=$HISTCONTROL${HISTCONTROL+,}ignoredups
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)} \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls -l --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# Custom aliases
alias emake='env $(find . -maxdepth 1 -name "*.env" -exec sed -e "/^#/d" -e "s/[[:space:]]\+=[[:space:]]\+/=/g" {} \;) make'
alias eremake='env $(find . -maxdepth 1 -name "*.env" -exec sed -e "/^#/d" -e "s/[[:space:]]\+=[[:space:]]\+/=/g" {} \;) remake'
alias e='env $(find . -maxdepth 1 -name "*.env" -exec sed -e "/^#/d" -e "s/[[:space:]]\+=[[:space:]]\+/=/g" {} \;)'
alias memacs='emacs -q -l ~/.emacs_mini &'

alias rgm='Rscript -e "graph_makefile()"'

# alias E="SUDO_EDITOR=\"emacsclient -a emacs\" sudoedit"

# some more ls aliases
#alias ll='ls -l'
#alias la='ls -A'
#alias l='ls -CF'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

dflymnt () { mkdir -p /home/yvan/dragonfly/$1; sshfs yvan@robin:/home/yvan/dragonfly/$1 /home/yvan/dragonfly/$1 ;}

# added lines for ipython (and psql to avoid -S)
export LESS="-RS"
# export EDITOR=emacsclient
# export VISUAL=emacsclient

# define color to additional file types
export LS_COLORS=$LS_COLORS:"di=1;36;40":"*.r=00;93":"ln=1;35":"*.mk=5;34":"*tex=0;93":"*.pdf=00;32":"*makefile=00;91":"*~=0;90":"*csv=0;94":"*rdata=0;94":"*.xls=00;32":"*dbf=0;94":"*.rnw=0;93":"*.py=0;93":"*.png=00;32":"*.jpg=00;32":"*.mp4=00;32":"*.flv=00;32":"*.tif=00;32":"*.tiff=00;32"


# A git aware bash prompt for ubuntu, that shows what branch you are on, and whether you have anything to commit.
# Add to your .bashrc file.
# Works with git version 1.7.9.5.
# Based on http://www.intridea.com/blog/2009/2/2/git-status-in-your-prompt
function parse_git_dirty {
[[ $(git status 2> /dev/null | tail -n1) != "nothing to commit (working directory clean)" ]] && echo "*"
}
function parse_git_branch {
git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/[\1$(parse_git_dirty)]/"
}
# export PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\W\[\033[00m\]$(parse_git_branch)$ '

if [ $HOSTNAME = "robin" ] ; then 
    export PS1='${debian_chroot:+($debian_chroot)}[\t]\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]$(parse_git_branch)$ ';
else
    export PS1='${debian_chroot:+($debian_chroot)}[\t]\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]$ '
fi

# export PS1='${debian_chroot:+($debian_chroot)}[\t]\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]$(parse_git_branch)$ '

# # alias R='grep --color=auto'
# export WORKON_HOME=~/virtualenvs
# source /usr/local/bin/virtualenvwrapper.sh
# export PIP_VIRTUALENV_BASE=~/virtualenvs
export EDC_HOME=/home/yvan/EDC
