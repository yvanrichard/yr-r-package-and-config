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

# if [ "$color_prompt" = yes ]; then
#     PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
# else
#     PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
# fi
# unset color_prompt force_color_prompt

# # If this is an xterm set the title to user@host:dir
# case "$TERM" in
# xterm*|rxvt*)
#     PS1="\[\e]0;${debian_chroot:+($debian_chroot)} \w\a\]$PS1"
#     ;;
# *)
#     ;;
# esac

## ------------------------------------------------------------------------------------------------
## After using https://ezprompt.net/:

# get current branch in git repo
function parse_git_branch() {
	BRANCH=`git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/'`
	if [ ! "${BRANCH}" == "" ]
	then
		# STAT=`parse_git_dirty`
		# echo "[${BRANCH}${STAT}]"
		echo "(${BRANCH})"
	else
		echo ""
	fi
}

# # get current status of git repo
# function parse_git_dirty {
# 	status=`git status 2>&1 | tee`
# 	dirty=`echo -n "${status}" 2> /dev/null | grep "modified:" &> /dev/null; echo "$?"`
# 	untracked=`echo -n "${status}" 2> /dev/null | grep "Untracked files" &> /dev/null; echo "$?"`
# 	ahead=`echo -n "${status}" 2> /dev/null | grep "Your branch is ahead of" &> /dev/null; echo "$?"`
# 	newfile=`echo -n "${status}" 2> /dev/null | grep "new file:" &> /dev/null; echo "$?"`
# 	renamed=`echo -n "${status}" 2> /dev/null | grep "renamed:" &> /dev/null; echo "$?"`
# 	deleted=`echo -n "${status}" 2> /dev/null | grep "deleted:" &> /dev/null; echo "$?"`
# 	bits=''
# 	if [ "${renamed}" == "0" ]; then
# 		bits=">${bits}"
# 	fi
# 	if [ "${ahead}" == "0" ]; then
# 		bits="*${bits}"
# 	fi
# 	if [ "${newfile}" == "0" ]; then
# 		bits="+${bits}"
# 	fi
# 	if [ "${untracked}" == "0" ]; then
# 		bits="?${bits}"
# 	fi
# 	if [ "${deleted}" == "0" ]; then
# 		bits="x${bits}"
# 	fi
# 	if [ "${dirty}" == "0" ]; then
# 		bits="!${bits}"
# 	fi
# 	if [ ! "${bits}" == "" ]; then
# 		echo " ${bits}"
# 	else
# 		echo ""
# 	fi
# }


## ------------------------------------------------------------------------------------------------


# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    # alias ls='ls -l --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

alias ls='exa -l --git --group --header'
alias tree='exa --tree --long --git --group --header'

# Custom aliases
alias emake='env $(find . -maxdepth 1 -name "*.env" -exec sed -e "/^#/d" -e "s/[[:space:]]\+=[[:space:]]\+/=/g" {} \;) make'
alias eremake='env $(find . -maxdepth 1 -name "*.env" -exec sed -e "/^#/d" -e "s/[[:space:]]\+=[[:space:]]\+/=/g" {} \;) remake'
alias e='env $(find . -maxdepth 1 -name "*.env" -exec sed -e "/^#/d" -e "s/[[:space:]]\+=[[:space:]]\+/=/g" {} \;)'
alias memacs='emacs -q -l ~/.emacs_mini &'

alias makereportdfly="rsync -avz --exclude generated --exclude '*~' /home/yvan/Dropbox/templates/report-dfly/ ."
alias makereporthtml="rsync -avz --exclude generated --exclude '*~' /home/yvan/Dropbox/templates/report-html/ ."
alias makereportaebr="rsync -avz --exclude generated --exclude '*~' /home/yvan/Dropbox/templates/report-aebr/ ."
alias makeshiny="rsync -avz --exclude '*~' /home/yvan/Dropbox/templates/shiny-app/ ."
alias makedocker="rsync -avz --exclude '*~' /home/yvan/Dropbox/templates/docker/ ."
alias makequartopres="rsync -avz --exclude '*~' /home/yvan/Dropbox/templates/quarto-presentation/ ."
alias makequartohtml="rsync -avz --exclude '*~' /home/yvan/Dropbox/templates/quarto-html/ ."

alias rgm='Rscript -e "graph_makefile()" &'
alias tmux='tmux -2 '
# alias emacs='GDK_NATIVE_WINDOWS=1 emacs '
alias upg='sudo apt update && sudo apt upgrade'

# alias E="SUDO_EDITOR=\"emacsclient -a emacs\" sudoedit"
shrinkpdf () { gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dPDFSETTINGS=/ebook -dNOPAUSE -dQUIET -dBATCH -sOutputFile=$1-smaller.pdf $1.pdf ;}

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

# dflymnt () { mkdir -p /home/yvan/dragonfly/$1; sshfs yvan@robin:/home/yvan/dragonfly/$1 /home/yvan/dragonfly/$1 ; cd /home/yvan/dragonfly/$1 ;}

# added lines for ipython (and psql to avoid -S)
export LESS="-RS"
# export EDITOR=emacsclient
# export VISUAL=emacsclient

# define color to additional file types
export LS_COLORS=$LS_COLORS:"di=1;36;40":"*.r=00;93":"ln=1;35":"*.mk=5;34":"*tex=0;93":"*.pdf=00;32":"*makefile=00;91":"*~=0;90":"*csv=0;94":"*rdata=0;94":"*.xls=00;32":"*dbf=0;94":"*.rnw=0;93":"*.py=0;93":"*.png=00;32":"*.jpg=00;32":"*.mp4=00;32":"*.flv=00;32":"*.tif=00;32":"*.tiff=00;32"


# A git aware bash prompt for ubuntu, that shows what branch you are on, and whether you have anything to commit.
# Add to your .bashrc file.
# Works with git version 1.7.9.5.

# # Based on http://www.intridea.com/blog/2009/2/2/git-status-in-your-prompt
# function parse_git_dirty {
# [[ $(git status 2> /dev/null | tail -n1) != "nothing to commit (working directory clean)" ]] && echo "*"
# }
# function parse_git_branch {
# git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/[\1$(parse_git_dirty)]/"
# }
# # export PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\W\[\033[00m\]$(parse_git_branch)$ '

# if [ $HOSTNAME = "huia" ] ; then 
#     export PS1='${debian_chroot:+($debian_chroot)}[\t]\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]$(parse_git_branch)$ ';
# else
#     export PS1='${debian_chroot:+($debian_chroot)}[\t]\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]$ '
# fi

# export PS1='${debian_chroot:+($debian_chroot)}[\t]\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]$(parse_git_branch)$ '

# # alias R='grep --color=auto'
# export WORKON_HOME=~/virtualenvs
# source /usr/local/bin/virtualenvwrapper.sh
# export PIP_VIRTUALENV_BASE=~/virtualenvs
export EDC_HOME=/home/yvan/EDC


# BASE16_SHELL=$HOME/.config/base16-shell/
# [ -n "$PS1" ] && [ -s $BASE16_SHELL/profile_helper.sh ] && eval "$($BASE16_SHELL/profile_helper.sh)"

if [[ $TERMINIX_ID ]]; then
        source /etc/profile.d/vte.sh
fi
# if [ $TILIX_ID ] || [ $VTE_VERSION ]; then
#         source /etc/profile.d/vte.sh
# fi

export UBUNTU_MENU_PROXY=emacs

# export PATH="$PATH:$HOME/miniconda/bin"

# # >>> conda initialize >>>
# # !! Contents within this block are managed by 'conda init' !!
# __conda_setup="$('/home/yvan/miniconda/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
# if [ $? -eq 0 ]; then
#     eval "$__conda_setup"
# else
#     if [ -f "/home/yvan/miniconda/etc/profile.d/conda.sh" ]; then
#         . "/home/yvan/miniconda/etc/profile.d/conda.sh"
#     else
#         export PATH="/home/yvan/miniconda/bin:$PATH"
#     fi
# fi
# unset __conda_setup
# # <<< conda initialize <<<

export PATH="$PATH:$HOME/miniconda3/bin"
export PATH="$PATH:$HOME/.local/bin"

export EDITOR=/usr/bin/vim.basic

alias vi=vim

alias mymonit='inotifywait -m -e modify,create,delete --timefmt "%F %T" --format "%T,%w%f,%e" --exclude ".*(#|\.git|\.quarto|~ )" -r /home/yvan/dragonfly/ >> /home/yvan/Documents/dragonfly-activity.log'


export PS1="\[\e[0m\]\`parse_git_branch\`\[\033[01;30m\]|\[\033[01;34m\]\D{%Y-%m-%d} \[\033[01;34m\]\t\[\033[\033[01;30m\]|\[\033[\033[01;32m\]\u@\[\033[01;32m\]\h\[\033[01;30m\]|\[\033[01;33m\]\W\[\e[0m\]$ "

# vterm_printf(){
#     if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
#         # Tell tmux to pass the escape sequences through
#         printf "\ePtmux;\e\e]%s\007\e\\" "$1"
#     elif [ "${TERM%%-*}" = "screen" ]; then
#         # GNU screen (screen, screen-256color, screen-256color-bce)
#         printf "\eP\e]%s\007\e\\" "$1"
#     else
#         printf "\e]%s\e\\" "$1"
#     fi
# }
# vterm_prompt_end(){
#     vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
# }
# PS1=$PS1'\[$(vterm_prompt_end)\]'
# PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND; }"'echo -ne "\033]0;${HOSTNAME}:${PWD}\007"'


alias fd='fdfind'
alias bat='batcat'

eval "$(starship init bash)"

export PATH="$PATH:$HOME/.cargo/bin"

## * FZF (helm for shell)
source /usr/share/doc/fzf/examples/key-bindings.bash
source /usr/share/doc/fzf/examples/completion.bash

export FZF_DEFAULT_OPTS='--layout=reverse --border'

# Use ~~ as the trigger sequence instead of the default **
export FZF_COMPLETION_TRIGGER='++'

# # Options to fzf command
# export FZF_COMPLETION_OPTS='--border --info=inline'

# # Setting fd as the default source for fzf
# export FZF_DEFAULT_COMMAND='fd --type f'

_fzf_compgen_path() {
  fd --hidden --follow --exclude ".git" . "$1"
}

# Use fd to generate the list for directory completion
_fzf_compgen_dir() {
  fd --type d --hidden --follow --exclude ".git" . "$1"
}

## * Integration of ripgrep-all and FZF
rgafzf() {
	RG_PREFIX="rga --files-with-matches"
	local file
	file="$(
		FZF_DEFAULT_COMMAND="$RG_PREFIX '$1'" \
			fzf --sort --preview="[[ ! -z {} ]] && rga --pretty --context 5 {q} {}" \
				--phony -q "$1" \
				--bind "change:reload:$RG_PREFIX {q}" \
				--preview-window="70%:wrap"
	)" &&
	echo "opening $file" &&
	xdg-open "$file"
}
