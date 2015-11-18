# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="vardyh"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable bi-weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment to change how many often would you like to wait before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git)

source $ZSH/oh-my-zsh.sh

unset GREP_OPTIONS

export LC_ALL=en_US.UTF-8
export LC_CTYPE=zh_CN.GB18030

export MINGW32_PATH=/usr/local/opt/mingw32/bin
export MINGW64_PATH=/usr/local/opt/mingw64/bin

export PATH=/usr/local/bin::/usr/local/sbin:$MINGW32_PATH:$MINGW64_PATH:$PATH:~/devel/scripts

alias less='less -r'
alias grep='grep --color=always'
alias wine='LC_ALL=zh_CN.UTF-8 wine'

export DBDIR=/usr/local/share/xsse/db
export DBDEV=~/devel/works/virdb
