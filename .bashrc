# export GPG_TTY=${tty}
# 
# gpg-connect-agent updatestartuptty /bye > /dev/null 2>&1

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

if which direnv &> /dev/null; then
    eval "$(direnv hook bash)"
else
    echo "Direnv not installed!"
fi

if which rustup &> /dev/null; then
    eval "$(rustup completions bash)"
fi
