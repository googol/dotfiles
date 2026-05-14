#!/usr/bin/env bash

use_nomad_token() {
    local -r nomad_address="${NOMAD_ADDR:-http://127.0.0.1:4646}"
    local -r sanitized_nomad_address="${nomad_address//[\/:]/_}"
    local -r nomad_token_dir="${XDG_CACHE_HOME:-"$HOME/.cache"}/nomad-token"
    local -r nomad_token_file="${nomad_token_dir}/${sanitized_nomad_address}"

    watch_file "$nomad_token_file"

    if [[ -f "$nomad_token_file" ]]; then
        export NOMAD_TOKEN="$(cat $nomad_token_file)"
    else
        log_error "Couldn't find a token file at $nomad_token_file. Try logging in with nomad-login"
    fi
}
