#!/usr/bin/env bash

shopt -pq inherit_errexit nullglob
set -euo pipefail

set +e
fwupdmgr get-updates &> /dev/null
FW_UPDATE_STATUS="$?"
set -e

case "$FW_UPDATE_STATUS" in
    0)
        echo "Updates available" >&2
        touch "${XDG_RUNTIME_DIR}/firmware_updates_available"
        ;;
    2)
        echo "No updates available" >&2
        rm -f "${XDG_RUNTIME_DIR}/firmware_updates_available"
        ;;
    *)
        echo "Unexpected result" >&2
        ;;
esac
