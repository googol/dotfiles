#!/usr/bin/env bash

# Usage: use go [version]
#
# Loads the specified go version into the environment.
# If no version is given, it is read from the `go` directive in go.mod.
#
# If a partial go version is passed (i.e. `1.22`), a fuzzy match
# is performed and the highest matching version installed is selected.
#
# Environment Variables:
#
# - $GO_VERSIONS (required)
#   Points to a folder that contains all the installed go versions. That
#   folder must exist. It must contain subdirectories named after the go
#   version, each with a bin/ directory containing the go executable.

use_go() {
  local version=${1:-}
  local search_version

  if [[ -z ${GO_VERSIONS:-} || ! -d $GO_VERSIONS ]]; then
    log_error "You must specify a \$GO_VERSIONS environment variable and the directory specified must exist!"
    return 1
  fi

  if [[ -z $version ]]; then
    if [[ -f "go.mod" ]]; then
      version="$(grep '^go ' go.mod | awk '{print $2}')"
      watch_file "go.mod"
    else
      log_error "I do not know which Go version to load because one has not been specified!"
      return 1
    fi
  fi

  # Search for the highest version matching $version in the folder
  search_version=$(semver_search "$GO_VERSIONS" "" "${version}")

  if [[ -z "${search_version}" ]]; then
    log_status "Unable to find Go version ($version) in ($GO_VERSIONS), trying to download it."

    if ! direnv-download-bin go "$version"; then
      log_error "Unable to download Go version $version."
      return 1
    fi

    search_version="${version}"
  fi

  go_prefix="${GO_VERSIONS}/${search_version}"

  log_status "version: ${version}"
  log_status "Search version: ${search_version}"
  log_status "go_prefix: ${go_prefix}"

  if [[ ! -d $go_prefix ]]; then
    log_error "Unable to find Go version ($version) in ($GO_VERSIONS)!"
    return 1
  fi

  if [[ ! -x $go_prefix/bin/go ]]; then
    log_error "Unable to load Go binary (go) for version ($version) in ($GO_VERSIONS)!"
    return 1
  fi

  PATH_add "$go_prefix/bin"

  log_status "Successfully loaded Go $(go version), from prefix ($go_prefix)"
}
