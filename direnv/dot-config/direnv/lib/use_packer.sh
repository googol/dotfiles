#!/usr/bin/env bash

# Usage: use packer <version>
#
# Loads the specified packer version into the environment.
#
# If a partial packer version is passed (i.e. `4.2`), a fuzzy match
# is performed and the highest matching version installed is selected.
#
# Environment Variables:
#
# - $PACKER_VERSIONS (required)
#   Points to a folder that contains all the installed packer versions. That
#   folder must exist. It must contain subdirectories named after the packer
#   version, and contain the packer executable for that version.

use_packer() {
  local version=${1:-}
  local search_version

  if [[ -z ${PACKER_VERSIONS:-} || ! -d $PACKER_VERSIONS ]]; then
    log_error "You must specify a \$PACKER_VERSIONS environment variable and the directory specified must exist!"
    return 1
  fi

  if [[ -z $version ]]; then
    if [[ -f ".packer-version" ]]; then
        version="$(cat ".packer-version")"
        watch_file ".packer-version"
    else 
        log_error "I do not know which Packer version to load because one has not been specified!"
        return 1
    fi
  fi

  # Search for the highest version matchin $version in the folder
  search_version=$(semver_search "$PACKER_VERSIONS" "" "${version}")

  if [[ -z "${search_version}" ]]; then
    log_status "Unable to find Packer version ($version) in ($PACKER_VERSIONS), trying to download it."

    if ! direnv-download-bin packer "$version"; then
        log_error "Unable to download Packer version $version."
        return 1
    fi

    search_version="${version}"
  fi

  packer_prefix="${PACKER_VERSIONS}/${search_version}"

  log_status "version: ${version}"
  log_status "Search version: ${search_version}"
  log_status "packer_prefix: ${packer_prefix}"

  if [[ ! -d $packer_prefix ]]; then
    log_status "Unable to find Packer version ($version) in ($PACKER_VERSIONS), trying to download it."

    if ! direnv-download-bin packer "$version"; then
        log_error "Unable to download Packer version $version."
        return 1
    fi
  fi

  if [[ ! -x $packer_prefix/packer ]]; then
    log_error "Unable to load Packer binary (packer) for version ($version) in ($PACKER_VERSIONS)!"
    return 1
  fi

  PATH_add "$packer_prefix"

  log_status "Successfully loaded Packer $(packer --version), from prefix ($packer_prefix)"
}

