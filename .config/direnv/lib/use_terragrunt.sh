#!/usr/bin/env bash

# Usage: use terragrunt <version>
#
# Loads the specified terragrunt version into the environment.
#
# If a partial terragrunt version is passed (i.e. `4.2`), a fuzzy match
# is performed and the highest matching version installed is selected.
#
# Environment Variables:
#
# - $TERRAGRUNT_VERSIONS (required)
#   Points to a folder that contains all the installed terragrunt versions. That
#   folder must exist. It must contain subdirectories named after the terragrunt
#   version, and contain the terragrunt executable for that version.
#
use_terragrunt() {
  local version=${1:-}
  local search_version

  if [[ -z ${TERRAGRUNT_VERSIONS:-} || ! -d $TERRAGRUNT_VERSIONS ]]; then
    log_error "You must specify a \$TERRAGRUNT_VERSIONS environment variable and the directory specified must exist!"
    return 1
  fi

  if [[ -z $version ]]; then
    log_error "I do not know which Terragrunt version to load because one has not been specified!"
    return 1
  fi

  # Search for the highest version matchin $version in the folder
  search_version=$(semver_search "$TERRAGRUNT_VERSIONS" "" "${version}")
  terragrunt_prefix="${TERRAGRUNT_VERSIONS}/${search_version}"

  if [[ ! -d $terragrunt_prefix ]]; then
    log_error "Unable to find Terragrunt version ($version) in ($TERRAGRUNT_VERSIONS)!"
    return 1
  fi

  if [[ ! -x $terragrunt_prefix/terragrunt ]]; then
    log_error "Unable to load Terragrunt binary (terragrunt) for version ($version) in ($TERRAGRUNT_VERSIONS)!"
    return 1
  fi

  PATH_add "$terragrunt_prefix"

  log_status "Successfully loaded Terragrunt $(terragrunt --version), from prefix ($terragrunt_prefix)"
}
