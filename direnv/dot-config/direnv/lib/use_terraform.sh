#!/usr/bin/env bash

# Usage: use terraform <version>
#
# Loads the specified terraform version into the environment.
#
# If a partial terraform version is passed (i.e. `4.2`), a fuzzy match
# is performed and the highest matching version installed is selected.
#
# Environment Variables:
#
# - $TERRAFORM_VERSIONS (required)
#   Points to a folder that contains all the installed terraform versions. That
#   folder must exist. It must contain subdirectories named after the terraform
#   version, and contain the terraform executable for that version.

use_terraform() {
  local version=${1:-}
  local search_version

  if [[ -z ${TERRAFORM_VERSIONS:-} || ! -d $TERRAFORM_VERSIONS ]]; then
    log_error "You must specify a \$TERRAFORM_VERSIONS environment variable and the directory specified must exist!"
    return 1
  fi

  if [[ -z $version ]]; then
    if [[ -f ".terraform-version" ]]; then
        version="$(cat ".terraform-version")"
        watch_file ".terraform-version"
    else 
        log_error "I do not know which Terraform version to load because one has not been specified!"
        return 1
    fi
  fi

  # Search for the highest version matchin $version in the folder
  search_version=$(semver_search "$TERRAFORM_VERSIONS" "" "${version}")

  if [[ -z "${search_version}" ]]; then
    log_status "Unable to find Terraform version ($version) in ($TERRAFORM_VERSIONS), trying to download it."

    if ! direnv-download-bin terraform "$version"; then
        log_error "Unable to download Terraform version $version."
        return 1
    fi

    search_version="${version}"
  fi

  terraform_prefix="${TERRAFORM_VERSIONS}/${search_version}"

  log_status "version: ${version}"
  log_status "Search version: ${search_version}"
  log_status "terraform_prefix: ${terraform_prefix}"

  if [[ ! -d $terraform_prefix ]]; then
    log_status "Unable to find Terraform version ($version) in ($TERRAFORM_VERSIONS), trying to download it."

    if ! direnv-download-bin terraform "$version"; then
        log_error "Unable to download Terraform version $version."
        return 1
    fi
  fi

  if [[ ! -x $terraform_prefix/terraform ]]; then
    log_error "Unable to load Terraform binary (terraform) for version ($version) in ($TERRAFORM_VERSIONS)!"
    return 1
  fi

  PATH_add "$terraform_prefix"

  log_status "Successfully loaded Terraform $(terraform --version), from prefix ($terraform_prefix)"
}

