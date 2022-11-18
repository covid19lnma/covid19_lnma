#!/bin/bash

# Function taken from https://github.com/rocker-org/rocker-versioned2/blob/master/scripts/install_tidyverse.sh
## build ARGs
set -e

# a function to install apt packages only if they are not installed
function apt_install() {
    if ! dpkg -s "$@" >/dev/null 2>&1; then
        if [ "$(find /var/lib/apt/lists/* | wc -l)" = "0" ]; then
            apt-get update
        fi
        apt-get install -y --no-install-recommends "$@"
    fi
}

apt_install \
    cmake \
    jags \
    libtcl8.6 \
    tk \
    libglpk-dev

# R packages
r /rocker_scripts/install.R

echo -e "\nInstall setup.R packages, done!"