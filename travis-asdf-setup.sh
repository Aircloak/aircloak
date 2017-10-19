#!/bin/bash

set -eo pipefail

echo "Using asdf to manage language versions"

function install_if_missing() {
  plugin=$1
  installed_plugins=`asdf plugin-list`
  echo
  echo "Checking for asdf $plugin plugin:"
  if [[ $installed_plugins == *"$plugin"* ]]
  then
    echo "... already installed"
  else
    echo "... missing."
    asdf plugin-add $plugin https://github.com/asdf-vm/asdf-$plugin.git
    echo "... $plugin plugin installed"
  fi
}

if [ -f "$HOME/.asdf/asdf.sh" ]; then
  echo "asdf is already installed"
else
  echo "Installing asdf"
  rm -rf $HOME/.asdf
  git clone https://github.com/asdf-vm/asdf.git $HOME/.asdf --branch v0.3.0
fi

source $HOME/.asdf/asdf.sh

echo "Verifying existence of required asdf-plugins"
install_if_missing "erlang"
install_if_missing "elixir"
install_if_missing "nodejs"

asdf update
asdf plugin-update --all

# Imports Node.js release team's OpenPGP keys to main keyring
bash ~/.asdf/plugins/nodejs/bin/import-release-team-keyring ||
  bash ~/.asdf/plugins/nodejs/bin/import-release-team-keyring ||
  bash ~/.asdf/plugins/nodejs/bin/import-release-team-keyring ||
  bash ~/.asdf/plugins/nodejs/bin/import-release-team-keyring ||
  bash ~/.asdf/plugins/nodejs/bin/import-release-team-keyring

echo "Installing language environments with asdf"
asdf install

# install Elixi/Erlang builders
source $HOME/.asdf/asdf.sh
mkdir -p $MIX_HOME
mix local.hex --force
mix local.rebar --force
