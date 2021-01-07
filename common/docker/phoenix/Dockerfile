FROM aircloak/elixir:$ELIXIR_VERSION

# Installs the node version specified in the .tool-versions in addition to
# yarn as a replacement for npm.
# The brunch node package is needed for building central, whereas
# the webpack node package is needed for building air.
# our app requires python to be installed to build cleanly, hence the python
# dependency too.
# For the nodejs installation to work, we also need to bootstrap the trust
# for signature validation of nodejs. For more information, see the
# asdf-vm readme at: https://github.com/asdf-vm/asdf-nodejs.
# The release team signatures are taken from: https://github.com/nodejs/node/#release-team
RUN \
  . /tmp/build_config/proxies.sh && \
  echo 'nodejs $NODEJS_VERSION' >> ~/.tool-versions && \
  bash -c ". ~/.asdf/asdf.sh && asdf plugin-add nodejs https://github.com/asdf-vm/asdf-nodejs.git" && \
  bash -c "~/.asdf/plugins/nodejs/bin/import-release-team-keyring"


RUN \
  bash -c ". ~/.asdf/asdf.sh && cd ~ && asdf install" && \
  apt-key adv --fetch-keys http://dl.yarnpkg.com/debian/pubkey.gpg && \
  echo "deb http://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list && \
  apt-get update && \
  apt-get install python yarn -y

CONFIG_YARN_PROXY

RUN bash -c ". ~/.bashrc && yarn global add brunch"
