FROM aircloak/elixir:$ELIXIR_VERSION
MAINTAINER Aircloak

MPI_INIT

# Installs the node version specified in the .tool-versions in addition to
# yarn as a replacement for npm. The brunch node package needed for building
# our app requires python to be installed to build cleanly, hence the python
# dependency too.
RUN \
  . /tmp/build_config/proxies.sh && \
  echo 'nodejs $NODEJS_VERSION' >> ~/.tool-versions && \
  bash -c ". ~/.asdf/asdf.sh && asdf plugin-add nodejs https://github.com/asdf-vm/asdf-nodejs.git" && \
  bash -c ". ~/.asdf/asdf.sh && cd ~ && asdf install" && \
  apt-key adv --fetch-keys http://dl.yarnpkg.com/debian/pubkey.gpg && \
  echo "deb http://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list && \
  apt-get update && \
  apt-get install python yarn -y && \
  bash -c ". ~/.bashrc && yarn global add brunch"
