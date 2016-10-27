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
  apt-get install python -y && \
  curl -o- -L https://yarnpkg.com/install.sh | bash && \
  bash -c ". ~/.bashrc && yarn global add brunch"
