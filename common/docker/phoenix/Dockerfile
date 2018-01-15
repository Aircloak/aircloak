FROM aircloak/elixir:$ELIXIR_VERSION
MAINTAINER Aircloak

MPI_INIT

# Installs the node version specified in the .tool-versions in addition to
# yarn as a replacement for npm. The brunch node package needed for building
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
  bash -c "gpg --keyserver pool.sks-keyservers.net --recv-keys 94AE36675C464D64BAFA68DD7434390BDBE9B9C5" && \
  bash -c "gpg --keyserver pool.sks-keyservers.net --recv-keys FD3A5288F042B6850C66B31F09FE44734EB7990E" && \
  bash -c "gpg --keyserver pool.sks-keyservers.net --recv-keys 71DCFD284A79C3B38668286BC97EC7A07EDE3FC1" && \
  bash -c "gpg --keyserver pool.sks-keyservers.net --recv-keys DD8F2338BAE7501E3DD5AC78C273792F7D83545D" && \
  bash -c "gpg --keyserver pool.sks-keyservers.net --recv-keys C4F0DFFF4E8C1A8236409D08E73BC641CC11F4C8" && \
  bash -c "gpg --keyserver pool.sks-keyservers.net --recv-keys B9AE9905FFD7803F25714661B63B535A4C206CA9" && \
  bash -c "gpg --keyserver pool.sks-keyservers.net --recv-keys 56730D5401028683275BD23C23EFEFE93C4CFFFE"


RUN \
  bash -c ". ~/.asdf/asdf.sh && cd ~ && asdf install" && \
  apt-key adv --fetch-keys http://dl.yarnpkg.com/debian/pubkey.gpg && \
  echo "deb http://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list && \
  apt-get update && \
  apt-get install python yarn -y

CONFIG_YARN_PROXY

RUN bash -c ". ~/.bashrc && yarn global add brunch"
