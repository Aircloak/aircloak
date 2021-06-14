FROM aircloak/base:$DEBIAN_VERSION

RUN apt-get install -y libz-dev autoconf libncurses5-dev libncurses-dev build-essential libssl-dev unixodbc-dev xsltproc fop

RUN \
  . /tmp/build_config/proxies.sh && \
  git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.1.0 && \
  echo '. ~/.asdf/asdf.sh' >> ~/.bashrc && \
  echo 'erlang $ERLANG_VERSION' >> ~/.tool-versions && \
  bash -c ". ~/.asdf/asdf.sh && asdf plugin-add erlang https://github.com/asdf-vm/asdf-erlang.git" && \
  bash -c ". ~/.asdf/asdf.sh && cd ~ && asdf install"
