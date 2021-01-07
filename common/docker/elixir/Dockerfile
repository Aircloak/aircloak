FROM aircloak/erlang:$ERLANG_VERSION

RUN apt-get install -y locales unzip

RUN \
  . /tmp/build_config/proxies.sh && \
  echo 'elixir $ELIXIR_VERSION' >> ~/.tool-versions && \
  bash -c ". ~/.asdf/asdf.sh && asdf plugin-add elixir https://github.com/asdf-vm/asdf-elixir.git" && \
  bash -c ". ~/.asdf/asdf.sh && cd ~ && asdf install" && \
  bash -c ". ~/.asdf/asdf.sh && mix local.hex --force && mix local.rebar --force"
