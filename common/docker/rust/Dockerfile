FROM aircloak/elixir:$ELIXIR_VERSION

RUN \
  . /tmp/build_config/proxies.sh && \
  echo 'rust $RUST_VERSION' >> ~/.tool-versions && \
  bash -c ". ~/.asdf/asdf.sh && asdf plugin-add rust https://github.com/code-lever/asdf-rust.git" && \
  bash -c ". ~/.asdf/asdf.sh && cd ~ && asdf install"
