function tcp_ports {
  environment_offset=$(
        cat "$(dirname ${BASH_SOURCE[0]})/tcp_ports.json" |
            egrep -v '//' |
            jq --raw-output '.environment_offsets.'$1
      )

  if [ "$environment_offset" = "null" ]; then
    echo "Invalid env provided: $1" >&2
    exit 1
  fi

  service_offsets=$(
        cat "$(dirname ${BASH_SOURCE[0]})/tcp_ports.json" |
            egrep -v '//' |
            jq --raw-output '.service_offsets | to_entries | map("\(.key) \(.value)")|.[]'
      )

  while read line; do
    IFS=" " read service_name service_offset < <(echo "$line")
    echo "$service_name $(($environment_offset + $service_offset))"
  done < <(echo "$service_offsets")
}

function get_tcp_port {
  tcp_ports $1 | egrep "^$2 " | awk '{print $2}'
}
