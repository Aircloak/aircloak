FROM osixia/openldap
COPY air/ldap/bootstrap.ldif /container/service/slapd/assets/config/bootstrap/ldif/custom/50-bootstrap.ldif
COPY air/priv/config/ca.crt /container/service/slapd/assets/certs/ca.crt
COPY air/priv/config/localhost.crt /container/service/slapd/assets/certs/localhost.crt
COPY air/priv/config/ldap.aircloak.crt /container/service/slapd/assets/certs/ldap.aircloak.crt
COPY air/priv/config/localhost.key /container/service/slapd/assets/certs/localhost.key
