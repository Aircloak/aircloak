FROM aircloak/phoenix:$NODEJS_VERSION
MAINTAINER Aircloak

# Install dependencies:
# - jq: needed to analyze package.json
RUN apt-get install jq -y
