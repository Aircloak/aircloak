FROM aircloak/phoenix:$NODEJS_VERSION
MAINTAINER Aircloak

# Install dependencies:
# - calibre: needed to compile offline versions of docs
# - jq: needed to analyze package.json
RUN apt-get install jq calibre -y
