# Authentication

All APIs, whether on the cloak or in the Aircloak web interface, are authenticated using client certificates.

```ruby
require 'net/http'
require 'uri'
require 'openssl'
require 'json'

class RestClient
  def self.key_from_file path, password
    OpenSSL::PKCS12.new File.read(path), password
  end

  def self.get path, api_key
    uri = URI.parse path
    http = Net::HTTP.new uri.host, uri.port
    http.read_timeout = 300
    http.use_ssl = true
    http.key = api_key.key
    http.cert = api_key.certificate
    http.verify_mode = OpenSSL::SSL::VERIFY_NONE
    response = http.get uri.request_uri
    JSON.parse response.body
  end

  def self.post path, payload, api_key, headers = {}
    uri = URI path
    https = Net::HTTP.new uri.host, uri.port
    https.use_ssl = true
    https.key = api_key.key
    https.cert = api_key.certificate
    https.verify_mode = OpenSSL::SSL::VERIFY_NONE
    request = Net::HTTP::Post.new uri.path
    headers.each_pair do |header_name, value|
      request.add_field header_name.to_s, value
    end
    response = https.request request, payload
    JSON.parse response.body
  end
end
```

```shell
wget --content-on-error \
     --output-document - \
     --method=<GET|POST|PUT|DELETE> \
     --certificate=<path-to-PEM-certificate> %> \
     --body-file=<file-to-upload-if-post-or-put> \
     https://<cloak-server>.cloak.aircloak.com/bulk_insert
```

To manage your keys, please visit the [keys](/keys) section in our web interface.

There are four types of key supported by our system. They differ in the scope of permissions they allow. The
keys are:

- data upload keys
- admin key
- key for running tasks
- key for issuing REST API calls against the web

With the exception of the _key for issuing REST API calls against the web_, all the keys above are used
against our cloaks.


### Data upload keys

The data upload key comes in two varieties. One which allows bulk uploading of data for __any user__, and one which only allows data upload for a __particular, specified user__.

The key which can be used for __any user__ is best suited for environments where the key can be strongly protected. Such environments include your own secure server systems.

Should this key get exposed, the adversary could irreversibly corrupt your database by uploading significant
amounts of bogus information. As you never see raw data once it has been uploaded, the extent to which your
database has been corrupted can be hard to quantify, and counteract.

Therefore, if there is a chance your key might have been exposed, please immediately revoke it in the
[keys](/keys) section of our web interface.

The key for uploading data for a __particular, specified user__, only allows data upload for the user that was
specified when the key was generated.
These keys are well suited for deployments where you are uploading data from deployed clients, where you have
less control over the key material.

These keys are generated through an authenticated API. Please contact us on
[support@aircloak.com](mailto:support@aircloak.com) should you be interested.


