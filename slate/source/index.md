---
title: Aircloak REST API Reference

language_tabs:
  - ruby

toc_footers:
  - <a href='#'>Sign Up for an API Key</a>

includes:
  - errors

search: true
---

# Introduction

This is the guide for the Aircloak cloak and web REST APIs. You can use these APIs to perform actions such as executing tasks against a cloak cluster, or access results of previous task executions.

All API access is authenticated. Please have a look at the authentication section for details.

The examples in this guide are given in ruby. If it is not clear how this applies to your own environment,
please reach out to us on [solutions@aircloak.com](mailto:solutions@aircloak.com), and we will be happy to assist.


# Authentication

All APIs, whether on the cloak or in the Aircloak web interface, are authenticated using client certificates.

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

The key which can be used for __any user__ is best suited for environments where the key can be strongly protect. Such environments include your own secure server systems.

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
[solutions@aircloak.com](mailto:solutions@aircloak.com) should you be interested.

# Web REST API

## Task results

### Get results for a specific task

```ruby
require 'net/http'
require 'uri'
require 'openssl'
require 'json'

def http_get(path, api_key)
  uri = URI.parse(path)
  http = Net::HTTP.new(uri.host, uri.port)
  http.read_timeout = 300
  http.use_ssl = true
  http.key = api_key.key
  http.cert = api_key.certificate
  http.verify_mode = OpenSSL::SSL::VERIFY_NONE
  http.get(uri.request_uri)
end

key_file_name = "my_api_key.pfx"
key_password = "my_password"
api_key = OpenSSL::PKCS12.new(File.read(key_file_name), key_password)
response = http_get("https://hello.aircloak.com/api/task_results/1", api_key)
```

> The above command returns JSON structured like this:

```json
{
  "success":true,
  "count":30,
  "page":1,
  "per_page":10,
  "items":[
    {
      "published_at":1421660484000,
      "buckets":[
        {"name":"height: 140","value":20},
        {"name":"height: 160","value":35}
      ],
      "exceptions":[]
    },
    {
      "published_at":1421660484000,
      "buckets":[
        {"name":"height: 180","value":25},
        {"name":"height: 200","value":25}
      ],
      "exceptions":[]
    }
  ]
}
```

This endpoint retrieves results for the given task. Retrieved results are ordered (newest come first) and paginated.

#### HTTP Request

`GET /api/task_results/task_id`

<aside class="notice">
In the path, you must replace `task_id` with the real id of the task.
</aside>

#### Query Parameters

Parameter | Default | Description
--------- | ------- | -----------
page      | 1       | Page number
per_page  | 10      | The number of items per page


# Cloak APIs
