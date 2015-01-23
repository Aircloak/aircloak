# Errors

## Web API

The Aircloak REST API uses the following error codes:

Error Code | Meaning
---------- | -------
401        | Unauthorized -- Your API key is wrong
404        | Not Found -- Invalid URL
422        | Unprocessable entity - the request could not be processed. The response body contains a detailed error description.
500        | Internal Server Error -- We had a problem with our server. Try again later.
503        | Service Unavailable -- We're temporarially offline for maintanance. Please try again later.


## Cloak API

The Cloak API uses the following error codes:

Error Code | Meaning
---------- | -------
401        | Unauthorized -- Your API key is wrong
404        | Not Found -- Invalid URL
413        | The payload sent by the client was too large
415        | Unsupported content encoding
422        | Unprocessable entity - the request could not be processed. The response body contains a detailed error description.
500        | Internal Server Error -- We had a problem with our server. Try again later.
