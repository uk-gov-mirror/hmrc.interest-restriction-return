We use standard HTTP status codes to show whether an API request succeeded or not. They are usually in the range:

- 200 to 299 if it succeeded, including code 202 if it was accepted by an API that needs to wait for further action
- 400 to 499 if it failed because of a client error by your application
- 500 to 599 if it failed because of an error on our server

Errors specific to each API are shown in the Endpoints section, under Response. See our [reference guide](https://developer.service.hmrc.gov.uk/api-documentation/docs/reference-guide#errors) for more on errors.

Validation errors will be returned with `INVALID_REQUEST` in the following format:<br>
```
[
    {
        "field": "/accountingPeriod/endDate",
        "value": "2019-12-01",
        "errors": [
            "End Date must be after start date"
        ]
    }
]
```
