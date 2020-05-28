interface zif_lso_log_trace
  public .

  types tt_traces type standard table of zlso_log_trace with empty key .

  "! <p class="shorttext synchronized" lang="en">Get HTTP status</p>
  "!
  "! @parameter http_status | <p class="shorttext synchronized" lang="en"></p>
  methods get_http_status
    returning value(http_status) type i .

  "! <p class="shorttext synchronized" lang="en">Get id</p>
  "!
  "! @parameter id | <p class="shorttext synchronized" lang="en"></p>
  methods get_id
    returning value(id) type zlso_log_trace-id .

  "! <p class="shorttext synchronized" lang="en">Get request method</p>
  "!
  "! @parameter request_method | <p class="shorttext synchronized" lang="en"></p>
  methods get_request_method
    returning value(request_method) type zlso_log_trace-request_method .

  "! <p class="shorttext synchronized" lang="en">Get the date when log was stripped of payload/headers</p>
  "!
  "! @parameter stripped_date | <p class="shorttext synchronized" lang="en"></p>
  methods get_stripped_date
    returning value(stripped_date) type zlso_log_trace-stripped_date.

  "! <p class="shorttext synchronized" lang="en">Get request headers</p>
  "!
  "! @parameter headers | <p class="shorttext synchronized" lang="en"></p>
  methods get_request_headers
    returning value(headers) type ref to zcl_lso_log_headers .

  "! <p class="shorttext synchronized" lang="en">Get request payload</p>
  "!
  "! @parameter payload | <p class="shorttext synchronized" lang="en"></p>
  methods get_request_payload
    returning value(payload) type ref to zcl_lso_log_payload .

  "! <p class="shorttext synchronized" lang="en">Get request URL</p>
  "!
  "! @parameter request_url | <p class="shorttext synchronized" lang="en"></p>
  methods get_request_url
    returning value(request_url) type zlso_log_trace-request_url .

  "! <p class="shorttext synchronized" lang="en">Get response headers</p>
  "!
  "! @parameter headers | <p class="shorttext synchronized" lang="en"></p>
  methods get_response_headers
    returning value(headers) type ref to zcl_lso_log_headers .

  "! <p class="shorttext synchronized" lang="en">Get response payload</p>
  "!
  "! @parameter payload | <p class="shorttext synchronized" lang="en"></p>
  methods get_response_payload
    returning value(payload) type ref to zcl_lso_log_payload .

  "! <p class="shorttext synchronized" lang="en">Has headers?</p>
  "!
  "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
  methods has_headers
    returning value(result) type abap_bool .

  "! <p class="shorttext synchronized" lang="en">Has payload?</p>
  "!
  "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
  methods has_payload
    returning value(result) type abap_bool .

  "! <p class="shorttext synchronized" lang="en">Has request headers?</p>
  "!
  "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
  methods has_request_headers
    returning value(result) type abap_bool .

  "! <p class="shorttext synchronized" lang="en">Has request payload?</p>
  "!
  "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
  methods has_request_payload
    returning value(result) type abap_bool .

  "! <p class="shorttext synchronized" lang="en">Has response headers?</p>
  "!
  "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
  methods has_response_headers
    returning value(result) type abap_bool .

  "! <p class="shorttext synchronized" lang="en">Has response payload?</p>
  "!
  "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
  methods has_response_payload
    returning value(result) type abap_bool .

  "! <p class="shorttext synchronized" lang="en">Is trace stripped of payload/headers?</p>
  "!
  "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
  methods is_stripped
    returning value(result) type abap_bool.

  "! <p class="shorttext synchronized" lang="en">Set HTTP status</p>
  "!
  "! @parameter http_status | <p class="shorttext synchronized" lang="en"></p>
  methods set_http_status
    importing http_status type i .

endinterface.
