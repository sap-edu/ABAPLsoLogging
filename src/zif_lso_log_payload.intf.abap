interface zif_lso_log_payload
  public .
  constants begin of c_mime_type .
  constants json type string value 'application/json'.
  constants html type string value 'text/html'.
  constants xml type string value 'text/xml'.
  constants text type string value 'text/plain'.
  constants end of c_mime_type .

  constants begin of c_type.
  constants request type zlso_log_payload-type value 'REQUEST'.
  constants response type zlso_log_payload-type value 'RESPONSE'.
  constants end of c_type .

  types tt_payloads type sorted table of zlso_log_payload with unique key primary_key components trace_id type seqnr.
  types tt_payload_lines type standard table of zlso_d_trace_payload with empty key.

  "! <p class="shorttext synchronized" lang="en">Get formatted payload (pretty printer)</p>
  "!
  "! @parameter formatted_payload | <p class="shorttext synchronized" lang="en"></p>
  methods get_formatted_payload
    returning value(formatted_payload) type string .

  "! <p class="shorttext synchronized" lang="en">Get MIME type</p>
  "!
  "! @parameter mime_type | <p class="shorttext synchronized" lang="en"></p>
  methods get_mime_type
    returning value(mime_type) type string .

  "! <p class="shorttext synchronized" lang="en">Get payload</p>
  "!
  "! @parameter payload | <p class="shorttext synchronized" lang="en"></p>
  methods get_payload
    returning value(payload) type string .

  "! <p class="shorttext synchronized" lang="en">Get payload string splitted into lines</p>
  "!
  "! @parameter payload_lines | <p class="shorttext synchronized" lang="en"></p>
  methods get_payload_lines
    returning value(payload_lines) type tt_payload_lines.

  "! <p class="shorttext synchronized" lang="en">Get trace id</p>
  "!
  "! @parameter trace_id | <p class="shorttext synchronized" lang="en"></p>
  methods get_trace_id
    returning value(trace_id) type zlso_log_payload-trace_id .

  "! <p class="shorttext synchronized" lang="en">Get payload type</p>
  "!
  "! @parameter type | <p class="shorttext synchronized" lang="en"></p>
  methods get_type
    returning value(type) type zlso_log_payload-type .

  "! <p class="shorttext synchronized" lang="en">Is HTML?</p>
  "!
  "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
  methods is_html
    returning value(result) type abap_bool .

  "! <p class="shorttext synchronized" lang="en">Is SAML?</p>
  "!
  "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
  methods is_saml
    returning value(result) type abap_bool .

  "! <p class="shorttext synchronized" lang="en">Is JSON?</p>
  "!
  "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
  methods is_json
    returning value(result) type abap_bool .

  "! <p class="shorttext synchronized" lang="en">Is XML?</p>
  "!
  "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
  methods is_xml
    returning value(result) type abap_bool .

  "! <p class="shorttext synchronized" lang="en">Get object with key</p>
  "!
  "! @parameter object | <p class="shorttext synchronized" lang="en"></p>
  methods get_object
    returning value(object) type zlso_s_log_payload.
endinterface.
