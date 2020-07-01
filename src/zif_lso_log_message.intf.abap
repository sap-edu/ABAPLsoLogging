interface zif_lso_log_message
  public .
  interfaces zif_lso_clone.

  types: begin of ts_split_variables,
           v1 type symsgv,
           v2 type symsgv,
           v3 type symsgv,
           v4 type symsgv,
         end of ts_split_variables.

  constants c_msgid type symsgid value 'ZLSO_LOG'.

  constants begin of c_type.
  constants success type symsgty value 'S'.
  constants warning type symsgty value 'W'.
  constants info    type symsgty value 'I'.
  constants error   type symsgty value 'E'.
  constants abort   type symsgty value 'A'.
  constants end of c_type.

  types ts_abap_stack type zlso_s_log_abap_stack.

  aliases clone for zif_lso_clone~clone.

  "! <p class="shorttext synchronized" lang="en">Get data</p>
  "! <strong>IMPORTANT</strong> - it is not stored on DB!
  "! @parameter data | <p class="shorttext synchronized" lang="en"></p>
  methods get_data
    returning value(data) type ref to data .

  "! <p class="shorttext synchronized" lang="en">Get date</p>
  "!
  "! @parameter date | <p class="shorttext synchronized" lang="en"></p>
  methods get_date
    returning value(date) type zlso_log_message-msg_date .

  "! <p class="shorttext synchronized" lang="en">Get log id</p>
  "!
  "! @parameter log_id | <p class="shorttext synchronized" lang="en"></p>
  methods get_log_id
    returning value(log_id) type zlso_log_message-log_id .

  "! <p class="shorttext synchronized" lang="en">Get log sequence number</p>
  "!
  "! @parameter log_seqnr | <p class="shorttext synchronized" lang="en"></p>
  methods get_log_seqnr
    returning value(log_seqnr) type zlso_log_message-log_seqnr .

  "! <p class="shorttext synchronized" lang="en">Get system message structure</p>
  "!
  "! @parameter symsg | <p class="shorttext synchronized" lang="en"></p>
  methods get_symsg
    returning value(symsg) type symsg .

  "! <p class="shorttext synchronized" lang="en">Get text</p>
  "!
  "! @parameter text | <p class="shorttext synchronized" lang="en"></p>
  methods get_text
    returning value(text) type string .

  "! <p class="shorttext synchronized" lang="en">Get time</p>
  "!
  "! @parameter time | <p class="shorttext synchronized" lang="en"></p>
  methods get_time
    returning value(time) type zlso_log_message-msg_time .

  "! <p class="shorttext synchronized" lang="en">Get time stamp</p>
  "!
  "! @parameter timestamp | <p class="shorttext synchronized" lang="en"></p>
  methods get_timestamp
    returning value(timestamp) type timestampl .

  "! <p class="shorttext synchronized" lang="en">Get trace object</p>
  "!
  "! @parameter trace | <p class="shorttext synchronized" lang="en"></p>
  methods get_trace
    returning value(trace) type ref to zif_lso_log_trace .

  "! <p class="shorttext synchronized" lang="en">Get request payload (from trace object)</p>
  "!
  "! @parameter request_payload | <p class="shorttext synchronized" lang="en"></p>
  methods get_request_payload
    returning value(request_payload) type string.

  "! <p class="shorttext synchronized" lang="en">Get response payload (from trace object)</p>
  "!
  "! @parameter response_payload | <p class="shorttext synchronized" lang="en"></p>
  methods get_response_payload
    returning value(response_payload) type string.

  "! <p class="shorttext synchronized" lang="en">Get message type</p>
  "!
  "! @parameter type | <p class="shorttext synchronized" lang="en"></p>
  methods get_type
    returning value(type) type zlso_log_message-msgty .

  "! <p class="shorttext synchronized" lang="en">Get message class</p>
  "!
  "! @parameter class | <p class="shorttext synchronized" lang="en"></p>
  methods get_class
    returning value(class) type zlso_log_message-msgid.

  "! <p class="shorttext synchronized" lang="en">Get message number</p>
  "!
  "! @parameter number | <p class="shorttext synchronized" lang="en"></p>
  methods get_number
    returning value(number) type zlso_log_message-msgno.

  "! <p class="shorttext synchronized" lang="en">Get the date when log was stripped of trace</p>
  "!
  "! @parameter stripped_date | <p class="shorttext synchronized" lang="en"></p>
  methods get_stripped_date
    returning value(stripped_date) type zlso_log_message-stripped_date.

  "! <p class="shorttext synchronized" lang="en">Get ABAP stack</p>
  "!
  "! @parameter abap_stack | <p class="shorttext synchronized" lang="en"></p>
  methods get_abap_stack
    returning value(abap_stack) type ts_abap_stack.

  "! <p class="shorttext synchronized" lang="en">Get object with key</p>
  "!
  "! @parameter object | <p class="shorttext synchronized" lang="en"></p>
  methods get_object
    returning value(object) type zlso_s_log_message.

  methods split
    returning value(variables) type ts_split_variables.

  "! <p class="shorttext synchronized" lang="en">Has trace?</p>
  "!
  "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
  methods has_trace
    returning value(result) type abap_bool .

  "! <p class="shorttext synchronized" lang="en">Is error message?</p>
  "!
  "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
  methods is_error
    returning value(result) type abap_bool .

  "! <p class="shorttext synchronized" lang="en">Is information message?</p>
  "!
  "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
  methods is_info
    returning value(result) type abap_bool .

  "! <p class="shorttext synchronized" lang="en">Is success message?</p>
  "!
  "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
  methods is_success
    returning value(result) type abap_bool .

  "! <p class="shorttext synchronized" lang="en">Is warning message?</p>
  "!
  "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
  methods is_warning
    returning value(result) type abap_bool .

  "! <p class="shorttext synchronized" lang="en">Is abort message?</p>
  "!
  "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
  methods is_abort
    returning value(result) type abap_bool .

  "! <p class="shorttext synchronized" lang="en">Is message stripped of trace?</p>
  "!
  "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
  methods is_stripped
    returning value(result) type abap_bool.

  "! <p class="shorttext synchronized" lang="en">Set data</p>
  "!
  "! @parameter data | <p class="shorttext synchronized" lang="en"></p>
  methods set_data
    importing data type ref to data .

  "! <p class="shorttext synchronized" lang="en">Set trace object</p>
  "!
  "! @parameter trace | <p class="shorttext synchronized" lang="en"></p>
  methods set_trace
    importing trace type ref to zcl_lso_log_trace.

  "! <p class="shorttext synchronized" lang="en">Set message type</p>
  "!
  "! @parameter msgty | <p class="shorttext synchronized" lang="en"></p>
  methods set_type
    importing msgty type zlso_log_message-msgty .

endinterface.
