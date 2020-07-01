interface zif_lso_log_factory
  public .
  constants begin of c_secs.
  constants one_hour type i value 3600.
  constants end of c_secs.

  " Log
  types tt_log_id type range of zlso_log-id .
  types tt_log_seqnr type range of zlso_log-seqnr .
  types tt_log_date type range of zlso_log-log_date .
  types tt_log_time type range of zlso_log-log_time .
  types tt_changed_by type range of zlso_log-changed_by .
  types tt_context type range of zlso_log-context .
  types tt_tcode type range of zlso_log-tcode .
  types tt_program type range of zlso_log-prog .
  " Message
  types tt_msgid type range of zlso_log_message-msgid .
  types tt_msgno type range of zlso_log_message-msgno .
  types tt_msgty type range of zlso_log_message-msgty .
  types tt_timestamp type range of zlso_log_message-timestamp .
  types tt_message_text type range of zlso_d_log_message_text.
  " Payload
  types tt_payload type range of zlso_log_payload-payload .
  " Headers
  types tt_header_value type range of zlso_log_headers-value .
  types tt_header_name type range of zlso_log_headers-name .
  " Trace
  types tt_http_status type range of zlso_log_trace-http_status .
  types tt_request_method type range of zlso_log_trace-request_method .
  types tt_request_url type range of zlso_log_trace-request_url .

  types begin of ts_last_message.
  types log_id        type zlso_log-id.
  types log_seqnr     type zlso_log-seqnr.
  types log_ref_id    type zlso_log-ref_id.
  types log_ref_seqnr type zlso_log-ref_seqnr.
  types msg_timestamp type zlso_log_message-timestamp.
  types msg_date      type zlso_log_message-msg_date.
  types msg_time      type zlso_log_message-msg_time.
  types end of ts_last_message.

  types tt_last_messages type sorted table of ts_last_message with unique key log_id log_seqnr .

  types begin of ts_object.
  types id type zlso_log-id.
  types seqnr type zlso_log-seqnr.
  types instance type ref to zcl_lso_log.
  types end of ts_object.

  types tt_objects type sorted table of ts_object with unique key id seqnr.

  "! Get log by ID
  "!
  "! @parameter id | ID of the log (zlso_log DB)
  "! @parameter seqnr | seqnr of the log (zlso_log DB)
  "! @parameter with_ref_logs | Read reference logs
  "! @parameter logs | Log object
  "! @raising zcx_lso_log | No logs found
  methods get
    importing id            type zlso_log-id
              seqnr         type zlso_log-seqnr optional
              with_ref_logs type abap_bool default abap_true
    returning value(logs)   type zlso_tt_logs
    raising   zcx_lso_log .

  "! <p class="shorttext synchronized" lang="en">Create logs from log handler</p>
  "!
  "! @parameter log_handler | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter logs | <p class="shorttext synchronized" lang="en"></p>
  methods create_from_handler
    importing log_handler type ref to zcl_lso_log_handler
    returning value(logs) type zlso_tt_logs.

  "! <p class="shorttext synchronized" lang="en">Create log by its structure</p>
  "!
  "! @parameter structure | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter log | <p class="shorttext synchronized" lang="en"></p>
  methods create_by_structure
    importing structure  type zlso_log
    returning value(log) type ref to zcl_lso_log.

  "! <p class="shorttext synchronized" lang="en">Find logs by various search criteria.</p>
  "! <p>Either Log Id or Log Dates parameters are needed because of SQL performance, exception is raised if no provided!</p>
  "!
  "! @parameter log_ids | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter log_seqnrs | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter log_dates | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter log_times | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter changed_bys | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter contexts | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter tcodes | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter programs | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter msgtys | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter msgids | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter msgnos | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter http_statuses | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter request_urls | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter request_methods | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter message_texts | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter request_payloads | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter req_header_names | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter req_header_values | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter res_header_names | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter res_header_values | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter response_payloads | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter with_ref_logs | <p class="shorttext synchronized" lang="en">Include reference logs?</p>
  "! @parameter logs | <p class="shorttext synchronized" lang="en"></p>
  "! @raising zcx_lso_log | <p class="shorttext synchronized" lang="en"></p>
  methods find
    importing log_ids           type tt_log_id optional
              log_seqnrs        type tt_log_seqnr optional
              log_dates         type tt_log_date optional
              log_times         type tt_log_time optional
              changed_bys       type tt_changed_by optional
              contexts          type tt_context optional
              tcodes            type tt_tcode optional
              programs          type tt_program optional
              msgids            type tt_msgid optional
              msgnos            type tt_msgno optional
              msgtys            type tt_msgty optional
              http_statuses     type tt_http_status optional
              request_urls      type tt_request_url optional
              request_methods   type tt_request_method optional
              message_texts     type tt_message_text optional
              request_payloads  type tt_payload optional
              response_payloads type tt_payload optional
              req_header_names  type tt_header_name optional
              req_header_values type tt_header_value optional
              res_header_names  type tt_header_name optional
              res_header_values type tt_header_value optional
              with_ref_logs     type abap_bool default abap_true
                preferred parameter log_ids
    returning value(logs)       type zlso_tt_logs
    raising   zcx_lso_log .

  "! <p class="shorttext synchronized" lang="en">Find last messages by log ids</p>
  "!
  "! @parameter log_ids | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter last_messages | <p class="shorttext synchronized" lang="en"></p>
  methods find_last_messages
    importing log_ids              type tt_log_id
    returning value(last_messages) type tt_last_messages.

  "! <p class="shorttext synchronized" lang="en">Does log exist on DB?</p>
  "!
  "! @parameter iv_id | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter iv_seqnr | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
  methods exists
    importing iv_id         type zlso_log-id
              iv_seqnr      type zlso_log-seqnr optional
    returning value(result) type abap_bool .

  "! <p class="shorttext synchronized" lang="en">Delete logs</p>
  "!
  "! @parameter log_ids | <p class="shorttext synchronized" lang="en"></p>
  methods delete
    importing log_ids type tt_log_id.

  "! <p class="shorttext synchronized" lang="en">Delete logs collection</p>
  "!
  "! @parameter logs | <p class="shorttext synchronized" lang="en"></p>
  methods delete_collection
    importing logs type zlso_tt_logs.

  "! <p class="shorttext synchronized" lang="en">Create logs ids range</p>
  "!
  "! @parameter logs | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter ids | <p class="shorttext synchronized" lang="en"></p>
  methods logs2ids
    importing logs       type zlso_tt_logs
    returning value(ids) type zif_lso_log_factory=>tt_log_id.

endinterface.
