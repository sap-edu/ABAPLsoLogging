interface zif_lso_log_cust_factory
  public .
  constants begin of id.
  constants delete_payload_after type zlso_log_cust-id value 'DELETE_PAYLOAD_AFTER'.
  constants delete_trace_after type zlso_log_cust-id value 'DELETE_TRACE_AFTER'.
  constants delete_message_after type zlso_log_cust-id value 'DELETE_MESSAGE_AFTER'.
  constants delete_log_after type zlso_log_cust-id value 'DELETE_LOG_AFTER'.
  constants end of id.

  types begin of ts_custom.
  types id type zlso_log_cust-id.
  types instance type ref to zif_lso_log_cust.
  types end of ts_custom.

  types tt_custom type hashed table of ts_custom with unique key primary_key components id.

  "! <p class="shorttext synchronized" lang="en">Get customizing value by id</p>
  "!
  "! @parameter id | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter read_db | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter custom | <p class="shorttext synchronized" lang="en"></p>
  "! @raising zcx_lso_log | <p class="shorttext synchronized" lang="en"></p>
  methods get
    importing id            type zlso_log_cust-id
              read_db       type abap_bool default abap_false
    returning value(custom) type ref to zif_lso_log_cust
    raising   zcx_lso_log.
endinterface.
