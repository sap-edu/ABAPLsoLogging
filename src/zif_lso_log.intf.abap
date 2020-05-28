interface zif_lso_log
  public .
  interfaces zif_lso_log_abstract.

  aliases add_message for zif_lso_log_abstract~add_message.

  constants c_langu_english type sy-langu value 'E'.
  constants c_time_zone_utc type ttzz-tzone value 'UTC'.

  constants begin of c_mode.
  constants dialog      type balmode value 'D'.
  constants batch       type balmode value 'B'.
  constants batch_input type balmode value 'I'.
  constants end of c_mode .

  constants begin of c_time.
  constants min type tims value '000000'.
  constants max type tims value '235959'.
  constants end of c_time .

  " Keep range helpers in the log interface to keep the package transportable.
  constants begin of c_option.
  constants equal                type ddoption value 'EQ'.
  constants not_equal            type ddoption value 'NE'.
  constants contains_pattern     type ddoption value 'CP'.
  constants not_contains_pattern type ddoption value 'NP'.
  constants between              type ddoption value 'BT'.
  constants end of c_option .

  constants begin of c_sign.
  constants include type ddsign value 'I'.
  constants exclude type ddsign value 'E'.
  constants end of c_sign .

  types begin of ts_key.
  types id type zlso_log-id.
  types seqnr type zlso_log-seqnr.
  types end of ts_key.

  types begin of ts_object.
  include type ts_key.
  types instance type ref to zif_lso_log.
  types end of ts_object.

  types tt_objects type hashed table of ts_object with unique key primary_key components id seqnr.

  "! <p class="shorttext synchronized" lang="en">Add message</p>
  "! <strong>OBSOLETE</strong> - backward compatibility, it's been replaced with ZIF_LSO_LOG_ABSTRACT~ADD_MESSAGE!
  "! @parameter message | <p class="shorttext synchronized" lang="en"></p>
  methods add
    importing message type ref to zcl_lso_log_message .

  "! <p class="shorttext synchronized" lang="en">Add reference log</p>
  "!
  "! @parameter ref_log | <p class="shorttext synchronized" lang="en"></p>
  methods add_ref_log
    importing ref_log type ref to zcl_lso_log.

  "! <p class="shorttext synchronized" lang="en">Get log id</p>
  "!
  "! @parameter log_id | <p class="shorttext synchronized" lang="en"></p>
  methods get_id
    returning value(id) type zlso_log-id .

  "! <p class="shorttext synchronized" lang="en">Get log run sequence number</p>
  "!
  "! @parameter seqnr | <p class="shorttext synchronized" lang="en"></p>
  methods get_seqnr
    returning value(seqnr) type zlso_log-seqnr .

  "! <p class="shorttext synchronized" lang="en">Get user that changed the log</p>
  "!
  "! @parameter changed_by | <p class="shorttext synchronized" lang="en"></p>
  methods get_changed_by
    returning value(changed_by) type zlso_log-changed_by .

  "! <p class="shorttext synchronized" lang="en">Get log creation context</p>
  "!
  "! @parameter context | <p class="shorttext synchronized" lang="en"></p>
  methods get_context
    returning value(context) type zlso_log-context.

  "! <p class="shorttext synchronized" lang="en">Get date</p>
  "!
  "! @parameter date | <p class="shorttext synchronized" lang="en"></p>
  methods get_date
    returning value(date) type zlso_log-log_date .

  "! <p class="shorttext synchronized" lang="en">Get time</p>
  "!
  "! @parameter time | <p class="shorttext synchronized" lang="en"></p>
  methods get_time
    returning value(time) type zlso_log-log_time .

  "! <p class="shorttext synchronized" lang="en">Get time stamp</p>
  "!
  "! @parameter timestamp | <p class="shorttext synchronized" lang="en"></p>
  methods get_timestamp
    returning value(timestamp) type zlso_log-timestamp .

  "! <p class="shorttext synchronized" lang="en">Get logs DB structure</p>
  "!
  "! @parameter structure | <p class="shorttext synchronized" lang="en"></p>
  methods get_structure
    returning value(structure) type zlso_log .

  "! <p class="shorttext synchronized" lang="en">Get log object with key</p>
  "!
  "! @parameter object | <p class="shorttext synchronized" lang="en"></p>
  methods get_object
    returning value(object) type ts_object.

  "! <p class="shorttext synchronized" lang="en">Get messages collection</p>
  "!
  "! @parameter with_ref | <p class="shorttext synchronized" lang="en">With messages from reference log?</p>
  "! @parameter messages | <p class="shorttext synchronized" lang="en"></p>
  methods get_messages
    importing with_ref        type abap_bool default abap_false
    returning value(messages) type ref to if_object_collection .

  "! <p class="shorttext synchronized" lang="en">Get reference logs collection</p>
  "!
  "! @parameter logs | <p class="shorttext synchronized" lang="en"></p>
  methods get_ref_logs
    returning value(logs) type ref to cl_object_collection .

  "! <p class="shorttext synchronized" lang="en">Get the date when log was stripped of messages</p>
  "!
  "! @parameter stripped_date | <p class="shorttext synchronized" lang="en"></p>
  methods get_stripped_date
    returning value(stripped_date) type zlso_log-stripped_date.

  "! <p class="shorttext synchronized" lang="en">Has log got an exception message?</p>
  "!
  "! @parameter cx_t100_key | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter with_ref | <p class="shorttext synchronized" lang="en">Check reference log?</p>
  "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
  methods has_exception
    importing cx_t100_key   type scx_t100key
              with_ref      type abap_bool default abap_true
    returning value(result) type abap_bool .

  "! <p class="shorttext synchronized" lang="en">Has log got a message?</p>
  "!
  "! @parameter message | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter with_ref | <p class="shorttext synchronized" lang="en">Check reference log?</p>
  "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
  methods has_message
    importing message       type ref to zcl_lso_log_message
              with_ref      type abap_bool default abap_true
    returning value(result) type abap_bool .

  "! <p class="shorttext synchronized" lang="en">Has log got messages?</p>
  "!
  "! @parameter with_ref | <p class="shorttext synchronized" lang="en">Check reference log?</p>
  "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
  methods has_messages
    importing with_ref      type abap_bool default abap_true
    returning value(result) type abap_bool .

  "! <p class="shorttext synchronized" lang="en">Has log got a message with particular type?</p>
  "!
  "! @parameter type | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter with_ref | <p class="shorttext synchronized" lang="en">Check reference log?</p>
  "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
  methods has_message_type
    importing type          type msgty
              with_ref      type abap_bool default abap_true
    returning value(result) type abap_bool .

  "! <p class="shorttext synchronized" lang="en">Has log got a particular reference log?</p>
  "!
  "! @parameter log | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter rv_result | <p class="shorttext synchronized" lang="en"></p>
  methods has_ref_log
    importing log           type ref to zcl_lso_log
    returning value(result) type abap_bool .

  "! <p class="shorttext synchronized" lang="en">Has log got reference logs?</p>
  "!
  "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
  methods has_ref_logs
    returning value(result) type abap_bool .

  "! <p class="shorttext synchronized" lang="en">Is log stripped of messages?</p>
  "!
  "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
  methods is_stripped
    returning value(result) type abap_bool.

  "! <p class="shorttext synchronized" lang="en">Lock the log</p>
  "!
  "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
  methods lock
    returning value(result) type abap_bool.

  "! <p class="shorttext synchronized" lang="en">Unlock the log</p>
  "!
  methods unlock .

  "! <p class="shorttext synchronized" lang="en">Save log run</p>
  "! Next sequence number will be generated.
  "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
  "! @raising zcx_lso_log | <p class="shorttext synchronized" lang="en"></p>
  methods save
    returning value(result) type abap_bool
    raising   zcx_lso_log .

  "! <p class="shorttext synchronized" lang="en">Delete the whole log</p>
  "! <strong>IMPORTANT</strong> - it removes the whole log with all dependant objects like messages, traces, payloads, headers...
  "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
  methods delete
    returning value(result) type abap_bool.
endinterface.
