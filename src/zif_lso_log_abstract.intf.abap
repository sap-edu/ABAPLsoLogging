interface zif_lso_log_abstract
  public .
  constants c_msgid type symsgid value 'ZLSO_LOG'.

  "! <p class="shorttext synchronized" lang="en">Add message from exception</p>
  "!
  "! @parameter cx | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter trace | <p class="shorttext synchronized" lang="en"></p>
  methods exception
    importing cx    type ref to if_message
              trace type ref to zcl_lso_log_trace optional .

  "! <p class="shorttext synchronized" lang="en">Add message</p>
  "!
  "! @parameter message | <p class="shorttext synchronized" lang="en"></p>
  methods add_message
    importing message type ref to zcl_lso_log_message .

  "! <p class="shorttext synchronized" lang="en">Add collection of messages</p>
  "!
  "! @parameter messages | <p class="shorttext synchronized" lang="en"></p>
  methods add_messages
    importing messages type ref to if_object_collection .

  "! <p class="shorttext synchronized" lang="en">Add message from structure</p>
  "!
  "! @parameter symsg | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter trace | <p class="shorttext synchronized" lang="en"></p>
  methods add_symsg
    importing symsg type symsg
              trace type ref to zcl_lso_log_trace optional.

  "! <p class="shorttext synchronized" lang="en">Add error</p>
  "!
  "! @parameter msgid | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter msgno | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter msgv1 | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter msgv2 | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter msgv3 | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter msgv4 | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter trace | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter message | <p class="shorttext synchronized" lang="en"></p>
  methods error
    importing msgid          type msgid
              msgno          type symsgno
              msgv1          type string optional
              msgv2          type string optional
              msgv3          type string optional
              msgv4          type string optional
              trace          type ref to zcl_lso_log_trace optional
    returning value(message) type ref to zcl_lso_log_message .

  "! <p class="shorttext synchronized" lang="en">Add information</p>
  "!
  "! @parameter msgid | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter msgno | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter msgv1 | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter msgv2 | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter msgv3 | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter msgv4 | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter trace | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter message | <p class="shorttext synchronized" lang="en"></p>
  methods info
    importing msgid          type msgid
              msgno          type symsgno
              msgv1          type string optional
              msgv2          type string optional
              msgv3          type string optional
              msgv4          type string optional
              trace          type ref to zcl_lso_log_trace optional
    returning value(message) type ref to zcl_lso_log_message .

  "! <p class="shorttext synchronized" lang="en">Add success</p>
  "!
  "! @parameter msgid | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter msgno | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter msgv1 | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter msgv2 | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter msgv3 | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter msgv4 | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter trace | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter message | <p class="shorttext synchronized" lang="en"></p>
  methods success
    importing msgid          type msgid
              msgno          type symsgno
              msgv1          type string optional
              msgv2          type string optional
              msgv3          type string optional
              msgv4          type string optional
              trace          type ref to zcl_lso_log_trace optional
    returning value(message) type ref to zcl_lso_log_message .

  "! <p class="shorttext synchronized" lang="en">Add warning</p>
  "!
  "! @parameter msgid | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter msgno | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter msgv1 | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter msgv2 | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter msgv3 | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter msgv4 | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter trace | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter message | <p class="shorttext synchronized" lang="en"></p>
  methods warning
    importing msgid          type msgid
              msgno          type symsgno
              msgv1          type string optional
              msgv2          type string optional
              msgv3          type string optional
              msgv4          type string optional
              trace          type ref to zcl_lso_log_trace optional
    returning value(message) type ref to zcl_lso_log_message .

  "! <p class="shorttext synchronized" lang="en">Add abort</p>
  "!
  "! @parameter msgid | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter msgno | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter msgv1 | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter msgv2 | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter msgv3 | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter msgv4 | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter trace | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter message | <p class="shorttext synchronized" lang="en"></p>
  methods abort
    importing msgid          type msgid
              msgno          type symsgno
              msgv1          type string optional
              msgv2          type string optional
              msgv3          type string optional
              msgv4          type string optional
              trace          type ref to zcl_lso_log_trace optional
    returning value(message) type ref to zcl_lso_log_message .

  "! <p class="shorttext synchronized" lang="en">Add message with type</p>
  "!
  "! @parameter msgty | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter msgid | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter msgno | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter msgv1 | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter msgv2 | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter msgv3 | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter msgv4 | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter trace | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter message | <p class="shorttext synchronized" lang="en"></p>
  methods message
    importing msgty          type msgty
              msgid          type msgid
              msgno          type symsgno
              msgv1          type string optional
              msgv2          type string optional
              msgv3          type string optional
              msgv4          type string optional
              trace          type ref to zcl_lso_log_trace optional
    returning value(message) type ref to zcl_lso_log_message .

  "! <p class="shorttext synchronized" lang="en">Add message with trace details</p>
  "!
  "! @parameter trace | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter msgid | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter msgno | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter msgty | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter msgv1 | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter msgv2 | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter msgv3 | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter msgv4 | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter http_status | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter message | <p class="shorttext synchronized" lang="en"></p>
  methods trace_message
    importing trace          type ref to zcl_lso_log_trace
              msgid          type zlso_log_message-msgid
              msgno          type zlso_log_message-msgno
              msgty          type zlso_log_message-msgty optional
              msgv1          type string optional
              msgv2          type string optional
              msgv3          type string optional
              msgv4          type string optional
              http_status    type zlso_log_trace-http_status optional
    returning value(message) type ref to zcl_lso_log_message .

  "! <p class="shorttext synchronized" lang="en">Clear messages</p>
  methods clear_messages.

  "! <p class="shorttext synchronized" lang="en">Get messages collection</p>
  "!
  "! @parameter type | <p class="shorttext synchronized" lang="en">Only messages with specific type</p>
  "! @parameter clone | <p class="shorttext synchronized" lang="en">Clone messages as new instances</p>
  "! @parameter messages | <p class="shorttext synchronized" lang="en"></p>
  methods get_messages
    importing type            type zlso_log_message-msgty optional
              clone           type abap_bool default abap_false
                preferred parameter type
    returning value(messages) type ref to if_object_collection .

  "! <p class="shorttext synchronized" lang="en">Get messages for RFC</p>
  "!
  "! @parameter type | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter messages_rfc | <p class="shorttext synchronized" lang="en"></p>
  methods get_messages_rfc
    importing type                type zlso_log_message-msgty optional
    returning value(messages_rfc) type zlso_tt_log_rfc_messages.

  "! <p class="shorttext synchronized" lang="en">Get messages table</p>
  "!
  "! @parameter symsgs | <p class="shorttext synchronized" lang="en"></p>
  methods get_symsgs
    returning value(symsgs) type hrpadfr_symsg .

  "! <p class="shorttext synchronized" lang="en">Get programm</p>
  "!
  "! @parameter program | <p class="shorttext synchronized" lang="en"></p>
  methods get_program
    returning value(program) type zlso_log-prog .

  "! <p class="shorttext synchronized" lang="en">Get transaction code</p>
  "!
  "! @parameter tcode | <p class="shorttext synchronized" lang="en"></p>
  methods get_tcode
    returning value(tcode) type zlso_log-tcode .

  "! <p class="shorttext synchronized" lang="en">Has log got an error/abort message?</p>
  "!
  "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
  methods has_error
    importing after         type zlso_log_message-timestamp default zif_lso_log_range=>c_timestampl-min
    returning value(result) type abap_bool .

  "! <p class="shorttext synchronized" lang="en">Has log got a warning message?</p>
  "!
  "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
  methods has_warning
    importing after         type zlso_log_message-timestamp default zif_lso_log_range=>c_timestampl-min
    returning value(result) type abap_bool .

  "! <p class="shorttext synchronized" lang="en">Has log got an info message?</p>
  "!
  "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
  methods has_info
    importing after         type zlso_log_message-timestamp default zif_lso_log_range=>c_timestampl-min
    returning value(result) type abap_bool .

  "! <p class="shorttext synchronized" lang="en">Has log got a success message?</p>
  "!
  "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
  methods has_success
    importing after         type zlso_log_message-timestamp default zif_lso_log_range=>c_timestampl-min
    returning value(result) type abap_bool .

  "! <p class="shorttext synchronized" lang="en">Has log got an abort message?</p>
  "!
  "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
  methods has_abort
    importing after         type zlso_log_message-timestamp default zif_lso_log_range=>c_timestampl-min
    returning value(result) type abap_bool .

  "! <p class="shorttext synchronized" lang="en">Has log got an exception message?</p>
  "!
  "! @parameter cx_t100_key | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
  methods has_exception
    importing cx_t100_key   type scx_t100key
    returning value(result) type abap_bool .

  "! <p class="shorttext synchronized" lang="en">Has log got a message?</p>
  "!
  "! @parameter message | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
  methods has_message
    importing message       type ref to zcl_lso_log_message
    returning value(result) type abap_bool .

  "! <p class="shorttext synchronized" lang="en">Has log got a message of type?</p>
  "!
  "! @parameter type | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter rv_result | <p class="shorttext synchronized" lang="en"></p>
  methods has_message_type
    importing type          type zlso_log_message-msgty            " YYYYMMDDhhmmssmmmuuun
              after         type zlso_log_message-timestamp default zif_lso_log_range=>c_timestampl-min
    returning value(result) type abap_bool.

  "! <p class="shorttext synchronized" lang="en">Has log got a this message class (id) and number?</p>
  "!
  "! @parameter msgid | <p class="shorttext synchronized" lang="en">Message Class ( zlso_sf_api ) </p>
  "! @parameter msgno | <p class="shorttext synchronized" lang="en">Message Number ( 003 ) </p>"!
  "! @parameter result | <p class="shorttext synchronized" lang="en"> True or False</p>
  methods has_message_key
    importing msgid         type zlso_log_message-msgid
              msgno         type zlso_log_message-msgno
    returning value(result) type abap_bool.

  "! <p class="shorttext synchronized" lang="en">Has log got messages?</p>
  "!
  "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
  methods has_messages
    returning value(result) type abap_bool .

  "! <p class="shorttext synchronized" lang="en">Set program</p>
  "!
  "! @parameter program | <p class="shorttext synchronized" lang="en"></p>
  methods set_program
    importing program type zlso_log-prog .

  "! <p class="shorttext synchronized" lang="en">Set transaction code</p>
  "!
  "! @parameter tcode | <p class="shorttext synchronized" lang="en"></p>
  methods set_tcode
    importing tcode type zlso_log-tcode .

  "! <p class="shorttext synchronized" lang="en">Display popup with messages (GUI)</p>
  "!
  "! @parameter title | <p class="shorttext synchronized" lang="en"></p>
  "! @parameter type | <p class="shorttext synchronized" lang="en">Only messages with type</p>
  methods display_messages_popup
    importing title type char70 optional
              type  type zlso_log_message-msgty optional
                preferred parameter title.
endinterface.
