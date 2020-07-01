interface zif_lso_log_handler
  public .
  interfaces zif_lso_log_abstract.

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
    importing msgid          type symsgid
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
    importing msgid          type symsgid
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
    importing msgid          type symsgid
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
    importing msgid          type symsgid
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
    importing msgty          type symsgty
              msgid          type symsgid
              msgno          type symsgno
              msgv1          type string optional
              msgv2          type string optional
              msgv3          type string optional
              msgv4          type string optional
              trace          type ref to zcl_lso_log_trace optional
    returning value(message) type ref to zcl_lso_log_message .

endinterface.
