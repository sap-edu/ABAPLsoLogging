interface zif_lso_cx
  public .
  interfaces if_message .
  interfaces if_t100_message .

  types ty_severity type zlso_d_message_severity.

  constants begin of severity.
  constants abort type zlso_d_message_severity value 'A'.
  constants error type zlso_d_message_severity value 'E'.
  constants end of severity.

  data mv_msgv1 type string read-only.
  data mv_msgv2 type string read-only .
  data mv_msgv3 type string read-only .
  data mv_msgv4 type string read-only .
  data mv_severity type ty_severity read-only.

  "! <p class="shorttext synchronized" lang="en">Get severity</p>
  "!
  "! @parameter severity | <p class="shorttext synchronized" lang="en"></p>
  methods get_severity
    returning value(severity) type ty_severity.

  "! <p class="shorttext synchronized" lang="en">Get message type based on severity</p>
  "!
  "! @parameter msgty | <p class="shorttext synchronized" lang="en">Error/Abort</p>
  methods get_msgty
    returning value(msgty) type zlso_log_message-msgty.

  "! <p class="shorttext synchronized" lang="en">Is severity ABORT?</p>
  "!
  "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
  methods is_abort
    returning value(result) type abap_bool.

  "! <p class="shorttext synchronized" lang="en">Has previous exception?</p>
  "!
  "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
  methods has_previous
    returning value(result) type abap_bool.

endinterface.
