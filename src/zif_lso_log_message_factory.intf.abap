interface zif_lso_log_message_factory
  public .
  types tt_msgty type range of zlso_log_message-msgty.

  "! <p class="shorttext synchronized" lang="en">Gets last message by log id within dates</p>
  methods get_last_by_log_id
    importing log_id         type zlso_log-id
              msgty          type symsgty optional
              date_from      type datn default '19000101'
              time_from      type timn default '000000'
              date_to        type datn default '99991231'
              time_to        type timn default '235959'
    returning value(message) type ref to zcl_lso_log_message
    raising   zcx_lso_log.

  "! <p class="shorttext synchronized" lang="en">Gets last error message by log id within dates</p>
  methods get_last_error_by_log_id
    importing log_id         type zlso_log-id
              date_from      type datn default '19000101'
              time_from      type timn default '000000'
              date_to        type datn default '99991231'
              time_to        type timn default '235959'
    returning value(message) type ref to zcl_lso_log_message
    raising   zcx_lso_log.

  "! <p class="shorttext synchronized" lang="en">Gets last success message by log id within dates</p>
  methods get_last_success_by_log_id
    importing log_id         type zlso_log-id
              date_from      type datn default '19000101'
              time_from      type timn default '000000'
              date_to        type datn default '99991231'
              time_to        type timn default '235959'
    returning value(message) type ref to zcl_lso_log_message
    raising   zcx_lso_log.

  "! <p class="shorttext synchronized" lang="en">Gets last warning message by log id within dates</p>
  methods get_last_warning_by_log_id
    importing log_id         type zlso_log-id
              date_from      type datn default '19000101'
              time_from      type timn default '000000'
              date_to        type datn default '99991231'
              time_to        type timn default '235959'
    returning value(message) type ref to zcl_lso_log_message
    raising   zcx_lso_log.
endinterface.
