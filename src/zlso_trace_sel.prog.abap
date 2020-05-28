*&---------------------------------------------------------------------*
*&  Include           ZLSO_TRACE_SEL
*&---------------------------------------------------------------------*
*
* Log ID
select-options p_id for zlso_log-id no intervals.
* Date
select-options p_ldate for zlso_log-log_date OBLIGATORY .
* Time
select-options p_ltime for zlso_log-log_time OBLIGATORY.
* Trigger-User
select-options p_chby for zlso_log-changed_by no intervals.
* Context
select-options p_ctx for zlso_log-context no intervals.
* tcode
select-options p_tcode for zlso_log-tcode no intervals.
* prog name
select-options p_prog for zlso_log-prog no intervals.
* Request URL
select-options p_url for zlso_log_trace-request_url no intervals.

select-options p_meth for zlso_log_trace-request_method no intervals.
* Request Payload
select-options p_rqpay for zlso_log_payload-payload no intervals.
* Response HTTP Code
select-options p_httprc for zlso_log_trace-http_status no intervals.
* Response Payload
select-options p_repay for zlso_log_payload-payload no intervals.
* Message Type, ID, and no
select-options p_msgty for zlso_log_message-msgty no intervals.
select-options p_msgid for zlso_log_message-msgid no intervals.
select-options p_msgno for zlso_log_message-msgno no intervals.

* request header field name and value
select-options p_rqhnam for zlso_log_headers-name no intervals.
select-options p_rqhval for zlso_log_headers-value no intervals.

* response header field name and value
select-options p_rshnam for zlso_log_headers-name no intervals.
select-options p_rshval for zlso_log_headers-value no intervals.

* Message Text
select-options p_msgtx for t100-text no intervals.

selection-screen begin of line.
selection-screen pushbutton 15(30) text-s01 user-command jump_to_stripper.
selection-screen end of line.

" On selection-screen action...
at selection-screen.
  case sy-ucomm.
    when 'JUMP_TO_STRIPPER'.
      perform jump_to_log_stripper.

    when others.
  endcase.
