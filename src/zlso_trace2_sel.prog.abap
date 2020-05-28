*&---------------------------------------------------------------------*
*&  Include           ZLSO_TRACE_SEL
*&---------------------------------------------------------------------*

* Log ID
select-options p_id for zlso_log-id no intervals.
* Date
select-options p_ldate for zlso_log-log_date OBLIGATORY .
* Time
select-options p_ltime for zlso_log-log_time OBLIGATORY.
* Trigger-User
select-options p_chby for zlso_log-changed_by no intervals.
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

* Message Text
select-options p_msgtx for t100-text no intervals.

* Real/Simulation data
PARAMETERS: p_real AS CHECKBOX DEFAULT 'X'.
PARAMETERS: p_sim AS CHECKBOX DEFAULT 'X'.
