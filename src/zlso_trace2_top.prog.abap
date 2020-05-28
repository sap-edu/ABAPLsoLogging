*&---------------------------------------------------------------------*
*& Include ZLSO_TRACE_TOP                                    Report ZLSO_TRACE
*&
*&---------------------------------------------------------------------*

report zlso_trace_top.

data: zlso_log type zlso_log.
data: zlso_log_trace type zlso_log_trace.
data: zlso_log_payload type zlso_log_payload.
data: zlso_log_message type zlso_log_message.
data: t100 type t100.
data: gt_logs type zlso_tt_log_objects.

constants begin of c_user_command.
constants back type sy-ucomm value '&F03'.
constants exit type sy-ucomm value '&F15'.
constants cancel type sy-ucomm value '&F12'.
constants double_click type sy-ucomm value 'DOUBLE_CLICK'.
constants end of c_user_command.


data go_lc_all type ref to cl_gui_custom_container.
data go_lc_top type ref to cl_gui_container.
data go_lc_bot type ref to cl_gui_container.
data go_lc_bot_left type ref to cl_gui_container.
data go_lc_bot_right type ref to cl_gui_container.
data gv_ok0100 type sy-ucomm.

data go_splitter_all type ref to cl_gui_splitter_container. "horizontal splitter
data go_splitter_pay type ref to cl_gui_splitter_container. "payloads

data go_trace_dis_app type ref to zcl_lso_trace_display.
