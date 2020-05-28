function-pool zlso_trace.                   "MESSAGE-ID ..

* INCLUDE LZLSO_TRACED...                    " Local class definition

constants begin of c_user_command.
constants back type sy-ucomm value '&F03'.
constants exit type sy-ucomm value '&F15'.
constants cancel type sy-ucomm value '&F12'.
constants double_click type sy-ucomm value 'DOUBLE_CLICK'.
constants end of c_user_command.


data go_lc_all type ref to cl_gui_custom_container.
data go_lc_all_top type ref to cl_gui_container.
data go_lc_all_bot type ref to cl_gui_container.
data go_lc_bot_url type ref to cl_gui_container.
data go_lc_bot_header_and_payload type ref to cl_gui_container.
data go_lc_request type ref to cl_gui_container.
data go_lc_response type ref to cl_gui_container.

data go_lc_request_payload type ref to cl_gui_container.
data go_lc_request_header type ref to cl_gui_container.
data go_lc_response_header type ref to cl_gui_container.

data go_lc_response_payload type ref to cl_gui_container.
data gv_ok0100 type sy-ucomm.

data go_splitter_all type ref to cl_gui_splitter_container. " horizontal splitter (red)
data go_splitter_bottom type ref to cl_gui_splitter_container. " bottom splitter (green)
data go_splitter_headers_payloads type ref to cl_gui_splitter_container. " split response and request headers and payloads (pink one)

data go_splitter_request type ref to cl_gui_splitter_container. " split request to header/payload (yellow)
data go_splitter_response type ref to cl_gui_splitter_container. " split response to header/payload (violet)



data go_trace_dis_app type ref to zcl_lso_trace_display.
