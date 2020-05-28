*----------------------------------------------------------------------*
***INCLUDE LZLSO_TRACEO01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0100 output.

  set pf-status '0100_MAIN'.
  set titlebar '0100_MAIN'.

  if go_splitter_all is bound.
    go_trace_dis_app->refresh_alv( ).
  else.

    " take a look at https://issues.wdf.sap.corp/browse/EDU-6331 to refer containers colors
    create object go_splitter_all " red container
      exporting
        link_dynnr = sy-dynnr
        link_repid = sy-repid
        parent     = cl_gui_container=>default_screen "go_lc_all
        rows       = 2
        columns    = 1.

    go_splitter_all->set_row_height( id = 1 height = zcl_lso_trace_display=>default_alv_height )." alv

    go_lc_all_top = go_splitter_all->get_container( row = 1 column = 1 ). " red container
    go_lc_all_bot = go_splitter_all->get_container( row = 2 column  = 1 ). " red container

    " create bottom splitter (green one)
    create object go_splitter_bottom
      exporting
        link_dynnr = sy-dynnr
        link_repid = sy-repid
        parent     = go_lc_all_bot
        rows       = 2
        columns    = 1.

    go_splitter_bottom->set_row_height( id = 1 height = zcl_lso_trace_display=>default_url_ctr_height )." url

    " get containers for url and headers/payloads
    go_lc_bot_url                = go_splitter_bottom->get_container( row = 1 column = 1 ). "green container
    go_lc_bot_header_and_payload = go_splitter_bottom->get_container( row = 2 column = 1 ). "green container


    " create splitter to split request and response (pink one)
    create object go_splitter_headers_payloads
      exporting
        link_dynnr = sy-dynnr
        link_repid = sy-repid
        parent     = go_lc_bot_header_and_payload
        rows       = 1
        columns    = 2.

    go_lc_request   = go_splitter_headers_payloads->get_container( row = 1 column = 1 ). " yellow
    go_lc_response  = go_splitter_headers_payloads->get_container( row = 1 column = 2 )." violet

    create object go_splitter_request " yellow
      exporting
        link_dynnr = sy-dynnr
        link_repid = sy-repid
        parent     = go_lc_request
        rows       = 2
        columns    = 1.

    go_splitter_request->set_row_height( id = 1 height = zcl_lso_trace_display=>default_header_ctr_height )." header

    create object go_splitter_response " violet
      exporting
        link_dynnr = sy-dynnr
        link_repid = sy-repid
        parent     = go_lc_response
        rows       = 2
        columns    = 1.

    go_splitter_response->set_row_height( id = 1 height = zcl_lso_trace_display=>default_header_ctr_height )." header

    go_lc_request_header   = go_splitter_request->get_container( row = 1 column = 1 ). " yellow
    go_lc_request_payload  = go_splitter_request->get_container( row = 2 column = 1 ). " yellow

    go_lc_response_header  = go_splitter_response->get_container( row = 1 column = 1 )." violet
    go_lc_response_payload = go_splitter_response->get_container( row = 2 column = 1 )." violet


    go_trace_dis_app->display( io_cont_alv              = go_lc_all_top
                               io_cont_request_payload  = go_lc_request_payload
                               io_cont_response_payload = go_lc_response_payload
                               io_cont_url              = go_lc_bot_url
                               io_cont_request_header   = go_lc_request_header
                               io_cont_response_header  = go_lc_response_header
                               io_main_splitter         = go_splitter_all
                               io_bottom_splitter       = go_splitter_bottom
                               io_request_splitter      = go_splitter_request
                               io_response_splitter     = go_splitter_response ).
  endif.

endmodule.                 " STATUS_0100  OUTPUT
