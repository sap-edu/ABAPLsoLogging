class zcl_lso_trace_display definition
  public
  final
  create public .

  public section.

    types: log_date_range type range of datum .

    types: log_id_range type range of zlso_d_log_id .

    constants: begin of c_action,
                 where_used        type sy-ucomm value 'WHERE_USED',
                 show_hide_details type sy-ucomm value 'SHOW_HIDE_DETAILS',
                 jump_to_stripper  type sy-ucomm value 'JUMP_TO_LOG_STRIPPER',
                 export            type sy-ucomm value 'EXPORT',
                 msg_long_txt      type sy-ucomm value 'LONG_TXT',
               end of c_action .

    constants default_alv_height type i value 40 ##NO_TEXT.
    constants default_url_ctr_height type i value 8 ##NO_TEXT.
    constants default_header_ctr_height type i value 20 ##NO_TEXT.

    methods refresh_alv .

    class-methods msg_type2icon
      importing
        !iv_mstype     type symsgty
      returning
        value(rv_icon) type char250_d .

    class-methods string2table
      importing
        !iv_string      type string
        !iv_line_length type i default 132  ##NUMBER_OK
      returning
        value(rt_table) type htmltable .

    methods collect_data
      importing
        !it_logs    type zlso_tt_log_objects
        !ir_context type ref to data .

    methods display
      importing
        !io_cont_alv              type ref to cl_gui_container
        !io_cont_request_payload  type ref to cl_gui_container
        !io_cont_response_payload type ref to cl_gui_container
        !io_cont_url              type ref to cl_gui_container
        !io_cont_response_header  type ref to cl_gui_container
        !io_cont_request_header   type ref to cl_gui_container
        !io_main_splitter         type ref to cl_gui_splitter_container
        !io_bottom_splitter       type ref to cl_gui_splitter_container
        !io_request_splitter      type ref to cl_gui_splitter_container
        !io_response_splitter     type ref to cl_gui_splitter_container .

    methods handle_double_click
          for event double_click of cl_salv_events_table
      importing
          !row
          !column .

    methods on_user_command
          for event added_function of cl_salv_events
      importing
          !e_salv_function .

    "! <p class="shorttext synchronized" lang="en">Clears variable before exit</p>
    methods free.

  protected section.

  private section.

    data main_splitter   type ref to cl_gui_splitter_container .
    data bottom_splitter type ref to cl_gui_splitter_container .

    data: messages                    type zlso_tt_log_top_alv,
          messages_dynamic_columns    type ref to data,
          messages_alv                type ref to cl_salv_table,
          messages_container          type ref to cl_gui_container .

    data: url_html_viewer             type ref to cl_gui_html_viewer,
          url_container               type ref to cl_gui_container.

    data: trace_payload_viewer type ref to zcl_lso_trace_payload_viewer.

    data: request_header_container    type ref to cl_gui_container,
          request_header_html_viewer  type ref to cl_gui_html_viewer,
          request_payload_container   type ref to cl_gui_container,
          request_splitter            type ref to cl_gui_splitter_container.

    data: response_header_container   type ref to cl_gui_container,
          response_header_html_viewer type ref to cl_gui_html_viewer,
          response_payload_container  type ref to cl_gui_container,
          response_splitter           type ref to cl_gui_splitter_container.


    data show_hide_details_status type i value 0 ##NO_TEXT.



    methods show_msg_long_txt .

    methods setup_alv_columns .

    methods create_dynamic_alv_table
      importing
        !ir_context type ref to data .

    methods display_payloads
      importing
        !row type zlso_s_log_top_alv .

    methods display_url
      importing
        !is_alv_row type zlso_s_log_top_alv .

    methods display_headers
      importing
        !is_alv_row type zlso_s_log_top_alv .

    methods format_header
      importing
        !header_fields         type tihttpnvp
      returning
        value(formated_header) type string .

    methods show_url_and_headers .

    methods collect_log_data
      importing
        !log    type ref to zcl_lso_log
        !parent type ref to zcl_lso_log optional .

    methods jump_to_log_stripper
      raising
        zcx_lso_log .

    methods export .

    methods get_selected_message
      returning
        value(selected) type zlso_s_log_top_alv .

endclass.



class zcl_lso_trace_display implementation.


  method collect_data.

    data top_alv like line of messages.

    clear me->messages.

    loop at it_logs assigning field-symbol(<log>).
      me->collect_log_data( <log>-instance ).
    endloop.

    sort me->messages by timestamp.

    " create dynamic alv data table
    me->create_dynamic_alv_table( ir_context ).

  endmethod.


  method collect_log_data.

    if not log->zif_lso_log~has_messages( abap_false ) and not log->zif_lso_log~has_ref_logs( ).
      " There is neither reference log nor messages!
      return.
    endif.

    if log->zif_lso_log~has_ref_logs( ) eq abap_true.
      data(iterator) = log->zif_lso_log~get_ref_logs( )->get_iterator( ).

      while iterator->has_next( ).
        " Collect data for the reference log recursively.
        me->collect_log_data( log    = cast zcl_lso_log( iterator->get_next( ) )
                              parent = log ).
      endwhile.
    endif.

    data(top_alv) = value zlso_s_log_top_alv( ).

    if parent is bound.
      top_alv-id        = parent->zif_lso_log~get_id( ).
      top_alv-seqnr     = parent->zif_lso_log~get_seqnr( ).
      top_alv-ref_id    = log->zif_lso_log~get_id( ).
      top_alv-ref_seqnr = log->zif_lso_log~get_seqnr( ).
    else.
      top_alv-id    = log->zif_lso_log~get_id( ).
      top_alv-seqnr = log->zif_lso_log~get_seqnr( ).
    endif.

    top_alv-changed_by = log->zif_lso_log~get_changed_by( ).
    top_alv-context    = log->zif_lso_log~get_context( ).
    top_alv-prog       = log->zif_lso_log_abstract~get_program( ).
    top_alv-tcode      = log->zif_lso_log_abstract~get_tcode( ).
    top_alv-instance   = log.
    top_alv-rowcolors  = value #( ( color-col = col_normal ) ).

    " Take into account only messages from the log! Reference log messages will be processed recursively.
    data(messages_iterator) = log->zif_lso_log~get_messages( abap_false )->get_iterator( ).

    while messages_iterator->has_next( ).
      data(message) = cast zcl_lso_log_message( messages_iterator->get_next( ) ).

      top_alv-msgty            = message->get_symsg( )-msgty.
      top_alv-msgid            = message->get_symsg( )-msgid.
      top_alv-msgno            = message->get_symsg( )-msgno.
      top_alv-msg              = message->get_text( ).
      top_alv-timestamp        = message->get_timestamp( ).
      top_alv-date             = message->get_date( ).
      top_alv-time             = message->get_time( ).
      top_alv-abap_program     = message->zif_lso_log_message~get_abap_stack( )-abap_program.
      top_alv-abap_include     = message->zif_lso_log_message~get_abap_stack( )-abap_include.
      top_alv-abap_source_line = message->zif_lso_log_message~get_abap_stack( )-abap_source_line.
      top_alv-abap_event       = message->zif_lso_log_message~get_abap_stack( )-abap_event.
      top_alv-type_icon        = me->msg_type2icon( message->get_type( ) ).

      if message->zif_lso_log_message~has_trace( ).
        top_alv-http_status    = message->get_trace( )->get_http_status( ).
        top_alv-request_url    = message->get_trace( )->get_request_url( ).
        top_alv-request_method = message->get_trace( )->get_request_method( ).
        top_alv-trace          = message->get_trace( ).
      elseif message->zif_lso_log_message~is_stripped( ).
        " EDU-6919 - add info about stripped trace.
        top_alv-request_url = |Stripped on { message->zif_lso_log_message~get_stripped_date( ) date = environment }|.
        clear top_alv-http_status.
        clear top_alv-request_method.
        clear top_alv-trace.
      else.
        clear top_alv-http_status.
        clear top_alv-request_url.
        clear top_alv-request_method.
        clear top_alv-trace.
      endif.

      " Check if message is reference log message or not
      if message->get_log_id( ) eq top_alv-id.
        " Message belongs to log entry
        top_alv-log_icon = '@CS@'.
      else.
        " Message belongs to reference log
        top_alv-log_icon = '@A2@'.
      endif.

      append top_alv to me->messages.
    endwhile.
  endmethod.


  method create_dynamic_alv_table.

    " add additional context data
    field-symbols: <contexts> type any table.
    assign ir_context->* to <contexts>.

    if <contexts> is not assigned.
      get reference of me->messages into me->messages_dynamic_columns.
    else.

      data:
        tab_new    type ref to cl_abap_tabledescr,
        tab_old    type ref to cl_abap_tabledescr,
        strucdescr type ref to cl_abap_structdescr,
        typedescr  type ref to cl_abap_typedescr.

      data: line_type type ref to cl_abap_structdescr. " Rowtype ref to RTTS
      data: tabletype type ref to cl_abap_tabledescr. " Internal table type ref to RTTS


      " describe base/constant alv structure
      line_type ?= cl_abap_typedescr=>describe_by_name( 'ZLSO_S_LOG_TOP_ALV' ).
      "add additional fields
      data(components) = line_type->get_components( ). " existing components

      " additional components
      tab_new    ?= cl_abap_typedescr=>describe_by_data( <contexts> ).
      strucdescr ?= tab_new->get_table_line_type( ).
      data(add_components) = strucdescr->get_components( ). " additional components

      delete add_components where name = 'LOG_ID' or name = 'LOG_SEQNR'.

      append lines of add_components to components.

      " create line type based on lt_components
      line_type ?= cl_abap_structdescr=>create( p_components = components ).

      tabletype ?= cl_abap_tabledescr=>create( p_line_type = line_type ).
      create data me->messages_dynamic_columns type handle tabletype.

      field-symbols: <newtab> type any table.
      assign me->messages_dynamic_columns->* to <newtab>.
      move-corresponding me->messages to <newtab>.

      " move additional context data.
      data(field_id)     = '<NEW>-ID'.
      data(field_log_id) = '<CONTEXT>-LOG_ID'.

      loop at <newtab> assigning field-symbol(<new>).
        assign (field_id) to field-symbol(<field_id>).

        loop at <contexts> assigning field-symbol(<context>).
          assign (field_log_id) to field-symbol(<field_log_id>).

          if <field_id> = <field_log_id>.
            move-corresponding <context> to <new>.
            exit.
          endif.
        endloop.

      endloop.

    endif.

  endmethod.


  method display.

    data: ls_layout_key type salv_s_layout_key.

    field-symbols: <ls_alv_table> type any table.
    assign me->messages_dynamic_columns->* to <ls_alv_table>.

    if me->messages_alv is bound.
      me->refresh_alv( ).
      return.
    endif.

    " create top ALV
    try.
        cl_salv_table=>factory(
              exporting r_container  = io_cont_alv
              importing r_salv_table = me->messages_alv
              changing  t_table      = <ls_alv_table> ).

        me->messages_container         = io_cont_alv.
        me->request_payload_container  = io_cont_request_payload.
        me->response_payload_container = io_cont_response_payload.
        me->url_container              = io_cont_url.
        me->response_header_container  = io_cont_response_header.
        me->request_header_container   = io_cont_request_header.
        me->main_splitter              = io_main_splitter.
        me->bottom_splitter            = io_bottom_splitter.
        me->request_splitter           = io_request_splitter.
        me->response_splitter          = io_response_splitter.

        data(functions) = me->messages_alv->get_functions( ).
        functions->set_default( if_salv_c_bool_sap=>true ).
        functions->set_print( if_salv_c_bool_sap=>false ).
        functions->set_group_aggregation( if_salv_c_bool_sap=>false ).
        functions->set_abc_analysis( if_salv_c_bool_sap=>false ).

        " Add 'Where Used'
        functions->add_function( name     = c_action-where_used
                                 icon     = '@3A@' " Run the RSTXICON report to get icon code
                                 text     = 'Where Used' ##NO_TEXT
                                 tooltip  = 'Show Where Used List in Source Code'  ##NO_TEXT
                                 position = if_salv_c_function_position=>right_of_salv_functions ).

        " Add 'Show URL button'
        functions->add_function( name     = c_action-show_hide_details
                                 icon     = '@8S@' " URL Icon
                                 text     = 'Show/Hide Details'  ##NO_TEXT
                                 tooltip  = 'Show/Hide Details'  ##NO_TEXT
                                 position = if_salv_c_function_position=>right_of_salv_functions ).

        try.
            " Jump to the Log Stripper transaction...
            functions->add_function( name     = c_action-jump_to_stripper
                                     icon     = '@11@' " ICON_DELETE
                                     text     = 'Log Stripper' ##NO_TEXT
                                     tooltip  = 'Log Stripper' ##NO_TEXT
                                     position = if_salv_c_function_position=>right_of_salv_functions ).
          catch cx_salv_existing
                cx_salv_wrong_call.
        endtry.


        " Add 'Export' Function
        functions->add_function( name     = c_action-export
                                 icon     = '@J2@' " Icon Excel Sheet
                                 text     = 'Export'  ##NO_TEXT
                                 tooltip  = 'Export'  ##NO_TEXT
                                 position = if_salv_c_function_position=>right_of_salv_functions ).


        try.
            " Show Long Msg Text
            functions->add_function( name     = c_action-msg_long_txt
                                     icon     = '@G2@' " ICON_DELETE
                                     text     = 'Long Message Text' ##NO_TEXT
                                     tooltip  = 'Long Message Text' ##NO_TEXT
                                     position = if_salv_c_function_position=>right_of_salv_functions ).
          catch cx_salv_existing
                cx_salv_wrong_call.
        endtry.


        set handler me->on_user_command for messages_alv->get_event( ).


        ls_layout_key-report = sy-repid.
        data(layout) = me->messages_alv->get_layout( ).
        layout->set_key( ls_layout_key ).
        layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
        layout->set_initial_layout( 'DEFAULT' ).

        data(functional_settings) = me->messages_alv->get_functional_settings( ).
        data(tooltips) = functional_settings->get_tooltips( ).

        tooltips->add_tooltip( type       = cl_salv_tooltip=>c_type_icon
                               value      = '@CS@'
                               tooltip    = 'Message belongs to log entry' ).

        tooltips->add_tooltip( type       = cl_salv_tooltip=>c_type_icon
                               value      = '@A2@'
                               tooltip    = 'Message belongs to reference log' ).

        " setup ALV columns
        me->setup_alv_columns( ).

        set handler me->handle_double_click for me->messages_alv->get_event( ).

        me->messages_alv->display( ).

        " Display payload of first message...
        if lines( messages ) > 0.
          data(selected_row) = me->messages[ 1 ].
          me->display_payloads( selected_row ).
          me->display_url(      selected_row ).
          me->display_headers(  selected_row ).
          me->show_url_and_headers(  ).
        endif.

      catch cx_root into data(lx_root).
        write: / lx_root->get_longtext( ).
    endtry.

  endmethod.


  method display_headers.

    data: local_content_url(100) type c.
    data: header_fields_as_a_string type string.

    if me->request_header_html_viewer is not bound.
      " Init req header html viewer
      create object me->request_header_html_viewer
        exporting
          parent             = request_header_container
        exceptions
          cntl_error         = 1
          cntl_install_error = 2
          dp_install_error   = 3
          dp_error           = 4.
      if sy-subrc ne 0.
        message 'Req Header Html View Initialization Error!'
          type zif_lso_log_message=>c_type-error.
      endif.
    endif.

    clear header_fields_as_a_string.

    if is_alv_row-trace is bound.
      if is_alv_row-trace->zif_lso_log_trace~has_request_headers( ).
        data(header_fields) = is_alv_row-trace->get_request_headers( )->get_headers( ).
        header_fields_as_a_string = format_header( header_fields ).
      elseif is_alv_row-trace->zif_lso_log_trace~is_stripped( ).
        header_fields_as_a_string = |Stripped on { is_alv_row-trace->zif_lso_log_trace~get_stripped_date( ) date = environment }|.
      endif.
    endif.

    data(view_content) = zcl_lso_trace_display=>string2table( header_fields_as_a_string ).

    me->request_header_html_viewer->load_data(
      exporting
        type                   = 'text'
        subtype                = 'plane'
      importing
        assigned_url           = local_content_url
      changing
        data_table             = view_content
      exceptions
        dp_invalid_parameter   = 1
        dp_error_general       = 2
        cntl_error             = 3
        html_syntax_notcorrect = 4
        others                 = 5 ).
    if sy-subrc <> 0.
      message |Loading Req Header Data Error - { sy-subrc }!|
        type zif_lso_log_message=>c_type-error.
    endif.

    me->request_header_html_viewer->show_url(
         exporting
           url                    = local_content_url
         exceptions
           cntl_error             = 1
           cnht_error_not_allowed = 2
           cnht_error_parameter   = 3
           dp_error_general       = 4
           others                 = 5 ).
    if sy-subrc <> 0.
      message |Req Header View Couldn't Be Displayed - { sy-subrc }!|
        type zif_lso_log_message=>c_type-error.
    endif.

    " and now response part
*    if me->mo_header_resp_html_view is bound.
*      me->mo_header_resp_html_view->free(  ).
*    endif.

    if me->response_header_html_viewer is not bound.
      " Init resp header html viewer
      create object me->response_header_html_viewer
        exporting
          parent             = response_header_container
        exceptions
          cntl_error         = 1
          cntl_install_error = 2
          dp_install_error   = 3
          dp_error           = 4.
      if sy-subrc ne 0.
        message 'Resp. Header Html View Initialization Error!'
          type zif_lso_log_message=>c_type-error.
      endif.
    endif.

    clear header_fields_as_a_string.

    if is_alv_row-trace is bound.
      if is_alv_row-trace->zif_lso_log_trace~has_response_headers( ).
        header_fields = is_alv_row-trace->get_response_headers( )->get_headers( ).
        header_fields_as_a_string = format_header( header_fields ).
      elseif is_alv_row-trace->zif_lso_log_trace~is_stripped( ).
        header_fields_as_a_string = |Stripped on { is_alv_row-trace->zif_lso_log_trace~get_stripped_date( ) date = environment }|.
      endif.
    endif.

    view_content = zcl_lso_trace_display=>string2table( header_fields_as_a_string ).

    me->response_header_html_viewer->load_data(
      exporting
        type                   = 'text'
        subtype                = 'plane'
      importing
        assigned_url           = local_content_url
      changing
        data_table             = view_content
      exceptions
        dp_invalid_parameter   = 1
        dp_error_general       = 2
        cntl_error             = 3
        html_syntax_notcorrect = 4
        others                 = 5 ).
    if sy-subrc <> 0.
      message |Loading Resp. Header Data Error - { sy-subrc }!|
        type zif_lso_log_message=>c_type-error.
    endif.

    me->response_header_html_viewer->show_url(
         exporting
           url                    = local_content_url
         exceptions
           cntl_error             = 1
           cnht_error_not_allowed = 2
           cnht_error_parameter   = 3
           dp_error_general       = 4
           others                 = 5 ).
    if sy-subrc <> 0.
      message |Resp Header View Couldn't Be Displayed - { sy-subrc }!|
        type zif_lso_log_message=>c_type-error.
    endif.


  endmethod.


  method display_payloads.

    data url(100) type c.
    data mime_type type string.


    if me->trace_payload_viewer is not bound.
      me->trace_payload_viewer = new #( request  = me->request_payload_container
                                        response = me->response_payload_container ).
    endif.

    me->trace_payload_viewer->show( row-trace ).

  endmethod.


  method display_url.

    data: local_content_url(100) type c.

    if me->url_html_viewer is not bound.
      " Init url html viewer
      create object me->url_html_viewer
        exporting
          parent             = url_container
          uiflag             = space
        exceptions
          cntl_error         = 1
          cntl_install_error = 2
          dp_install_error   = 3
          dp_error           = 4.
      if sy-subrc ne 0.
        message 'Url Html View Initialization Error!'
          type zif_lso_log_message=>c_type-error.
      endif.
    endif.

    data(url_view_content) = zcl_lso_trace_display=>string2table( |<html><head><style>body \{font-family: "Courier New"; font-size: small;\}</style></head><body>{ is_alv_row-request_url }</body></html>| ).

    me->url_html_viewer->load_data(
      exporting
        type                   = 'text'
        subtype                = 'html'
      importing
        assigned_url           = local_content_url
      changing
        data_table             = url_view_content
      exceptions
        dp_invalid_parameter   = 1
        dp_error_general       = 2
        cntl_error             = 3
        html_syntax_notcorrect = 4
        others                 = 5 ).
    if sy-subrc <> 0.
      message |Loading URL Data Error - { sy-subrc }!|
        type zif_lso_log_message=>c_type-error.
    endif.

    me->url_html_viewer->show_url(
         exporting
           url                    = local_content_url
         exceptions
           cntl_error             = 1
           cnht_error_not_allowed = 2
           cnht_error_parameter   = 3
           dp_error_general       = 4
           others                 = 5 ).
    if sy-subrc <> 0.
      message |URL View Couldn't Be Displayed - { sy-subrc }!|
        type zif_lso_log_message=>c_type-error.
    endif.

  endmethod.


  method export.

    data(export_rows) = me->messages.

    " Move data to export structure and add payloads
    loop at export_rows assigning field-symbol(<export>).
      if <export>-trace is bound.
        if <export>-trace->has_request_payload( ).
          <export>-request_payload = <export>-trace->get_request_payload( )->get_formatted_payload( ).
        endif.
        if <export>-trace->has_response_payload( ).
          <export>-response_payload = <export>-trace->get_response_payload( )->get_formatted_payload( ).
        endif.
      endif.
    endloop.

    try.
        " Remove any reference columns... (class instance / interface)
        " Because excel cannot handle it
        data(export_stripped) = new zcl_lso_table_sculptor( ref #( export_rows ) )->remove_reference_columns( ).
        field-symbols <export_stripped> type any table.
        assign export_stripped->* to <export_stripped>.

        " Create Excel and bind table
        data(excel) = new zcl_excel( ).
        excel->add_new_worksheet( conv #( |Messages| ) )->bind_table( <export_stripped> ).

      catch zcx_excel into data(x_excel).
        message |Excel for download could not be created: { x_excel->get_longtext( ) }|
           type zif_lso_log_message=>c_type-error.
    endtry.

    " Prepare output
    data(filename) = value string( ).
    data(path)     = value string( ).
    data(fullpath) = value string( ).

    data(writer)     = cast zif_excel_writer( new zcl_excel_writer_2007( ) ).
    data(xdata)      = writer->write_file( excel ).
    data(byte_count) = xstrlen( xdata ).
    data(rawdata)    = cl_bcs_convert=>xstring_to_solix( iv_xstring = xdata ).

    " Get file details.
    cl_gui_frontend_services=>file_save_dialog(
      exporting
        default_extension         = 'xlsx'
        default_file_name         = |logs{ sy-datum }-{ sy-uzeit }.xlsx|
      changing
        filename                  = filename
        path                      = path
        fullpath                  = fullpath
      exceptions
        cntl_error                = 1
        error_no_gui              = 2
        not_supported_by_gui      = 3
        invalid_default_file_name = 4
        others                    = 5 ).

    if sy-subrc eq 0 and fullpath is not initial.
      cl_gui_frontend_services=>gui_download(
        exporting
          bin_filesize              = byte_count
          filename                  = fullpath
          filetype                  = 'BIN'
        changing
          data_tab                  = rawdata
        exceptions
          file_write_error          = 1
          no_batch                  = 2
          gui_refuse_filetransfer   = 3
          invalid_type              = 4
          no_authority              = 5
          unknown_error             = 6
          header_not_allowed        = 7
          separator_not_allowed     = 8
          filesize_not_allowed      = 9
          header_too_long           = 10
          dp_error_create           = 11
          dp_error_send             = 12
          dp_error_write            = 13
          unknown_dp_error          = 14
          access_denied             = 15
          dp_out_of_memory          = 16
          disk_full                 = 17
          dp_timeout                = 18
          file_not_found            = 19
          dataprovider_exception    = 20
          control_flush_error       = 21
          not_supported_by_gui      = 22
          error_no_gui              = 23
          others                    = 24 ).

      if sy-subrc <> 0.
        message |Excell download error - { sy-subrc }|
          type zif_lso_log_message=>c_type-error.
      endif.
    endif.

  endmethod.


  method format_header.
    clear formated_header.
    loop at header_fields assigning field-symbol(<header_field>).
      formated_header = |{ formated_header }{ cl_abap_char_utilities=>newline }{ <header_field>-name }:{ <header_field>-value } |.
    endloop.
  endmethod.


  method free.
    " Clear variables before exit.
    clear me->messages_alv.

    if me->trace_payload_viewer is bound.
      me->trace_payload_viewer->free( ).
      clear me->trace_payload_viewer.
    endif.

    if me->request_header_html_viewer is bound.
      me->request_header_html_viewer->free( ).
      clear me->request_header_html_viewer.
    endif.

    if me->response_header_html_viewer is bound.
      me->response_header_html_viewer->free( ).
      clear me->response_header_html_viewer.
    endif.

    if me->url_html_viewer is bound.
      me->url_html_viewer->free( ).
      clear me->url_html_viewer.
    endif.
  endmethod.


  method get_selected_message.

    "Get selected row
    data(selections) = messages_alv->get_selections( ).
    data(rows) = selections->get_selected_rows( ).
    data(row) = rows[ 1 ] .
    selected = messages[ row ].

  endmethod.


  method handle_double_click.
    if row is not initial.
      data(ls_sel_row) = me->messages[ row ].
      me->display_payloads( ls_sel_row ).
      me->display_url( ls_sel_row ).
      me->display_headers( ls_sel_row ).
    endif.
  endmethod.


  method jump_to_log_stripper.
    new zcl_lso_log_stripper( )->execute( ).
  endmethod.


  method msg_type2icon.

    case iv_mstype.
        " ERROR
      when zif_lso_log_message=>c_type-error.
        call function 'ICON_CREATE'
          exporting
            name   = icon_led_red
            info   = ''
          importing
            result = rv_icon.

        " WARNING
      when zif_lso_log_message=>c_type-warning.
        "rv_icon = '@08@'.
        call function 'ICON_CREATE'
          exporting
            name   = icon_led_yellow
            info   = ''
          importing
            result = rv_icon.

        " SUCCESS
      when zif_lso_log_message=>c_type-success.
        call function 'ICON_CREATE'
          exporting
            name   = icon_led_green
            info   = ''
          importing
            result = rv_icon.

        " INFO
      when zif_lso_log_message=>c_type-info.
        call function 'ICON_CREATE'
          exporting
            name   = icon_message_information_small
            info   = ''
          importing
            result = rv_icon.

        " ABORT
      when zif_lso_log_message=>c_type-abort.
        call function 'ICON_CREATE'
          exporting
            name   = icon_message_critical_small
            info   = ''
          importing
            result = rv_icon.

      when others.
        rv_icon = space.
    endcase.

  endmethod.


  method on_user_command.

    try.
        case e_salv_function.

            " Show message class and number in where used list
          when c_action-where_used.
            data(selected) = get_selected_message( ).
            zcl_lso_log_utils=>show_message_in_code( iv_class  = selected-msgid
                                                     iv_number = conv #( selected-msgno ) ).
          when c_action-show_hide_details.
            if me->show_hide_details_status = 0.
              me->show_hide_details_status = 1.
            else.
              me->show_hide_details_status = 0.
            endif.
            show_url_and_headers(  ).

          when c_action-jump_to_stripper.
            me->jump_to_log_stripper( ).


          when c_action-export.
            me->export( ).

          when c_action-msg_long_txt.
            me->show_msg_long_txt( ).

          when others.
        endcase.

      catch cx_root into data(x).
        message x->get_text( ) type zif_lso_log_message=>c_type-success
          display like zif_lso_log_message=>c_type-error.
    endtry.

  endmethod.


  method refresh_alv.

    try.
        field-symbols: <ls_alv_table> type any table.
        assign me->messages_dynamic_columns->* to <ls_alv_table>.

        me->messages_alv->set_data( changing t_table = <ls_alv_table> ).
        me->setup_alv_columns( ).
        me->messages_alv->refresh( refresh_mode = if_salv_c_refresh=>full
                                 s_stable = value lvc_s_stbl( row = abap_true col = abap_true ) ).
        cl_gui_cfw=>flush( ).

      catch cx_salv_no_new_data_allowed .

    endtry.

  endmethod.


  method setup_alv_columns.

    if me->messages_alv is not bound.
      return.
    endif.

    try.

        data(columns) = me->messages_alv->get_columns( ).
        columns->set_optimize( abap_true ).
        columns->set_color_column( 'ROWCOLORS' ).

        data(column) = columns->get_column( 'LOG_ICON' ).
        columns->set_column_position( columnname = 'LOG_ICON' position = 1 ).

        data(column_table) = cast cl_salv_column_table( column ).
        column_table->set_icon( if_salv_c_bool_sap=>true ).

        column = columns->get_column( 'DATE' ).
        columns->set_column_position( columnname = 'DATE' position = 2 ).

        column = columns->get_column( 'TIME' ).
        columns->set_column_position( columnname = 'TIME' position = 3 ).
        column->set_short_text( 'Time'(s02) ).
        column->set_medium_text('Time'(m02)  ).
        column->set_long_text( column->get_medium_text( ) ).
        column->set_alignment( ).

        column = columns->get_column( 'TYPE_ICON' ).
        columns->set_column_position( columnname = 'TYPE_ICON' position = 4 ).
        column->set_short_text( 'Type'(s09) ).
        column_table = cast cl_salv_column_table( column ).
        column_table->set_icon( if_salv_c_bool_sap=>true ).

        column = columns->get_column( 'MSG' ).
        columns->set_column_position( columnname = 'MSG' position = 5 ).
        column->set_short_text( 'Messages'(s01) ).
        column->set_medium_text('Messages'(m01)  ).
        column->set_long_text( column->get_medium_text( ) ).
        column->set_alignment( ).

        column = columns->get_column( 'REQUEST_METHOD' ).
        columns->set_column_position( columnname = 'REQUEST_METHOD' position = 10 ).
        column->set_short_text( 'Req. Met.'(s05) ).
        column->set_medium_text('Req. Method'(m05)  ).
        column->set_long_text( column->get_medium_text( ) ).
        column->set_alignment( ).

        column = columns->get_column( 'HTTP_STATUS' ).
        columns->set_column_position( columnname = 'HTTP_STATUS' position = 11 ) ##NUMBER_OK.
        column->set_short_text( 'HTTP RC'(s03) ).
        column->set_medium_text('HTTP RC'(m03)  ).
        column->set_long_text( column->get_medium_text( ) ).
        column->set_alignment( ).
        column->set_zero( abap_false ).

        column = columns->get_column( 'REQUEST_URL' ).
        columns->set_column_position( columnname = 'REQUEST_URL' position = 12 ) ##NUMBER_OK.
        column->set_short_text( 'Req. URL'(s04) ).
        column->set_medium_text('Request URL'(m04)  ).
        column->set_long_text( column->get_medium_text( ) ).
        column->set_alignment( ).

        columns->get_column( 'ID'         )->set_visible( space ).
        columns->get_column( 'TIMESTAMP'  )->set_visible( space ).
        columns->get_column( 'MSGTY'      )->set_visible( space ).
        columns->get_column( 'MSGID'      )->set_visible( space ).
        columns->get_column( 'MSGNO'      )->set_visible( space ).
        columns->get_column( 'SEQNR'      )->set_visible( space ).
        columns->get_column( 'REF_ID'     )->set_visible( space ).
        columns->get_column( 'REF_SEQNR'  )->set_visible( space ).

        columns->get_column( 'INSTANCE'         )->set_technical( abap_true ).
        columns->get_column( 'TRACE'            )->set_technical( abap_true ).
        columns->get_column( 'REQUEST_PAYLOAD'  )->set_technical( abap_true ).
        columns->get_column( 'RESPONSE_PAYLOAD' )->set_technical( abap_true ).

        column = columns->get_column( 'REF_ID' ).
        column->set_short_text( 'Ref. ID'(s06) ).
        column->set_medium_text('Ref. Log ID'(m06)  ).
        column->set_long_text( column->get_medium_text( ) ).
        column->set_alignment( ).

        column = columns->get_column( 'REF_SEQNR' ).
        column->set_short_text( 'Ref. SEQNR'(s07) ).
        column->set_medium_text('Ref. Log SEQNR'(m07)  ).
        column->set_long_text( column->get_medium_text( ) ).
        column->set_alignment( ).

        column = columns->get_column( 'ABAP_PROGRAM' ).
        column->set_short_text( 'ABAP Prog.'(s10) ).
        column->set_medium_text('ABAP Program'(m10)  ).
        column->set_long_text( column->get_medium_text( ) ).
        column->set_visible( if_salv_c_bool_sap=>false ).

        column = columns->get_column( 'ABAP_INCLUDE' ).
        column->set_short_text( 'ABAP Incl.'(s11) ).
        column->set_medium_text( 'ABAP Include'(m11) ).
        column->set_long_text( column->get_medium_text( ) ).
        column->set_visible( if_salv_c_bool_sap=>false ).

        column = columns->get_column( 'ABAP_SOURCE_LINE' ).
        column->set_short_text( 'ABAP Line'(s12) ).
        column->set_medium_text( 'ABAP Source Line'(m12) ).
        column->set_long_text( column->get_medium_text( ) ).
        column->set_visible( if_salv_c_bool_sap=>false ).

        column = columns->get_column( 'ABAP_EVENT' ).
        column->set_short_text( 'ABAP Event'(s13) ).
        column->set_medium_text( column->get_short_text( ) ).
        column->set_long_text( column->get_medium_text( ) ).
        column->set_visible( if_salv_c_bool_sap=>false ).

      catch cx_salv_not_found into data(x_salv_not_found).
        write: / x_salv_not_found->get_longtext( ).

      catch cx_salv_msg into data(x_salv_msg).
        write: / x_salv_msg->get_longtext( ).

      catch cx_salv_data_error  into data(x_salv_data_error).
        write: / x_salv_msg->get_longtext( ).

      catch cx_sy_move_cast_error into data(x_move_cast_error). "#EC NO_HANDLER
        write: / x_move_cast_error->get_longtext( ).
    endtry.

  endmethod.


  method show_msg_long_txt.
    data(selected) = get_selected_message( ).

    zcl_lso_cor_utility=>show_message_longtext(
                                              exporting
                                                iv_message_class  = selected-msgid
                                                iv_message_number = conv #( selected-msgno ) ).



  endmethod.


  method show_url_and_headers.
    if me->show_hide_details_status = 0. " hide
      bottom_splitter->set_row_height( exporting id = 1 height = 0 ). " url
      request_splitter->set_row_height(  id = 1 height = 0 )." header
      response_splitter->set_row_height( id = 1 height = 0 )." header
    else.
      bottom_splitter->set_row_height( exporting id = 1 height = me->default_url_ctr_height ). " url
      request_splitter->set_row_height( id = 1 height = zcl_lso_trace_display=>default_header_ctr_height )." header
      response_splitter->set_row_height( id = 1 height = zcl_lso_trace_display=>default_header_ctr_height )." header
    endif.
  endmethod.


  method string2table.
    clear rt_table[].

    if iv_line_length > 0.
      data(lv_line_length) = iv_line_length.
    else.
      lv_line_length = 132  ##NUMBER_OK.
    endif.

    call function 'CONVERT_STRING_TO_TABLE'
      exporting
        i_string         = iv_string
        i_tabline_length = lv_line_length
      tables
        et_table         = rt_table.
  endmethod.
endclass.
