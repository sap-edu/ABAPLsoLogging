class zcl_lso_log_trace definition
  public
  final
  create public
  global friends zcl_lso_log_trace_factory.

  public section.
    interfaces zif_lso_log_trace.

    " Backward compatibility
    aliases get_http_status for zif_lso_log_trace~get_http_status.
    aliases get_id for zif_lso_log_trace~get_id.
    aliases get_request_method for zif_lso_log_trace~get_request_method.
    aliases get_request_headers for zif_lso_log_trace~get_request_headers.
    aliases get_request_payload for zif_lso_log_trace~get_request_payload.
    aliases get_request_url for zif_lso_log_trace~get_request_url.
    aliases get_stripped_date for zif_lso_log_trace~get_stripped_date.
    aliases get_response_headers for zif_lso_log_trace~get_response_headers.
    aliases get_response_payload for zif_lso_log_trace~get_response_payload.
    aliases has_headers for zif_lso_log_trace~has_headers.
    aliases has_payload for zif_lso_log_trace~has_payload.
    aliases has_request_headers for zif_lso_log_trace~has_request_headers.
    aliases has_request_payload for zif_lso_log_trace~has_request_payload.
    aliases has_response_headers for zif_lso_log_trace~has_response_headers.
    aliases has_response_payload for zif_lso_log_trace~has_response_payload.
    aliases is_stripped for zif_lso_log_trace~is_stripped.
    aliases set_http_status for zif_lso_log_trace~set_http_status.

    class-methods save_collection
      importing traces        type ref to if_object_collection
                with_commit   type abap_bool default abap_false
      returning value(result) type abap_bool
      raising   zcx_lso_log .

    class-methods conv_table_to_json
      importing input_tab       type any table
      returning value(json_str) type string .

    methods constructor
      importing id               type zlso_log_trace-id optional
                request_url      type string
                request_method   type string
                http_status      type i
                request_payload  type string
                response_payload type string
                request_headers  type tihttpnvp optional
                response_headers type tihttpnvp optional .

  protected section.
  private section.
    data id type zlso_log_trace-id .
    data http_status type zlso_log_trace-http_status .
    data request_method type zlso_log_trace-request_method .
    data request_url type zlso_log_trace-request_url .
    data stripped_date type zlso_log_trace-stripped_date .
    data saved type abap_bool .
    data request_payload type ref to zcl_lso_log_payload .
    data response_payload type ref to zcl_lso_log_payload .
    data request_headers type ref to zcl_lso_log_headers .
    data response_headers type ref to zcl_lso_log_headers .

    methods create_id
      returning value(next_id) type zlso_log_trace-id .

    methods is_saved
      returning value(result) type abap_bool .

    methods set_saved
      importing saved type abap_bool .

    methods set_stripped_date
      importing date type zlso_log_trace-stripped_date.
endclass.


class zcl_lso_log_trace implementation.

  method constructor.
    if id is not initial.
      me->id = id.
    else.
      me->id = me->create_id( ).
    endif.

    me->request_url = request_url.
    me->request_method = request_method.
    me->http_status = http_status.

    if request_payload is not initial.
      " Instantiate request payload object.
      me->request_payload = zcl_lso_log_payload=>create_request( trace_id = me->id
                                                                 payload  = request_payload ).
    endif.

    if response_payload is not initial.
      " Instantiate response payload object.
      me->response_payload	= zcl_lso_log_payload=>create_response( trace_id = me->id
                                                                    payload  = response_payload ).
    endif.

    if request_headers is not initial.
      " Instantiate request headers object.
      me->request_headers = zcl_lso_log_headers=>create_request( trace_id = me->id
                                                                 headers  = request_headers ).
    endif.

    if response_headers is not initial.
      " Instantiate request headers object.
      me->response_headers = zcl_lso_log_headers=>create_response( trace_id = me->id
                                                                   headers  = response_headers ).
    endif.
  endmethod.

  method conv_table_to_json.

    json_str = /ui2/cl_json=>serialize( data        = input_tab
                                        compress    = abap_true
                                        pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).
    data strings type table of string.
    split json_str at '","' into table strings.
    json_str = ||.
    loop at strings assigning field-symbol(<string>).
      if lines( strings ) = sy-tabix.
        json_str = |{ json_str }{ <string> }|.
      else.
        json_str = |{ json_str }{ <string> }",{ cl_abap_char_utilities=>newline }    "|.
      endif.
    endloop.

    replace all occurrences of '[{' in json_str with |[ \{ { cl_abap_char_utilities=>newline }    |.
    replace all occurrences of '}]' in json_str with |{ cl_abap_char_utilities=>newline } \} ] |.
    replace all occurrences of '},{' in json_str with |{ cl_abap_char_utilities=>newline }  \},\{ { cl_abap_char_utilities=>newline }    |.

  endmethod.

  method create_id.
    data(id) = value zlso_log_trace-id( ).

    call function 'NUMBER_GET_NEXT'
      exporting
        nr_range_nr             = '00'
        object                  = 'ZLSO_TRCID'
      importing
        number                  = id
      exceptions
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        others                  = 8.

    if sy-subrc eq 0 and id is not initial.
      next_id = |{ sy-sysid }{ id }|.
    else.
      try.
          " Use uuid if no range is not working.
          next_id = cl_system_uuid=>if_system_uuid_static~create_uuid_c22( ).
        catch cx_uuid_error.
          wait up to 1 seconds.
          retry.
      endtry.
    endif.
  endmethod.


  method zif_lso_log_trace~get_http_status.
    http_status = me->http_status.
  endmethod.


  method zif_lso_log_trace~get_id.
    id = me->id.
  endmethod.


  method zif_lso_log_trace~get_request_headers.
    headers = me->request_headers.
  endmethod.


  method zif_lso_log_trace~get_request_method.
    request_method = me->request_method.
  endmethod.


  method zif_lso_log_trace~get_request_payload.
    payload = me->request_payload.
  endmethod.


  method zif_lso_log_trace~get_request_url.
    request_url = me->request_url.
  endmethod.


  method zif_lso_log_trace~get_response_headers.
    headers = me->response_headers.
  endmethod.


  method zif_lso_log_trace~get_response_payload.
    payload = me->response_payload.
  endmethod.


  method zif_lso_log_trace~has_headers.
    result = boolc( me->request_headers is bound or me->response_headers is bound ).
  endmethod.


  method zif_lso_log_trace~has_payload.
    result = boolc( me->request_payload is bound or me->response_payload is bound ).
  endmethod.


  method zif_lso_log_trace~has_request_headers.
    result = boolc( me->request_headers is bound ).
  endmethod.


  method zif_lso_log_trace~has_request_payload.
    result = boolc( me->request_payload is bound ).
  endmethod.


  method zif_lso_log_trace~has_response_headers.
    result = boolc( me->response_headers is bound ).
  endmethod.


  method zif_lso_log_trace~has_response_payload.
    result = boolc( me->response_payload is bound ).
  endmethod.


  method zif_lso_log_trace~get_stripped_date.
    stripped_date = me->stripped_date.
  endmethod.


  method zif_lso_log_trace~is_stripped.
    result = boolc( me->stripped_date is not initial ).
  endmethod.


  method zif_lso_log_trace~set_http_status.
    me->http_status = http_status.
  endmethod.


  method is_saved.
    result = me->saved.
  endmethod.


  method save_collection.
    types begin of lty_s_payload.
    types trace_id type zlso_log_payload-trace_id.
    types type type zlso_log_payload-type.
    types instance type ref to zcl_lso_log_payload.
    types end of lty_s_payload.

    types lty_t_payload type sorted table of lty_s_payload with unique key trace_id type.

    types begin of lty_s_headers.
    types trace_id type zlso_log_headers-trace_id.
    types type type zlso_log_headers-type.
    types instance type ref to zcl_lso_log_headers.
    types end of lty_s_headers.

    types lty_t_headers type sorted table of lty_s_headers with unique key trace_id type.

    data(lt_traces)  = value zif_lso_log_trace=>tt_traces( ).
    data(lt_payload) = value lty_t_payload( ).
    data(lt_headers) = value lty_t_headers( ).

    data(iterator) = traces->get_iterator( ).

    while iterator->has_next( ) eq abap_true.
      data(trace) = cast zcl_lso_log_trace( iterator->get_next( ) ).

      if trace->is_saved( ) eq abap_true.
        continue.
      endif.

      if trace->zif_lso_log_trace~get_id( ) is initial.
        " No ID for trace object!
        raise exception type zcx_lso_log
          exporting
            textid = zcx_lso_log=>no_trace_id.
      endif.

      " Check if unique payload and header objects have been already added to table.
      if not line_exists( lt_payload[ trace_id = trace->zif_lso_log_trace~get_id( ) ] ).
        " No payload yet, add it to the table for saving.
        if trace->zif_lso_log_trace~has_request_payload( ).
          " Request...
          insert value lty_s_payload( trace_id = trace->zif_lso_log_trace~get_id( )
                                      type     = trace->zif_lso_log_trace~get_request_payload( )->get_type( )
                                      instance = trace->zif_lso_log_trace~get_request_payload( ) )
            into table lt_payload.
        endif.

        if trace->zif_lso_log_trace~has_response_payload( ).
          " Response...
          insert value lty_s_payload( trace_id = trace->zif_lso_log_trace~get_id( )
                                      type     = trace->zif_lso_log_trace~get_response_payload( )->get_type( )
                                      instance = trace->zif_lso_log_trace~get_response_payload( ) )
            into table lt_payload.
        endif.
      endif.

      if not line_exists( lt_headers[ trace_id = trace->zif_lso_log_trace~get_id( ) ] ).
        " No headers yet, add it to the table for saving.
        if trace->zif_lso_log_trace~has_request_headers( ).
          " Request...
          insert value lty_s_headers( trace_id = trace->zif_lso_log_trace~get_id( )
                                      type     = trace->zif_lso_log_trace~get_request_headers( )->get_type( )
                                      instance = trace->zif_lso_log_trace~get_request_headers( ) )
            into table lt_headers.
        endif.

        if trace->zif_lso_log_trace~has_response_headers( ).
          " Response...
          insert value lty_s_headers( trace_id = trace->zif_lso_log_trace~get_id( )
                                      type     = trace->zif_lso_log_trace~get_response_headers( )->get_type( )
                                      instance = trace->zif_lso_log_trace~get_response_headers( ) )
            into table lt_headers.
        endif.
      endif.

      " Prepare trace data table for saving.
      insert value zlso_log_trace( id             = trace->zif_lso_log_trace~get_id( )
                                   http_status    = trace->zif_lso_log_trace~get_http_status( )
                                   request_url    = trace->zif_lso_log_trace~get_request_url( )
                                   request_method = trace->zif_lso_log_trace~get_request_method( )
                                   stripped_date  = trace->zif_lso_log_trace~get_stripped_date( ) )
        into table lt_traces.
    endwhile.

    try.
        if lt_payload[] is not initial.
          data(lo_payloads) = new cl_object_collection( ).

          loop at lt_payload assigning field-symbol(<ls_payload>).
            lo_payloads->add( <ls_payload>-instance ).
          endloop.

          " Save trace payload in DB.
          zcl_lso_log_payload=>save_collection( lo_payloads ).
        endif.

        if lt_headers[] is not initial.
          data(lo_headers) = new cl_object_collection( ).

          loop at lt_headers assigning field-symbol(<ls_headers>).
            lo_headers->add( <ls_headers>-instance ).
          endloop.

          " Save trace payload in DB.
          zcl_lso_log_headers=>save_collection( lo_headers ).
        endif.

        if lt_traces[] is not initial.
          " Save traces in DB.
          insert zlso_log_trace from table @lt_traces accepting duplicate keys.
        endif.

        result = boolc( sy-dbcnt > 0 ).

        iterator = traces->get_iterator( ).

        while iterator->has_next( ) eq abap_true.
          trace = cast zcl_lso_log_trace( iterator->get_next( ) ).

          if trace->zif_lso_log_trace~get_id( ) is initial.
            trace->set_saved( abap_false ).
          elseif trace->is_saved( ) eq abap_false.
            trace->set_saved( result ).
          endif.
        endwhile.

        if with_commit eq abap_true.
          commit work.
        endif.
      catch cx_sy_open_sql_db into data(lo_cx_sql).
        if with_commit eq abap_true.
          rollback work.
        endif.

        " Save problem, raise an exception.
        raise exception type zcx_lso_log
          exporting
            textid            = zcx_lso_log=>save_error
            mv_msgv1          = 'ZLSO_LOG_TRACE'
            mv_exception_text = lo_cx_sql->get_text( ).
    endtry.
  endmethod.


  method set_saved.
    me->saved = saved.
  endmethod.


  method set_stripped_date.
    me->stripped_date = date.
  endmethod.

endclass.
