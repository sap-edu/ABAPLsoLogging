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
    aliases get_object for zif_lso_log_trace~get_object.
    aliases has_headers for zif_lso_log_trace~has_headers.
    aliases has_payload for zif_lso_log_trace~has_payload.
    aliases has_request_headers for zif_lso_log_trace~has_request_headers.
    aliases has_request_payload for zif_lso_log_trace~has_request_payload.
    aliases has_response_headers for zif_lso_log_trace~has_response_headers.
    aliases has_response_payload for zif_lso_log_trace~has_response_payload.
    aliases is_stripped for zif_lso_log_trace~is_stripped.
    aliases set_http_status for zif_lso_log_trace~set_http_status.

    class-methods save_collection
      importing traces        type zlso_tt_log_traces
                with_commit   type abap_bool default abap_false
      returning value(result) type abap_bool
      raising   zcx_lso_log .

    methods constructor
      importing id               type zlso_log_trace-id optional
                request_url      type string
                request_method   type string
                http_status      type i
                request_payload  type string
                response_payload type string
                request_headers  type if_web_http_request=>name_value_pairs optional
                response_headers type if_web_http_request=>name_value_pairs optional .

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
      returning value(id) type zlso_log_trace-id .

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


  method create_id.
    try.
        " Number range created by the /EDU/CL_TRCNR_CREATE class.
        cl_numberrange_runtime=>number_get( exporting nr_range_nr = '01'
                                                      object      = '/EDU/TRCNR'
                                            importing number      = data(number) ).
        id = number.
      catch cx_nr_object_not_found cx_number_ranges.
        clear id.
    endtry.

    if id is initial.
      try.
          id = cl_system_uuid=>if_system_uuid_static~create_uuid_c32( ).
        catch cx_uuid_error.
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


  method zif_lso_log_trace~get_object.
    object = value #( id       = me->id
                      instance = me ).
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
    data(db_traces) = value zif_lso_log_trace=>tt_traces( ).
    data(payloads) = value zlso_tt_log_payloads( ).
    data(headers) = value zlso_tt_log_headers( ).

    loop at traces using key object_key reference into data(trace_object).
      data(trace) = cast zcl_lso_log_trace( trace_object->instance ).

      if trace is not bound or trace->is_saved( ).
        continue.
      endif.

      if trace->zif_lso_log_trace~get_id( ) is initial.
        " No ID for trace object!
        raise exception type zcx_lso_log exporting textid = zcx_lso_log=>no_trace_id.
      endif.

      " Check if unique payload and header objects have been already added to table.
      if not line_exists( payloads[ key object_key components trace_id = trace->zif_lso_log_trace~get_id( ) ] ).
        " No payload yet, add it to the table for saving.
        if trace->zif_lso_log_trace~has_request_payload( ).
          " Request...
          insert value #( trace_id = trace->zif_lso_log_trace~get_id( )
                          type     = trace->zif_lso_log_trace~get_request_payload( )->get_type( )
                          instance = trace->zif_lso_log_trace~get_request_payload( ) ) into table payloads.
        endif.

        if trace->zif_lso_log_trace~has_response_payload( ).
          " Response...
          insert value #( trace_id = trace->zif_lso_log_trace~get_id( )
                          type     = trace->zif_lso_log_trace~get_response_payload( )->get_type( )
                          instance = trace->zif_lso_log_trace~get_response_payload( ) ) into table payloads.
        endif.
      endif.

      if not line_exists( headers[ key object_key components trace_id = trace->zif_lso_log_trace~get_id( ) ] ).
        " No headers yet, add it to the table for saving.
        if trace->zif_lso_log_trace~has_request_headers( ).
          " Request...
          insert value #( trace_id = trace->zif_lso_log_trace~get_id( )
                          type     = trace->zif_lso_log_trace~get_request_headers( )->get_type( )
                          instance = trace->zif_lso_log_trace~get_request_headers( ) ) into table headers.
        endif.

        if trace->zif_lso_log_trace~has_response_headers( ).
          " Response...
          insert value #( trace_id = trace->zif_lso_log_trace~get_id( )
                          type     = trace->zif_lso_log_trace~get_response_headers( )->get_type( )
                          instance = trace->zif_lso_log_trace~get_response_headers( ) ) into table headers.
        endif.
      endif.

      " Prepare trace data table for saving.
      insert value #( id             = trace->zif_lso_log_trace~get_id( )
                      http_status    = trace->zif_lso_log_trace~get_http_status( )
                      request_url    = trace->zif_lso_log_trace~get_request_url( )
                      request_method = trace->zif_lso_log_trace~get_request_method( )
                      stripped_date  = trace->zif_lso_log_trace~get_stripped_date( ) ) into table db_traces.
    endloop.

    try.
        if payloads[] is not initial.
          " Save trace payload in DB.
          zcl_lso_log_payload=>save_collection( payloads ).
        endif.

        if headers[] is not initial.
          " Save trace payload in DB.
          zcl_lso_log_headers=>save_collection( headers ).
        endif.

        if db_traces[] is not initial.
          " Save traces in DB.
          insert zlso_log_trace from table @db_traces accepting duplicate keys.
        endif.

        result = boolc( sy-dbcnt > 0 ).

        loop at traces using key object_key reference into trace_object.
          trace = cast zcl_lso_log_trace( trace_object->instance ).

          trace->set_saved( cond #( when trace->zif_lso_log_trace~get_id( ) is initial then abap_false
                                    else result ) ).
        endloop.

        if with_commit eq abap_true.
          commit work.
        endif.
      catch cx_sy_open_sql_db into data(cx_sql).
        if with_commit eq abap_true.
          rollback work.
        endif.

        " Save problem, raise an exception.
        raise exception type zcx_lso_log
          exporting
            textid            = zcx_lso_log=>save_error
            mv_msgv1          = 'ZLSO_LOG_TRACE'
            mv_exception_text = cx_sql->get_text( ).
    endtry.
  endmethod.


  method set_saved.
    me->saved = saved.
  endmethod.


  method set_stripped_date.
    me->stripped_date = date.
  endmethod.

endclass.
