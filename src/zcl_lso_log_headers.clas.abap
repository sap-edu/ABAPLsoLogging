class zcl_lso_log_headers definition
  public
  final
  create public .

  public section.
    types tt_db_headers type sorted table of zlso_log_headers with unique key primary_key components trace_id type name.

    constants begin of c_type.
    constants request type zlso_log_payload-type value 'REQUEST'.
    constants response type zlso_log_payload-type value 'RESPONSE'.
    constants end of c_type .

    class-methods create_request
      importing trace_id        type zlso_log_payload-trace_id
                headers         type if_web_http_request=>name_value_pairs
      returning value(instance) type ref to zcl_lso_log_headers .

    class-methods create_response
      importing trace_id        type zlso_log_payload-trace_id
                headers         type if_web_http_request=>name_value_pairs
      returning value(instance) type ref to zcl_lso_log_headers .

    class-methods save_collection
      importing headers       type zlso_tt_log_headers
                with_commit   type abap_bool default abap_false
      returning value(result) type abap_bool
      raising   zcx_lso_log .

    methods constructor
      importing trace_id type zlso_log_payload-trace_id
                type     type zlso_log_payload-type
                headers  type if_web_http_request=>name_value_pairs.

    methods get_headers
      returning value(headers) type if_web_http_request=>name_value_pairs.

    methods get_trace_id
      returning value(trace_id) type zlso_log_payload-trace_id .

    methods get_type
      returning value(type) type zlso_log_payload-type .

    methods get_object
      returning value(object) type zlso_s_log_header.

    methods is_saved
      returning value(result) type abap_bool .

  protected section.
  private section.
    data saved type abap_bool.
    data headers type if_web_http_request=>name_value_pairs.
    data trace_id type zlso_log_headers-trace_id.
    data type type zlso_log_headers-type.

    methods set_saved
      importing saved type abap_bool default abap_true .
endclass.


class zcl_lso_log_headers implementation.

  method constructor.
    me->trace_id = trace_id.
    me->type = type.
    me->headers = headers.
  endmethod.


  method create_request.
    instance = new zcl_lso_log_headers( trace_id = trace_id
                                        type     = c_type-request
                                        headers  = headers ).
  endmethod.


  method create_response.
    instance = new zcl_lso_log_headers( trace_id = trace_id
                                        type     = c_type-response
                                        headers  = headers ).
  endmethod.


  method get_headers.
    headers = me->headers.
  endmethod.


  method get_trace_id.
    trace_id = me->trace_id.
  endmethod.


  method get_type.
    type = me->type.
  endmethod.


  method get_object.
    object = value #( trace_id = me->trace_id
                      type     = me->type
                      instance = me ).
  endmethod.


  method is_saved.
    result = me->saved.
  endmethod.


  method save_collection.
    data(db_headers) = value tt_db_headers( ).

    loop at headers using key object_key reference into data(object).
      data(header) = cast zcl_lso_log_headers( object->instance ).

      if header is not bound or header->is_saved( ).
        continue.
      endif.

      data(lt_headers) = header->get_headers( ).

      loop at lt_headers assigning field-symbol(<ls_header>).
        " Prepare header data table for saving.
        insert value #( trace_id = header->get_trace_id( )
                        type     = header->get_type( )
                        name     = <ls_header>-name
                        value    = <ls_header>-value ) into table db_headers.
      endloop.
    endloop.

    try.
        if db_headers[] is not initial.
          " Save trace header in DB.
          insert zlso_log_headers from table @db_headers accepting duplicate keys.
        endif.

        result = boolc( sy-dbcnt > 0 ).

        loop at headers using key object_key reference into object.
          header = cast zcl_lso_log_headers( object->instance ).

          header->set_saved( cond #( when header->get_trace_id( ) is initial then abap_false
                                     else result ) ).
        endloop.

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
            mv_msgv1          = 'ZLSO_LOG_HEADERS'
            mv_exception_text = lo_cx_sql->get_text( ).
    endtry.
  endmethod.


  method set_saved.
    me->saved = saved.
  endmethod.

endclass.
