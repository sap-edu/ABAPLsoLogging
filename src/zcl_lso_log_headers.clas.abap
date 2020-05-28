class zcl_lso_log_headers definition
  public
  final
  create public .

  public section.

    constants:
      begin of c_type.
    constants request type zlso_log_payload-type value 'REQUEST'.
    constants response type zlso_log_payload-type value 'RESPONSE'.
    constants end of c_type .

    class-methods create_request
      importing trace_id        type zlso_log_payload-trace_id
                headers         type tihttpnvp
      returning value(instance) type ref to zcl_lso_log_headers .

    class-methods create_response
      importing trace_id        type zlso_log_payload-trace_id
                headers         type tihttpnvp
      returning value(instance) type ref to zcl_lso_log_headers .

    class-methods save_collection
      importing headers       type ref to if_object_collection
                with_commit   type abap_bool default abap_false
      returning value(result) type abap_bool
      raising   zcx_lso_log .

    methods constructor
      importing trace_id type zlso_log_payload-trace_id
                type     type zlso_log_payload-type
                headers  type tihttpnvp .

    methods get_headers
      returning value(headers) type tihttpnvp .

    methods get_trace_id
      returning value(trace_id) type zlso_log_payload-trace_id .

    methods get_type
      returning value(type) type zlso_log_payload-type .

    methods is_saved
      returning value(result) type abap_bool .

  protected section.
  private section.
    types tt_payload_lines type standard table of zlso_d_trace_payload with default key .

    data saved type abap_bool .
    data headers type tihttpnvp .
    data trace_id type zlso_log_headers-trace_id .
    data type type zlso_log_headers-type .

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


  method is_saved.
    result = me->saved.
  endmethod.


  method save_collection.
    data lt_db_headers type table of zlso_log_headers.

    data(lo_iterator) = headers->get_iterator( ).

    while lo_iterator->has_next( ) eq abap_true.
      data(lo_headers) = cast zcl_lso_log_headers( lo_iterator->get_next( ) ).

      if lo_headers->is_saved( ) eq abap_true.
        continue.
      endif.

      data(lt_headers) = lo_headers->get_headers( ).

      loop at lt_headers assigning field-symbol(<ls_header>).
*       Prepare header data table for saving.
        insert value zlso_log_headers(
            trace_id = lo_headers->get_trace_id( )
            type     = lo_headers->get_type( )
            name    = <ls_header>-name
            value   = <ls_header>-value )
          into table lt_db_headers.
      endloop.
    endwhile.

    try.
        if lt_db_headers[] is not initial.
*         Save trace header in DB.
          insert zlso_log_headers
            from table @lt_db_headers
            accepting duplicate keys.
        endif.

        result = boolc( sy-dbcnt > 0 ).

        lo_iterator = headers->get_iterator( ).

        while lo_iterator->has_next( ) eq abap_true.
          lo_headers = cast zcl_lso_log_headers( lo_iterator->get_next( ) ).

          if lo_headers->get_trace_id( ) is initial.
            lo_headers->set_saved( abap_false ).
          elseif lo_headers->is_saved( ) eq abap_false.
            lo_headers->set_saved( result ).
          endif.
        endwhile.

        if with_commit eq abap_true.
          commit work.
        endif.
      catch cx_sy_open_sql_db into data(lo_cx_sql).
        if with_commit eq abap_true.
          rollback work.
        endif.

*       Save problem, raise an exception.
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
