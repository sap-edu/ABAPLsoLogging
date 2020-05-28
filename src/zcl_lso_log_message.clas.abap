class zcl_lso_log_message definition
  public
  final
  create public
  global friends zcl_lso_log_message_builder
                 zcl_lso_log_message_factory.

  public section.
    interfaces zif_lso_log_message.

    " Backwards compatibility
    aliases get_data for zif_lso_log_message~get_data.
    aliases get_date for zif_lso_log_message~get_date.
    aliases get_log_id for zif_lso_log_message~get_log_id.
    aliases get_log_seqnr for zif_lso_log_message~get_log_seqnr.
    aliases get_symsg for zif_lso_log_message~get_symsg.
    aliases get_text for zif_lso_log_message~get_text.
    aliases get_time for zif_lso_log_message~get_time.
    aliases get_timestamp for zif_lso_log_message~get_timestamp.
    aliases get_trace for zif_lso_log_message~get_trace.
    aliases get_request_payload for zif_lso_log_message~get_request_payload.
    aliases get_response_payload for zif_lso_log_message~get_response_payload.
    aliases get_type for zif_lso_log_message~get_type.
    aliases get_class for zif_lso_log_message~get_class.
    aliases get_number for zif_lso_log_message~get_number.
    aliases get_abap_stack for zif_lso_log_message~get_abap_stack.
    aliases has_trace for zif_lso_log_message~has_trace.
    aliases is_error for zif_lso_log_message~is_error.
    aliases is_info for zif_lso_log_message~is_info.
    aliases is_success for zif_lso_log_message~is_success.
    aliases is_warning for zif_lso_log_message~is_warning.
    aliases is_abort for zif_lso_log_message~is_abort.
    aliases set_data for zif_lso_log_message~set_data.
    aliases set_trace for zif_lso_log_message~set_trace.
    aliases set_type for zif_lso_log_message~set_type.
    aliases get_stripped_date for zif_lso_log_message~get_stripped_date.
    aliases is_stripped for zif_lso_log_message~is_stripped.
    aliases split for zif_lso_log_message~split.
    aliases clone for if_os_clone~clone.

    class-methods save_collection
      importing log_id        type zlso_log_message-log_id
                log_seqnr     type zlso_log_message-log_seqnr
                messages      type ref to if_object_collection
                with_commit   type abap_bool default abap_false
      returning value(result) type abap_bool
      raising   zcx_lso_log .

    methods constructor
      importing msgid type msgid
                msgno type symsgno
                msgty type msgty
                msgv1 type string                   optional
                msgv2 type string                   optional
                msgv3 type string                   optional
                msgv4 type string                   optional
                trace type ref to zcl_lso_log_trace optional .

  protected section.

  private section.
    types tt_messages type sorted table of zlso_log_message with unique key log_id log_seqnr timestamp .

    class-data last_timestamp type timestampl .

    data log_id type zlso_log_message-log_id .
    data log_seqnr type zlso_log_message-log_seqnr .
    data date type datum .
    data time type tims .
    data timestamp type timestampl .
    data symsg type symsg .
    data text type string .
    data data type ref to data .
    data trace type ref to zcl_lso_log_trace .
    data abap_stack type zif_lso_log_message=>ts_abap_stack.
    data stripped_date type zlso_log_message-stripped_date.

    class-methods lock
      importing log_id        type zlso_log_message-log_id
                log_seqnr     type zlso_log_message-log_seqnr
      returning value(result) type abap_bool .

    class-methods unlock
      importing log_id    type zlso_log_message-log_id
                log_seqnr type zlso_log_message-log_seqnr .

    methods split_msgv
      importing msgv  type string
      changing  msgv1 type symsgv optional
                msgv2 type symsgv optional
                msgv3 type symsgv optional
                msgv4 type symsgv optional .

    methods set_log_id
      importing log_id type zlso_log_message-log_id .

    methods set_log_seqnr
      importing log_seqnr type zlso_log_message-log_seqnr .

    methods set_timestamp
      importing timestamp type zlso_log_message-timestamp.

    methods set_stripped_date
      importing stripped_date type zlso_log_message-stripped_date.

    methods set_abap_stack
      importing abap_stack type zif_lso_log_message=>ts_abap_stack.

    methods setup_abap_stack.
endclass.


class zcl_lso_log_message implementation.

  method constructor.
    data lv_msgv1 type symsgv.
    data lv_msgv2 type symsgv.
    data lv_msgv3 type symsgv.
    data lv_msgv4 type symsgv.

    if msgv1 is supplied.
      me->split_msgv( exporting msgv  = msgv1
                       changing msgv1 = lv_msgv1
                                msgv2 = lv_msgv2
                                msgv3 = lv_msgv3
                                msgv4 = lv_msgv4 ).
    endif.

    if msgv2 is supplied.
      me->split_msgv( exporting msgv  = msgv2
                       changing msgv2 = lv_msgv2
                                msgv3 = lv_msgv3
                                msgv4 = lv_msgv4 ).
    endif.

    if msgv3 is supplied.
      me->split_msgv( exporting msgv  = msgv3
                       changing msgv3 = lv_msgv3
                                msgv4 = lv_msgv4 ).
    endif.

    if msgv4 is supplied.
      me->split_msgv( exporting msgv  = msgv4
                       changing msgv4 = lv_msgv4 ).
    endif.

    me->symsg = value symsg( msgty = msgty
                             msgid = msgid
                             msgno = msgno
                             msgv1 = lv_msgv1
                             msgv2 = lv_msgv2
                             msgv3 = lv_msgv3
                             msgv4 = lv_msgv4 ).

    me->trace = trace.

    " Setup technical ABAP stack details.
    me->setup_abap_stack( ).

    " Time stamp in long form (YYYYMMDDhhmmssmmmuuun) is needed to have proper messages sequence order.
    " Generate new (current) UTC time stamp.
    get time stamp field me->timestamp.

    try.
        me->timestamp = cl_abap_tstmp=>normalize( me->timestamp ).
      catch cx_parameter_invalid_range
            cx_parameter_invalid_type.
    endtry.

    try.
        " Set date/time from UTC time stamp.
        zcl_lso_log_utils=>utc_tstmp_2_datetime( exporting iv_tstmp = me->timestamp
                                                 importing ev_date  = me->date
                                                           ev_time  = me->time ).
      catch cx_parameter_invalid_type
            cx_parameter_invalid_range.
        me->date = sy-datum.
        me->time = sy-uzeit.
    endtry.
  endmethod.


  method zif_lso_log_message~get_class.
    class = me->symsg-msgid.
  endmethod.


  method zif_lso_log_message~get_data.
    data = me->data.
  endmethod.


  method zif_lso_log_message~get_date.
    date = me->date.
  endmethod.


  method zif_lso_log_message~get_log_id.
    log_id = me->log_id.
  endmethod.


  method zif_lso_log_message~get_log_seqnr.
    log_seqnr = me->log_seqnr.
  endmethod.


  method zif_lso_log_message~get_number.
    number = me->symsg-msgno.
  endmethod.


  method zif_lso_log_message~get_request_payload.
    if me->trace is bound and me->trace->zif_lso_log_trace~has_request_payload( ).
      request_payload = me->trace->get_request_payload( )->get_payload( ).
    endif.
  endmethod.


  method zif_lso_log_message~get_response_payload.
    if me->trace is bound and me->trace->zif_lso_log_trace~has_response_payload( ).
      response_payload = me->trace->get_response_payload( )->get_payload( ).
    endif.
  endmethod.


  method zif_lso_log_message~get_stripped_date.
    stripped_date = me->stripped_date.
  endmethod.


  method zif_lso_log_message~get_symsg.
    symsg = me->symsg.
  endmethod.


  method zif_lso_log_message~get_text.
    if me->text is initial.
      data(t100_message) = new cl_t100_message( the_msg_class  = me->symsg-msgid
                                                the_msg_number = me->symsg-msgno ).

      t100_message->set_substitution_table( value name2value_table( ( name = cl_t100_message=>msgv1_name value = me->symsg-msgv1 )
                                                                    ( name = cl_t100_message=>msgv2_name value = me->symsg-msgv2 )
                                                                    ( name = cl_t100_message=>msgv3_name value = me->symsg-msgv3 )
                                                                    ( name = cl_t100_message=>msgv4_name value = me->symsg-msgv4 ) ) ).
      me->text = t100_message->if_message~get_text( ).
    endif.

    text = me->text.
  endmethod.


  method zif_lso_log_message~get_time.
    time = me->time.
  endmethod.


  method zif_lso_log_message~get_timestamp.
    timestamp = me->timestamp.
  endmethod.


  method zif_lso_log_message~get_trace.
    trace = me->trace.
  endmethod.


  method zif_lso_log_message~get_type.
    type = me->symsg-msgty.
  endmethod.


  method zif_lso_log_message~get_abap_stack.
    abap_stack = me->abap_stack.
  endmethod.


  method zif_lso_log_message~has_trace.
    result = boolc( me->trace is bound ).
  endmethod.


  method zif_lso_log_message~is_error.
    result = boolc( me->symsg-msgty eq zif_lso_log_message=>c_type-error ).
  endmethod.


  method zif_lso_log_message~is_info.
    result = boolc( me->symsg-msgty eq zif_lso_log_message=>c_type-info ).
  endmethod.


  method zif_lso_log_message~is_stripped.
    result = boolc( me->stripped_date is not initial ).
  endmethod.


  method zif_lso_log_message~is_success.
    result = boolc( me->symsg-msgty eq zif_lso_log_message=>c_type-success ).
  endmethod.


  method zif_lso_log_message~is_warning.
    result = boolc( me->symsg-msgty eq zif_lso_log_message=>c_type-warning ).
  endmethod.


  method zif_lso_log_message~is_abort.
    result = boolc( me->symsg-msgty eq zif_lso_log_message=>c_type-abort ).
  endmethod.


  method zif_lso_log_message~set_data.
    me->data = data.
  endmethod.


  method zif_lso_log_message~set_trace.
    me->trace = trace.
  endmethod.


  method zif_lso_log_message~set_type.
    me->symsg-msgty = msgty.
  endmethod.


  method setup_abap_stack.
    data(stack) = new zcl_lso_abap_stack( )->zif_lso_abap_stack~before_pattern( '*CL_LSO_LOG*' ).

    me->set_abap_stack( value #( abap_program     = stack-progname
                                 abap_include     = stack-includename
                                 abap_source_line = stack-line
                                 abap_event       = stack-event ) ).
  endmethod.


  method set_log_id.
    me->log_id = log_id.
  endmethod.


  method set_log_seqnr.
    me->log_seqnr = log_seqnr.
  endmethod.


  method set_stripped_date.
    me->stripped_date = stripped_date.
  endmethod.


  method set_timestamp.
    me->timestamp = timestamp.

    if me->timestamp is not initial.
      try.
          " Set date/time from UTC time stamp.
          zcl_lso_log_utils=>utc_tstmp_2_datetime( exporting iv_tstmp = me->timestamp
                                                   importing ev_date  = me->date
                                                             ev_time  = me->time ).
        catch cx_parameter_invalid_type
              cx_parameter_invalid_range.
      endtry.
    endif.
  endmethod.


  method set_abap_stack.
    me->abap_stack = abap_stack.
  endmethod.


  method split_msgv.
    constants c_msgv_len type i value 50.

    data: lv_len        type                   i,
          lv_len1       type                   i,
          lv_len2       type                   i,
          lv_len3       type                   i,
          lv_len4       type                   i,
          lv_sum1       type                   i,
          lv_sum2       type                   i,
          lv_sum3       type                   i,
          lv_sum4       type                   i,
          lv_msgv1_full type                   abap_bool,
          lv_msgv2_full type                   abap_bool,
          lv_msgv3_full type                   abap_bool,
          lv_msgv4_full type                   abap_bool,
          lt_msgv       type standard table of symsgv,
          lv_msgv       type                   symsgv.

    data lv_text type text1024.

    lv_len = strlen( msgv ).

    if lv_len eq 0.
      return.
    endif.

    " Length of domain MSGV1 is 50 chars, to present longer message there's a need to split string into four parts.
    lv_text = msgv.

    call function 'SPLIT_LINE'
      exporting
        text       = lv_text
        len        = strlen( lv_text )
        maxlen     = c_msgv_len
        sep_before = '/([{'
      tables
        result_tab = lt_msgv.

    loop at lt_msgv into lv_msgv.
      lv_len = strlen( lv_msgv ).
      lv_len1 = strlen( msgv1 ).
      lv_len2 = strlen( msgv2 ).
      lv_len3 = strlen( msgv3 ).
      lv_len4 = strlen( msgv4 ).

      lv_sum1 = lv_len + lv_len1.
      lv_sum2 = lv_len + lv_len2.
      lv_sum3 = lv_len + lv_len3.
      lv_sum4 = lv_len + lv_len4.

      if msgv1 is supplied and lv_msgv1_full eq abap_false and lv_sum1 <= c_msgv_len.
        if msgv1 is initial.
          msgv1 = lv_msgv.
        else.
          concatenate msgv1 lv_msgv
            into msgv1
            separated by space.
        endif.
      elseif msgv2 is supplied and lv_msgv2_full eq abap_false and lv_sum2 <= c_msgv_len.
        " Variable msgv1 is full, next part of the string will be put into msgv2
        lv_msgv1_full = abap_true.

        if msgv2 is initial.
          msgv2 = lv_msgv.
        else.
          concatenate msgv2 lv_msgv
            into msgv2
            separated by space.
        endif.

      elseif msgv3 is supplied and lv_msgv3_full eq abap_false and lv_sum3 <= c_msgv_len.
        " Variable msgv2 is full, next part of the string will be put into msgv3
        lv_msgv2_full = abap_true.

        if msgv3 is initial.
          msgv3 = lv_msgv.
        else.
          concatenate msgv3 lv_msgv
            into msgv3
            separated by space.
        endif.

      elseif msgv4 is supplied and lv_sum4 <= c_msgv_len.
        " Variable msgv3 is full, next part of the string will be put into msgv4
        lv_msgv3_full = abap_true.

        if msgv4 is initial.
          msgv4 = lv_msgv.
        else.
          concatenate msgv4 lv_msgv
            into msgv4
            separated by space.
        endif.

      endif.
    endloop.
  endmethod.


  method lock.
    " Lock log messages table against potential modifications for given log id/log seqnr.
    call function 'ENQUEUE_EZLSO_LOG_MSG'
      exporting
        log_id         = log_id
        log_seqnr      = log_seqnr
      exceptions
        foreign_lock   = 1
        system_failure = 2
        others         = 3.

    result = boolc( sy-subrc eq 0 ).
  endmethod.


  method save_collection.
    " Lock messages table for given log id/log seqnr.
    if not zcl_lso_log_message=>lock( log_id    = log_id
                                      log_seqnr = log_seqnr ).

      wait up to 1 seconds.

      " Try to lock again...
      if not zcl_lso_log_message=>lock( log_id    = log_id
                                        log_seqnr = log_seqnr ).
        " Log Message table is locked - raise an exception!
        raise exception type zcx_lso_log
          exporting
            textid   = zcx_lso_log=>lock_error
            mv_msgv1 = |ZLSO_LOG_MESSAGE table { log_id }/{ log_seqnr }|.
      endif.
    endif.

    data(db_messages) = value tt_messages( ).
    data(traces_map) = new zcl_lso_object_map( ).

    data(iterator) = messages->get_iterator( ).

    " Prepare messages data for saving.
    while iterator->has_next( ).
      data(message) = cast zcl_lso_log_message( iterator->get_next( ) ).

      if message->zif_lso_log_message~get_log_id( ) is not initial and
         message->zif_lso_log_message~get_log_id( ) eq log_id and
         message->zif_lso_log_message~get_log_seqnr( ) is not initial and
         message->zif_lso_log_message~get_log_seqnr( ) ne log_seqnr.
        " Message is not related to this particular log run, it will not be saved.
        continue.
      endif.

      data(trace_id) = value zlso_log_message-trace_id( ).

      if message->zif_lso_log_message~has_trace( ).
        trace_id = message->zif_lso_log_message~get_trace( )->get_id( ).

        " Check if given trace object has been already added to the map?
        if not traces_map->if_object_map~contains_key( trace_id ).
          " No trace yet, add it to the map. It should contain only unique traces objects.
          traces_map->put( key   = trace_id
                           value = message->zif_lso_log_message~get_trace( ) ).
        endif.
      endif.

      " Set log id/seqnr, protection against potential multiple saves.
      message->set_log_id( log_id ).
      message->set_log_seqnr( log_seqnr ).

      data(message_symsg) = message->zif_lso_log_message~get_symsg( ).

      data(db_message) = value zlso_log_message( log_id           = log_id
                                                 log_seqnr        = log_seqnr
                                                 timestamp        = message->zif_lso_log_message~get_timestamp( )
                                                 trace_id         = trace_id
                                                 msg_date         = message->zif_lso_log_message~get_date( )
                                                 msg_time         = message->zif_lso_log_message~get_time( )
                                                 msgty            = message_symsg-msgty
                                                 msgid            = message_symsg-msgid
                                                 msgno            = message_symsg-msgno
                                                 msgv1            = message_symsg-msgv1
                                                 msgv2            = message_symsg-msgv2
                                                 msgv3            = message_symsg-msgv3
                                                 msgv4            = message_symsg-msgv4
                                                 abap_program     = message->zif_lso_log_message~get_abap_stack( )-abap_program
                                                 abap_include     = message->zif_lso_log_message~get_abap_stack( )-abap_include
                                                 abap_source_line = message->zif_lso_log_message~get_abap_stack( )-abap_source_line
                                                 abap_event       = message->zif_lso_log_message~get_abap_stack( )-abap_event
                                                 stripped_date    = message->zif_lso_log_message~get_stripped_date( ) ).

      " Insert message structure to a temporary table for further saving.
      insert db_message into table db_messages.

      data(subrc) = sy-subrc.

      " In case of INSERT problem try to process message in the loop till message is added correctly.
      while subrc ne 0.
        " Message couldn't be added to the temporary messages table.
        " Probably time stamp field is the same for few messages.
        try.
            " Try to add fraction of one second to time stamp so that it is unique and can be added to the temporary messages table.
            message->timestamp = cl_abap_tstmp=>add( tstmp = message->timestamp
                                                     secs  = '0.0000001' ).
          catch cx_parameter_invalid_range
                cx_parameter_invalid_type.
            try.
                message->timestamp = cl_abap_tstmp=>normalize( message->timestamp ).
              catch cx_parameter_invalid_range
                    cx_parameter_invalid_type . "##NO_HANDLER
            endtry.

            retry.
        endtry.

        " Modify message DB structure with new time stamp before adding.
        db_message-timestamp = message->zif_lso_log_message~get_timestamp( ).

        " Try to insert message with modified time stamp to the temporary messages table.
        insert db_message into table db_messages.

        " Set INSERT result into local variable so that it can be checked in WHILE loop.
        subrc = sy-subrc.
      endwhile.
    endwhile.

    try.
        if db_messages[] is not initial.
          " Save messages in DB.
          insert zlso_log_message from table @db_messages accepting duplicate keys.
        endif.

        result = boolc( sy-dbcnt > 0 ).

        if result eq abap_true and traces_map->is_empty( ) eq abap_false.
          " Save traces data.
          zcl_lso_log_trace=>save_collection( traces_map->get_values( ) ).
        endif.

        if with_commit eq abap_true.
          commit work.
        endif.
      catch cx_sy_open_sql_db into data(lo_cx_sql).
        if with_commit eq abap_true.
          rollback work.
        endif.

        " Unlock messages table for given log id/log seqnr.
        zcl_lso_log_message=>unlock( log_id    = log_id
                                     log_seqnr = log_seqnr ).

        " Save problem, raise an exception.
        raise exception type zcx_lso_log
          exporting
            textid            = zcx_lso_log=>save_error
            mv_msgv1          = |ZLSO_LOG_MESSAGE { log_id }/{ log_seqnr }|
            mv_exception_text = lo_cx_sql->get_text( ).
    endtry.

    " Unlock messages table for given log id/log seqnr.
    zcl_lso_log_message=>unlock( log_id    = log_id
                                 log_seqnr = log_seqnr ).
  endmethod.


  method unlock.
    call function 'DEQUEUE_EZLSO_LOG_MSG'
      exporting
        log_id    = log_id
        log_seqnr = log_seqnr.
  endmethod.


  method zif_lso_log_message~split.
    me->split_msgv( exporting msgv  = me->get_text( )
                    changing  msgv1 = variables-v1
                              msgv2 = variables-v2
                              msgv3 = variables-v3
                              msgv4 = variables-v4 ).
  endmethod.


  method if_os_clone~clone.
    result = new zcl_lso_log_message( msgid = me->symsg-msgid
                                      msgno = me->symsg-msgno
                                      msgty = me->symsg-msgty
                                      msgv1 = conv #( me->symsg-msgv1 )
                                      msgv2 = conv #( me->symsg-msgv2 )
                                      msgv3 = conv #( me->symsg-msgv3 )
                                      msgv4 = conv #( me->symsg-msgv4 )
                                      trace = me->trace ).

    cast zcl_lso_log_message( result )->set_timestamp( me->get_timestamp( ) ).
  endmethod.

endclass.
