class zcl_lso_log_abstract definition
  public
  create public
  abstract.

  public section.
    interfaces zif_lso_log_abstract.

  protected section.
    data tcode    type zlso_log-tcode.
    data program  type zlso_log-prog.
    data messages type zlso_tt_log_messages.

  private section.
    methods add_t100_exception
      importing cx    type ref to if_t100_message
                trace type ref to zcl_lso_log_trace optional .

    methods conv_t100_cx_to_message
      importing cx             type ref to if_t100_message
                trace          type ref to zcl_lso_log_trace optional
      returning value(message) type ref to zcl_lso_log_message .

endclass.


class zcl_lso_log_abstract implementation.

  method zif_lso_log_abstract~exception.
    try.
        if cx is instance of zif_lso_cx_message.
          " Get log message directly from the exception object.
          data(message) = cast zif_lso_cx_message( cx )->get_log_message( ).
          message->set_trace( trace ).

          me->zif_lso_log_abstract~add_message( message ).
        else.
          " Check if exception class is built with message class (if implements the IF_T100_MESSAGE interface).
          me->add_t100_exception( cx    = cast if_t100_message( cx )
                                  trace = trace ).
        endif.
      catch cx_sy_move_cast_error.
        " Normal object exception without message class.
        " Error message will be added with ZLSO_LOG message class.
        me->zif_lso_log_abstract~add_message( new #( msgty = zif_lso_log_message=>c_type-error
                                                     msgid = zif_lso_log_message=>c_msgid
                                                     msgno = '001'
                                                     msgv1 = cx->get_text( )
                                                     trace = trace ) ).
    endtry.
  endmethod.


  method zif_lso_log_abstract~add_message.
    insert message->get_object( ) into table me->messages.
  endmethod.


  method zif_lso_log_abstract~add_messages.
    insert lines of messages into table me->messages.
  endmethod.


  method zif_lso_log_abstract~add_symsg.
    me->zif_lso_log_abstract~add_message( new #( msgty = symsg-msgty
                                                 msgid = symsg-msgid
                                                 msgno = symsg-msgno
                                                 msgv1 = conv #( symsg-msgv1 )
                                                 msgv2 = conv #( symsg-msgv2 )
                                                 msgv3 = conv #( symsg-msgv3 )
                                                 msgv4 = conv #( symsg-msgv4 )
                                                 trace = trace ) ).
  endmethod.


  method zif_lso_log_abstract~clear_messages.
    clear me->messages.
  endmethod.


  method zif_lso_log_abstract~get_messages.
    loop at me->messages reference into data(message).
      if clone eq abap_true.
        data(clone_instance) = cast zcl_lso_log_message( message->instance->clone( ) ).
        data(clone_object) = clone_instance->zif_lso_log_message~get_object( ).
        message = ref #( clone_object ).
      endif.

      if type is initial.
        insert message->* into table messages.
      elseif message->instance->get_type( ) eq type.
        insert message->* into table messages.
      endif.
    endloop.
  endmethod.


  method zif_lso_log_abstract~get_messages_rfc.
    loop at me->zif_lso_log_abstract~get_messages( type ) reference into data(message).
      insert value #( type   = message->instance->get_type( )
                      class  = message->instance->get_class( )
                      number = message->instance->get_number( )
                      text   = message->instance->get_text( ) ) into table messages_rfc.
    endloop.
  endmethod.


  method zif_lso_log_abstract~get_symsgs.
    loop at me->messages reference into data(message).
      insert message->instance->get_symsg( ) into table symsgs.
    endloop.
  endmethod.


  method zif_lso_log_abstract~get_program.
    program = me->program.
  endmethod.


  method zif_lso_log_abstract~get_tcode.
    tcode = me->tcode.
  endmethod.


  method zif_lso_log_abstract~has_error.
    result = me->zif_lso_log_abstract~has_message_type( type  = zif_lso_log_message=>c_type-error
                                                        after = after ).

    if result ne abap_true.
      " Abort is also an error!
      result = me->zif_lso_log_abstract~has_abort( after ).
    endif.
  endmethod.


  method zif_lso_log_abstract~has_abort.
    result = me->zif_lso_log_abstract~has_message_type( type  = zif_lso_log_message=>c_type-abort
                                                        after = after ).
  endmethod.


  method zif_lso_log_abstract~has_exception.
    if me->messages[] is initial.
      " No messages collected, leave the method.
      return.
    endif.

    data(cx_message) = new zcl_lso_log_message( msgty = zif_lso_log_message=>c_type-error
                                                msgid = cx_t100_key-msgid
                                                msgno = cx_t100_key-msgno ).

    loop at me->messages reference into data(message).
      " Check only message key id, type, number.
      if cx_message->get_symsg( ) eq corresponding symsg( message->instance->get_symsg( ) except msgv1 msgv2 msgv3 msgv4 ).
        result = abap_true.
        return.
      endif.
    endloop.
  endmethod.


  method zif_lso_log_abstract~has_info.
    result = me->zif_lso_log_abstract~has_message_type( type  = zif_lso_log_message=>c_type-info
                                                        after = after  ).
  endmethod.


  method zif_lso_log_abstract~has_message.
    if me->messages[] is initial.
      " No messages collected, leave the method.
      return.
    endif.

    loop at me->messages reference into data(collected_message).
      if message->get_symsg( ) eq collected_message->instance->get_symsg( ).
        result = abap_true.
        return.
      endif.
    endloop.
  endmethod.


  method zif_lso_log_abstract~has_message_key.
    if me->messages[] is initial.
      " No messages collected, leave the method.
      return.
    endif.

    loop at me->messages reference into data(collected_message).
      if collected_message->instance->get_symsg( )-msgid = msgid and " e.g. ZLSO_SF_API
         collected_message->instance->get_symsg( )-msgno = msgno.    " e.g. 003
        result = abap_true.
        return.
      endif.
    endloop.
  endmethod.


  method zif_lso_log_abstract~has_messages.
    result = boolc( me->messages[] is not initial ).
  endmethod.


  method zif_lso_log_abstract~has_message_type.
    if me->zif_lso_log_abstract~has_messages( ) eq abap_false.
      result = abap_false.
      return.
    endif.

    loop at me->messages reference into data(message).
      if message->instance->get_type( )      eq type and
         message->instance->get_timestamp( ) gt after.
        result = abap_true.
        return.
      endif.
    endloop.
  endmethod.


  method zif_lso_log_abstract~has_success.
    result = me->zif_lso_log_abstract~has_message_type( type = zif_lso_log_message=>c_type-success
                                                        after = after ).
  endmethod.


  method zif_lso_log_abstract~has_warning.
    result = me->zif_lso_log_abstract~has_message_type( type  = zif_lso_log_message=>c_type-warning
                                                        after = after  ).
  endmethod.


  method zif_lso_log_abstract~set_program.
    me->program = program.
  endmethod.


  method zif_lso_log_abstract~set_tcode.
    me->tcode = tcode.
  endmethod.


  method add_t100_exception.
    " Add an error message generated from exception built with message class (t100).
    me->zif_lso_log_abstract~add_message( me->conv_t100_cx_to_message( cx    = cast if_t100_message( cx )
                                                                       trace = trace ) ).
  endmethod.


  method conv_t100_cx_to_message.
    data object type ref to object.

    data(msgv1) = value string( ).
    data(msgv2) = value string( ).
    data(msgv3) = value string( ).
    data(msgv4) = value string( ).

    data(message_struct) = value symsg( msgid = cx->t100key-msgid
                                        msgno = cx->t100key-msgno ).

    " Get imported exception class name.
    data(class_name) = cl_abap_classdescr=>get_class_name( cx ).

    create object object type (class_name).

    try.
        " Cast down to specific object to have an access to all public object's attributes that
        " are not part of IF_T100_MESSAGE interface.
        object ?= cx.

        " Add message variables.
        assign object->(cx->t100key-attr1) to field-symbol(<msgv>).

        if sy-subrc eq 0.
          msgv1 = <msgv>.
        endif.

        assign object->(cx->t100key-attr2) to <msgv>.

        if sy-subrc eq 0.
          msgv2 = <msgv>.
        endif.

        assign object->(cx->t100key-attr3) to <msgv>.

        if sy-subrc eq 0.
          msgv3 = <msgv>.
        endif.

        assign object->(cx->t100key-attr4) to <msgv>.

        if sy-subrc eq 0.
          msgv4 = <msgv>.
        endif.
      catch cx_sy_move_cast_error.
        msgv1 = cx->t100key-attr1.
        msgv2 = cx->t100key-attr2.
        msgv3 = cx->t100key-attr3.
        msgv4 = cx->t100key-attr4.
    endtry.

    try.
        " Try to get message type based on severity.
        data(msgty) = cast zif_lso_cx( cx )->get_msgty( ).
      catch cx_sy_move_cast_error.
        msgty = zif_lso_log_message=>c_type-error.
    endtry.

    " Instantiate an error message generated from exception built with message class.
    message = new #( msgty = msgty
                     msgid = message_struct-msgid
                     msgno = message_struct-msgno
                     msgv1 = msgv1
                     msgv2 = msgv2
                     msgv3 = msgv3
                     msgv4 = msgv4
                     trace = trace ).
  endmethod.


  method zif_lso_log_abstract~message.
    message = new zcl_lso_log_message( msgty = msgty
                                       msgid = msgid
                                       msgno = msgno
                                       msgv1 = msgv1
                                       msgv2 = msgv2
                                       msgv3 = msgv3
                                       msgv4 = msgv4
                                       trace = trace ).

    me->zif_lso_log_abstract~add_message( message ).
  endmethod.


  method zif_lso_log_abstract~error.
    message = me->zif_lso_log_abstract~message( msgty = zif_lso_log_message=>c_type-error
                                                msgid = msgid
                                                msgno = msgno
                                                msgv1 = msgv1
                                                msgv2 = msgv2
                                                msgv3 = msgv3
                                                msgv4 = msgv4
                                                trace = trace ).
  endmethod.


  method zif_lso_log_abstract~info.
    message = me->zif_lso_log_abstract~message( msgty = zif_lso_log_message=>c_type-info
                                                msgid = msgid
                                                msgno = msgno
                                                msgv1 = msgv1
                                                msgv2 = msgv2
                                                msgv3 = msgv3
                                                msgv4 = msgv4
                                                trace = trace ).
  endmethod.


  method zif_lso_log_abstract~success.
    message = me->zif_lso_log_abstract~message( msgty = zif_lso_log_message=>c_type-success
                                                msgid = msgid
                                                msgno = msgno
                                                msgv1 = msgv1
                                                msgv2 = msgv2
                                                msgv3 = msgv3
                                                msgv4 = msgv4
                                                trace = trace ).
  endmethod.


  method zif_lso_log_abstract~warning.
    message = me->zif_lso_log_abstract~message( msgty = zif_lso_log_message=>c_type-warning
                                                msgid = msgid
                                                msgno = msgno
                                                msgv1 = msgv1
                                                msgv2 = msgv2
                                                msgv3 = msgv3
                                                msgv4 = msgv4
                                                trace = trace ).
  endmethod.


  method zif_lso_log_abstract~abort.
    message = me->zif_lso_log_abstract~message( msgty = zif_lso_log_message=>c_type-abort
                                                msgid = msgid
                                                msgno = msgno
                                                msgv1 = msgv1
                                                msgv2 = msgv2
                                                msgv3 = msgv3
                                                msgv4 = msgv4
                                                trace = trace ).
  endmethod.


  method zif_lso_log_abstract~trace_message.
    data(trace_http_status) = cond #( when http_status is not initial then conv i( http_status )
                                      else trace->get_http_status( ) ).

    data(trace_msgty) = value zlso_log_message-msgty( ).

    data(status) = new /edu/cl_http_status( trace_http_status ).

    if msgty is not initial.
      trace_msgty = msgty.
    else.
    " Determine message type from the trace HTTP status.
      trace_msgty = cond #( when status->is_success( ) then zif_lso_log_message=>c_type-success
                            else zif_lso_log_message=>c_type-error ).
    endif.

    data(trace_msgv1) = value string( ).

    if msgv1 is not initial.
      trace_msgv1 = msgv1.
    else.
      " HTTP Status code and HTTP method have been already stored in trace db table.
      " However in case of $batch transfer, trace object http status is related to the whole API call,
      " it makes sense to present it as a part of message if different than the one stored in trace itself.
      " For instance:
      " &1 (200) - ... Transfer...
      trace_msgv1 = status->get_reason( ) &&
                    cond #( when trace_http_status ne trace->get_http_status( ) then | ({ trace_http_status })| ).
    endif.

    " Collect trace message in the log handler.
    message = me->zif_lso_log_abstract~message( msgty = trace_msgty
                                                msgid = msgid
                                                msgno = msgno
                                                msgv1 = trace_msgv1
                                                msgv2 = msgv2
                                                msgv3 = msgv3
                                                msgv4 = msgv4
                                                trace = trace ).

    " Where used for message...
    message id msgid type trace_msgty number msgno
      with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 into data(lv_dummy).
  endmethod.

endclass.
