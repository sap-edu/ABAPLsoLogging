class zcl_lso_log_handler definition
  inheriting from zcl_lso_log_abstract
  public
  final
  create public .

  public section.
    interfaces zif_lso_log_handler.

    " Backward compatibility
    " Abstract
    aliases: add_message            for zif_lso_log_abstract~add_message,
             add_messages           for zif_lso_log_abstract~add_messages,
             add_symsg              for zif_lso_log_abstract~add_symsg,

             display_messages_popup for zif_lso_log_abstract~display_messages_popup,

             get_messages           for zif_lso_log_abstract~get_messages,
             get_messages_rfc       for zif_lso_log_abstract~get_messages_rfc,
             get_symsgs             for zif_lso_log_abstract~get_symsgs,
             get_program            for zif_lso_log_abstract~get_program,
             get_tcode              for zif_lso_log_abstract~get_tcode,

             has_error              for zif_lso_log_abstract~has_error,
             has_info               for zif_lso_log_abstract~has_info,
             has_success            for zif_lso_log_abstract~has_success,
             has_warning            for zif_lso_log_abstract~has_warning,
             has_message            for zif_lso_log_abstract~has_message,
             has_messages           for zif_lso_log_abstract~has_messages,
             has_message_key        for zif_lso_log_abstract~has_message_key,

             exception              for zif_lso_log_abstract~exception,
             error                  for zif_lso_log_abstract~error,
             warning                for zif_lso_log_abstract~warning,
             success                for zif_lso_log_abstract~success,
             info                   for zif_lso_log_abstract~info,
             abort                  for zif_lso_log_abstract~abort,

             trace_message          for zif_lso_log_abstract~trace_message,

             set_tcode              for zif_lso_log_abstract~set_tcode,
             set_program            for zif_lso_log_abstract~set_program.

    methods constructor.

    methods to_log
      returning value(log) type ref to zif_lso_log.

  protected section.
  private section.
endclass.


class zcl_lso_log_handler implementation.

  method constructor.
    super->constructor( ).
  endmethod.

  method zif_lso_log_handler~error.
    message = me->zif_lso_log_handler~message( msgty = zif_lso_log_message=>c_type-error
                                               msgid = msgid
                                               msgno = msgno
                                               msgv1 = msgv1
                                               msgv2 = msgv2
                                               msgv3 = msgv3
                                               msgv4 = msgv4
                                               trace = trace ).
  endmethod.

  method zif_lso_log_handler~info.
    message = me->zif_lso_log_handler~message( msgty = zif_lso_log_message=>c_type-info
                                               msgid = msgid
                                               msgno = msgno
                                               msgv1 = msgv1
                                               msgv2 = msgv2
                                               msgv3 = msgv3
                                               msgv4 = msgv4
                                               trace = trace ).
  endmethod.


  method zif_lso_log_handler~success.
    message = me->zif_lso_log_handler~message( msgty = zif_lso_log_message=>c_type-success
                                               msgid = msgid
                                               msgno = msgno
                                               msgv1 = msgv1
                                               msgv2 = msgv2
                                               msgv3 = msgv3
                                               msgv4 = msgv4
                                               trace = trace ).
  endmethod.


  method zif_lso_log_handler~warning.
    message = me->zif_lso_log_handler~message( msgty = zif_lso_log_message=>c_type-warning
                                               msgid = msgid
                                               msgno = msgno
                                               msgv1 = msgv1
                                               msgv2 = msgv2
                                               msgv3 = msgv3
                                               msgv4 = msgv4
                                               trace = trace ).
  endmethod.


  method zif_lso_log_handler~message.
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


  method zif_lso_log_handler~trace_message.
    data(trace_http_status) = cond #( when http_status is not initial then conv i( http_status )
                                      else trace->get_http_status( ) ).

    data(trace_msgty) = value zlso_log_message-msgty( ).

    if msgty is not initial.
      trace_msgty = msgty.
    else.
      " Determine message type from the trace HTTP status.
      if cl_rest_status_code=>is_success( trace_http_status ) eq abap_true.
        trace_msgty = zif_lso_log_message=>c_type-success.
      else.
        trace_msgty = zif_lso_log_message=>c_type-error.
      endif.
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
      trace_msgv1 = cl_rest_status_code=>get_reason_phrase( trace_http_status ) &&
                    cond #( when trace_http_status ne trace->get_http_status( ) then | ({ trace_http_status })| ).
    endif.

    " Collect trace message in the log handler.
    message = me->zif_lso_log_handler~message( msgty = trace_msgty
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


  method to_log.
    log = new zcl_lso_log( ).

    log->zif_lso_log_abstract~set_tcode( cond #( when me->get_tcode( ) is not initial then me->get_tcode( )
                                                 when sy-tcode is not initial then sy-tcode
                                                 else sy-cprog ) ).
    log->zif_lso_log_abstract~set_program( cond #( when me->get_program( ) is not initial then me->get_program( )
                                                   else sy-cprog ) ).
    log->zif_lso_log_abstract~add_messages( me->zif_lso_log_abstract~get_messages( ) ).
  endmethod.

endclass.
