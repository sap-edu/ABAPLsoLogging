class zcl_lso_log_message_factory definition
  public
  final
  create private .

  public section.
    interfaces zif_lso_log_message_factory.

    aliases get_last_by_log_id for zif_lso_log_message_factory~get_last_by_log_id.
    aliases get_last_error_by_log_id for zif_lso_log_message_factory~get_last_error_by_log_id.
    aliases get_last_success_by_log_id for zif_lso_log_message_factory~get_last_success_by_log_id.
    aliases get_last_warning_by_log_id for zif_lso_log_message_factory~get_last_warning_by_log_id.

    class-methods instance
      returning value(factory) type ref to zif_lso_log_message_factory.

  protected section.
  private section.
    class-data self type ref to zif_lso_log_message_factory.

    methods create
      importing message_structure type zlso_log_message
                trace             type ref to zcl_lso_log_trace optional
      returning value(message)    type ref to zcl_lso_log_message.
endclass.


class zcl_lso_log_message_factory implementation.

  method instance.
    if zcl_lso_log_message_factory=>self is not bound.
      zcl_lso_log_message_factory=>self = new zcl_lso_log_message_factory( ).
    endif.

    factory = zcl_lso_log_message_factory=>self.
  endmethod.


  method zif_lso_log_message_factory~get_last_error_by_log_id.
    message = me->get_last_by_log_id( log_id    = log_id
                                      msgty     = zif_lso_log_message=>c_type-error
                                      date_from = date_from
                                      time_from = time_from
                                      date_to   = date_to
                                      time_to   = time_to ).
  endmethod.


  method zif_lso_log_message_factory~get_last_success_by_log_id.
    message = me->get_last_by_log_id( log_id    = log_id
                                      msgty     = zif_lso_log_message=>c_type-success
                                      date_from = date_from
                                      time_from = time_from
                                      date_to   = date_to
                                      time_to   = time_to ).
  endmethod.


  method zif_lso_log_message_factory~get_last_warning_by_log_id.
    message = me->get_last_by_log_id( log_id    = log_id
                                      msgty     = zif_lso_log_message=>c_type-warning
                                      date_from = date_from
                                      time_from = time_from
                                      date_to   = date_to
                                      time_to   = time_to ).
  endmethod.


  method zif_lso_log_message_factory~get_last_by_log_id.
    if msgty is not initial.
      data(msgtys) = value zif_lso_log_message_factory=>tt_msgty( ( sign   = zif_lso_range=>c_sign-include
                                                                    option = zif_lso_range=>c_option-equal
                                                                    low    = msgty ) ).
    endif.

    if date_from is not initial.
      cl_abap_tstmp=>systemtstmp_syst2utc( exporting syst_date = date_from syst_time = time_from
                                           importing utc_tstmp = data(ts_from) ).
    endif.
    if date_to is not initial.
      cl_abap_tstmp=>systemtstmp_syst2utc( exporting syst_date = date_to syst_time = time_to
                                           importing utc_tstmp = data(ts_to) ).
    endif.
    select *
      from zlso_log_message
     where log_id   eq @log_id
       and msgty    in @msgtys
       and timestamp between @ts_from and @ts_to
    order by timestamp descending
    into table @data(messages) up to 1 rows.

    if sy-subrc ne 0.
      " Not found - &1 &2 &3 &4
      raise exception type zcx_lso_log
        exporting
          textid   = zcx_lso_log=>not_found
          mv_msgv1 = |ZLSO_LOG_MESSAGE|
          mv_msgv2 = |{ log_id }|
          mv_msgv3 = |{ msgty }|
          mv_msgv4 = |{ date_from date = environment } { date_to date = environment }|.
    endif.

    " Message was found, instantiate it.
    message = me->create( messages[ 1 ] ).
  endmethod.


  method create.
    message = new zcl_lso_log_message( msgty = message_structure-msgty
                                       msgid = message_structure-msgid
                                       msgno = message_structure-msgno
                                       msgv1 = conv #( message_structure-msgv1 )
                                       msgv2 = conv #( message_structure-msgv2 )
                                       msgv3 = conv #( message_structure-msgv3 )
                                       msgv4 = conv #( message_structure-msgv4 )
                                       trace = trace ).

    message->set_log_id( message_structure-log_id ).
    message->set_log_seqnr( message_structure-log_seqnr ).
    message->set_timestamp( message_structure-timestamp ).
    message->set_stripped_date( message_structure-stripped_date ).
    message->set_abap_stack( corresponding #( message_structure ) ).
  endmethod.

endclass.
