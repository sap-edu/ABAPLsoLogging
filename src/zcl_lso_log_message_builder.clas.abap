class zcl_lso_log_message_builder definition
  public
  final
  create public .

  public section.

    methods constructor .

    methods get_msgid
      returning value(msgid) type zlso_log_message-msgid .

    methods get_msgno
      returning value(msgno) type zlso_log_message-msgno .

    methods get_msgty
      returning value(msgty) type zlso_log_message-msgty .

    methods build
      returning value(message) type ref to zcl_lso_log_message .

    methods set_log_id
      importing log_id          type zlso_log_message-log_id
      returning value(instance) type ref to zcl_lso_log_message_builder.

    methods set_log_seqnr
      importing log_seqnr       type zlso_log_message-log_seqnr
      returning value(instance) type ref to zcl_lso_log_message_builder.

    methods set_msgid
      importing msgid           type zlso_log_message-msgid
      returning value(instance) type ref to zcl_lso_log_message_builder.

    methods set_msgno
      importing msgno           type zlso_log_message-msgno
      returning value(instance) type ref to zcl_lso_log_message_builder.

    methods set_msgty
      importing msgty           type zlso_log_message-msgty
      returning value(instance) type ref to zcl_lso_log_message_builder.

    methods set_msgv1
      importing msgv            type string
      returning value(instance) type ref to zcl_lso_log_message_builder.

    methods set_msgv2
      importing msgv            type string
      returning value(instance) type ref to zcl_lso_log_message_builder.

    methods set_msgv3
      importing msgv            type string
      returning value(instance) type ref to zcl_lso_log_message_builder.

    methods set_msgv4
      importing msgv            type string
      returning value(instance) type ref to zcl_lso_log_message_builder.

    methods set_msgvars
      importing msgv1           type string optional
                msgv2           type string optional
                msgv3           type string optional
                msgv4           type string optional
      returning value(instance) type ref to zcl_lso_log_message_builder.

    methods set_timestamp
      importing tstmp           type timestampl
      returning value(instance) type ref to zcl_lso_log_message_builder.

    methods set_trace
      importing trace           type ref to zif_lso_log_trace
      returning value(instance) type ref to zcl_lso_log_message_builder.

    methods set_stripped_date
      importing stripped_date   type zlso_log_message-stripped_date
      returning value(instance) type ref to zcl_lso_log_message_builder.

    methods set_abap_stack
      importing abap_stack      type zif_lso_log_message=>ts_abap_stack
      returning value(instance) type ref to zcl_lso_log_message_builder.

  protected section.
  private section.
    data trace type ref to zif_lso_log_trace .
    data log_id type zlso_log_message-log_id .
    data log_seqnr type zlso_log_message-log_seqnr .
    data msgid type zlso_log_message-msgid .
    data msgno type zlso_log_message-msgno .
    data msgty type zlso_log_message-msgty .
    data msgv1 type string .
    data msgv2 type string .
    data msgv3 type string .
    data msgv4 type string .
    data timestamp type timestampl .
    data abap_stack type zif_lso_log_message=>ts_abap_stack.
    data stripped_date type zlso_log_message-stripped_date.
    data abap_stack_initialized type abap_bool.
endclass.


class zcl_lso_log_message_builder implementation.

  method constructor.
    " Log Message Builder takes advantage of OO Builder pattern.
    " https://en.wikipedia.org/wiki/Builder_pattern
    " Its role it to return ZCL_LSO_LOG_MESSAGE object instance based of different set of parameters.
    " http://zevolving.com/2013/04/abap-objects-design-patterns-builder/
  endmethod.


  method get_msgid.
    msgid = me->msgid.
  endmethod.


  method get_msgno.
    msgno = me->msgno.
  endmethod.


  method get_msgty.
    msgty = me->msgty.
  endmethod.


  method set_log_id.
    me->log_id = log_id.

    " Builder pattern
    instance = me.
  endmethod.


  method set_log_seqnr.
    me->log_seqnr = log_seqnr.

    " Builder pattern
    instance = me.
  endmethod.


  method set_msgid.
    me->msgid = msgid.

    " Builder pattern
    instance = me.
  endmethod.


  method set_msgno.
    me->msgno = msgno.

    " Builder pattern
    instance = me.
  endmethod.


  method set_msgty.
    me->msgty = msgty.

    " Builder pattern
    instance = me.
  endmethod.


  method set_msgv1.
    me->msgv1 = msgv.

    " Builder pattern
    instance = me.
  endmethod.


  method set_msgv2.
    me->msgv2 = msgv.

    " Builder pattern
    instance = me.
  endmethod.


  method set_msgv3.
    me->msgv3 = msgv.

    " Builder pattern
    instance = me.
  endmethod.


  method set_msgv4.
    me->msgv4 = msgv.

    " Builder pattern
    instance = me.
  endmethod.


  method set_msgvars.
    me->set_msgv1( msgv1 ).
    me->set_msgv2( msgv2 ).
    me->set_msgv3( msgv3 ).
    me->set_msgv4( msgv4 ).

    " Builder pattern
    instance = me.
  endmethod.


  method set_timestamp.
    me->timestamp = tstmp.

    " Builder pattern
    instance = me.
  endmethod.


  method set_stripped_date.
    me->stripped_date = stripped_date.

    " Builder pattern
    instance = me.
  endmethod.


  method set_trace.
    me->trace = trace.

    " Builder pattern
    instance = me.
  endmethod.


  method set_abap_stack.
    me->abap_stack = abap_stack.

    " ABAP stack has been initialized.
    me->abap_stack_initialized = abap_true.

    " Builder pattern
    instance = me.
  endmethod.


  method build.
    message = new zcl_lso_log_message( msgty = me->msgty
                                       msgid = me->msgid
                                       msgno = me->msgno
                                       msgv1 = me->msgv1
                                       msgv2 = me->msgv2
                                       msgv3 = me->msgv3
                                       msgv4 = me->msgv4
                                       trace = me->trace ).

    if me->log_id is not initial.
      message->set_log_id( me->log_id ).
    endif.

    if me->log_seqnr is not initial.
      message->set_log_seqnr( me->log_seqnr ).
    endif.

    if me->timestamp is not initial.
      message->set_timestamp( me->timestamp ).
    endif.

    if me->stripped_date is not initial.
      message->set_stripped_date( me->stripped_date ).
    endif.

    if me->abap_stack_initialized eq abap_true.
      message->set_abap_stack( me->abap_stack ).
    endif.
  endmethod.

endclass.
