class zcx_lso_message definition
  public
  inheriting from zcx_lso
  create public .

  public section.
    interfaces zif_lso_cx_message .

    methods constructor
      importing textid   like if_t100_message=>t100key optional
                previous like previous optional
                mv_msgv1 type string optional
                mv_msgv2 type string optional
                mv_msgv3 type string optional
                mv_msgv4 type string optional
                severity type zif_lso_cx=>ty_severity optional .

  protected section.
    methods get_vars
      importing cx          type ref to zcx_lso_message
      returning value(vars) type zif_lso_cx_message=>ts_vars.

  private section.
    data text type string.

    methods get_cx_struct
      importing cx            type ref to zcx_lso_message
      returning value(struct) type zif_lso_cx_message=>ts_struct.

    methods create_log_message
      importing cx                 type ref to zcx_lso_message
      returning value(log_message) type ref to zcl_lso_log_message.
endclass.


class zcx_lso_message implementation.

  method constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = previous
                        mv_msgv1 = mv_msgv1
                        mv_msgv2 = mv_msgv2
                        mv_msgv3 = mv_msgv3
                        mv_msgv4 = mv_msgv4
                        severity = severity ).

    clear me->textid.

    if textid is initial.
      if_t100_message~t100key = if_t100_message=>default_textid.
    else.
      if_t100_message~t100key = textid.
    endif.
  endmethod.


  method zif_lso_cx_message~get_log_message.
    log_message = me->create_log_message( me ).

    if log_message is bound and log_message->get_text( ) is initial and me->zif_lso_cx~has_previous( ) and me->previous is instance of zcx_lso_message.
      " No exception text, try to get it from the previous one if bound.
      log_message = me->create_log_message( cast #( me->previous ) ).
    endif.
  endmethod.


  method zif_lso_cx_message~get_symsg.
    data(log_message) = me->zif_lso_cx_message~get_log_message( ).

    symsg = cond #( when log_message is bound then log_message->get_symsg( ) ).
  endmethod.


  method zif_lso_cx_message~get_struct.
    struct = me->get_cx_struct( me ).
  endmethod.


  method create_log_message.
    data(struct) = me->get_cx_struct( cx ).

    log_message = new zcl_lso_log_message( msgid = struct-msgid
                                           msgno = struct-msgno
                                           msgty = struct-msgty
                                           msgv1 = struct-msgv1
                                           msgv2 = struct-msgv2
                                           msgv3 = struct-msgv3
                                           msgv4 = struct-msgv4 ).
  endmethod.


  method get_cx_struct.
    struct = value #( msgid = cx->if_t100_message~t100key-msgid
                      msgno = cx->if_t100_message~t100key-msgno
                      msgty = cx->zif_lso_cx~get_msgty( ) ).

    struct = corresponding #( base ( struct ) me->get_vars( cx ) ).
  endmethod.


  method get_vars.
    data object type ref to object.

    " Get exception class name.
    data(class_name) = cl_abap_classdescr=>get_class_name( cx ).

    create object object type (class_name).

    try.
        " Cast down to specific object to have an access to all public object's attributes that are not part of IF_T100_MESSAGE interface.
        object ?= cx.

        " Add message variables.
        assign object->(cx->if_t100_message~t100key-attr1) to field-symbol(<msgv1>).
        assign object->(cx->if_t100_message~t100key-attr2) to field-symbol(<msgv2>).
        assign object->(cx->if_t100_message~t100key-attr3) to field-symbol(<msgv3>).
        assign object->(cx->if_t100_message~t100key-attr4) to field-symbol(<msgv4>).
      catch cx_sy_move_cast_error.
        unassign <msgv1>.
        unassign <msgv2>.
        unassign <msgv3>.
        unassign <msgv4>.
    endtry.

    vars = value #( msgv1 = cond #( when <msgv1> is assigned then <msgv1> )
                    msgv2 = cond #( when <msgv2> is assigned then <msgv2> )
                    msgv3 = cond #( when <msgv3> is assigned then <msgv3> )
                    msgv4 = cond #( when <msgv4> is assigned then <msgv4> ) ).
  endmethod.

endclass.
