class zcx_lso definition
  public
  inheriting from cx_static_check
  abstract
  create public .

  public section.
    interfaces zif_lso_cx .

    " Methods
    aliases get_severity for zif_lso_cx~get_severity.
    aliases get_msgty    for zif_lso_cx~get_msgty.
    aliases is_abort     for zif_lso_cx~is_abort.

    " Variables
    aliases mv_msgv1    for zif_lso_cx~mv_msgv1 .
    aliases mv_msgv2    for zif_lso_cx~mv_msgv2 .
    aliases mv_msgv3    for zif_lso_cx~mv_msgv3 .
    aliases mv_msgv4    for zif_lso_cx~mv_msgv4 .
    aliases mv_severity for zif_lso_cx~mv_severity.

    methods constructor
      importing textid   like if_t100_message=>t100key optional
                previous like previous optional
                mv_msgv1 type string optional
                mv_msgv2 type string optional
                mv_msgv3 type string optional
                mv_msgv4 type string optional
                severity type zif_lso_cx=>ty_severity optional.

    class-methods vars_from_cx
      importing io_lso_cx type ref to zif_lso_cx
      exporting ev_msgv1  type string
                ev_msgv2  type string
                ev_msgv3  type string
                ev_msgv4  type string.

  protected section.
  private section.
endclass.


class zcx_lso implementation.

  method constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = previous ).

    me->mv_msgv1 = mv_msgv1 .
    me->mv_msgv2 = mv_msgv2 .
    me->mv_msgv3 = mv_msgv3 .
    me->mv_msgv4 = mv_msgv4 .

    me->mv_severity = severity.

    clear me->textid.

    if textid is initial.
      if_t100_message~t100key = if_t100_message=>default_textid.
    else.
      if_t100_message~t100key = textid.
    endif.
  endmethod.


  method zif_lso_cx~get_severity.
    severity = me->mv_severity.
  endmethod.


  method zif_lso_cx~get_msgty.
    msgty = cond #( when me->is_abort( ) then zif_lso_log_message=>c_type-abort
                                         else zif_lso_log_message=>c_type-error ).
  endmethod.


  method zif_lso_cx~is_abort.
    result = boolc( me->mv_severity eq zif_lso_cx=>severity-abort ).
  endmethod.


  method zif_lso_cx~has_previous.
    result = boolc( me->previous is bound ).
  endmethod.


  method vars_from_cx.
    data lo_generic type ref to object.

    clear ev_msgv1.
    clear ev_msgv2.
    clear ev_msgv3.
    clear ev_msgv4.

    " Get imported exception class name.
    data(lv_class_name) = cl_abap_classdescr=>get_class_name( io_lso_cx ).

    create object lo_generic type (lv_class_name).

    try.
        " Cast down to specific object to have an access to all public object's attributes
        " that are not part of IF_T100_MESSAGE interface.
        lo_generic ?= io_lso_cx.

        " Add message vars.
        assign lo_generic->(io_lso_cx->if_t100_message~t100key-attr1)
          to field-symbol(<lv_msgv>).

        if sy-subrc eq 0.
          ev_msgv1 = <lv_msgv>.
        endif.

        assign lo_generic->(io_lso_cx->if_t100_message~t100key-attr2)
          to <lv_msgv>.

        if sy-subrc eq 0.
          ev_msgv2 = <lv_msgv>.
        endif.

        assign lo_generic->(io_lso_cx->if_t100_message~t100key-attr3)
          to <lv_msgv>.

        if sy-subrc eq 0.
          ev_msgv3 = <lv_msgv>.
        endif.

        assign lo_generic->(io_lso_cx->if_t100_message~t100key-attr4)
          to <lv_msgv>.

        if sy-subrc eq 0.
          ev_msgv4 = <lv_msgv>.
        endif.
      catch cx_sy_move_cast_error.
        ev_msgv1 = io_lso_cx->if_t100_message~t100key-attr1.
        ev_msgv2 = io_lso_cx->if_t100_message~t100key-attr2.
        ev_msgv3 = io_lso_cx->if_t100_message~t100key-attr3.
        ev_msgv4 = io_lso_cx->if_t100_message~t100key-attr4.
    endtry.
  endmethod.

endclass.
