class zcx_lso_log definition
  public
  inheriting from zcx_lso_message
  final
  create public .

  public section.
    constants:
      begin of zcx_lso_log,
        msgid type symsgid value 'ZLSO_LOG',
        msgno type symsgno value '000',
        attr1 type scx_attrname value 'MV_MSGV1',
        attr2 type scx_attrname value 'MV_MSGV2',
        attr3 type scx_attrname value 'MV_MSGV3',
        attr4 type scx_attrname value 'MV_MSGV4',
      end of zcx_lso_log .
    constants:
      begin of next_seqnr_failure,
        msgid type symsgid value 'ZLSO_LOG',
        msgno type symsgno value '002',
        attr1 type scx_attrname value 'MV_MSGV1',
        attr2 type scx_attrname value '',
        attr3 type scx_attrname value '',
        attr4 type scx_attrname value '',
      end of next_seqnr_failure .
    constants:
      begin of lock_error,
        msgid type symsgid value 'ZLSO_LOG',
        msgno type symsgno value '003',
        attr1 type scx_attrname value 'MV_MSGV1',
        attr2 type scx_attrname value '',
        attr3 type scx_attrname value '',
        attr4 type scx_attrname value '',
      end of lock_error .
    constants:
      begin of no_parameters,
        msgid type symsgid value 'ZLSO_LOG',
        msgno type symsgno value '005',
        attr1 type scx_attrname value 'MV_MSGV1',
        attr2 type scx_attrname value 'MV_MSGV2',
        attr3 type scx_attrname value 'MV_MSGV3',
        attr4 type scx_attrname value 'MV_MSGV4',
      end of no_parameters .
    constants:
      begin of save_error,
        msgid type symsgid value 'ZLSO_LOG',
        msgno type symsgno value '004',
        attr1 type scx_attrname value 'MV_MSGV1',
        attr2 type scx_attrname value 'MV_EXCEPTION_TEXT',
        attr3 type scx_attrname value '',
        attr4 type scx_attrname value '',
      end of save_error .
    constants:
      begin of cross_reference_error,
        msgid type symsgid value 'ZLSO_LOG',
        msgno type symsgno value '007',
        attr1 type scx_attrname value 'MV_MSGV1',
        attr2 type scx_attrname value 'MV_MSGV2',
        attr3 type scx_attrname value '',
        attr4 type scx_attrname value '',
      end of cross_reference_error .
    constants:
      begin of not_found,
        msgid type symsgid value 'ZLSO_LOG',
        msgno type symsgno value '008',
        attr1 type scx_attrname value 'MV_MSGV1',
        attr2 type scx_attrname value 'MV_MSGV2',
        attr3 type scx_attrname value 'MV_MSGV3',
        attr4 type scx_attrname value 'MV_MSGV4',
      end of not_found .
    constants:
      begin of no_trace_id,
        msgid type symsgid value 'ZLSO_LOG',
        msgno type symsgno value '009',
        attr1 type scx_attrname value '',
        attr2 type scx_attrname value '',
        attr3 type scx_attrname value '',
        attr4 type scx_attrname value '',
      end of no_trace_id .
    constants:
      begin of customizing_value_not_found,
        msgid type symsgid value 'ZLSO_LOG',
        msgno type symsgno value '010',
        attr1 type scx_attrname value 'MV_MSGV1',
        attr2 type scx_attrname value '',
        attr3 type scx_attrname value '',
        attr4 type scx_attrname value '',
      end of customizing_value_not_found .
    constants:
      begin of no_tcode_auth,
        msgid type symsgid value 'ZLSO_LOG',
        msgno type symsgno value '012',
        attr1 type scx_attrname value 'MV_MSGV1',
        attr2 type scx_attrname value '',
        attr3 type scx_attrname value '',
        attr4 type scx_attrname value '',
      end of no_tcode_auth .
    constants:
      " Unit Test Message :) &1 &2 &3 &4
      begin of unit_test_message,
        msgid type symsgid value 'ZLSO_LOG',
        msgno type symsgno value '999',
        attr1 type scx_attrname value 'MV_MSGV1',
        attr2 type scx_attrname value 'MV_MSGV2',
        attr3 type scx_attrname value 'MV_MSGV3',
        attr4 type scx_attrname value 'MV_MSGV4',
      end of unit_test_message .

    data mv_exception_text type string .

    methods constructor
      importing
        !textid            like if_t100_message=>t100key optional
        !previous          like previous optional
        !mv_msgv1          type string optional
        !mv_msgv2          type string optional
        !mv_msgv3          type string optional
        !mv_msgv4          type string optional
        !mv_exception_text type string optional
        severity           type zif_lso_cx=>ty_severity optional.
  protected section.
  private section.
endclass.



class zcx_lso_log implementation.


  method constructor ##ADT_SUPPRESS_GENERATION.
    call method super->constructor
      exporting
        previous = previous
        mv_msgv1 = mv_msgv1
        mv_msgv2 = mv_msgv2
        mv_msgv3 = mv_msgv3
        mv_msgv4 = mv_msgv4
        severity = severity.

    me->mv_exception_text = mv_exception_text .

    clear me->textid.
    if textid is initial.
      if_t100_message~t100key = zcx_lso_log .
    else.
      if_t100_message~t100key = textid.
    endif.
  endmethod.
endclass.
