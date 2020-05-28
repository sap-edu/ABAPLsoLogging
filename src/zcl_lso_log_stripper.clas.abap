class zcl_lso_log_stripper definition
  public
  final
  create public .

  public section.
    constants c_tcode type sy-tcode value 'ZLSO_LOG_STRIPPER'.

    "! <p class="shorttext synchronized" lang="en">Execute Log Stripper transaction</p>
    methods execute raising zcx_lso_log.

  protected section.
  private section.
endclass.


class zcl_lso_log_stripper implementation.

  method execute.
    try.
        call transaction c_tcode with authority-check.
      catch cx_sy_authorization_error.
        raise exception type zcx_lso_log
          exporting
            textid   = zcx_lso_log=>no_tcode_auth
            mv_msgv1 = conv #( c_tcode ).
    endtry.
  endmethod.

endclass.
