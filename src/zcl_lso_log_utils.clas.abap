class zcl_lso_log_utils definition
  public
  final
  create public .

  public section.

    class-methods utc_tstmp_2_datetime
      importing
        !iv_tstmp type timestampl
      exporting
        !ev_date  type dats
        !ev_time  type tims
      raising
        cx_parameter_invalid_type
        cx_parameter_invalid_range .

    class-methods show_message_in_code
      importing
        !iv_class  type rsdag-arbgb default 'ZLSO_TASK'
        !iv_number type t100-msgnr  default '036'.

  protected section.
  private section.
endclass.


class zcl_lso_log_utils implementation.

  method show_message_in_code.
    call function 'Z_LSO_TRACE_SHOW_MSG_IN_CODE'
      exporting
        iv_message_class  = iv_class
        iv_message_number = iv_number.
  endmethod.


  method utc_tstmp_2_datetime.
    clear ev_date.
    clear ev_time.

    data(tstmp_no_milisecs) = value timestamp( ).

    " Convert UTC Time Stamp to the one with no miliseconds.
    " The SYSTEMTSTMP_UTC2SYST method imports time stamp value in TIMESTAMP type.
    " For instance this time stamp - 20160509113859.5350000 - is converted to the TIMESTAMP 20160509113860.
    " Time 113860 is wrong (60 seconds) and in result it cannot be converted to proper date/time.
    " The MOVE method converts problematic time to the 113900 value.
    cl_abap_tstmp=>move(
      exporting
        tstmp_src = iv_tstmp
      importing
        tstmp_tgt = tstmp_no_milisecs ).

    " Convert time stamp into date and time.
    cl_abap_tstmp=>systemtstmp_utc2syst(
      exporting
        utc_tstmp = tstmp_no_milisecs
      importing
        syst_date = ev_date
        syst_time = ev_time ).
  endmethod.

endclass.
