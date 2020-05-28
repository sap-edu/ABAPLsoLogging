class zcl_lso_log_cust definition
  public
  final
  create private
  global friends zif_lso_log_cust_factory.

  public section.
    interfaces zif_lso_log_cust.

    aliases get_id for zif_lso_log_cust~get_id.
    aliases get_value for zif_lso_log_cust~get_value.
    aliases set_value for zif_lso_log_cust~set_value.
    aliases save for zif_lso_log_cust~save.

    methods constructor
      importing id    type zlso_log_cust-id
                value type zlso_log_cust-value.

  protected section.
  private section.
    data id type zlso_log_cust-id.
    data value type zlso_log_cust-value.
endclass.


class zcl_lso_log_cust implementation.

  method constructor.
    me->id = id.
    me->value = value.
  endmethod.


  method zif_lso_log_cust~get_id.
    id = me->id.
  endmethod.


  method zif_lso_log_cust~get_value.
    value = me->value.
  endmethod.


  method zif_lso_log_cust~set_value.
    me->value = value.
  endmethod.


  method zif_lso_log_cust~save.
    " Update customizing value on DB.
    update zlso_log_cust set value = @me->value where id = @me->id.

    result = boolc( sy-dbcnt eq 1 ).
  endmethod.

endclass.
