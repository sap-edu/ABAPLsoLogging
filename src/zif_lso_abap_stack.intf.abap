interface zif_lso_abap_stack
  public .

  methods get
    returning value(stack) type cl_abap_get_call_stack=>formatted_entry_stack.

  methods before_ref
    importing object       type ref to object
    returning value(entry) type cl_abap_get_call_stack=>formatted_entry.

  methods before_pattern
    importing pattern      type string
    returning value(entry) type cl_abap_get_call_stack=>formatted_entry.

endinterface.
