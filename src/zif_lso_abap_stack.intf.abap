interface zif_lso_abap_stack
  public .

  types begin of ts_entry.
  types stack_depth type i.
  types kind type c length 30.
  types progname type c length 40.
  types includename type c length 40.
  types line type i.
  types event type c length 61.
  types end of ts_entry.

  types tt_entries type standard table of ts_entry with empty key.

  methods get
*    returning value(stack) type cl_abap_get_call_stack=>formatted_entry_stack
    returning value(stack) type tt_entries.

  methods before_ref
    importing object       type ref to object
*    returning value(entry) type cl_abap_get_call_stack=>formatted_entry
    returning value(entry) type ts_entry.

  methods before_pattern
    importing pattern      type string
*    returning value(entry) type cl_abap_get_call_stack=>formatted_entry
    returning value(entry) type ts_entry.

endinterface.
