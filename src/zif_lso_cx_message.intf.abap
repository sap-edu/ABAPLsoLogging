interface zif_lso_cx_message
  public .
  types begin of ts_vars.
  types msgv1 type string.
  types msgv2 type string.
  types msgv3 type string.
  types msgv4 type string.
  types end of ts_vars.

  types begin of ts_struct.
  types msgid type symsgid.
  types msgno type symsgno.
  types msgty type symsgty.
  include type ts_vars.
  types end of ts_struct.

  methods get_log_message
    returning value(log_message) type ref to zcl_lso_log_message.

  methods get_symsg
    returning value(symsg) type symsg.

  methods get_struct
    returning value(struct) type ts_struct.

endinterface.
