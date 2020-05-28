class zcl_lso_log_range definition
  public
  final
  create public .

  public section.
    interfaces zif_lso_log_range.

    "! <p class="shorttext synchronized" lang="en">Split table for being used in OpenSQL 'IN'.</p>
    "! <p>SAP note 635318 - Open SQL: Size restrictions for commands.</p>
    "! <p>IN with a list in the WHERE clause, must not contain more than 1000 elements.
    "!    The length of the statement in the database must not exceed 28672 characters (with Unicode, 14336 characters).</p>
    "!
    "! @parameter table | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter split_tables | <p class="shorttext synchronized" lang="en"></p>
    methods split_table_for_opensql_in
      importing table               type any table
      returning value(split_tables) type zif_lso_log_range=>tt_data.

  protected section.
  private section.
endclass.



class zcl_lso_log_range implementation.

  method split_table_for_opensql_in.
    " How To use it:
    " field-symbols <table> type standard table.
    "
    " Split ids range table into smaller chunks to have it safely used in OpenSQL IN statement.
    " loop at new zcl_lso_range( )->split_table_for_opensql_in( ids ) into data(ids_split).
    "  assign ids_split->* to <table>.
    "
    "  IMPORTANT - use APPENDING instead of INSERT into table!
    "  select * from table
    "    APPENDING table @data(table)
    "    where id in @<table>.
    "
    "endloop.
    field-symbols <split_table> type standard table.

    data split_table type ref to data.

    data(table_descr) = cast cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( table ) ).

    loop at table assigning field-symbol(<row>).
      if ( sy-tabix - 1 ) mod zif_lso_log_range=>c_max_opensql_in = 0.
        create data split_table type handle table_descr.
        insert split_table into table split_tables.
        assign split_table->* to <split_table>.
      endif.

      if <split_table> is assigned.
        insert <row> into table <split_table>.
      endif.
    endloop.
  endmethod.

endclass.
