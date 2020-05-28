class zcl_lso_table_sculptor definition
  public
  final
  create public .

  public section.

    methods constructor
      importing
        table type ref to data.

    "! <p class="shorttext synchronized" lang="en">Remove Columns with Type Ref to classes or interfaces</p>
    methods remove_reference_columns
      returning
        value(new_table) type ref to data.

    methods get_components
      returning
        value(components) type abap_component_tab . "cl_abap_structdescr=>component_table( )

  protected section.

  private section.

    data: table type ref to data.

endclass.

class zcl_lso_table_sculptor implementation.

  method constructor.

    me->table = table.

  endmethod.

  method get_components.

    field-symbols: <old_table> type any table.
    assign me->table->* to <old_table>.

    data(table_describer)     = cast cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( <old_table> ) ).
    data(structure_describer) = cast cl_abap_structdescr( table_describer->get_table_line_type( ) ).

    components = structure_describer->get_components( ).

  endmethod.

  method remove_reference_columns.

    data(components) = get_components( ).
    data(new_components) = value cl_abap_structdescr=>component_table( ).

    " Remove all components that are of reference type
    loop at components assigning field-symbol(<component>).
      if <component>-type->kind = cl_abap_typedescr=>kind_ref or
         <component>-type->kind = cl_abap_typedescr=>kind_class or
         <component>-type->kind = cl_abap_typedescr=>kind_intf.
        continue.
      else.
        append <component> to new_components.
      endif.
    endloop.

    " If there where no reference components we don't need to reconstruct the table...
    if new_components = components.
      new_table = me->table.
      return.
    endif.

    " create line type and table type based on new components
    data(line_type)  = cast cl_abap_structdescr( cl_abap_structdescr=>create( new_components ) ).
    data(table_type) = cast cl_abap_tabledescr(  cl_abap_tabledescr=>create(  line_type ) ).

    " Create new table with reduced-column-table-type
    create data new_table type handle table_type.
    field-symbols: <new_table> type any table.
    assign new_table->* to <new_table>.

    " Create Reference to current (old) table layout
    field-symbols: <old_table> type any table.
    assign me->table->* to <old_table>.

    " Move corresponding data from old to new table..
    <new_table> = corresponding #( <old_table> ).

  endmethod.

endclass.
