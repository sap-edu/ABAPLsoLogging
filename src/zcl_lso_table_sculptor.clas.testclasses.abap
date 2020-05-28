class ltc_table_sculptor definition deferred.
class zcl_lso_table_sculptor definition local friends ltc_table_sculptor.

class ltc_table_sculptor definition final for testing
  duration short
  risk level harmless.

  private section.

    methods  setup.


    types: begin of ts_with_reference_columns,
             string        type string,
             char1         type char1,
             char2         type char2,
             char255       type char255,
             abap_bool     type abap_bool,
             int4          type int4,
             ref_interface type ref to if_http_client,
             ref_class     type ref to cl_http_client,
           end of ts_with_reference_columns.

    types: tt_with_refrence_columns type standard table of ts_with_reference_columns with default key.

    methods  get_table_with_2_ref_cols
      returning
        value(table) type tt_with_refrence_columns.

    methods  remove_reference_columns     for testing raising cx_static_check.

endclass.

class ltc_table_sculptor implementation.

  method setup.

  endmethod.

  method remove_reference_columns.

    try.
        " Given we have a table with 2 reference columns
        data(table)          = get_table_with_2_ref_cols( ).

        data(sculptor) = new zcl_lso_table_sculptor( ref #( table ) ).

        " When we remove them with the sculptor
        data(stripped_table) = sculptor->remove_reference_columns( ).

        " Then .. the stripped table does not have the 2 reference columns anymore.
        " ( No need to dereference with "ref #( )" as stripped_table is already dereferenced )
        data(sculptor_stripped) = new zcl_lso_table_sculptor( stripped_table ).

        cl_abap_unit_assert=>assert_equals( act = lines( sculptor_stripped->get_components( ) )
                                            exp = lines( sculptor->get_components( ) ) - 2
                                            msg = |Expected to have 2 fewer columns in the stripped table| ).

        " AND ... When we pass the stripped table to the Excel export it does not crash.
        field-symbols <stripped_table> type any table.
        assign stripped_table->* to <stripped_table>.

        data(excel) = new zcl_excel( ).
        excel->add_new_worksheet( )->bind_table( <stripped_table> ).

      catch cx_root into data(x).
        cl_abap_unit_assert=>assert_equals( exp = |Pink fluffy Unicorns| act = x->get_longtext( ) ).
    endtry.

  endmethod.

  method get_table_with_2_ref_cols.

    cl_http_client=>create_by_url( exporting url = |https://answers.sap.com|
                                   importing client = data(client_interface) ).

    data(client_class) = cast cl_http_client( client_interface ).

    table = value tt_with_refrence_columns( ( string        = |string1|
                                              char1         = |1|
                                              char2         = |11|
                                              char255       = |öalskdfjaölsfkj4ohaddkac5234512|
                                              abap_bool     = abap_true
                                              int4          = 11111111
                                              ref_class     = client_class
                                              ref_interface = client_interface )
                                            ( string        = |string2|
                                              char1         = |1|
                                              char2         = |22|
                                              char255       = |öalskdfjaölsfkj4ohaddkac5234512|
                                              abap_bool     = abap_true
                                              int4          = 22222222
                                              ref_class     = client_class
                                              ref_interface = client_interface ) ).

  endmethod.



endclass.
