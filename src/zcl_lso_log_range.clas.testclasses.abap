*"* use this source file for your ABAP unit test classes
class ltc_lso_range definition final for testing
  duration short
  risk level harmless.

  private section.
    data cut type ref to zcl_lso_log_range.

    class-methods class_setup.
    class-methods class_teardown.

    methods setup.
    methods teardown.

    methods split_table_for_opensql_in for testing raising cx_static_check.

endclass.


class ltc_lso_range implementation.

  method class_setup.
  endmethod.


  method class_teardown.
*   Rollback is executed automatically...
  endmethod.


  method setup.
    me->cut = new #( ).
  endmethod.


  method teardown.
  endmethod.


  method split_table_for_opensql_in.
    types ltt_selopts type standard table of selopt with empty key.

    field-symbols <table> type standard table.
    field-symbols <data> type ref to data.

    data(selopts) = value ltt_selopts( ).

    " When there are some selection options ready for being used in Open SQL statement.
    do 2420 times.
      insert value #( sign   = zif_lso_log_range=>c_sign-include
                      option = zif_lso_log_range=>c_option-equal
                      low    = sy-index )
        into table selopts.
    enddo.

    data(split_tables) = me->cut->split_table_for_opensql_in( selopts ).

    " Then selection options table will be split into several smaller ones.
    cl_abap_unit_assert=>assert_equals( act = lines( split_tables )
                                        exp = 5 ).

    " Looping at split tables...
    loop at split_tables into data(split_table).
      assign split_table->* to <table>.
    endloop.

    " Test number of entries for each split.
    " Split 1
    assign split_tables[ 1 ] to <data>.
    assign <data>->* to <table>.

    cl_abap_unit_assert=>assert_equals( act = lines( <table> )
                                        exp = zif_lso_log_range=>c_max_opensql_in ).

    " Split 2
    assign split_tables[ 2 ] to <data>.
    assign <data>->* to <table>.

    cl_abap_unit_assert=>assert_equals( act = lines( <table> )
                                        exp = zif_lso_log_range=>c_max_opensql_in ).

    " Split 3
    assign split_tables[ 3 ] to <data>.
    assign <data>->* to <table>.

    cl_abap_unit_assert=>assert_equals( act = lines( <table> )
                                        exp = zif_lso_log_range=>c_max_opensql_in ).

    " Split 4
    assign split_tables[ 4 ] to <data>.
    assign <data>->* to <table>.

    cl_abap_unit_assert=>assert_equals( act = lines( <table> )
                                        exp = zif_lso_log_range=>c_max_opensql_in ).

    " Split 5
    assign split_tables[ 5 ] to <data>.
    assign <data>->* to <table>.

    cl_abap_unit_assert=>assert_equals( act = lines( <table> )
                                        exp = 420 ).
  endmethod.

endclass.
