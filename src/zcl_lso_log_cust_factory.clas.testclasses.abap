*"* use this source file for your ABAP unit test classes
class ltc_lso_log_cust_factory definition final for testing
  duration short
  risk level harmless.

  private section.
    data cut type ref to zcl_lso_log_cust_factory.

    class-methods class_teardown.
    methods setup.

    methods get for testing raising zcx_lso_log.
    methods get_not_existing for testing raising zcx_lso_log.
    methods get_delete_logs_after for testing raising zcx_lso_log.
    methods get_delete_messages_after for testing raising zcx_lso_log.
    methods get_delete_payloads_after for testing raising zcx_lso_log.
    methods get_delete_traces_after for testing raising zcx_lso_log.
endclass.


class ltc_lso_log_cust_factory implementation.

  method class_teardown.
*   Rollback is executed automatically...
  endmethod.

  method setup.

    me->cut = cast zcl_lso_log_cust_factory( zcl_lso_log_cust_factory=>instance( ) ).

  endmethod.

  method get.

    " Try to find customizing object with existing id.
    data(custom) = me->cut->zif_lso_log_cust_factory~get( zif_lso_log_cust_factory=>id-delete_log_after ).

    " Is customizing found?
    cl_abap_unit_assert=>assert_not_initial( custom ).

    " Is customizing id as expected?
    cl_abap_unit_assert=>assert_equals( act = custom->get_id( ) exp = zif_lso_log_cust_factory=>id-delete_log_after ).

    " Has customizing object any value?
    cl_abap_unit_assert=>assert_not_initial( custom->get_value( ) ).

  endmethod.


  method get_not_existing.
    try.
        " Try to find customizing object with NOT existing id.
        data(custom) = me->cut->zif_lso_log_cust_factory~get( '!!!_DUMMY_NOT_EXISTING_!!!' ).
      catch zcx_lso_log into data(cx_log).
        " Was expected exception raised?
        cl_abap_unit_assert=>assert_equals( act = cx_log->if_t100_message~t100key
                                            exp = cx_log->customizing_value_not_found ).
    endtry.

    " Custom object wasn't found?
    cl_abap_unit_assert=>assert_initial( custom ).
  endmethod.


  method get_delete_logs_after.
    cl_abap_unit_assert=>assert_not_initial( me->cut->get_delete_logs_after( ) ).
  endmethod.


  method get_delete_messages_after.
    cl_abap_unit_assert=>assert_not_initial( me->cut->get_delete_messages_after( ) ).
  endmethod.


  method get_delete_payloads_after.
    cl_abap_unit_assert=>assert_not_initial( me->cut->get_delete_payloads_after( ) ).
  endmethod.


  method get_delete_traces_after.
    cl_abap_unit_assert=>assert_not_initial( me->cut->get_delete_traces_after( ) ).
  endmethod.

endclass.
