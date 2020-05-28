*"* use this source file for your ABAP unit test classes
class ltc_lso_log_cust definition deferred.
class zcl_lso_log_cust definition local friends ltc_lso_log_cust.

class ltc_lso_log_cust definition final for testing
  duration short
  risk level harmless.

  private section.
    constants begin of c_custom.
    constants id type zlso_log_cust-id value 'LTC_LSO_LOG_CUST_ID'.
    constants value type zlso_log_cust-value value 'LTC_LSO_LOG_CUST_VALUE'.
    constants end of c_custom.

    data cut type ref to zcl_lso_log_cust.

    class-methods class_setup.
    class-methods class_teardown.

    methods setup.
    methods teardown.

    methods get_id for testing.
    methods get_value for testing.
    methods save for testing.
endclass.


class ltc_lso_log_cust implementation.

  method class_setup.
  endmethod.


  method class_teardown.
*   Rollback is executed automatically...
  endmethod.


  method setup.
    data(cust) = value zlso_log_cust( id    = c_custom-id
                                      sysid = sy-sysid
                                      value = c_custom-value ).

    " Add new dummy customizing value.
    insert zlso_log_cust from @cust.

    me->cut = new #( id    = cust-id
                     value = cust-value ).
  endmethod.


  method teardown.
    " Delete dummy customizing value.
    delete from zlso_log_cust where id    = @c_custom-id
                                and sysid = @sy-sysid.
  endmethod.


  method get_id.
    cl_abap_unit_assert=>assert_equals( act = me->cut->zif_lso_log_cust~get_id( ) exp = c_custom-id ).
  endmethod.


  method get_value.
    cl_abap_unit_assert=>assert_equals( act = me->cut->zif_lso_log_cust~get_value( ) exp = c_custom-value ).
  endmethod.


  method save.
    " Was save successful?
    cl_abap_unit_assert=>assert_true( me->cut->save( ) ).
  endmethod.

endclass.
