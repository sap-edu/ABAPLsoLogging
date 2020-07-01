*"* use this source file for your ABAP unit test classes
class ltc_lso_abap_stack definition final for testing
  duration short
  risk level harmless.

  private section.
    data cut type ref to zcl_lso_abap_stack.

    class-methods class_setup.
    class-methods class_teardown.

    methods setup.
    methods teardown.

    methods get for testing.
    methods before_pattern for testing raising cx_static_check.
    methods before_ref for testing raising cx_static_check.
endclass.


class ltc_lso_abap_stack implementation.

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


  method get.
    cl_abap_unit_assert=>assert_not_initial( me->cut->zif_lso_abap_stack~get( ) ).
  endmethod.

  method before_pattern.
    data(exp) = value zif_lso_abap_stack=>ts_entry( stack_depth = 0
                                                    kind        = 'METHOD'
                                                    progname    = 'ZCL_LSO_ABAP_STACK============CP'
                                                    includename = 'ZCL_LSO_ABAP_STACK============CCAU'
                                                    line        = '61'
                                                    event       = 'LTC_LSO_ABAP_STACK=>BEFORE_PATTERN' ).

    data(act) = me->cut->zif_lso_abap_stack~before_pattern( 'LTC_LSO_ABAP_STACK*' ).
    act-stack_depth = exp-stack_depth = 0. " Ignore stack depth, it differs by system and call context of unit test

    cl_abap_unit_assert=>assert_equals( act = act
                                        exp = exp
                                        msg = |If the expected line differs, you propbably adjusted this unit test...| ).
  endmethod.


  method before_ref.
    data(exp) = value zif_lso_abap_stack=>ts_entry( stack_depth = 0
                                                    kind        = 'METHOD'
                                                    progname    = 'ZCL_LSO_ABAP_STACK============CP'
                                                    includename = 'ZCL_LSO_ABAP_STACK============CCAU'
                                                    line        = '79'
                                                    event       = 'LTC_LSO_ABAP_STACK=>BEFORE_REF' ).

    data(act) = me->cut->zif_lso_abap_stack~before_ref( me ).
    act-stack_depth = exp-stack_depth = 0. " Ignore stack depth, it differs by system and call context of unit test

    cl_abap_unit_assert=>assert_equals( act = act
                                        exp = exp
                                        msg = |If the expected line differs, you propbably adjusted this unit test...|  ).
  endmethod.

endclass.
