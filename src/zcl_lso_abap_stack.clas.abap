class zcl_lso_abap_stack definition
  public
  final
  create public .

  public section.
    interfaces zif_lso_abap_stack.

    methods constructor.

  protected section.
  private section.
    data class_descr type ref to cl_abap_classdescr.
endclass.


class zcl_lso_abap_stack implementation.

  method constructor.
    me->class_descr = cast cl_abap_classdescr( cl_abap_classdescr=>describe_by_object_ref( me ) ).
  endmethod.


  method zif_lso_abap_stack~before_ref.
    data(class_descr) = cast cl_abap_classdescr( cl_abap_classdescr=>describe_by_object_ref( object ) ).

    entry = me->zif_lso_abap_stack~before_pattern( |{ class_descr->get_relative_name( ) }*| ).
  endmethod.


  method zif_lso_abap_stack~before_pattern.
    data(abap_call_stack) = me->zif_lso_abap_stack~get( ).

    data(tabix) = value sy-tabix( ).

    loop at abap_call_stack reference into data(stack).
      " Do not take ABAP_STACT class into account.
      if stack->progname cs me->class_descr->get_relative_name( ).
        continue.
      endif.

      if stack->progname np pattern and stack->event np pattern.
        tabix = sy-tabix.
        exit.
      endif.
    endloop.

    if tabix > 0.
      stack = ref #( abap_call_stack[ tabix ] ).

      if stack->progname cs 'UNIT_TEST_CLASS'.
        " Unit Test Method invoker, skip to the previous one in the stack.
        tabix = tabix - 1.
      endif.
    endif.

    if tabix > 0.
      data(entry_ref) = ref #( abap_call_stack[ tabix ] optional ).
    endif.

    if entry_ref is bound.
      entry = entry_ref->*.
    endif.
  endmethod.


  method zif_lso_abap_stack~get.
    stack = cl_abap_get_call_stack=>format_call_stack_with_struct( cl_abap_get_call_stack=>get_call_stack( ) ).
  endmethod.

endclass.
