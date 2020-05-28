class zcl_lso_log_builder definition
  public
  final
  create public.

  public section.
    methods constructor.

    methods add_message
      importing message     type ref to zcl_lso_log_message
      returning value(self) type ref to zcl_lso_log_builder.

    methods add_messages
      importing messages    type ref to if_object_collection
      returning value(self) type ref to zcl_lso_log_builder.

    methods add_ref_log
      importing ref_log     type ref to zcl_lso_log
      returning value(self) type ref to zcl_lso_log_builder.

    methods set_log_id
      importing id          type zlso_log-id
      returning value(self) type ref to zcl_lso_log_builder.

    methods set_timestamp
      importing timestamp   type zlso_log-timestamp
      returning value(self) type ref to zcl_lso_log_builder.

    methods set_date
      importing date        type zlso_log-log_date
      returning value(self) type ref to zcl_lso_log_builder.

    methods set_time
      importing time        type zlso_log-log_time
      returning value(self) type ref to zcl_lso_log_builder.

    methods set_tcode
      importing tcode       type zlso_log-tcode
      returning value(self) type ref to zcl_lso_log_builder.

    methods set_program
      importing program     type zlso_log-prog
      returning value(self) type ref to zcl_lso_log_builder.

    methods set_changed_by
      importing changed_by  type zlso_log-changed_by
      returning value(self) type ref to zcl_lso_log_builder.

    methods set_stripped_date
      importing stripped_date   type zlso_log-stripped_date
      returning value(instance) type ref to zcl_lso_log_builder.

    methods build
      returning value(log) type ref to zcl_lso_log.

  protected section.
  private section.
    data id type zlso_log-id.
    data tcode type zlso_log-tcode.
    data program type zlso_log-prog.
    data timestamp type zlso_log-timestamp.
    data date type zlso_log-log_date.
    data time type zlso_log-log_time.
    data changed_by type zlso_log-changed_by.
    data stripped_date type zlso_log-stripped_date.
    data messages type ref to cl_object_collection.
    data ref_logs type ref to cl_object_collection.

endclass.


class zcl_lso_log_builder implementation.

  method constructor.
    super->constructor( ).

    me->messages = new #( ).
    me->ref_logs = new #( ).
  endmethod.


  method add_messages.
    data(iterator) = messages->get_iterator( ).

    while iterator->has_next( ).
      me->add_message( cast zcl_lso_log_message( iterator->get_next( ) ) ).
    endwhile.

    " Builder pattern
    self = me.
  endmethod.


  method add_message.
    me->messages->add( message ).

    " Builder pattern
    self = me.
  endmethod.


  method add_ref_log.
    me->ref_logs->add( ref_log ).

    " Builder pattern
    self = me.
  endmethod.


  method set_log_id.
    me->id = id.

    " Builder pattern
    self = me.
  endmethod.


  method set_timestamp.
    me->timestamp = timestamp.

    if timestamp is not initial.
      data(tstmp_date) = value dats( ).
      data(tstmp_time) = value tims( ).

      try.
          " Get date/time from time stamp.
          zcl_lso_log_utils=>utc_tstmp_2_datetime(
            exporting
              iv_tstmp = me->timestamp
            importing
              ev_date  = tstmp_date
              ev_time  = tstmp_time ).
        catch cx_parameter_invalid_type
              cx_parameter_invalid_range.
          clear tstmp_date.
          clear tstmp_time.
      endtry.

      if tstmp_date is not initial.
        me->set_date( tstmp_date ).
      endif.

      if tstmp_time is not initial.
        me->set_time( tstmp_time ).
      endif.
    endif.

    " Builder pattern
    self = me.
  endmethod.


  method set_date.
    me->date = date.

    " Builder pattern
    self = me.
  endmethod.


  method set_time.
    me->time = time.

    " Builder pattern
    self = me.
  endmethod.


  method set_changed_by.
    me->changed_by = changed_by.

    " Builder pattern
    self = me.
  endmethod.


  method set_program.
    me->program = program.

    " Builder pattern
    self = me.
  endmethod.


  method set_tcode.
    me->tcode = tcode.

    " Builder pattern
    self = me.
  endmethod.


  method set_stripped_date.
    me->stripped_date = stripped_date.

    " Builder pattern
    instance = me.
  endmethod.


  method build.
    log = new zcl_lso_log( me->id ).

    if me->date is not initial.
      log->set_date( me->date ).
    endif.

    if me->time is not initial.
      log->set_time( me->time ).
    endif.

    if me->tcode is not initial.
      log->zif_lso_log_abstract~set_tcode( me->tcode ).
    endif.

    if me->program is not initial.
      log->zif_lso_log_abstract~set_program( me->program ).
    endif.

    if me->changed_by is not initial.
      log->set_changed_by( me->changed_by ).
    endif.

    if me->timestamp is not initial.
      log->set_timestamp( me->timestamp ).
    endif.

    if me->stripped_date is not initial.
      log->set_stripped_date( me->stripped_date ).
    endif.

    log->add_messages( messages ).

    if not me->ref_logs->is_empty( ).
      data(iterator) = me->ref_logs->get_iterator( ).

      while iterator->has_next( ).
        log->zif_lso_log~add_ref_log( cast zcl_lso_log( iterator->get_next( ) ) ).
      endwhile.
    endif.
  endmethod.

endclass.
