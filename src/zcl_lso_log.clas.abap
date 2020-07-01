class zcl_lso_log definition
  inheriting from zcl_lso_log_abstract
  public
  final
  create public
  global friends zcl_lso_log_factory
                 zcl_lso_log_builder.

  public section.
    interfaces zif_lso_log.
    interfaces zif_lso_comparable.

    constants lock_object type if_abap_lock_object=>tv_name value '/EDU/ELOG'.

    " Backward compatibility
    " Abstract
    aliases:
             add_message            for zif_lso_log_abstract~add_message,
             add_messages           for zif_lso_log_abstract~add_messages,
             add_symsg              for zif_lso_log_abstract~add_symsg,

             get_messages_rfc       for zif_lso_log_abstract~get_messages_rfc,
             get_symsgs             for zif_lso_log_abstract~get_symsgs,
             get_program            for zif_lso_log_abstract~get_program,
             get_tcode              for zif_lso_log_abstract~get_tcode,

             has_abort              for zif_lso_log_abstract~has_abort,
             has_error              for zif_lso_log_abstract~has_error,
             has_info               for zif_lso_log_abstract~has_info,
             has_success            for zif_lso_log_abstract~has_success,
             has_warning            for zif_lso_log_abstract~has_warning,
             has_message_key        for zif_lso_log_abstract~has_message_key,

             exception              for zif_lso_log_abstract~exception,
             error                  for zif_lso_log_abstract~error,
             warning                for zif_lso_log_abstract~warning,
             success                for zif_lso_log_abstract~success,
             info                   for zif_lso_log_abstract~info,
             abort                  for zif_lso_log_abstract~abort,
             trace_message          for zif_lso_log_abstract~trace_message.
    " Log
    aliases: add                    for zif_lso_log~add,
             add_ref_log            for zif_lso_log~add_ref_log,

             get_id                 for zif_lso_log~get_id,
             get_seqnr              for zif_lso_log~get_seqnr,

             get_messages           for zif_lso_log~get_messages,
             get_changed_by         for zif_lso_log~get_changed_by,
             get_date               for zif_lso_log~get_date,
             get_time               for zif_lso_log~get_time,
             get_timestamp          for zif_lso_log~get_timestamp,
             get_structure          for zif_lso_log~get_structure,
             get_object             for zif_lso_log~get_object,
             get_ref_logs           for zif_lso_log~get_ref_logs,

             has_exception          for zif_lso_log~has_exception,
             has_message            for zif_lso_log~has_message,
             has_messages           for zif_lso_log~has_messages,
             has_message_type       for zif_lso_log~has_message_type,

             has_ref_log            for zif_lso_log~has_ref_log,
             has_ref_logs           for zif_lso_log~has_ref_logs,

             lock                   for zif_lso_log~lock,
             unlock                 for zif_lso_log~unlock,
             save                   for zif_lso_log~save,
             delete                 for zif_lso_log~delete.

    methods constructor
      importing id type zlso_log-id optional .

    class-methods set_context
      importing context type zlso_d_log_context.

    class-methods clear_context.

  protected section.

  private section.
    types tt_objects type hashed table of zlso_s_log with unique key id seqnr.

    class-data log_context type zlso_d_log_context.

    data id type zlso_log-id .
    data seqnr type zlso_log-seqnr .
    data timestamp type timestampl .
    data date type zlso_log-log_date .
    data time type zlso_log-log_time .
    data mode type zlso_log-log_mode .
    data changed_by type zlso_log-changed_by .
    data context type zlso_d_log_context.
    data stripped_date type zlso_log-stripped_date.
    data ref_logs type zlso_tt_logs.
    data cross_refs type tt_objects.

    methods create_id
      returning value(id) type zlso_log-id .

    methods check_cross_reference
      importing log           type ref to zif_lso_log
      returning value(result) type abap_bool
      raising   zcx_lso_log.

    methods get_next_seqnr
      returning value(seqnr) type zlso_log-seqnr
      raising   zcx_lso_log .

    methods get_messages_to_save
      returning value(messages) type zlso_tt_log_messages.

    methods set_seqnr
      importing seqnr type zlso_log-seqnr .

    methods set_timestamp
      importing timestamp type zlso_log-timestamp.

    methods set_date
      importing date type zlso_log-log_date.

    methods set_time
      importing time type zlso_log-log_time.

    methods set_changed_by
      importing changed_by type zlso_log-changed_by.

    methods set_stripped_date
      importing stripped_date type zlso_log-stripped_date.

    methods fill_with_timestamp
      importing log_structure type ref to zlso_log.

    methods save_ref_logs
      returning value(saved_ref_logs) type zlso_tt_logs
      raising   zcx_lso_log.

    methods save_db
      importing log_structure type ref to zlso_log
      returning value(result) type abap_bool
      raising   zcx_lso_log.

    methods lock_before_save
      raising zcx_lso_log.

    methods unlock_after_save.
endclass.


class zcl_lso_log implementation.

  method constructor.
    super->constructor( ).

    me->id = id.
    me->ref_logs = value #( ).

    "Assign context if set.
    me->context = zcl_lso_log=>log_context.
  endmethod.


  method zif_lso_log~add.
    me->zif_lso_log_abstract~add_message( message ).
  endmethod.


  method zif_lso_log~add_ref_log.
    insert ref_log->get_object( ) into table me->ref_logs.
  endmethod.


  method zif_lso_log~get_id.
    if me->id is initial.
      " Lazy loading not to generate next id from number range if not needed.
      me->id = me->create_id( ).
    endif.

    id = me->id.
  endmethod.


  method zif_lso_log~get_seqnr.
    seqnr = me->seqnr.
  endmethod.


  method zif_lso_log~get_changed_by.
    changed_by = me->changed_by.
  endmethod.


  method zif_lso_log~get_date.
    date = me->date.
  endmethod.


  method zif_lso_log~get_time.
    time = me->time.
  endmethod.


  method zif_lso_log~get_timestamp.
    timestamp = me->timestamp.
  endmethod.


  method zif_lso_log~get_messages.
    insert lines of me->messages into table messages.

    if with_ref eq abap_true and me->zif_lso_log~has_ref_logs( ).
      " Get also messages from reference logs.
      loop at me->ref_logs using key object_key reference into data(ref_log).
        insert lines of ref_log->instance->get_messages( with_ref ) into table messages.
      endloop.
    endif.
  endmethod.


  method zif_lso_log~get_structure.
    structure = value #( id         = me->id
                         seqnr      = me->seqnr
                         timestamp  = me->timestamp
                         log_date   = me->date
                         log_time   = me->time
                         log_mode   = me->mode
                         changed_by = me->changed_by
                         tcode      = me->tcode
                         prog       = me->program ).
  endmethod.


  method zif_lso_log~get_ref_logs.
    logs = me->ref_logs.
  endmethod.


  method zif_lso_log~get_stripped_date.
    stripped_date = me->stripped_date.
  endmethod.


  method zif_lso_log~get_object.
    object = value #( id       = me->id
                      seqnr    = me->seqnr
                      instance = me ).
  endmethod.


  method zif_lso_log~has_exception.
    " Is there an exception message in the log?
    result = me->zif_lso_log_abstract~has_exception( cx_t100_key ).

    if result eq abap_true.
      return.
    endif.

    if with_ref eq abap_true and me->zif_lso_log~has_ref_logs( ).
      " Check reference logs if exception was added there.
      loop at me->ref_logs using key object_key reference into data(ref_log).
        result = ref_log->instance->has_exception( cx_t100_key ).

        if result eq abap_true.
          " Exception message has been found, no need to check further reference logs.
          return.
        endif.
      endloop.
    endif.
  endmethod.


  method zif_lso_log~has_message.
    " Is there a message in the log?
    result = me->zif_lso_log_abstract~has_message( message ).

    if result eq abap_true.
      return.
    endif.

    if with_ref eq abap_true and me->zif_lso_log~has_ref_logs( ).
      " Check reference logs if message was added there.
      loop at me->ref_logs using key object_key reference into data(ref_log).
        result = ref_log->instance->has_message( message  = message
                                                 with_ref = with_ref ).
        if result eq abap_true.
          " Message has been found, no need to check further reference logs.
          return.
        endif.
      endloop.
    endif.
  endmethod.


  method zif_lso_log~has_messages.
    result = me->zif_lso_log_abstract~has_messages( ).

    if result eq abap_true.
      return.
    endif.

    if with_ref eq abap_true and me->zif_lso_log~has_ref_logs( ).
      " Check reference logs if messages were added there.
      loop at me->ref_logs using key object_key reference into data(ref_log).
        result = ref_log->instance->has_messages( with_ref ).

        if result eq abap_true.
          " Message has been found, no need to check further reference logs.
          return.
        endif.
      endloop.
    endif.
  endmethod.


  method zif_lso_log~has_message_type.
    data(messages) = me->zif_lso_log~get_messages( with_ref ).

    loop at messages using key object_key reference into data(message).
      if message->instance->get_type( ) eq type.
        result = abap_true.
        return.
      endif.
    endloop.
  endmethod.


  method zif_lso_log~has_ref_log.
    if not me->zif_lso_log~has_ref_logs( ).
      return.
    endif.

    result = boolc( line_exists( me->ref_logs[ key object_key components id    = log->get_id( )
                                                                         seqnr = log->get_seqnr( ) ] ) ).
  endmethod.


  method zif_lso_log~has_ref_logs.
    result = boolc( me->ref_logs is not initial ).
  endmethod.


  method zif_lso_log~is_stripped.
    result = xsdbool( me->stripped_date is not initial ).
  endmethod.


  method zif_lso_log~delete.
    data(id) = me->zif_lso_log~get_id( ).

    delete from zlso_log_payload
      where trace_id in (
        select id
          from zlso_log_trace
         where id in (
           select trace_id
             from zlso_log_message
            where log_id eq @id )
      ).

    delete from zlso_log_headers
      where trace_id in (
        select id
          from zlso_log_trace
         where id in (
           select trace_id
             from zlso_log_message
            where log_id eq @id )
      ).

    delete from zlso_log_trace
      where id in (
        select trace_id
          from zlso_log_message
         where log_id eq @id ).

    delete from zlso_log_message where log_id eq @id.
    delete from zlso_log where id eq @id.

    result = boolc( sy-subrc eq 0 ).

    commit work.
  endmethod.


  method set_seqnr.
    me->seqnr = seqnr.
  endmethod.


  method set_timestamp.
    me->timestamp = timestamp.
  endmethod.


  method set_date.
    me->date = date.
  endmethod.


  method set_time.
    me->time = time.
  endmethod.


  method set_changed_by.
    me->changed_by = changed_by.
  endmethod.


  method set_stripped_date.
    me->stripped_date = stripped_date.
  endmethod.


  method zif_lso_log~lock.
    data(lock) = cl_abap_lock_object_factory=>get_instance( lock_object ).
    data(id) = me->get_id( ).

    " Lock log table against potential modifications for given id.
    lock->enqueue( it_parameter = value #( ( name = 'ID' value = ref #( id ) ) )
                   _wait        = if_abap_lock_object=>cs_wait-yes ).
  endmethod.


  method zif_lso_log~unlock.
    data(lock) = cl_abap_lock_object_factory=>get_instance( lock_object ).
    data(id) = me->get_id( ).

    " Unlock log table for further modifications.
    lock->dequeue( it_parameter = value #( ( name = 'ID' value = ref #( id ) ) ) ).
  endmethod.


  method zif_lso_log~save.
    if me->zif_lso_log~has_messages( abap_true ) eq abap_false.
      " There are messages to be stored neither in main nor referenced log - no need to store the log.
      return.
    endif.

    clear me->cross_refs[].

    if me->zif_lso_log~has_ref_logs( ).
      " Check if there is a cross reference relation between logs, raise an exception if so!
      " A --> B
      " B --> A
      me->check_cross_reference( me ).
    endif.

    " Lock log id before saving procedure.
    me->lock_before_save( ).

    try.
        " Process referenced logs first.
        if me->zif_lso_log~has_ref_logs( ).
          data(saved_ref_logs) = me->save_ref_logs( ).
        endif.

        " Get messages that haven't been saved yet, do not consider messages from referenced logs.
        " These ones will be saved with reference log save.
        data(messages_to_save) = me->get_messages_to_save( ).

        if messages_to_save[] is initial and saved_ref_logs[] is initial.
          " There are neither messages to be saved nor reference logs saved.
          result = abap_false.

          " Unlock the log table if the caller triggered saving the log even though no additional messages where added.
          me->unlock_after_save( ).

          return.
        endif.

        " Initialize structure for log saving.
        data(ls_log) = value zlso_log( id            = me->zif_lso_log~get_id( )
                                       seqnr         = me->get_next_seqnr( )
                                       changed_by    = cl_abap_context_info=>get_user_alias( )
                                       tcode         = me->tcode
                                       prog          = me->program
                                       context       = me->context
                                       stripped_date = me->zif_lso_log~get_stripped_date( ) ).

        " Set time stamp/date/time for this run.
        me->fill_with_timestamp( ref #( ls_log ) ).

        " Set program mode - Batch/Batch input/Dialog.
        if me->mode is not initial.
          ls_log-log_mode = me->mode.
        else.
          ls_log-log_mode = cond #( when sy-batch is not initial then zif_lso_log=>c_mode-batch
                                    else zif_lso_log=>c_mode-dialog ).
        endif.

        " Save data into DB
        if me->zif_lso_log~has_ref_logs( ) and saved_ref_logs[] is not initial.
          loop at me->ref_logs using key object_key reference into data(ref_log).
            " Process only saved reference logs!
            if line_exists( saved_ref_logs[ key object_key components id    = ref_log->instance->get_id( )
                                                                      seqnr = ref_log->instance->get_seqnr( ) ] ).
              ls_log-ref_id    = ref_log->instance->get_id( ).
              ls_log-ref_seqnr = ref_log->instance->get_seqnr( ).

              result = me->save_db( ref #( ls_log ) ).
            endif.
          endloop.
        else.
          result = me->save_db( ref #( ls_log ) ).
        endif.

        " Set current run data.
        me->seqnr      = ls_log-seqnr.
        me->timestamp  = ls_log-timestamp.
        me->date       = ls_log-log_date.
        me->time       = ls_log-log_time.
        me->mode       = ls_log-log_mode.
        me->changed_by = ls_log-changed_by.
        me->tcode      = ls_log-tcode.
        me->program    = ls_log-prog.

        if messages_to_save[] is not initial.
          " Save messages.
          zcl_lso_log_message=>save_collection( log_id    = ls_log-id
                                                log_seqnr = ls_log-seqnr
                                                messages  = messages_to_save ).
        endif.

        commit work and wait.

        " Unlock the log table.
        me->unlock_after_save( ).
      cleanup.
        " Unlock the log table in case exception was raised!
        me->unlock_after_save( ).
    endtry.
  endmethod.


  method save_ref_logs.
    loop at me->ref_logs using key object_key reference into data(ref_log).
      data(instance) = cast zcl_lso_log( ref_log->instance ).

      " Set reference log context if not set.
      if instance->context is initial.
        instance->context = me->context.
      endif.

      " Save reference log.
      data(is_saved) = ref_log->instance->save( ).

      if is_saved ne abap_true.
        " Check if reference has been already saved in the past?
        select single @abap_true
          from zlso_log
         where id    = @ref_log->id
           and seqnr = @ref_log->seqnr
          into @is_saved.
      endif.

      data(ref_relation_saved) = abap_false.

      if is_saved eq abap_true.
        " Check if there is already saved relation between parent and the reference log?
        select single @abap_true
          from zlso_log
         where id        = @me->id
           and ref_id    = @ref_log->id
           and ref_seqnr = @ref_log->seqnr
          into @ref_relation_saved.
      endif.

      if is_saved eq abap_true and ref_relation_saved ne abap_true.
        " Reference log saved and there is no relation with its parent yet.
        insert ref_log->instance->get_object( ) into table saved_ref_logs.
      endif.
    endloop.
  endmethod.


  method create_id.
    try.
        " Number range created by the /EDU/CL_LOGNR_CREATE class.
        cl_numberrange_runtime=>number_get( exporting nr_range_nr = '01'
                                                      object      = '/EDU/LOGNR'
                                            importing number      = data(number) ).
        id = number.
      catch cx_nr_object_not_found cx_number_ranges.
        clear id.
    endtry.

    if id is initial.
      try.
          id = cl_system_uuid=>if_system_uuid_static~create_uuid_c32( ).
        catch cx_uuid_error.
      endtry.
    endif.
  endmethod.


  method get_next_seqnr.
    data(id) = me->get_id( ).

    data(max_seqnr) = value zlso_log-seqnr( ).

    select max( seqnr )
      from zlso_log
     where id eq @id
      into @max_seqnr.

    seqnr = max_seqnr + 1.

    if max_seqnr > seqnr.
      " Log seqnr field value has been exceeded!
      raise exception type zcx_lso_log
        exporting
          textid   = zcx_lso_log=>next_seqnr_failure
          mv_msgv1 = |ZLSO_LOG { id }|.
    endif.
  endmethod.


  method check_cross_reference.
    data(object) = ref #( me->cross_refs[ id    = log->get_id( )
                                          seqnr = log->get_seqnr( ) ] optional ).

    " Check also an instance, not only the key!
    " Seqnr is initial before save but still there can be two reference logs with the same id but different messages:
    " - new instance of LogRef 1 with Message 1
    " - new instance of LogRef 1 with Message 2
    if object is bound and object->instance eq log.
      result = abap_true.
      return.
    endif.

    insert log->get_object( ) into table me->cross_refs.

    if log->has_ref_logs( ).
      loop at log->get_ref_logs( ) using key object_key reference into data(ref_log).
        " Check cross reference recursively.
        if me->check_cross_reference( ref_log->instance ).
          " Cross Reference error - &1 &2
          raise exception type zcx_lso_log
            exporting
              textid   = zcx_lso_log=>cross_reference_error
              mv_msgv1 = |{ log->get_id( ) }/{ log->get_seqnr( ) }|
              mv_msgv2 = |{ ref_log->instance->get_id( ) }/{ ref_log->instance->get_seqnr( ) }|.
        endif.
      endloop.
    endif.
  endmethod.


  method fill_with_timestamp.
    " 20200311090947.3125490
    get time stamp field log_structure->timestamp.

    data(tstmp_date) = value datn( ).
    data(tstmp_time) = value timn( ).

    try.
        " Get date/time from time stamp.
        zcl_lso_log_utils=>utc_tstmp_2_datetime( exporting iv_tstmp = log_structure->timestamp
                                                 importing ev_date  = tstmp_date
                                                           ev_time  = tstmp_time ).
      catch cx_parameter_invalid_type
            cx_parameter_invalid_range.
        tstmp_date = cl_abap_context_info=>get_system_date( ).
        tstmp_time = cl_abap_context_info=>get_system_time( ).
    endtry.

    log_structure->log_date = tstmp_date.
    log_structure->log_time = tstmp_time.
  endmethod.


  method save_db.
    try.
        " Save log in DB.
        insert zlso_log from @log_structure->*.

        result = boolc( sy-dbcnt > 0 ).
      catch cx_sy_open_sql_db into data(lo_cx_sql).
        " Log couldn't be saved, raise an exception.
        raise exception type zcx_lso_log
          exporting
            textid            = zcx_lso_log=>save_error
            mv_msgv1          = |ZLSO_LOG { log_structure->id }/{ log_structure->seqnr }/{ log_structure->ref_id }/{ log_structure->ref_seqnr }|
            mv_exception_text = lo_cx_sql->get_text( ).
    endtry.
  endmethod.


  method get_messages_to_save.
    loop at me->messages using key object_key reference into data(message).
      if message->instance->get_log_id( ) is initial and message->instance->get_log_seqnr( ) is initial.
        insert message->* into table messages.
      endif.
    endloop.
  endmethod.


  method zif_lso_comparable~equals.
    try.
        data(log) = cast zif_lso_log( io_object ).

        rv_is_equal = boolc( me eq log or ( me->zif_lso_log~get_id( )    eq log->get_id( ) and
                                            me->zif_lso_log~get_seqnr( ) eq log->get_seqnr( ) ) ).
      catch cx_sy_move_cast_error.
        rv_is_equal = abap_false.
    endtry.
  endmethod.


  method set_context.
    zcl_lso_log=>log_context = context.
  endmethod.


  method clear_context.
    clear zcl_lso_log=>log_context.
  endmethod.


  method zif_lso_log~get_context.
    context = me->context.
  endmethod.


  method lock_before_save.
    try.
        me->zif_lso_log~lock( ).
      catch cx_abap_foreign_lock
            cx_abap_lock_failure.
        raise exception type zcx_lso_log
          exporting
            textid   = zcx_lso_log=>lock_error
            mv_msgv1 = |Log { me->zif_lso_log~get_id( ) }|.
    endtry.
  endmethod.


  method unlock_after_save.
    try.
        me->zif_lso_log~unlock( ).
      catch cx_abap_lock_failure.
    endtry.
  endmethod.

endclass.
