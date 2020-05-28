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

    " Backward compatibility
    " Abstract
    aliases:
             add_message            for zif_lso_log_abstract~add_message,
             add_messages           for zif_lso_log_abstract~add_messages,
             add_symsg              for zif_lso_log_abstract~add_symsg,

             display_messages_popup for zif_lso_log_abstract~display_messages_popup,

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
    data objects type zif_lso_log=>tt_objects.
    data ref_logs type ref to cl_object_collection.

    " Separate table variable for reference logs objects - PERFORMANCE Reason!!!
    data ref_logs_objects type zif_lso_log=>tt_objects.

    methods create_id
      returning value(id) type zlso_log-id .

    methods check_cross_reference
      importing log           type ref to zcl_lso_log
      returning value(result) type abap_bool
      raising   zcx_lso_log.

    methods get_next_seqnr
      returning value(seqnr) type zlso_log-seqnr
      raising   zcx_lso_log .

    methods get_messages_to_save
      returning value(messages) type ref to if_object_collection.

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
      returning value(saved_ref_logs) type zif_lso_log=>tt_objects
      raising   zcx_lso_log.

    methods save_db
      importing log_structure type ref to zlso_log
      returning value(result) type abap_bool
      raising   zcx_lso_log.

    methods lock_before_save
      raising zcx_lso_log.

    methods clear_ref_logs_cache.

    methods cache_ref_log
      importing ref_log type ref to zif_lso_log.

    methods cache_ref_logs.
endclass.


class zcl_lso_log implementation.

  method constructor.
    super->constructor( ).

    me->id = id.
    me->ref_logs = new cl_object_collection( ).

    "Assign context if set.
    me->context = zcl_lso_log=>log_context.
  endmethod.


  method zif_lso_log~add.
    me->zif_lso_log_abstract~add_message( message ).
  endmethod.


  method zif_lso_log~add_ref_log.
    me->ref_logs->add( ref_log ).

    " PERFORMANCE - cache reference log in the table.
    me->cache_ref_log( ref_log ).
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
    " Instantiate new messages collection not to destroy collected messages per log (It will contain messages objects from several logs).
    messages = new cl_object_collection( ).

    data(iterator) = me->zif_lso_log_abstract~get_messages( )->get_iterator( ).

    while iterator->has_next( ).
      cast cl_object_collection( messages )->add( cast zcl_lso_log_message( iterator->get_next( ) ) ).
    endwhile.

    if with_ref eq abap_true and me->zif_lso_log~has_ref_logs( ).
      " Get also messages from reference logs.
      iterator = me->ref_logs->get_iterator( ).

      while iterator->has_next( ).
        data(ref_messages_iterator) = cast zcl_lso_log( iterator->get_next( ) )->zif_lso_log~get_messages( with_ref )->get_iterator( ).

        while ref_messages_iterator->has_next( ).
          cast cl_object_collection( messages )->add( ref_messages_iterator->get_next( ) ).
        endwhile.
      endwhile.
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
      data(iterator) = me->zif_lso_log~get_ref_logs( )->get_iterator( ).

      while iterator->has_next( ).
        result = cast zcl_lso_log( iterator->get_next( ) )->zif_lso_log~has_exception( cx_t100_key ).

        if result eq abap_true.
          " Exception message has been found, no need to check further reference logs.
          return.
        endif.
      endwhile.
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
      data(iterator) = me->zif_lso_log~get_ref_logs( )->get_iterator( ).

      while iterator->has_next( ).
        result = cast zcl_lso_log( iterator->get_next( ) )->zif_lso_log~has_message( message  = message
                                                                                     with_ref = with_ref ).

        if result eq abap_true.
          " Message has been found, no need to check further reference logs.
          return.
        endif.
      endwhile.
    endif.
  endmethod.


  method zif_lso_log~has_messages.
    result = me->zif_lso_log_abstract~has_messages( ).

    if result eq abap_true.
      return.
    endif.

    if with_ref eq abap_true and me->zif_lso_log~has_ref_logs( ).
      " Check reference logs if messages were added there.
      data(iterator) = me->zif_lso_log~get_ref_logs( )->get_iterator( ).

      while iterator->has_next( ).
        result = cast zcl_lso_log( iterator->get_next( ) )->zif_lso_log~has_messages( with_ref ).

        if result eq abap_true.
          " Message has been found, no need to check further reference logs.
          return.
        endif.
      endwhile.
    endif.
  endmethod.


  method zif_lso_log~has_message_type.
    data(messages) = me->zif_lso_log~get_messages( with_ref ).

    data(iterator) = messages->get_iterator( ).

    while iterator->has_next( ).
      data(message) = cast zcl_lso_log_message( iterator->get_next( ) ).

      if message->get_type( ) eq type.
        result = abap_true.
        return.
      endif.
    endwhile.
  endmethod.


  method zif_lso_log~has_ref_log.
    if not me->zif_lso_log~has_ref_logs( ).
      return.
    endif.

    if me->ref_logs_objects[] is initial.
      " Collection to table for performance reason!
      me->cache_ref_logs( ).
    endif.

    result = boolc( line_exists( me->ref_logs_objects[ id    = log->zif_lso_log~get_id( )
                                                       seqnr = log->zif_lso_log~get_seqnr( ) ] ) ).
  endmethod.


  method zif_lso_log~has_ref_logs.
    result = boolc( not me->ref_logs->is_empty( ) ).
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
    " Lock log table against potential modifications for given log id.
    call function 'ENQUEUE_EZLSO_LOG'
      exporting
        id             = me->get_id( )
      exceptions
        foreign_lock   = 1
        system_failure = 2
        others         = 3.

    result = boolc( sy-subrc eq 0 ).
  endmethod.


  method zif_lso_log~unlock.
    " Unlock log table for potential modifications.
    call function 'DEQUEUE_EZLSO_LOG'
      exporting
        id = me->get_id( ).
  endmethod.


  method zif_lso_log~save.
    if me->zif_lso_log~has_messages( abap_true ) eq abap_false.
      " There are messages to be stored neither in main nor referenced log - no need to store the log.
      return.
    endif.

    clear me->objects[].

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

        if messages_to_save->is_empty( ) and saved_ref_logs[] is initial.
          " There are neither messages to be saved nor reference logs saved.
          result = abap_false.

          " Unlock the log table if the caller triggered saving the log even though no additional messages where added.
          me->zif_lso_log~unlock( ).

          return.
        endif.

        " Initialize structure for log saving.
        data(ls_log) = value zlso_log( id            = me->zif_lso_log~get_id( )
                                       seqnr         = me->get_next_seqnr( )
                                       changed_by    = sy-uname
                                       tcode         = cond #( when me->tcode   is not initial then me->tcode   else sy-tcode )
                                       prog          = cond #( when me->program is not initial then me->program else sy-cprog )
                                       context       = me->context
                                       stripped_date = me->zif_lso_log~get_stripped_date( ) ).

        " Set time stamp/date/time for this run.
        me->fill_with_timestamp( ref #( ls_log ) ).

        " Set program mode - Batch/Batch input/Dialog.
        if me->mode is not initial.
          ls_log-log_mode = me->mode.
        else.
          ls_log-log_mode = cond #( when sy-batch is not initial then zif_lso_log=>c_mode-batch
                                    when sy-binpt is not initial then zif_lso_log=>c_mode-batch_input
                                    else zif_lso_log=>c_mode-dialog ).
        endif.

        " Save data into DB
        if me->zif_lso_log~has_ref_logs( ) and saved_ref_logs[] is not initial.
          data(iterator) = me->ref_logs->get_iterator( ).

          while iterator->has_next( ).
            data(ref_log) = cast zcl_lso_log( iterator->get_next( ) ).

            " Process only saved reference logs!
            if line_exists( saved_ref_logs[ id    = ref_log->zif_lso_log~get_id( )
                                            seqnr = ref_log->zif_lso_log~get_seqnr( ) ] ).
              ls_log-ref_id    = ref_log->zif_lso_log~get_id( ).
              ls_log-ref_seqnr = ref_log->zif_lso_log~get_seqnr( ).

              result = me->save_db( ref #( ls_log ) ).
            endif.
          endwhile.
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

        if not messages_to_save->is_empty( ).
          " Save messages.
          zcl_lso_log_message=>save_collection( log_id    = ls_log-id
                                                log_seqnr = ls_log-seqnr
                                                messages  = messages_to_save ).
        endif.

        commit work and wait.

        " Unlock the log table.
        me->zif_lso_log~unlock( ).
      cleanup.
        " Unlock the log table in case exception was raised!
        me->zif_lso_log~unlock( ).
    endtry.
  endmethod.


  method save_ref_logs.
    data(iterator) = me->ref_logs->get_iterator( ).

    while iterator->has_next( ).
      data(ref_log) = cast zcl_lso_log( iterator->get_next( ) ).

      " Set reference log context if not set.
      if ref_log->context is initial.
        ref_log->context = me->context.
      endif.

      " Save reference log.
      data(is_saved) = ref_log->zif_lso_log~save( ).

      if is_saved ne abap_true.
        " Check if reference has been already saved in the past?
        select single @abap_true
          from zlso_log
          into @is_saved
         where id    = @ref_log->id
           and seqnr = @ref_log->seqnr.
      endif.

      data(ref_relation_saved) = abap_false.

      if is_saved eq abap_true.
        " Check if there is already saved relation between parent and the reference log?
        select single @abap_true
          from zlso_log
          into @ref_relation_saved
         where id        = @me->id
           and ref_id    = @ref_log->id
           and ref_seqnr = @ref_log->seqnr.
      endif.

      if is_saved eq abap_true and ref_relation_saved ne abap_true.
        " Reference log saved and there is no relation with its parent yet.
        insert value #( id       = ref_log->zif_lso_log~get_id( )
                        seqnr    = ref_log->zif_lso_log~get_seqnr( )
                        instance = ref_log )
          into table saved_ref_logs.
      endif.
    endwhile.

    " Clear reference logs objects cache.
    " The sequence number is set during saving, reference logs cache key is not valid any more.
    me->clear_ref_logs_cache( ).
  endmethod.


  method create_id.
    data(next_id) = value das_res_id( ).

    call function 'NUMBER_GET_NEXT'
      exporting
        nr_range_nr             = '00'
        object                  = 'ZLSO_LOGID'
      importing
        number                  = next_id
      exceptions
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        others                  = 8.

    if next_id ne 0 and sy-subrc = 0.
      id = |{ sy-sysid }{ next_id }|.
    else.
      try.
          " Use uuid if no range is not working.
          id = cl_system_uuid=>if_system_uuid_static~create_uuid_c22( ).
        catch cx_uuid_error.
          wait up to 1 seconds.
          retry.
      endtry.
    endif.
  endmethod.


  method get_next_seqnr.
    data(id) = me->get_id( ).

    data(max_seqnr) = value zlso_log-seqnr( ).

    select max( seqnr )
      into @max_seqnr
      from zlso_log
     where id eq @id.

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
    data(object) = ref #( me->objects[ id    = log->zif_lso_log~get_id( )
                                       seqnr = log->zif_lso_log~get_seqnr( ) ] optional ).

    " Check also an instance, not only the key!
    " Seqnr is initial before save but still there can be two reference logs with the same id but different messages:
    " - new instance of LogRef 1 with Message 1
    " - new instance of LogRef 1 with Message 2
    if object is bound and object->instance eq log.
      result = abap_true.
      return.
    endif.

    insert value #( id       = log->zif_lso_log~get_id( )
                    seqnr    = log->zif_lso_log~get_seqnr( )
                    instance = log )
      into table me->objects.

    if log->zif_lso_log~has_ref_logs( ).
      data(iterator) = log->ref_logs->get_iterator( ).

      while iterator->has_next( ).
        data(ref_log) = cast zcl_lso_log( iterator->get_next( ) ).

        if me->check_cross_reference( ref_log ).
          " Cross Reference error - &1 &2
          raise exception type zcx_lso_log
            exporting
              textid   = zcx_lso_log=>cross_reference_error
              mv_msgv1 = |{ log->zif_lso_log~get_id( ) }/{ log->zif_lso_log~get_seqnr( ) }|
              mv_msgv2 = |{ ref_log->zif_lso_log~get_id( ) }/{ ref_log->zif_lso_log~get_seqnr( ) }|.
        endif.
      endwhile.
    endif.
  endmethod.


  method fill_with_timestamp.
    " 20200311090947.3125490
    get time stamp field log_structure->timestamp.

    data(tstmp_date) = value dats( ).
    data(tstmp_time) = value tims( ).

    try.
*       Get date/time from time stamp.
        zcl_lso_log_utils=>utc_tstmp_2_datetime(
          exporting
            iv_tstmp = log_structure->timestamp
          importing
            ev_date  = tstmp_date
            ev_time  = tstmp_time ).
      catch cx_parameter_invalid_type
            cx_parameter_invalid_range.
        tstmp_date = sy-datum.
        tstmp_time = sy-uzeit.
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
    messages = new cl_object_collection( ).

    data(iterator) = me->zif_lso_log_abstract~get_messages( )->get_iterator( ).

    while iterator->has_next( ).
      data(message) = cast zcl_lso_log_message( iterator->get_next( ) ).

      if message->get_log_id( ) is initial and message->get_log_seqnr( ) is initial.
        cast cl_object_collection( messages )->add( message ).
      endif.
    endwhile.
  endmethod.


  method zif_lso_comparable~equals.
    try.
        data(log) = cast zcl_lso_log( io_object ).

        rv_is_equal = boolc( me eq log or ( me->zif_lso_log~get_id( )    eq log->zif_lso_log~get_id( ) and
                                            me->zif_lso_log~get_seqnr( ) eq log->zif_lso_log~get_seqnr( ) ) ).
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
    " Try to lock the log table for given log id.
    while me->zif_lso_log~lock( ) eq abap_false.
      data(lv_index) = sy-index.

      " Log couldn't be locked, wait 1 second and try to lock it again.
      wait up to 1 seconds.

      if lv_index > 10.
        " Method tried to lock the log dozen of times and it's still locked - raise an exception!
        raise exception type zcx_lso_log
          exporting
            textid   = zcx_lso_log=>lock_error
            mv_msgv1 = |Log { me->zif_lso_log~get_id( ) }|.
      endif.
    endwhile.
  endmethod.


  method clear_ref_logs_cache.
    clear me->ref_logs_objects[].
  endmethod.


  method cache_ref_logs.
    data(iterator) = me->ref_logs->get_iterator( ).

    while iterator->has_next( ).
      me->cache_ref_log( cast zif_lso_log( iterator->get_next( ) ) ).
    endwhile.
  endmethod.


  method cache_ref_log.
    insert value #( id       = ref_log->get_id( )
                    seqnr    = ref_log->get_seqnr( )
                    instance = ref_log )
      into table me->ref_logs_objects.
  endmethod.

endclass.
