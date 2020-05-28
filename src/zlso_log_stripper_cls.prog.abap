*&---------------------------------------------------------------------*
*&  Include  zlso_log_stripper_cls
*&---------------------------------------------------------------------*
class lcx_report definition inheriting from cx_static_check.
  public section.
    constants no_parameters type sotr_conc value 'NO_PARAMS'.
    constants timestamp_calculation_error type sotr_conc value 'TSTMP_CALC_ERROR'.

    data msgv1 type string.
    data msgv2 type string.

    methods constructor
      importing textid   like textid optional
                previous like previous optional
                msgv1    type string optional
                msgv2    type string optional.

    methods if_message~get_text redefinition.
endclass.


class lcx_report implementation.

  method constructor.
    super->constructor( textid   = textid
                        previous = previous ).

    me->msgv1 = msgv1.
    me->msgv2 = msgv2.
  endmethod.


  method if_message~get_text.
    case me->textid.
      when no_parameters.
        result = |{ 'No parameters provided'(e01) } - { me->msgv1 }|.

      when timestamp_calculation_error.
        result = |{ me->msgv1 } { 'time stamp calc. error'(e04) } - { me->msgv2 }|.

      when others.
        result = me->msgv1.
    endcase.
  endmethod.

endclass.


class lcl_main definition create private.

  public section.
    class-methods create
      returning value(result) type ref to lcl_main.

    methods constructor.

    methods initialization
      importing delete_payloads_after type ref to int4
                delete_traces_after   type ref to int4
                delete_messages_after type ref to int4
                delete_logs_after     type ref to int4.

    methods get_customizing
      importing id            type zlso_log_cust-id
      returning value(custom) type ref to zif_lso_log_cust
      raising   zcx_lso_log.

    methods get_customizing_number
      importing id           type zlso_log_cust-id
      returning value(value) type zlso_log_cust-value.

    methods strip
      importing test_mode             type abap_bool
                log_ids               type zif_lso_log_factory=>tt_log_id
                delete_payloads_after type i
                delete_traces_after   type i
                delete_messages_after type i
                delete_logs_after     type i
                no_db_update          type abap_bool
      raising   lcx_report zcx_lso_log.

    methods display_output.

  protected section.
  private section.
    constants c_seconds_per_day type int4 value 86400.
    constants c_alv_layout type slis_vari value '/DEFAULT'.

    types begin of ts_data.
    types program type zlso_log-prog.
    types msgid type zlso_log_message-msgid.
    types deleted_logs type i.
    types deleted_messages type i.
    types deleted_traces type i.
    types deleted_payloads type i.
    types deleted_headers type i.
    types end of ts_data.

    types tt_data type standard table of ts_data with empty key
      with unique sorted key log components program msgid.

    types begin of ts_date_time.
    types date type zlso_log_message-msg_date.
    types time type zlso_log_message-msg_time.
    types end of ts_date_time.

    class-data self type ref to lcl_main.

    data custom_factory type ref to zif_lso_log_cust_factory.
    data test_mode type abap_bool.
    data log_ids type zif_lso_log_factory=>tt_log_id.
    data timestamp type timestampl.
    data data type tt_data.
    data stripped_date type zlso_log-stripped_date.
    data alv type ref to cl_salv_table.
    data with_db_commit type abap_bool.

    methods calculate_timestamp
      importing delete_after     type i
                type             type string
      returning value(timestamp) type timestampl
      raising   lcx_report.

    methods strip_payloads
      importing timestamp type timestampl
      raising   lcx_report.

    methods strip_headers
      importing timestamp type timestampl
      raising   lcx_report.

    methods strip_traces
      importing timestamp type timestampl
      raising   lcx_report.

    methods strip_messages
      importing timestamp type timestampl
      raising   lcx_report.

    methods strip_logs
      importing timestamp type timestampl
      raising   lcx_report.

    methods init_alv_columns.
    methods init_alv_functions.
    methods init_alv_layout.

    methods is_batch
      returning value(result) type abap_bool.

    methods progress_bar
      importing text type any.

    methods timestamp_2_datetime
      importing timestamp        type timestampl
      returning value(date_time) type ts_date_time.
endclass.


class lcl_main implementation.

  method create.
    if lcl_main=>self is not bound.
      lcl_main=>self = new #( ).
    endif.

    result = lcl_main=>self.
  endmethod.


  method constructor.
    me->custom_factory = zcl_lso_log_cust_factory=>instance( ).
  endmethod.


  method initialization.

*    Removed to keep ZLSO_LOG package transportable.
*    " Check authorization for transaction first.
*    call function 'AUTHORITY_CHECK_TCODE'
*      exporting
*        tcode  = 'ZLSO_LOG_STRIPPER'
*      exceptions
*        ok     = 1
*        not_ok = 2
*        others = 3.
*    if sy-subrc <> 1.
*      message 'No Authorization!'(e02) type zif_lso_log_message=>c_type-error.
*    endif.

    data(security) = new zcl_lso_log_security( dialog = abap_true ).

    if not security->is_technical_support( ).
      " No Authorization to run this tool, exit with error!
      message 'No Authorization!'(e02) type zif_lso_log_message=>c_type-error.
    endif.

    " Set customizing values (numbers) for log stripper.
    delete_payloads_after->* = me->get_customizing_number( zif_lso_log_cust_factory=>id-delete_payload_after ).
    delete_traces_after->*   = me->get_customizing_number( zif_lso_log_cust_factory=>id-delete_trace_after ).
    delete_messages_after->* = me->get_customizing_number( zif_lso_log_cust_factory=>id-delete_message_after ).
    delete_logs_after->*     = me->get_customizing_number( zif_lso_log_cust_factory=>id-delete_log_after ).

  endmethod.


  method get_customizing_number.
    try.
        " Try to convert customizing value into number.
        value = me->custom_factory->get( id      = id
                                         read_db = abap_true )->get_value( ).
      catch zcx_lso_log into data(cx_log).
        message cx_log->get_text( ) type zif_lso_log_message=>c_type-success
          display like zif_lso_log_message=>c_type-error.
      catch cx_sy_conversion_no_number into data(cx_conv).
        message |Customizing { id } - { cx_conv->get_text( ) }| type zif_lso_log_message=>c_type-success
          display like zif_lso_log_message=>c_type-error.
    endtry.
  endmethod.


  method get_customizing.
    custom = me->custom_factory->get( id ).
  endmethod.


  method strip.
    me->test_mode = test_mode.
    me->log_ids = log_ids.
    me->with_db_commit = xsdbool( no_db_update eq abap_false ).

    if test_mode eq abap_true and log_ids[] is initial.
      raise exception type lcx_report
        exporting
          textid = lcx_report=>no_parameters
          msgv1  = 'LOG_ID'.
    endif.

    clear me->data[].

    if test_mode eq abap_false.
      " NO test mode, strip logs according to the customizing values.
      clear me->log_ids.
    endif.

    me->stripped_date = sy-datum.

    " Get current UTC time stamp.
    get time stamp field me->timestamp.

    " Calculate pay load time stamp in the past based on the customizing object.
    data(payload_timestamp) = me->calculate_timestamp( delete_after = delete_payloads_after
                                                       type         = 'DELETE PAYLOADS/HEADERS AFTER' ).

    " Display progress indicator for stripping...
    me->progress_bar( 'Stripping payloads...'(002) ).

    " Strip old pay loads.
    me->strip_payloads( payload_timestamp ).

    " Display progress indicator for stripping...
    me->progress_bar( 'Stripping headers...'(003) ).

    " Strip old headers based on pay load time stamp.
    me->strip_headers( payload_timestamp ).

    " Calculate trace time stamp in the past based on the customizing object.
    data(trace_timestamp) = me->calculate_timestamp( delete_after = delete_traces_after
                                                     type         = 'DELETE TRACES AFTER' ).

    " Display progress indicator for stripping...
    me->progress_bar( 'Stripping traces...'(004) ).

    " Strip old traces.
    me->strip_traces( trace_timestamp ).

    " Calculate message time stamp in the past based on the customizing object.
    data(message_timestamp) = me->calculate_timestamp( delete_after = delete_messages_after
                                                       type         = 'DELETE MESSAGES AFTER' ).

    " Display progress indicator for stripping...
    me->progress_bar( 'Stripping messages...'(005) ).

    " Strip old messages.
    me->strip_messages( message_timestamp ).

    " Calculate log time stamp in the past based on the customizing object.
    data(log_timestamp) = me->calculate_timestamp( delete_after = delete_logs_after
                                                   type         = 'DELETE LOGS AFTER').

    " Display progress indicator for stripping...
    me->progress_bar( 'Stripping logs...'(006) ).

    " Strip old logs.
    me->strip_logs( log_timestamp ).

    " Final deletion or test mode :)
    if me->with_db_commit eq abap_true.
      commit work.
    else.
      rollback work.
    endif.
  endmethod.


  method calculate_timestamp.
    if me->timestamp is initial.
      " Set current UTC time stamp, if not set so far.
      get time stamp field me->timestamp.
    endif.

    data(days) = delete_after.

    if days <= 0.
      raise exception type lcx_report
        exporting
          textid = lcx_report=>no_parameters.
    endif.

    try.
        " Convert number of days from customizing into seconds.
        data(seconds) = days * c_seconds_per_day.

        " Calculate time stamp in the past.
        timestamp = cl_abap_tstmp=>subtractsecs( tstmp = me->timestamp
                                                 secs  = seconds ).
      catch cx_sy_conversion_no_number into data(cx_conversion).
        raise exception type lcx_report
          exporting
            textid = lcx_report=>timestamp_calculation_error
            msgv1  = type
            msgv2  = cx_conversion->get_text( ).
      catch cx_parameter_invalid_range
            cx_parameter_invalid_type into data(cx_parameter).
        raise exception type lcx_report
          exporting
            textid = lcx_report=>timestamp_calculation_error
            msgv1  = type
            msgv2  = cx_parameter->get_text( ).
    endtry.
  endmethod.


  method strip_payloads.
    field-symbols <table> type standard table.

    " Select log/message data before deletion to have some numbers collected in the output table.
    select log~id, log~seqnr, log~prog as program,
           msg~msgid, msg~timestamp, msg~trace_id
      from zlso_log as log inner join zlso_log_message as msg on msg~log_id    = log~id
                                                             and msg~log_seqnr = log~seqnr
      into table @data(objects)
     where log~id        in @me->log_ids
       and msg~timestamp <= @timestamp
       and msg~trace_id  ne ''
       and exists (
          select @abap_true
            from zlso_log_payload
           where trace_id = msg~trace_id
       )
     order by msg~timestamp.

    if sy-subrc ne 0.
      " No payloads for being deleted.
      return.
    endif.

    " Delete payloads...
    delete from zlso_log_payload
     where trace_id in (
        select trace_id
          from zlso_log_message
         where log_id    in @me->log_ids
           and timestamp <= @timestamp
      ).

    " Reflect deleted data in the output table.
    loop at objects reference into data(object).
      data(data_ref) = ref #( me->data[ key log components program = object->program
                                                           msgid   = object->msgid ] optional ).

      if data_ref is not bound.
        insert value ts_data( program = object->program
                              msgid   = object->msgid )
          into table me->data reference into data_ref.
      endif.

      data_ref->deleted_payloads = data_ref->deleted_payloads + 1.
    endloop.

    if me->with_db_commit ne abap_true.
      return.
    endif.

    " Range of deleted traces ids.
    data(trace_ids) = value zcl_lso_log_trace_factory=>tt_r_id( for o in objects where ( trace_id is not initial )
                                                                                       ( sign   = zif_lso_log=>c_sign-include
                                                                                         option = zif_lso_log=>c_option-equal
                                                                                         low    = o-trace_id ) ).

    if trace_ids[] is not initial.
      " Make trace ids unique.
      sort trace_ids.
      delete adjacent duplicates from trace_ids.

      " Split traces ids range table into smaller chunks to have safely used in OpenSQL update statement.
      loop at new zcl_lso_log_range( )->split_table_for_opensql_in( trace_ids ) into data(trace_ids_split).
        assign trace_ids_split->* to <table>.

        " Update trace with stripped date.
        update zlso_log_trace set stripped_date = @me->stripped_date where id in @<table>.
      endloop.
    endif.
  endmethod.


  method strip_headers.
    field-symbols <table> type standard table.

    " Select log/message data before deletion to have some numbers collected in the output table.
    select log~id, log~seqnr, log~prog as program,
           msg~msgid, msg~timestamp, msg~trace_id
      from zlso_log as log inner join zlso_log_message as msg on msg~log_id    = log~id
                                                             and msg~log_seqnr = log~seqnr
      into table @data(objects)
     where log~id        in @me->log_ids
       and msg~timestamp <= @timestamp
       and msg~trace_id  ne ''
       and exists (
          select @abap_true
            from zlso_log_headers
           where trace_id = msg~trace_id
       )
     order by msg~timestamp.

    if sy-subrc ne 0.
      " No headers for being deleted.
      return.
    endif.

    " Delete headers...
    delete from zlso_log_headers
     where trace_id in (
        select trace_id
          from zlso_log_message
         where log_id    in @me->log_ids
           and timestamp <= @timestamp
      ).

    " Reflect deleted data in the output table.
    loop at objects reference into data(object).
      data(data_ref) = ref #( me->data[ key log components program = object->program
                                                           msgid   = object->msgid ] optional ).

      if data_ref is not bound.
        insert value ts_data( program = object->program
                              msgid   = object->msgid )
          into table me->data reference into data_ref.
      endif.

      data_ref->deleted_headers = data_ref->deleted_headers + 1.
    endloop.

    if me->with_db_commit ne abap_true.
      return.
    endif.

    " Range of deleted traces ids.
    data(trace_ids) = value zcl_lso_log_trace_factory=>tt_r_id( for o in objects where ( trace_id is not initial )
                                                                                       ( sign   = zif_lso_log=>c_sign-include
                                                                                         option = zif_lso_log=>c_option-equal
                                                                                         low    = o-trace_id ) ).

    if trace_ids[] is not initial.
      " Make trace ids unique.
      sort trace_ids.
      delete adjacent duplicates from trace_ids.

      " Split traces ids range table into smaller chunks to have safely used in OpenSQL update statement.
      loop at new zcl_lso_log_range( )->split_table_for_opensql_in( trace_ids ) into data(trace_ids_split).
        assign trace_ids_split->* to <table>.

        " Update trace with stripped date.
        update zlso_log_trace set stripped_date = @me->stripped_date where id in @<table>.
      endloop.
    endif.
  endmethod.


  method strip_traces.
    field-symbols <table> type standard table.

    " Select log/message data before deletion to have some numbers collected in the output table.
    select log~id, log~seqnr, log~prog as program,
           msg~msgid, msg~timestamp, msg~trace_id
      from zlso_log as log inner join zlso_log_message as msg on msg~log_id    = log~id
                                                             and msg~log_seqnr = log~seqnr
      into table @data(objects)
     where log~id        in @me->log_ids
       and msg~timestamp <= @timestamp
       and msg~trace_id  ne ''
       and exists (
          select @abap_true
            from zlso_log_trace
           where id = msg~trace_id
       )
       and not exists (
          " Be sure there is no payload left for the trace that is about to be deleted!
          select @abap_true
            from zlso_log_payload
           where trace_id = msg~trace_id
       )
       and not exists (
          " Be sure there are no headers left for the trace that is about to be deleted!
          select @abap_true
            from zlso_log_headers
           where trace_id = msg~trace_id
       )
     order by msg~timestamp.

    if sy-subrc ne 0.
      " No traces for being deleted.
      return.
    endif.

    " Delete traces (only the ones that do not have payload/headers any more)...
    delete from zlso_log_trace
     where id in (
        select trace_id
          from zlso_log_message
         where log_id    in @me->log_ids
           and timestamp <= @timestamp
       )
       and not exists (
          select @abap_true
            from zlso_log_payload
           where trace_id = zlso_log_trace~id
       )
       and not exists (
          select @abap_true
            from zlso_log_headers
           where trace_id = zlso_log_trace~id
       ).

    " Reflect deleted data in the output table.
    loop at objects reference into data(object).
      data(data_ref) = ref #( me->data[ key log components program = object->program
                                                           msgid   = object->msgid ] optional ).

      if data_ref is not bound.
        insert value ts_data( program = object->program
                              msgid   = object->msgid )
          into table me->data reference into data_ref.
      endif.

      data_ref->deleted_traces = data_ref->deleted_traces + 1.
    endloop.

    if me->with_db_commit ne abap_true.
      return.
    endif.

    " Range of deleted traces ids.
    data(trace_ids) = value zcl_lso_log_trace_factory=>tt_r_id( for o in objects where ( id is not initial )
                                                                                       ( sign   = zif_lso_log=>c_sign-include
                                                                                         option = zif_lso_log=>c_option-equal
                                                                                         low    = o-trace_id ) ).

    if trace_ids[] is not initial.
      " Make trace ids unique.
      sort trace_ids.
      delete adjacent duplicates from trace_ids.

      " Split traces ids range table into smaller chunks to have safely used in OpenSQL update statement.
      loop at new zcl_lso_log_range( )->split_table_for_opensql_in( trace_ids ) into data(trace_ids_split).
        assign trace_ids_split->* to <table>.

        " Update message with stripped date.
        update zlso_log_message set stripped_date = @me->stripped_date where trace_id in @<table>.
      endloop.
    endif.
  endmethod.


  method strip_messages.
    types begin of ts_log_tstmp.
    types id type zif_lso_log=>ts_key-id.
    types min type zlso_log_message-timestamp.
    types max type zlso_log_message-timestamp.
    types end of ts_log_tstmp.

    types tt_logs_tstmp type hashed table of ts_log_tstmp with unique key id.

    types begin of ts_log_key_tstmp.
    include type zif_lso_log=>ts_key.
    types min type zlso_log_message-timestamp.
    types max type zlso_log_message-timestamp.
    types end of ts_log_key_tstmp.

    types tt_logs_keys_tstmp type hashed table of ts_log_key_tstmp with unique key id seqnr.

    types tt_logs_keys type hashed table of zif_lso_log=>ts_key with unique key id seqnr.

    " Select log/message data before deletion to have some numbers collected in the output table.
    select log~id, log~seqnr, log~prog as program,
           msg~msgid, msg~timestamp, msg~trace_id
      from zlso_log as log inner join zlso_log_message as msg on msg~log_id    = log~id
                                                             and msg~log_seqnr = log~seqnr
      into table @data(objects)
     where log~id        in @me->log_ids
       and msg~timestamp <= @timestamp
       and not exists (
          " Be sure there is no trace left for the message that is about to be deleted!
          select @abap_true
            from zlso_log_trace
           where id = msg~trace_id
       )
     order by msg~timestamp.

    if sy-subrc ne 0.
      " No messages for being deleted.
      return.
    endif.

    " Delete messages (only the ones that do not have trace any more)...
    delete from zlso_log_message
     where log_id    in @me->log_ids
       and timestamp <= @timestamp
       and not exists (
          select @abap_true
            from zlso_log_trace
           where id = zlso_log_message~trace_id
       ).

    " Reflect deleted data in the output table.
    loop at objects reference into data(object).
      data(data_ref) = ref #( me->data[ key log components program = object->program
                                                           msgid   = object->msgid ] optional ).

      if data_ref is not bound.
        insert value ts_data( program = object->program
                              msgid   = object->msgid )
          into table me->data reference into data_ref.
      endif.

      data_ref->deleted_messages = data_ref->deleted_messages + 1.
    endloop.

    if me->with_db_commit ne abap_true.
      return.
    endif.

    data(stripped_logs_keys) = value tt_logs_keys( ).
    data(stripped_logs_tstmp) = value tt_logs_tstmp( ) .

    loop at objects reference into object.
      data(log_key) = ref #( stripped_logs_keys[ id    = object->id
                                                 seqnr = object->seqnr ] optional ).

      if log_key is not bound.
        " Collect unique log keys.
        insert value #( id    = object->id
                        seqnr = object->seqnr )
          into table stripped_logs_keys reference into log_key.
      endif.

      " Collect unique logs, no sequence number needed!
      data(log_tstmp) = ref #( stripped_logs_tstmp[ id = object->id ] optional ).

      if log_tstmp is not bound.
        insert corresponding #( log_key->* )
          into table stripped_logs_tstmp reference into log_tstmp.
      endif.

      if log_tstmp->min is initial or log_tstmp->min > object->timestamp.
        " Find earliest message date before DB deletion.
        log_tstmp->min = object->timestamp.
      endif.

      if log_tstmp->max < object->timestamp.
        " Find latest message date before DB deletion.
        log_tstmp->max = object->timestamp.
      endif.
    endloop.

    loop at stripped_logs_keys reference into log_key.
      " Update log run with stripped date.
      update zlso_log set stripped_date = @me->stripped_date where id    = @log_key->id
                                                               and seqnr = @log_key->seqnr.
    endloop.

    " Next logs that are about to be updated with new generic message regarding stripped ones.
    data(logs_keys_tstmp) = value tt_logs_keys_tstmp( ).

    loop at stripped_logs_tstmp reference into log_tstmp.
      data(next_logs_keys) = value tt_logs_keys( ).

      " Is there further log run with messages?
      select log_id as id, min( log_seqnr ) as seqnr
        from zlso_log_message
        into table @next_logs_keys
        where log_id    = @log_tstmp->id
          and timestamp > @log_tstmp->max
        group by log_id.

      if next_logs_keys[] is initial.
        " Check if log has its parents with further messages?
        select log_id as id, min( log_seqnr ) as seqnr
          from zlso_log_message
          into table @next_logs_keys
         where log_id in (
              select id
                from zlso_log
               where ref_id = @log_tstmp->id
           )
           and timestamp > @log_tstmp->max
         group by log_id.
      endif.

      loop at next_logs_keys reference into log_key.
        " Check if there's already next log key with time stamp collected.
        " These log will be then updated with new generic message about deletion details.
        data(log_key_tstmp) = ref #( logs_keys_tstmp[ id    = log_key->id
                                                      seqnr = log_key->seqnr ] optional ).

        if log_key_tstmp is not bound.
          insert corresponding #( log_key->* )
            into table logs_keys_tstmp reference into log_key_tstmp.
        endif.

        if log_key_tstmp->min is initial or log_key_tstmp->min > log_tstmp->min.
          " Earliest deleted message
          log_key_tstmp->min = log_tstmp->min.
        endif.

        if log_key_tstmp->max < log_tstmp->max.
          " Latest deleted message
          log_key_tstmp->max = log_tstmp->max.
        endif.
      endloop.
    endloop.

    loop at logs_keys_tstmp reference into log_key_tstmp.
      " Convert time stamp into date and time.
      data(min) = me->timestamp_2_datetime( log_key_tstmp->min ).
      data(max) = me->timestamp_2_datetime( log_key_tstmp->max ).

      " Messages stripped; &1 - &2
      data(generic) = new zcl_lso_log_message_builder( )->set_log_id( log_key_tstmp->id
                                                       )->set_log_seqnr( log_key_tstmp->seqnr
                                                       )->set_msgid( zif_lso_log_abstract=>c_msgid
                                                       )->set_msgty( zif_lso_log_message=>c_type-info
                                                       )->set_msgno( '011'
                                                       )->set_msgv1( |{ min-date date = environment } { min-time time = environment }|
                                                       )->set_msgv2( |{ max-date date = environment } { max-time time = environment }|
                                                       )->build( ).

      " Where used...
      message id zif_lso_log_abstract=>c_msgid type zif_lso_log_message=>c_type-info
        number '011' with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 into data(lv_dummy).

      data(collection) = new cl_object_collection( ).
      collection->add( generic ).

      try.
          " Save generic message that past ones were stripped, otherwise it is not displayed in the ZLSO_TRACE.
          zcl_lso_log_message=>save_collection( log_id    = log_key_tstmp->id
                                                log_seqnr = log_key_tstmp->seqnr
                                                messages  = collection ).
        catch zcx_lso_log.
      endtry.
    endloop.
  endmethod.


  method strip_logs.
    select *
      from zlso_log as log
      into table @data(logs)
     where log~id        in @me->log_ids
       and log~timestamp <= @timestamp
       and not exists (
          " Be sure there will be no orphan message left after deleting the log itself.
          select @abap_true
            from zlso_log_message
           where log_id    = log~id
             and log_seqnr = log~seqnr
       ).

    if sy-subrc ne 0.
      " No logs for being deleted.
      return.
    endif.

    " Delete logs (only the ones that do not have messages any more)...
    delete from zlso_log
     where id        in @me->log_ids
       and timestamp <= @timestamp
       and not exists (
          select @abap_true
            from zlso_log_message
           where log_id    = zlso_log~id
             and log_seqnr = zlso_log~seqnr
       ).

    " Reflect deleted data in the output table.
    loop at logs reference into data(log).
      " Empty key for log...
      data(data_ref) = ref #( me->data[ key log components program = ''
                                                           msgid   = '' ] optional ).

      if data_ref is not bound.
        insert value ts_data( program = ''
                              msgid   = '' )
          into table me->data reference into data_ref.
      endif.

      data_ref->deleted_logs = data_ref->deleted_logs + 1.
    endloop.
  endmethod.


  method display_output.
    if me->is_batch( ) and me->data[] is initial.
      " Do not output anything if report executed in batch mode and nothing has been deleted.
      return.
    endif.

    try.
        cl_salv_table=>factory(
          importing
            r_salv_table = me->alv
          changing
            t_table      = me->data ).

        me->init_alv_columns( ).
        me->init_alv_functions( ).
        me->init_alv_layout( ).

        me->alv->display( ).
      catch cx_salv_msg into data(cx_salv).
        message cx_salv->get_text( ) type zif_lso_log_message=>c_type-error.
    endtry.
  endmethod.


  method init_alv_columns.
    data column type ref to cl_salv_column.

    data(columns) = me->alv->get_columns( ).
    columns->set_optimize( if_salv_c_bool_sap=>true ).

    try.
        column = columns->get_column( 'DELETED_LOGS' ).
        column->set_short_text( 'Del.Logs'(c01) ).
        column->set_medium_text( 'Deleted Logs'(c02) ).
        column->set_long_text( column->get_medium_text( ) ).
      catch cx_salv_not_found.
    endtry.

    try.
        column = columns->get_column( 'DELETED_MESSAGES' ).
        column->set_short_text( 'Del.Msg.'(c03) ).
        column->set_medium_text( 'Deleted Messages'(c04) ).
        column->set_long_text( column->get_medium_text( ) ).
      catch cx_salv_not_found.
    endtry.

    try.
        column = columns->get_column( 'DELETED_TRACES' ).
        column->set_short_text( 'Del.Traces'(c05) ).
        column->set_medium_text( 'Deleted Traces'(c06) ).
        column->set_long_text( column->get_medium_text( ) ).
      catch cx_salv_not_found.
    endtry.

    try.
        column = columns->get_column( 'DELETED_PAYLOADS' ).
        column->set_short_text( 'Del.Pay.'(c07) ).
        column->set_medium_text( 'Deleted Payloads'(c08) ).
        column->set_long_text( column->get_medium_text( ) ).
      catch cx_salv_not_found.
    endtry.

    try.
        column = columns->get_column( 'DELETED_HEADERS' ).
        column->set_short_text( 'Del.Head.'(c09) ).
        column->set_medium_text( 'Deleted Headers'(c10) ).
        column->set_long_text( column->get_medium_text( ) ).
      catch cx_salv_not_found.
    endtry.
  endmethod.


  method init_alv_functions.
    data(functions) = me->alv->get_functions( ).
    functions->set_all( if_salv_c_bool_sap=>true ).
    functions->set_abc_analysis( if_salv_c_bool_sap=>false ).
    functions->set_graphics( if_salv_c_bool_sap=>false ).
  endmethod.


  method is_batch.
    result = boolc( sy-batch eq abap_true ).
  endmethod.


  method init_alv_layout.
    data(layout) = me->alv->get_layout( ).
    layout->set_key( value #( report = sy-repid ) ).
    layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
    layout->set_initial_layout( c_alv_layout ).
  endmethod.


  method progress_bar.
    " Just display progress bar...
    cl_progress_indicator=>progress_indicate( i_text               = text
                                              i_output_immediately = abap_true ).
  endmethod.


  method timestamp_2_datetime.
    try.
        zcl_lso_log_utils=>utc_tstmp_2_datetime( exporting iv_tstmp = timestamp
                                                 importing ev_date  = date_time-date
                                                           ev_time  = date_time-time ).
      catch cx_parameter_invalid_type
            cx_parameter_invalid_range.
        data(c_timestamp) = conv string( timestamp ).
        date_time-date = c_timestamp(8).
        date_time-time = c_timestamp+8(6).
    endtry.
  endmethod.

endclass.
