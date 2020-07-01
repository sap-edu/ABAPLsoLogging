class zcl_lso_log_factory definition
  public
  final
  create private .

  public section.
    interfaces zif_lso_log_factory.

    aliases get for zif_lso_log_factory~get.
    aliases find for zif_lso_log_factory~find.
    aliases find_last_messages for zif_lso_log_factory~find_last_messages.
    aliases create_from_handler for zif_lso_log_factory~create_from_handler.
    aliases create_by_structure for zif_lso_log_factory~create_by_structure.
    aliases exists for zif_lso_log_factory~exists.
    aliases delete for zif_lso_log_factory~delete.
    aliases delete_collection for zif_lso_log_factory~delete_collection.
    aliases logs2ids for zif_lso_log_factory~logs2ids.

    class-methods instance
      returning value(factory) type ref to zif_lso_log_factory.

  protected section.
  private section.
    types begin of ts_params.
    " Log
    types ids type zif_lso_log_factory=>tt_log_id .
    types seqnrs type zif_lso_log_factory=>tt_log_seqnr .
    types dates type zif_lso_log_factory=>tt_log_date .
    types times type zif_lso_log_factory=>tt_log_time .
    types changed_bys type zif_lso_log_factory=>tt_changed_by .
    types contexts type zif_lso_log_factory=>tt_context .
    types tcodes type zif_lso_log_factory=>tt_tcode .
    types programs type zif_lso_log_factory=>tt_program .
    types log_timestamps type zif_lso_log_factory=>tt_timestamp.
    " Message
    types msgids type zif_lso_log_factory=>tt_msgid .
    types msgtys type zif_lso_log_factory=>tt_msgty .
    types msgnos type zif_lso_log_factory=>tt_msgno .
    types message_timestamps type zif_lso_log_factory=>tt_timestamp.
    types message_texts type zif_lso_log_factory=>tt_message_text .
    " Trace
    types request_methods type zif_lso_log_factory=>tt_request_method .
    types request_urls type zif_lso_log_factory=>tt_request_url .
    types http_statuses type zif_lso_log_factory=>tt_http_status .
    " Payload
    types request_payloads type zif_lso_log_factory=>tt_payload .
    types response_payloads type zif_lso_log_factory=>tt_payload .
    " Headers
    types request_header_values type zif_lso_log_factory=>tt_header_value .
    types request_header_names type zif_lso_log_factory=>tt_header_name .
    types response_header_values type zif_lso_log_factory=>tt_header_value .
    types response_header_names type zif_lso_log_factory=>tt_header_name .
    types end of ts_params.

    types begin of ts_data .
    include type zlso_log as log renaming with suffix _log.
    include type zlso_log_message as msg renaming with suffix _msg.
    include type zlso_log_trace as trace renaming with suffix _trace.
    types end of ts_data .

    types tt_data type standard table of ts_data with empty key.

    types begin of ts_hierarchy.
    include type zlso_log.
    types level type i.
    types end of ts_hierarchy.

    types tt_hierarchy type sorted table of ts_hierarchy with unique key primary_key components id seqnr ref_id
      with non-unique sorted key ref components ref_id.

    class-data self type ref to zif_lso_log_factory.

    data params type ts_params.
    data with_ref_logs type abap_bool.
    data hierarchy type tt_hierarchy.
    data logs type zlso_tt_logs.
    data hierarchy_logs type zlso_tt_logs.
    data last_messages type zif_lso_log_factory=>tt_last_messages.

    methods validate_find_params
      importing params type ref to ts_params
      raising   zcx_lso_log.

    methods adjust_find_params
      importing params type ref to ts_params.

    methods adjust_find_text_pattern
      importing text_patterns type ref to data.

    methods create_hierarchy_down
      importing ids   type ts_params-ids
                level type i optional.

    methods create_hierarchy_up
      importing ids   type ts_params-ids
                level type i optional.

    methods adjust_id_param
      importing params type ref to ts_params.

    methods date_time_2_timestamp
      importing date             type d
                time             type t
      returning value(timestamp) type timestamp.

    methods find_by_parameters
      importing params      type ref to ts_params
      returning value(data) type tt_data.

    methods find_db
      importing params      type ref to ts_params
                ids_split   type zif_lso_log_factory=>tt_log_id optional
      returning value(data) type tt_data.

    methods collect_found_traces
      importing data           type tt_data
      returning value(objects) type zlso_tt_log_traces.

    methods build_hierarchy
      importing data type tt_data.

    methods create
      importing log_structure type zlso_log
      returning value(log)    type ref to zcl_lso_log.

    methods create_found
      returning value(logs) type zlso_tt_logs.

    methods process_by_hierarchy_up
      importing log type ref to zlso_s_log.

    methods adjust_latest_message_with_ref
      importing last_message type ref to zif_lso_log_factory=>ts_last_message
                log_id       type zlso_log-id.
endclass.


class zcl_lso_log_factory implementation.

  method instance.
    if zcl_lso_log_factory=>self is not bound.
      zcl_lso_log_factory=>self = new zcl_lso_log_factory( ).
    endif.

    factory = zcl_lso_log_factory=>self.
  endmethod.


  method zif_lso_log_factory~get.
    logs = me->find( log_ids       = value #( ( low = id  sign = zif_lso_log=>c_sign-include  option = zif_lso_log=>c_option-equal ) )
                     log_seqnrs    = cond #( when seqnr is not initial then value #( ( low = seqnr  sign = zif_lso_log=>c_sign-include  option = zif_lso_log=>c_option-equal ) )
                                             else value #( ) )
                     with_ref_logs = with_ref_logs ).
  endmethod.


  method zif_lso_log_factory~create_from_handler.
    data(timestamp) = value timestampl( ).
    get time stamp field timestamp.

    data(log) = new zcl_lso_log_builder( )->set_timestamp( timestamp
                                         " )->set_program( sy-cprog ##TODO
                                         " )->set_tcode( sy-tcode ##TODO
                                         )->set_changed_by( sy-uname
                                         )->add_messages( log_handler->zif_lso_log_abstract~get_messages( )
                                         )->build( ).

    logs = value #( ( log->get_object( ) ) ).
  endmethod.


  method zif_lso_log_factory~create_by_structure.
    log = me->create( structure ).
  endmethod.


  method zif_lso_log_factory~exists.
    if iv_seqnr is not initial.
      data(where) = |seqnr eq @iv_seqnr|.
    endif.

    select single @abap_true
      from zlso_log
      where id = @iv_id
        and (where)
       into @result.
  endmethod.


  method zif_lso_log_factory~delete.
    if log_ids[] is not initial.
      " Select logs that will be deleted taking into account possible ids exclusions.
      select id
        from zlso_log
       where id in @log_ids
        into table @data(ids).
    endif.

    loop at ids assigning field-symbol(<id>) where id is not initial.
      new zcl_lso_log( <id>-id )->zif_lso_log~delete( ).
    endloop.
  endmethod.


  method zif_lso_log_factory~delete_collection.
    loop at logs using key object_key reference into data(log).
      log->instance->delete( ).
    endloop.
  endmethod.


  method zif_lso_log_factory~find_last_messages.
    field-symbols <table> type standard table.

    data(params) = value ts_params( ids = log_ids ).

    " Initialize hierarchy relations between logs (main->reference 1st level->reference 2nd level->...).
    me->hierarchy = value #( ).

    if params-ids[] is not initial.
      me->create_hierarchy_down( params-ids ).

      if me->hierarchy[] is not initial.
        " Adjust Ids parameter with potential referenced logs.
        me->adjust_id_param( ref #( params ) ).
      endif.
    endif.

    me->last_messages = value #( ).

    " EDU-6694 - Split logs ids against potential OpenSQL short dump (Runtime Error DBSQL_STMNT_TOO_LARGE, Exception CX_SY_OPEN_SQL_DB).
    loop at new zcl_lso_range( )->split( params-ids ) into data(logs_ids_split).
      assign logs_ids_split->* to <table>.

      select log~id as log_id,
             log~seqnr as log_seqnr,
             max( msg~timestamp ) as msg_timestamp
        from zlso_log as log
        left outer join zlso_log_message as msg
          on msg~log_id    = log~id
         and msg~log_seqnr = log~seqnr
       where log~id    in @<table>
         and log~seqnr in (
              select max( seqnr )
                from zlso_log
               where id = log~id
          )
       group by log~id, log~seqnr
       appending corresponding fields of table @me->last_messages.
    endloop.

    loop at me->last_messages assigning field-symbol(<last_message>).
      if <last_message>-log_id not in log_ids.
        continue.
      endif.

      insert value #( log_id        = <last_message>-log_id
                      log_seqnr     = <last_message>-log_seqnr
                      msg_timestamp = <last_message>-msg_timestamp )
          into table last_messages reference into data(last_message).

      if line_exists( me->hierarchy[ id    = last_message->log_id
                                     seqnr = last_message->log_seqnr ] ).
        " There are some reference logs, adjust latest message with the latest reference log message information.
        me->adjust_latest_message_with_ref( last_message = last_message
                                            log_id       = last_message->log_id ).
      endif.

      if last_message->msg_timestamp is not initial.
        try.
            " Convert UTC time stamp into date/time.
            zcl_lso_log_utils=>utc_tstmp_2_datetime( exporting iv_tstmp = last_message->msg_timestamp
                                                     importing ev_date  = last_message->msg_date
                                                               ev_time  = last_message->msg_time ).
          catch cx_parameter_invalid_type
                cx_parameter_invalid_range.
            clear last_message->msg_date.
            clear last_message->msg_time.
        endtry.
      endif.
    endloop.
  endmethod.


  method adjust_latest_message_with_ref.
    loop at me->hierarchy assigning field-symbol(<hierarchy>) where id = log_id.
      loop at me->last_messages reference into data(last_message_ref) where log_id eq <hierarchy>-ref_id.
        " Check if reference message was added later?
        if last_message->msg_timestamp < last_message_ref->msg_timestamp.
          last_message->log_ref_id    = last_message_ref->log_id.
          last_message->log_ref_seqnr = last_message_ref->log_seqnr.
          last_message->msg_timestamp = last_message_ref->msg_timestamp.
        endif.

        if last_message_ref->log_id is not initial.
          " There is still some hierarchy down to be checked.
          me->adjust_latest_message_with_ref( last_message = last_message
                                              log_id       = last_message_ref->log_id ).
        endif.
      endloop.
    endloop.
  endmethod.


  method zif_lso_log_factory~find.
    " Prepare search criteria parameters.
    me->params = value #( ids                    = log_ids
                          seqnrs                 = log_seqnrs
                          dates                  = log_dates
                          times                  = log_times
                          changed_bys            = changed_bys
                          contexts               = contexts
                          programs               = programs
                          tcodes                 = tcodes
                          http_statuses          = http_statuses
                          message_texts          = message_texts
                          msgtys                 = msgtys
                          msgids                 = msgids
                          msgnos                 = msgnos
                          request_methods        = request_methods
                          request_urls           = request_urls
                          request_payloads       = request_payloads
                          response_payloads      = response_payloads
                          request_header_names   = req_header_names
                          request_header_values  = req_header_values
                          response_header_names  = res_header_names
                          response_header_values = res_header_values ).

    " Include reference logs in the output?
    me->with_ref_logs = with_ref_logs.

    " Validate provided parameters (search criteria).
    me->validate_find_params( ref #( me->params ) ).

    " Initialize hierarchy relations between logs (main->reference 1st level->reference 2nd level->...).
    me->hierarchy = value #( ).

    " Adjust/change some parameters before DB select.
    me->adjust_find_params( ref #( me->params ) ).

    " Select data from DB according to the passed search criteria parameters.
    data(data) = me->find_by_parameters( ref #( me->params ) ).

    if data[] is initial.
      " No messages found!
      return.
    endif.

    " Collect trace objects based on found messages.
    data(traces) = me->collect_found_traces( data ).

    " Initialize log objects, will be filled with found data.
    me->logs = value zlso_tt_logs( ).

    " Process found logs/messages.
    loop at data assigning field-symbol(<data>).
      " Check if there is already log object with given key.
      data(log) = ref #( me->logs[ key object_key components id    = <data>-id_log
                                                             seqnr = <data>-seqnr_log ] optional ).

      if log is initial.
        " No log object, create a new one.
        insert value #( id       = <data>-id_log
                        seqnr    = <data>-seqnr_log
                        instance = me->create( value #( id            = <data>-id_log
                                                        seqnr         = <data>-seqnr_log
                                                        timestamp     = <data>-timestamp_log
                                                        log_date      = <data>-log_date_log
                                                        log_time      = <data>-log_time_log
                                                        log_mode      = <data>-log_mode_log
                                                        changed_by    = <data>-changed_by_log
                                                        context       = <data>-context_log
                                                        tcode         = <data>-tcode_log
                                                        prog          = <data>-prog_log
                                                        stripped_date = <data>-stripped_date_log ) ) )
          into table me->logs reference into log.
      endif.

      if log->instance is bound.
        " Instantiate message object with 'Builder' pattern.
        data(message_builder) = new zcl_lso_log_message_builder( )->set_log_id( <data>-id_log
                                                                 )->set_log_seqnr( <data>-seqnr_log
                                                                 )->set_timestamp( <data>-timestamp_msg
                                                                 )->set_msgty( <data>-msgty_msg
                                                                 )->set_msgid( <data>-msgid_msg
                                                                 )->set_msgno( <data>-msgno_msg
                                                                 )->set_msgv1( conv #( <data>-msgv1_msg )
                                                                 )->set_msgv2( conv #( <data>-msgv2_msg )
                                                                 )->set_msgv3( conv #( <data>-msgv3_msg )
                                                                 )->set_msgv4( conv #( <data>-msgv4_msg )
                                                                 )->set_stripped_date( <data>-stripped_date_msg ).

        message_builder->set_abap_stack( value #( abap_program     = <data>-abap_program_msg
                                                  abap_include     = <data>-abap_include_msg
                                                  abap_source_line = <data>-abap_source_line_msg
                                                  abap_event       = <data>-abap_event_msg ) ).

        " Set the trace object to message builder if found.
        if <data>-id_trace is not initial.
          data(trace) = ref #( traces[ key object_key components id = <data>-id_trace ] optional ).

          if trace is bound.
            message_builder->set_trace( trace->instance ).
          endif.
        endif.

        " Add message to the log.
        log->instance->add_message( message_builder->build( ) ).
      endif.
    endloop.

    if me->with_ref_logs eq abap_true.
      " Build hierarchy based on found logs.
      me->build_hierarchy( data ).
    endif.

    " Prepare output log objects based on hierarchy, parent log will be instantiated if needed.
    logs = me->create_found( ).
  endmethod.


  method create_found.
    me->hierarchy_logs = value #( ).

    " Create final result taking into account the whole hierarchy relations (main->reference logs).
    loop at me->logs reference into data(log).
      me->process_by_hierarchy_up( log ).
    endloop.

    logs[] = me->hierarchy_logs[].
  endmethod.


  method process_by_hierarchy_up.
    if me->hierarchy[] is not initial.
      " Check if this log exists in the hierarchy as a reference one.
      data(hierarchy_up) = ref #( me->hierarchy[ key ref components ref_id = log->id ] optional ).
    endif.

    if hierarchy_up is bound.
      " Try to find object in already collected ones.
      data(log_up) = ref #( me->logs[ key object_key components id    = hierarchy_up->id
                                                                seqnr = hierarchy_up->seqnr ] optional ).

      if log_up is not bound.
        insert me->create( corresponding #( hierarchy_up->* ) )->get_object( )
          into table me->logs reference into log_up.
      endif.

      " Has parent object got a reference log already?
      if not log_up->instance->has_ref_log( log->instance ).
        " Nope, set current log as a reference one.
        log_up->instance->add_ref_log( log->instance ).

        " Go up in the hierarchy.
        me->process_by_hierarchy_up( log_up ).
      endif.
    else.
      if not line_exists( me->hierarchy_logs[ key object_key components id    = log->id
                                                                        seqnr = log->seqnr ] ).
        " There is no upper log, add it to final table.
        insert log->* into table me->hierarchy_logs.
      endif.
    endif.
  endmethod.


  method collect_found_traces.
    data(trace_ids) = value zcl_lso_log_trace_factory=>tt_r_id( for wa in data where ( id_trace is not initial )
                                                                                     ( sign   = zif_lso_log=>c_sign-include
                                                                                       option = zif_lso_log=>c_option-equal
                                                                                       low    = wa-id_trace ) ).
    if trace_ids[] is not initial.
      sort trace_ids.
      delete adjacent duplicates from trace_ids.

      objects = zcl_lso_log_trace_factory=>create_by_ids( trace_ids ).
    endif.
  endmethod.


  method validate_find_params.
    " Check provided parameters...
    if params->ids[] is initial and params->dates[] is initial.
      " Some basic log parameters MUST be provided for the performance reason.
      " Text searching with no other parameters can kill DB.
      raise exception type zcx_lso_log
        exporting
          textid   = zcx_lso_log=>no_parameters
          mv_msgv1 = |Log Ids/Log dates|.
    endif.
  endmethod.


  method adjust_find_params.
    " Log Id
    if params->ids[] is not initial and me->with_ref_logs eq abap_true.
      me->create_hierarchy_down( params->ids ).

      if me->hierarchy[] is not initial.
        " Adjust IDs parameter with potential referenced logs.
        me->adjust_id_param( params ).
      endif.
    endif.

    " Log date
    if params->dates[] is not initial.
      " Update date range table if only one provided (either begin or end date).
      loop at params->dates assigning field-symbol(<date>) where low  is initial
                                                              or high is initial.
        if <date>-low is not initial and <date>-high is initial.
          <date>-high = <date>-low.
        endif.

        if <date>-low is initial and <date>-high is not initial.
          <date>-low = <date>-high.
        endif.
      endloop.

      " Log time
      if params->times[] is initial.
        " No time range parameter, add it with initial min/max values.
        params->times = value #( ( sign   = zif_lso_log=>c_sign-include
                                   option = zif_lso_log=>c_option-between
                                   low    = zif_lso_log=>c_time-min
                                   high   = zif_lso_log=>c_time-max ) ).
      endif.

      " Message time stamp
      params->message_timestamps = value #( ).

      " Process provided date range.
      loop at params->dates assigning <date>.
        " Process provided time range.
        loop at params->times assigning field-symbol(<time>).
          " Prepare range table for message time stamp.
          params->message_timestamps = value #( ( sign   = <date>-sign
                                                  option = <date>-option
                                                  low    = me->date_time_2_timestamp( date = <date>-low  time = <time>-low )
                                                  high   = me->date_time_2_timestamp( date = <date>-high time = <time>-high ) ) ).
        endloop.
      endloop.

      " Log time stamp
      params->log_timestamps = params->message_timestamps.

      " Log time stamp will be used in SQL select statement to make it work faster.
      " Normally time stamps for messages are earlier than log time stamp because
      " time stamp for log is generated during log sequence save (messages were collected prior to log save).
      " Therefore log high time stamp needs be increased by an hour approximately.
      loop at params->log_timestamps assigning field-symbol(<log_tstmp>).
        try.
            <log_tstmp>-high = cl_abap_tstmp=>add( tstmp = <log_tstmp>-high
                                                   secs  = zif_lso_log_factory=>c_secs-one_hour ).
          catch cx_parameter_invalid_range
                cx_parameter_invalid_type.
        endtry.
      endloop.
    endif.

    " Text search can be case insensitive starting from Net Weaver 7.51!!!
    " https://blogs.sap.com/2016/10/18/abap-news-release-7.51-case-insensitive-search-sql-new-functions/
    if params->message_texts[] is not initial.
      me->adjust_find_text_pattern( ref #( params->message_texts ) ).
    endif.

    if params->request_payloads[] is not initial.
      me->adjust_find_text_pattern( ref #( params->request_payloads ) ).
    endif.

    if params->response_payloads[] is not initial.
      me->adjust_find_text_pattern( ref #( params->response_payloads ) ).
    endif.

    if params->request_header_names[] is not initial.
      me->adjust_find_text_pattern( ref #( params->request_header_names ) ).
    endif.

    if params->request_header_values[] is not initial.
      me->adjust_find_text_pattern( ref #( params->request_header_values ) ).
    endif.

    if params->response_header_names[] is not initial.
      me->adjust_find_text_pattern( ref #( params->response_header_names ) ).
    endif.

    if params->response_header_values[] is not initial.
      me->adjust_find_text_pattern( ref #( params->response_header_values ) ).
    endif.
  endmethod.


  method create_hierarchy_down.
    field-symbols <table> type standard table.

    " Split IDs range table into smaller chunks to have it safely used in OpenSQL update statement.
    loop at new zcl_lso_range( )->split( ids ) into data(ids_split).
      assign ids_split->* to <table>.

      " Check if there are reference logs for provided ones.
      select distinct id, seqnr, ref_id, ref_seqnr, timestamp,
                      log_date, log_time, log_mode, changed_by, context,
                      tcode, prog, stripped_date,
                      @level as level
        from zlso_log
       where id     in @<table>
         and ref_id ne ''
         order by primary key
         appending table @data(hierarchy_down).
    endloop.

    if hierarchy_down[] is initial.
      return.
    endif.

    data(down_ids) = value ts_params-ids( ).

    loop at hierarchy_down assigning field-symbol(<hierarchy>).
      insert corresponding #( <hierarchy> ) into table me->hierarchy.

      if sy-subrc ne 0.
        continue.
      endif.

      if <hierarchy>-ref_id is not initial.
        insert value #( sign   = zif_lso_log=>c_sign-include
                        option = zif_lso_log=>c_option-equal
                        low    = <hierarchy>-ref_id )
          into table down_ids.
      endif.
    endloop.

    if down_ids[] is not initial.
      sort down_ids.
      delete adjacent duplicates from down_ids.

      " There is no OpenSQL recursive select statement, it will be done by recursive method.
      me->create_hierarchy_down( ids   = down_ids
                                 level = level + 1 ).
    endif.
  endmethod.


  method build_hierarchy.
    " Check hierarchy up, parent needs to be found if it is a reference log.
    data(found_ids) = value ts_params-ids( for wa in data ( sign   = zif_lso_log=>c_sign-include
                                                            option = zif_lso_log=>c_option-equal
                                                            low    = wa-id_log ) ).

    if found_ids[] is not initial.
      sort found_ids.
      delete adjacent duplicates from found_ids.

      me->create_hierarchy_up( found_ids ).
    endif.
  endmethod.


  method create_hierarchy_up.
    field-symbols <table> type standard table.

    " Split IDs range table into smaller chunks to have it safely used in OpenSQL update statement.
    loop at new zcl_lso_range( )->split( ids ) into data(ids_split).
      assign ids_split->* to <table>.

      " Check if there are reference logs for provided ones.
      select distinct id, seqnr, ref_id, ref_seqnr, timestamp,
                      log_date, log_time, log_mode, changed_by, context,
                      tcode, prog, stripped_date,
                      @level as level
        from zlso_log
       where ref_id in @<table>
        order by primary key
        appending table @data(hierarchy_up).
    endloop.

    if hierarchy_up[] is initial.
      return.
    endif.

    data(up_ids) = value ts_params-ids( ).

    loop at hierarchy_up assigning field-symbol(<hierarchy>).
      " Build hierarchy only to the particular level (based on provided log id parameter).
      if me->params-ids[] is not initial and <hierarchy>-id not in me->params-ids.
        continue.
      endif.

      " Is this hierarchy level already collected?
      if line_exists( me->hierarchy[ id     = <hierarchy>-id
                                     seqnr  = <hierarchy>-seqnr
                                     ref_id = <hierarchy>-ref_id ] ).
        continue.
      endif.

      insert corresponding #( <hierarchy> ) into table me->hierarchy.

      if sy-subrc ne 0.
        continue.
      endif.

      if <hierarchy>-id is not initial.
        insert value #( sign   = zif_lso_log=>c_sign-include
                        option = zif_lso_log=>c_option-equal
                        low    = <hierarchy>-id )
          into table up_ids.
      endif.
    endloop.

    if up_ids[] is not initial.
      sort up_ids.
      delete adjacent duplicates from up_ids.

      " There is no OpenSQL recursive select statement, it will be done by recursive method.
      me->create_hierarchy_up( ids   = up_ids
                               level = level - 1 ).
    endif.
  endmethod.


  method adjust_id_param.
    " Sorted table therefore reference log id is checked in if statement.
    loop at me->hierarchy assigning field-symbol(<hierarchy>).
      if <hierarchy>-ref_id is not initial.
        insert value #( sign   = zif_lso_log=>c_sign-include
                        option = zif_lso_log=>c_option-equal
                        low    = <hierarchy>-ref_id )
          into table params->ids.
      endif.
    endloop.

    sort params->ids.
    delete adjacent duplicates from params->ids.
  endmethod.


  method adjust_find_text_pattern.
    field-symbols <text_patterns> type standard table.

    assign text_patterns->* to <text_patterns>.

    if sy-subrc ne 0.
      return.
    endif.

    loop at <text_patterns> assigning field-symbol(<pattern>).
      assign component 'OPTION' of structure <pattern> to field-symbol(<option>).

      if sy-subrc ne 0.
        continue.
      endif.

      data(adjust_pattern) = abap_false.

      case <option>.
        when zif_lso_log=>c_option-equal.
          <option> = zif_lso_log=>c_option-contains_pattern.
          adjust_pattern = abap_true.

        when zif_lso_log=>c_option-not_equal.
          <option> = zif_lso_log=>c_option-not_contains_pattern.
          adjust_pattern = abap_true.

        when zif_lso_log=>c_option-contains_pattern.
          adjust_pattern = abap_true.

        when others.
          continue.
      endcase.

      assign component 'LOW' of structure <pattern> to field-symbol(<low>).

      if sy-subrc ne 0.
        continue.
      endif.

      " For 'Contains Pattern'/'Not contains pattern' options text need to contain some wild card characters.
      if adjust_pattern eq abap_true.
        data(match) = match( val = <low> regex = '^\*.*\*$' case = abap_false ).

        " Text pattern will be updated only if it does not contain '*' char at the beginning and at the end.
        if match is initial.
          <low> = |*{ <low> }*|.
        endif.
      endif.
    endloop.
  endmethod.


  method date_time_2_timestamp.
    try.
        " Message time stamp is generated in UTC time.
        " Time parameter is specific for user's time zone, therefore it needs to be converted into UTC.
        cl_abap_tstmp=>systemtstmp_syst2utc( exporting syst_date = date
                                                       syst_time = time
                                             importing utc_tstmp = timestamp ).
      catch cx_parameter_invalid_range .
        convert date date time time
          daylight saving time abap_false
          into time stamp timestamp
          time zone zif_lso_log=>c_time_zone_utc.
    endtry.
  endmethod.


  method find_by_parameters.
    if params->ids[] is not initial.
      " Split IDs range table into smaller chunks to have it safely used in OpenSQL IN statement.
      loop at new zcl_lso_range( )->split( params->ids ) into data(ids_split).
        assign ids_split->* to field-symbol(<ids_split>).

        insert lines of me->find_db( params    = params
                                     ids_split = <ids_split> )
          into table data.
      endloop.
    else.
      " No logs IDs, find data based on all other parameters.
      insert lines of me->find_db( params ) into table data.
    endif.
  endmethod.


  method find_db.
    " Select data according to the passed search criteria.
    select distinct log~id as id_log, log~seqnr as seqnr_log,
                    log~timestamp as timestamp_log, log~log_date as log_date_log, log~log_time as log_time_log,
                    log~log_mode as log_mode_log, log~changed_by as changed_by_log,
                    log~context as context_log,
                    log~tcode as tcode_log, log~prog as prog_log, log~stripped_date as stripped_date_log,
                    message~timestamp as timestamp_msg,
                    message~msgty as msgty_msg, message~msgid as msgid_msg,
                    message~msgno as msgno_msg, message~msgv1 as msgv1_msg,
                    message~msgv2 as msgv2_msg, message~msgv3 as msgv3_msg, message~msgv4 as msgv4_msg,
                    message~abap_program as abap_program_msg, message~abap_include as abap_include_msg,
                    message~abap_source_line as abap_source_line_msg, message~abap_event as abap_event_msg,
                    message~stripped_date as stripped_date_msg,
##TODO
*                    t100~text as t100_text,
                    trace~id as id_trace, trace~http_status as http_status_trace,
                    trace~request_url as request_url_trace, trace~request_method as request_method_trace,
                    trace~stripped_date as stripped_date_trace
      from zlso_log as log
      inner join zlso_log_message as message
        on message~log_id    = log~id
       and message~log_seqnr = log~seqnr
##TODO
*      left outer join t100                             "#EC CI_BUFFJOIN
*        on t100~sprsl = @zif_lso_log=>c_langu_english
*       and t100~arbgb = message~msgid
*       and t100~msgnr = message~msgno
      left outer join zlso_log_trace as trace
        on trace~id = message~trace_id
      " request payload
      left outer join zlso_log_payload as request_payload
        on request_payload~trace_id = trace~id
       and request_payload~type     = @zif_lso_log_payload=>c_type-request
      " response payload
      left outer join zlso_log_payload as response_payload
        on response_payload~trace_id = trace~id
       and response_payload~type     = @zif_lso_log_payload=>c_type-response
      " request headers
      left outer join zlso_log_headers as request_headers
        on request_headers~trace_id = trace~id
       and request_headers~type     = @zif_lso_log_payload=>c_type-request
      " response headers
      left outer join zlso_log_headers as response_headers
        on response_headers~trace_id = trace~id
       and response_headers~type     = @zif_lso_log_payload=>c_type-response
      where log~id                   in @ids_split
        and log~seqnr                in @params->seqnrs
        and log~timestamp            in @params->log_timestamps
        and log~changed_by           in @params->changed_bys
        and log~context              in @params->contexts
        and log~tcode                in @params->tcodes
        and log~prog                 in @params->programs
        and message~timestamp        in @params->message_timestamps
        and message~msgty            in @params->msgtys
        and message~msgid            in @params->msgids
        and message~msgno            in @params->msgnos
        and ( message~msgv1          in @params->message_texts
           or message~msgv2          in @params->message_texts
           or message~msgv3          in @params->message_texts
           or message~msgv4          in @params->message_texts
##TODO
*           or t100~text              in @params->message_texts
                                                               ) "#EC CI_CMPLX_WHERE
        and trace~http_status        in @params->http_statuses
        and trace~request_url        in @params->request_urls
        and trace~request_method     in @params->request_methods
        and request_payload~payload  in @params->request_payloads
        and response_payload~payload in @params->response_payloads
        and request_headers~name     in @params->request_header_names
        and request_headers~value    in @params->request_header_values
        and response_headers~name    in @params->response_header_names
        and response_headers~value   in @params->response_header_values
      order by log~id ascending, log~seqnr ascending, log~timestamp ascending,
               message~timestamp ascending
      into corresponding fields of table @data.
  endmethod.


  method create.
    log = new zcl_lso_log( log_structure-id ).

    log->seqnr         = log_structure-seqnr.
    log->timestamp     = log_structure-timestamp.
    log->date          = log_structure-log_date.
    log->time          = log_structure-log_time.
    log->mode          = log_structure-log_mode.
    log->changed_by    = log_structure-changed_by.
    log->context       = log_structure-context.
    log->tcode         = log_structure-tcode.
    log->program       = log_structure-prog.
    log->stripped_date = log_structure-stripped_date.

    if log->timestamp is not initial.
      try.
          " Convert log time stamp into date and time.
          zcl_lso_log_utils=>utc_tstmp_2_datetime( exporting iv_tstmp = log->timestamp
                                                   importing ev_date  = data(date)
                                                             ev_time  = data(time) ).
        catch cx_parameter_invalid_type
              cx_parameter_invalid_range.
          date = log_structure-log_date.
          time = log_structure-log_time.
      endtry.

      if log->date ne date.
        log->date = date.
      endif.

      if log->time ne time.
        log->time = time.
      endif.
    endif.
  endmethod.


  method zif_lso_log_factory~logs2ids.
    ids = value #( for l in logs ( sign   = zif_lso_range=>c_sign-include
                                   option = zif_lso_range=>c_option-equal
                                   low    = l-id ) ).
  endmethod.

endclass.
