class zcl_lso_log_payload definition
  public
  final
  create public .

  public section.
    interfaces zif_lso_log_payload.

    " Backward compatibility
    aliases get_formatted_payload for zif_lso_log_payload~get_formatted_payload.
    aliases get_mime_type         for zif_lso_log_payload~get_mime_type.
    aliases get_payload           for zif_lso_log_payload~get_payload.
    aliases get_payload_lines     for zif_lso_log_payload~get_payload_lines.
    aliases get_trace_id          for zif_lso_log_payload~get_trace_id.
    aliases get_type              for zif_lso_log_payload~get_type.
    aliases get_object            for zif_lso_log_payload~get_object.

    aliases is_html               for zif_lso_log_payload~is_html.
    aliases is_json               for zif_lso_log_payload~is_json.
    aliases is_xml                for zif_lso_log_payload~is_xml.
    aliases is_saml               for zif_lso_log_payload~is_saml.

    types tt_db_payloads type sorted table of zlso_log_payload with unique key primary_key components trace_id type seqnr.

    class-methods create_request
      importing trace_id        type zlso_log_payload-trace_id
                payload         type string
      returning value(instance) type ref to zcl_lso_log_payload .

    class-methods create_response
      importing trace_id        type zlso_log_payload-trace_id
                payload         type string
      returning value(instance) type ref to zcl_lso_log_payload .

    class-methods save_collection
      importing payloads      type zlso_tt_log_payloads
                with_commit   type abap_bool default abap_false
      returning value(result) type abap_bool
      raising   zcx_lso_log .

    methods constructor
      importing trace_id type zlso_log_payload-trace_id
                type     type zlso_log_payload-type
                payload  type string .

  protected section.
  private section.
    data saved type abap_bool .
    data mime_type type string .
    data payload type string .
    data trace_id type zlso_log_payload-trace_id .
    data type type zlso_log_payload-type .

    methods format_batch_json_response
      importing payload                  type string
      returning value(formatted_payload) type string .

    methods parse_payload
      importing payload               type string
      returning value(parsed_payload) type string .

    methods pretty_printer
      importing type                     type if_sxml=>xml_stream_type
                payload                  type string
      returning value(formatted_payload) type string .

    methods is_saved
      returning value(result) type abap_bool .

    methods set_saved
      importing saved type abap_bool default abap_true .
ENDCLASS.



CLASS ZCL_LSO_LOG_PAYLOAD IMPLEMENTATION.


  method constructor.
    me->trace_id = trace_id.
    me->type = type.
    me->payload = me->parse_payload( payload ).
  endmethod.


  method create_request.
    instance = new zcl_lso_log_payload( trace_id = trace_id
                                        type     = zif_lso_log_payload=>c_type-request
                                        payload  = payload ).
  endmethod.


  method create_response.
    instance = new zcl_lso_log_payload( trace_id = trace_id
                                        type     = zif_lso_log_payload=>c_type-response
                                        payload  = payload ).
  endmethod.


  method is_saved.
    result = me->saved.
  endmethod.


  method zif_lso_log_payload~get_formatted_payload.
    data(payload) = me->get_payload( ).
    data(mime_type) = me->get_mime_type( ).

    case mime_type.
        " 'text/plain'
      when zif_lso_log_payload=>c_mime_type-text.
        " No formatting needs to be done normally...
        " Check if it is a batch json payload response
        formatted_payload = me->format_batch_json_response( payload ).

        " 'application/json; charset=utf-8'
        "  when /ui2/if_rest_const=>gc_content_type_json.
      when zif_lso_log_payload=>c_mime_type-json.
        "   JSON - Pretty Printer
        formatted_payload = me->pretty_printer( type    = if_sxml=>co_xt_json
                                                payload = payload ).

        " 'text/xml'
      when zif_lso_log_payload=>c_mime_type-xml.
        " XML - Pretty Printer
        formatted_payload = me->pretty_printer( type    = if_sxml=>co_xt_xml10
                                                payload = payload ).

        " 'text/html'
      when zif_lso_log_payload=>c_mime_type-html.
        "   HTML - Pretty Printer
        formatted_payload = me->pretty_printer( type    = if_sxml=>co_xt_xml10 "HTML is type of XML
                                                payload = payload ).

      when others.
        " No formatting needs to be done.
        formatted_payload = payload.

    endcase.
  endmethod.


  method zif_lso_log_payload~get_mime_type.
    if me->mime_type is initial.
      case abap_true.
          " XML
        when me->is_xml( ).
          " text/xml
          me->mime_type = zif_lso_log_payload=>c_mime_type-xml.

          " HTML
        when me->is_html( ).
          " text/html
          me->mime_type = zif_lso_log_payload=>c_mime_type-html.

          " JSON
        when me->is_json( ).
          " application/json; charset=utf-8
          me->mime_type = zif_lso_log_payload=>c_mime_type-json.

          " 'Pretty Printer' for JSON...
          data(writer) = cl_sxml_string_writer=>create( if_sxml=>co_xt_json ).

          try.
              call transformation id source xml me->payload
                                     result xml writer.
            catch cx_transformation_error.
              " JSON transformation error - mime type is not the JSON one.
              me->mime_type = zif_lso_log_payload=>c_mime_type-text.
          endtry.

          " TEXT
        when others.
          " text/plain
          me->mime_type = zif_lso_log_payload=>c_mime_type-text.
      endcase.
    endif.

    mime_type = me->mime_type.
  endmethod.


  method zif_lso_log_payload~get_payload.
    payload = me->payload.
  endmethod.


  method zif_lso_log_payload~get_payload_lines.
    try.
        payload_lines = new zcl_string_splitter( )->split( me->payload ).
      catch cx_sy_strg_par_val.
        " Wrong splitters REGEX?
        clear payload_lines.
    endtry.
  endmethod.


  method zif_lso_log_payload~get_trace_id.
    trace_id = me->trace_id.
  endmethod.


  method zif_lso_log_payload~get_type.
    type = me->type.
  endmethod.


  method zif_lso_log_payload~is_saml.
*   Check if the first payload characters are '<saml'.
    result = boolc( find( val = me->payload sub = '<saml:Issuer>' ) > -1 ).
  endmethod.


  method zif_lso_log_payload~is_html.
*   Check if the first payload characters are either '<!doctype' or '<html'.
    result = boolc( find( val = me->payload regex = '^\s*<(!doctype)|(html)' ) > -1 ).
  endmethod.


  method zif_lso_log_payload~is_json.
*   Check if the first payload character is either the '[' or '{' one.
    result = boolc( find( val = me->payload regex = '^\s*[{\[]' ) > -1 ).
  endmethod.


  method zif_lso_log_payload~is_xml.
    try.
        data(xpayload) = cl_abap_conv_codepage=>create_out( )->convert( me->payload ).

        data(xml_reader) = cl_sxml_string_reader=>create( xpayload ).
        data(xml_writer) = cl_sxml_string_writer=>create( ).

        " Can payload be transformed to the XML?
        call transformation id source xml xml_reader
                               result xml xml_writer.

        result = abap_true.
      catch cx_parameter_invalid_range
            cx_sy_conversion_codepage
            cx_transformation_error
            cx_sxml_illegal_argument_error.
        result = abap_false.
    endtry.
  endmethod.


  method zif_lso_log_payload~get_object.
    object = value #( trace_id = me->trace_id
                      type     = me->type
                      instance = me ).
  endmethod.


  method save_collection.
    data(db_payloads) = value tt_db_payloads( ).

    loop at payloads using key object_key reference into data(payload_object).
      data(payload) = cast zcl_lso_log_payload( payload_object->instance ).

      if payload is not bound or payload->is_saved( ).
        continue.
      endif.

      loop at payload->zif_lso_log_payload~get_payload_lines( ) assigning field-symbol(<line>).
        data(lv_seqnr) = sy-tabix.

        " Prepare payload data table for saving.
        insert value zlso_log_payload( trace_id = payload->get_trace_id( )
                                       type     = payload->get_type( )
                                       seqnr    = lv_seqnr
                                       payload  = <line> ) into table db_payloads.
      endloop.
    endloop.

    try.
        if db_payloads[] is not initial.
          " Save trace payload in DB.
          insert zlso_log_payload from table @db_payloads accepting duplicate keys.
        endif.

        result = boolc( sy-dbcnt > 0 ).

        loop at payloads using key object_key reference into payload_object.
          payload = cast zcl_lso_log_payload( payload_object->instance ).

          payload->set_saved( cond #( when payload->get_trace_id( ) is initial then abap_false
                                      else result ) ).
        endloop.

        if with_commit eq abap_true.
          commit work.
        endif.
      catch cx_sy_open_sql_db into data(lo_cx_sql).
        if with_commit eq abap_true.
          rollback work.
        endif.

        " Save problem, raise an exception.
        raise exception type zcx_lso_log
          exporting
            textid            = zcx_lso_log=>save_error
            mv_msgv1          = 'ZLSO_LOG_PAYLOAD'
            mv_exception_text = lo_cx_sql->get_text( ).
    endtry.
  endmethod.


  method parse_payload.
    " Get rid of white chars from the beginning/end of payload.
    parsed_payload = condense( payload ).

*    " Get rid of not printable chars from payload.
*    replace all occurrences of regex '[^[:print:]]' in parsed_payload with ''.

    " Sensitive data markers which values will be replaced with '***' content.
    " I.e.: <username>user</username> -> <username>***</username>
    data(lt_sensitive_data) = value string_table(
    " ( |username| )
      ( |password| ) ).

    " Replace sensitive data in a loop for each defined marker.
    loop at lt_sensitive_data assigning field-symbol(<lv_sensitive_data>).
      <lv_sensitive_data> = condense( <lv_sensitive_data> ).

      " Both XML and JSON payload will be parsed and anonymized accordingly.
      " Regex
      "   ([<"][^>"\}]*password[>"](:?"?))([^<",]+)((</[^>]*password>)|")
      " will replace password with '***' for both xml
      "   <password>MySecretPass</username> -> <password>***</password>
      " and JSON
      "   ..."password":"MySecretPass"... -> ..."password":"***"...
      parsed_payload = replace(
        val   = parsed_payload
        regex = |([<"][^>"\}]*{ <lv_sensitive_data> }[>"](:?"?))([^<",]+)((</[^>]*{ <lv_sensitive_data> }>)\|")|
        with  = '$1***$4'
        case  = abap_false ).
    endloop.
  endmethod.


  method pretty_printer.
    data(writer) = cl_sxml_string_writer=>create( type ).
    writer->if_sxml_writer~set_option( if_sxml_writer=>co_opt_linebreaks ).
    writer->if_sxml_writer~set_option( if_sxml_writer=>co_opt_indent ).

    try.
        call transformation id
          source xml payload
          result xml writer.

        try.
            " Make payload pretty :)
            formatted_payload = cl_abap_conv_codepage=>create_in( )->convert( writer->get_output( ) ).
          catch cx_parameter_invalid_range cx_sy_conversion_codepage.
            " Conversion problem - use payload without formatting.
            formatted_payload = payload.
        endtry.
      catch cx_transformation_error into data(lo_cx_trans).
        " Transformation error - use payload without formatting.
        formatted_payload = payload.
    endtry.
  endmethod.


  method set_saved.
    me->saved = saved.
  endmethod.


  method format_batch_json_response.
    constants cv_odata_start type string value '{"@odata.context":"$metadata#Users/$entity","'.

    data lt_string_batch type table of string.
    data lt_string_odata type table of string.
    data lv_formatted_payload type string.

    data(lv_payload) = payload.

    if lv_payload cs '--batch' and
       lv_payload cs cv_odata_start .
      "Ok... it's a LMS User entity return payload... continue...
    else.
      formatted_payload = payload.
      return. "It's something else.
    endif.

    try.
        split lv_payload at '--batch' into table lt_string_batch.
        loop at lt_string_batch assigning field-symbol(<lv_string>).

          split <lv_string> at cv_odata_start into table lt_string_odata.
          data(lv_lines) = lines( lt_string_odata ).
          if lv_lines = 0.
            continue.

          elseif lv_lines = 1.

            lv_formatted_payload = |{ lv_formatted_payload }| &&
                                   |{ cl_abap_char_utilities=>newline }| &&
                                   |--batch{ lt_string_odata[ 1 ] }|.

          elseif lv_lines = 2.
            data(lv_odata_payload) = |{ cv_odata_start }{ lt_string_odata[ 2 ] }|.

            "JSON - Pretty Printer
            lv_odata_payload = me->pretty_printer( type    = if_sxml=>co_xt_json
                                                   payload = lv_odata_payload ).

            lv_formatted_payload = |{ lv_formatted_payload  }| &&
                                   |{ cl_abap_char_utilities=>newline }| &&
                                   |--batch{ lt_string_odata[ 1 ] }| &&
                                   |{ lv_odata_payload }|.
          else.
            "More than 2 lines?? should not happend
            data(lv_error) = abap_true.
          endif.
        endloop.
      catch cx_sy_itab_line_not_found into data(lx_line_not_found).
        lv_error = abap_true.
    endtry.

    if lv_error = abap_true.
      "Fuck... error...
      formatted_payload = payload.
      return.
    endif.

    "Check difference between orginial payload in characters...
    data(lv_check_formatted) = lv_formatted_payload.
    data(lv_check_original)  = payload.

    replace all occurrences of cl_abap_char_utilities=>newline in lv_check_formatted  with space.
    replace all occurrences of cl_abap_char_utilities=>newline in lv_check_original   with space.
    replace all occurrences of cl_abap_char_utilities=>cr_lf   in lv_check_formatted  with space.
    replace all occurrences of cl_abap_char_utilities=>cr_lf   in lv_check_original   with space.
    replace all occurrences of regex '[^[:print:]]+$'          in lv_check_formatted  with space  ignoring case.
    replace all occurrences of regex '[^[:print:]]+$'          in lv_check_original   with space  ignoring case.

    condense lv_check_formatted no-gaps.
    condense lv_check_original no-gaps.

    "No clue why there is a 1 character difference in each returned user payload...
    "All looks perfect... therefore allow for 1% of difference...

    data(lv_length_original) = strlen( lv_check_original ).
    data(lv_length_formatted) = strlen( lv_check_formatted ).

    data(lv_difference) = lv_length_original - lv_length_formatted.
    data(lv_diff_percent) = lv_difference / ( lv_length_original / 100 ).

    if lv_diff_percent <= 1.
      formatted_payload = lv_formatted_payload.
    else.
      formatted_payload = payload.
    endif.
  endmethod.
ENDCLASS.
