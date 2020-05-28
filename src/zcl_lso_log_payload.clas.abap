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

    aliases is_html               for zif_lso_log_payload~is_html.
    aliases is_json               for zif_lso_log_payload~is_json.
    aliases is_xml                for zif_lso_log_payload~is_xml.
    aliases is_saml               for zif_lso_log_payload~is_saml.

    class-methods create_request
      importing trace_id        type zlso_log_payload-trace_id
                payload         type string
      returning value(instance) type ref to zcl_lso_log_payload .

    class-methods create_response
      importing trace_id        type zlso_log_payload-trace_id
                payload         type string
      returning value(instance) type ref to zcl_lso_log_payload .

    class-methods save_collection
      importing payloads      type ref to if_object_collection
                with_commit   type abap_bool default abap_false
      returning value(result) type abap_bool
      raising   zcx_lso_log .

    methods constructor
      importing trace_id type zlso_log_payload-trace_id
                type     type zlso_log_payload-type
                payload  type string .

  protected section.
  private section.
    constants c_max_line_length type i value 1333.          "#EC NOTEXT

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
endclass.



class zcl_lso_log_payload implementation.


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

            lv_formatted_payload =
              |{ lv_formatted_payload }| &&
              |{ cl_abap_char_utilities=>newline }| &&
              |--batch{ lt_string_odata[ 1 ] }|.

          elseif lv_lines = 2.

            data(lv_odata_payload) = |{ cv_odata_start }{ lt_string_odata[ 2 ] }|.

            "JSON - Pretty Printer
            lv_odata_payload = me->pretty_printer(
              type    = if_sxml=>co_xt_json
              payload = lv_odata_payload ).

            lv_formatted_payload =
              |{ lv_formatted_payload  }| &&
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


  method is_saved.
    result = me->saved.
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
            formatted_payload = cl_abap_codepage=>convert_from( writer->get_output( ) ).
          catch cx_parameter_invalid_range
                cx_sy_codepage_converter_init
                cx_sy_conversion_codepage
                cx_parameter_invalid_type.
            " Convertion problem - use payload without formatting.
            formatted_payload = payload.
        endtry.
      catch cx_transformation_error into data(lo_cx_trans).
        " Transformation error - use payload without formatting.
        formatted_payload = payload.
    endtry.
  endmethod.


  method save_collection.
    data(lt_db_payload) = value zlso_tt_log_trace_payloads( ).

    data(lo_iterator) = payloads->get_iterator( ).

    while lo_iterator->has_next( ) eq abap_true.
      data(lo_payload) = cast zcl_lso_log_payload( lo_iterator->get_next( ) ).

      if lo_payload->is_saved( ) eq abap_true.
        continue.
      endif.

      data(lt_payload) = lo_payload->zif_lso_log_payload~get_payload_lines( ).

      loop at lt_payload assigning field-symbol(<lv_payload>).
        data(lv_seqnr) = sy-tabix.

        " Prepare payload data table for saving.
        insert value zlso_log_payload( trace_id = lo_payload->get_trace_id( )
                                       type     = lo_payload->get_type( )
                                       seqnr    = lv_seqnr
                                       payload  = <lv_payload> )
          into table lt_db_payload.
      endloop.
    endwhile.

    try.
        if lt_db_payload[] is not initial.
          " Save trace payload in DB.
          insert zlso_log_payload from table @lt_db_payload accepting duplicate keys.
        endif.

        result = boolc( sy-dbcnt > 0 ).

        lo_iterator = payloads->get_iterator( ).

        while lo_iterator->has_next( ) eq abap_true.
          lo_payload = cast zcl_lso_log_payload( lo_iterator->get_next( ) ).

          if lo_payload->get_trace_id( ) is initial.
            lo_payload->set_saved( abap_false ).
          elseif lo_payload->is_saved( ) eq abap_false.
            lo_payload->set_saved( result ).
          endif.
        endwhile.

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


  method set_saved.
    me->saved = saved.
  endmethod.


  method zif_lso_log_payload~get_formatted_payload.
    data(payload) = me->get_payload( ).
    data(mime_type) = me->get_mime_type( ).

    case mime_type.
        " 'text/plain'
      when if_xms_resource=>mimetype_text_plain.
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
      when if_xms_resource=>mimetype_text_xml.
        " XML - Pretty Printer
        formatted_payload = me->pretty_printer( type    = if_sxml=>co_xt_xml10
                                                payload = payload ).

        " 'text/html'
      when if_xms_resource=>mimetype_text_html.
        "   HTML - Pretty Printer
        formatted_payload = me->pretty_printer( type    = if_sxml=>co_xt_xml10 "HTML is type of XML
                                                payload = payload ).

      when others.
        " No formatting needs to be done.
        formatted_payload = payload.

    endcase.

    formatted_payload = |{ formatted_payload }|.

  endmethod.


  method zif_lso_log_payload~get_mime_type.
    if me->mime_type is initial.
      case abap_true.
          " XML
        when me->is_xml( ).
          " text/xml
          me->mime_type = if_xms_resource=>mimetype_text_xml.

          " HTML
        when me->is_html( ).
          " text/html
          me->mime_type = if_xms_resource=>mimetype_text_html.

          " JSON
        when me->is_json( ).
          " application/json; charset=utf-8
          " me->mv_mime_type = /ui2/if_rest_const=>gc_content_type_json.
          me->mime_type = zif_lso_log_payload=>c_mime_type-json.

          " 'Pretty Printer' for JSON...
          data(writer) = cl_sxml_string_writer=>create( if_sxml=>co_xt_json ).

          try.
              call transformation id
                source xml me->payload
                result xml writer.
            catch cx_transformation_error.
              " JSON transformation error - mime type is not the JSON one.
              me->mime_type = if_xms_resource=>mimetype_text_plain.
          endtry.

          " TEXT
        when others.
          " text/plain
          me->mime_type = if_xms_resource=>mimetype_text_plain.
      endcase.
    endif.

    mime_type = me->mime_type.
  endmethod.


  method zif_lso_log_payload~get_payload.
    payload = me->payload.
  endmethod.


  method zif_lso_log_payload~get_payload_lines.
    " Max capacity of CHAR variable.
    constants lc_max_char_length type i value 65535.

    types lty_line type c length c_max_line_length.
    types lty_t_lines type standard table of lty_line with default key.

    data lo_c_payload type ref to data.

    data(payload) = condense( me->payload ).

    " Get the payload length.
    data(length) = strlen( payload ).

    if length < 1.
      return.
    endif.

    data(lines) = value lty_t_lines( ).

    if me->zif_lso_log_payload~is_xml( ).
      " If the payload is XML split it by tag to maintain properXML structure
      data(xml_payload) = value string_table( ).

      split payload at '>' into table xml_payload.

      data(line) = value string( ).

      data(xml_lines) = lines( xml_payload ).

      loop at xml_payload into data(xml_line).
        data(line_copy) = line.

        condense xml_line.

        line = |{ line }{ xml_line }>|.

        if strlen( line ) gt c_max_line_length.
          "Flush
          insert conv #( line_copy ) into table lines.

          line = |{ xml_line }>|.
        elseif sy-tabix eq xml_lines.
          "Flush at end
          insert conv #( line ) into table lines.
        endif.
      endloop.

      loop at lines assigning field-symbol(<line>).
        insert <line> into table payload_lines.
      endloop.

    else.
      data(current_length) = value i( ).

      while length > 0.
        " Protection against too long payload!
        if length > lc_max_char_length.
          " Payload length is longer than max number of char variable - it will be processed in parts.
          current_length = lc_max_char_length.
        else.
          " Payload length is less than max number of char variable - it will be processed as whole.
          current_length = length.
        endif.

        " Create var defined as char with length of passed payload parameter.
        create data lo_c_payload type c length current_length.

        " Assign ref data to field-symbol so that it can be splitted into table of lines.
        assign lo_c_payload->* to field-symbol(<lv_c_payload>).

        " Payload in CHAR that will be splitted into lines.
        <lv_c_payload> = payload(current_length).

        " Split payload into lines.
        call function 'SPLIT_LINE'
          exporting
            text       = <lv_c_payload>
            len        = current_length
            maxlen     = c_max_line_length
            sep_before = '([{<'
            sep_behind = ',.)]}>'
          tables
            result_tab = lines.

        loop at lines assigning <line>.
          insert <line> into table payload_lines.
        endloop.

        if length > lc_max_char_length.
          " Payload length is greater than max number of char variable and its first part has been already processed - consider next part of payload.
          payload = payload+current_length.
          length = strlen( payload ).
        else.
          " Payload length is less than max number of char variable and it has been already processed - leave the loop.
          length = 0.
        endif.
      endwhile.
    endif.
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
    " Go for DOM class to check if payload is of XML type.
    data(document) = new cl_xml_document( ).

    result = boolc( document->parse_string( payload ) eq 0 ) .

    document->free( ).
  endmethod.
endclass.
