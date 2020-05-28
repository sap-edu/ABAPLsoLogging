class zcl_lso_trace_payload_viewer definition
  public
  final
  create public.

  public section.

    methods constructor
      importing
        request  type ref to cl_gui_container
        response type ref to cl_gui_container.

    "! Show the request and response payload of the passed trace object
    "! If no trace object is passed or it is not bound, a generic message is shown
    "! @parameter trace | Trace Object
    methods show
      importing
        trace type ref to zcl_lso_log_trace optional.

    methods free.

  protected section.

  private section.

    types: begin of ts_payload,
             container type ref to cl_gui_container,
             viewer    type ref to cl_gui_html_viewer,
             payload   type ref to zcl_lso_log_payload,
           end of ts_payload.

    data: request  type ts_payload.
    data: response type ts_payload.

    methods get_placeholder
      importing
        text           type string default 'no payload data'
      returning
        value(payload) type ref to zcl_lso_log_payload.

    methods display
      importing
        payload type zcl_lso_trace_payload_viewer=>ts_payload.



endclass.

class zcl_lso_trace_payload_viewer implementation.

  method constructor.

    me->request-container  = request.
    me->response-container = response.

    try.
        me->request-viewer  = new #( parent = me->request-container ).
        me->response-viewer = new #( parent = me->response-container ).

      catch cx_root into data(x).
        message |Trace View initialization error! { x->get_longtext( ) }|
          type zif_lso_log_message=>c_type-error.
    endtry.

  endmethod.

  method get_placeholder.

    payload = new #( trace_id = |DisplayDummy|
                     payload  = text
                     type     = |text/plain| ).

  endmethod.

  method show.

    if trace is not bound.
      me->request-payload  = get_placeholder( ).
      me->response-payload = get_placeholder( ).
    elseif trace->is_stripped( ).
      me->request-payload  = get_placeholder( |Stripped on { trace->get_stripped_date( ) date = environment }| ).
      me->response-payload = get_placeholder( |Stripped on { trace->get_stripped_date( ) date = environment }| ).
    else.
      me->request-payload  = cond #( when trace->has_request_payload( )  then trace->get_request_payload( )  else get_placeholder( ) ).
      me->response-payload = cond #( when trace->has_response_payload( ) then trace->get_response_payload( ) else get_placeholder( ) ).
    endif.

    me->display( request ).
    me->display( response ).

  endmethod.

  method display.

    data(string) = payload-payload->get_formatted_payload( ).
    data(mime_type) = payload-payload->get_mime_type( ).
    split mime_type at '/' into data(type) data(subtype).

    if payload-payload->is_html( ).
      string = |{ string }|.
    else.
      if payload-payload->is_xml( ) or
         payload-payload->is_saml( ).
        string = escape( val = string format = cl_abap_format=>e_xml_attr ).
      endif.
      data(class)  = cond #( when payload-payload->is_json( )  then |language-json|
                             when payload-payload->is_xml( )   then |language-xml|
                             when payload-payload->is_saml( )  then |language-xml|
                             else |language-markup| ).

      string =
      |<!DOCTYPE html>                                                  | &&
      |<META http-equiv=Content-Type content="text/html; charset=utf-8">| &&
      |<html>                                                           | &&
      |<head>| &&
      |<link href="https://{ sy-sysid }.wdf.sap.corp/sap(bD1lbiZjPTAwMQ==)/bc/bsp/sap/zlso_log/prism.css" rel="stylesheet" /></head>| &&
      |<body style="background-color:#F5F2F0;">| &&
      |<pre><code id="code" style="display:block; font-size:10pt;" class="{ class }">{ string } </code></pre>       | &&
      |<script src="https://{ sy-sysid }.wdf.sap.corp/sap(bD1lbiZjPTAwMQ==)/bc/bsp/sap/zlso_log/prism.js"></script>| &&
      |</body>                                                          | &&
      |</html>                                                          |.

    endif.

    data sap_codepage   type cpcodepage.
    data abap_encoding  type abap_encod.
    data codepage       type sychar60.
    data encoding       type sychar60.

    codepage = 'UTF-8'. " 'ISO-8859-1'.

    call function 'SCP_CODEPAGE_BY_EXTERNAL_NAME'
      exporting
        external_name = codepage
*       KIND          = 'H'
      importing
        sap_codepage  = sap_codepage
      exceptions
        not_found     = 1
        others        = 2.
    if sy-subrc <> 0.
      sap_codepage = '4110'. "UTF-8
    endif.

    encoding = abap_encoding = sap_codepage.

    set locale language 'L'.
    data(converter)       = cl_abap_conv_out_ce=>create( encoding = abap_encoding ).
    data(size)            = strlen( string ).

    converter->write( data = string n = size ).

    data(buffer)          = converter->get_buffer( ).
    data(xml_size)        = xstrlen( buffer ).
    data(buffer_rows)     = xml_size div 1024.
    data(buffer_reminder) = xml_size mod 1024.

    types: ts_buffer(1024) type x.
    data line   type ts_buffer.
    data lines  type standard table of ts_buffer.
    data offset type i.
    clear offset.

    do buffer_rows times.
      line = buffer+offset(1024).
      append line to lines.
      add 1024 to offset.
    enddo.
    if buffer_reminder > 0.
      append buffer+offset(buffer_reminder) to lines.
    endif.

    data(url)  = value char100( ).
    payload-viewer->load_data( exporting
*                                         type         = |{ type }|
*                                         subtype      = |{ subtype }|
                                         charset      = codepage
                                         encoding     = encoding
                                         size         = xml_size
                               importing assigned_url = url
                               changing  data_table   = lines ).

    payload-viewer->show_url( url ).

  endmethod.

  method free.

    me->request-viewer->free( ).
    me->response-viewer->free( ).

  endmethod.


endclass.
