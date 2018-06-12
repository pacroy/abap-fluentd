CLASS zcl_fdlog DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING io_http TYPE REF TO if_http_client OPTIONAL,
      log
        IMPORTING is_data TYPE any.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: ao_http TYPE REF TO if_http_client.
ENDCLASS.



CLASS zcl_fdlog IMPLEMENTATION.

  METHOD constructor.

    IF ( io_http IS NOT BOUND ).
      cl_http_client=>create_by_url(
        EXPORTING
          url = 'http://app.chairat.me:8888/npl'
        IMPORTING
          client = ao_http ).
    ELSE.
      ao_http = io_http.
    ENDIF.

  ENDMETHOD.

  METHOD log.

    DATA(lv_data) = /ui2/cl_json=>serialize( is_data ).

    ao_http->request->set_version( if_http_request=>co_protocol_version_1_1 ).
    ao_http->request->set_content_type( if_rest_media_type=>gc_appl_json ).
    ao_http->request->set_cdata( lv_data ).
    ao_http->send( EXPORTING timeout = 1 EXCEPTIONS OTHERS = 1 ).

    ao_http->receive( EXCEPTIONS OTHERS = 1 ).

    ao_http->response->get_status( IMPORTING code = DATA(lv_code) reason = DATA(lv_reason) ).
    DATA(lv_response) = ao_http->response->get_cdata( ).

  ENDMETHOD.

ENDCLASS.
