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
    DATA: ao_rest TYPE REF TO cl_rest_http_client.
ENDCLASS.



CLASS zcl_fdlog IMPLEMENTATION.

  METHOD constructor.

    DATA: lo_http TYPE REF TO if_http_client.

    IF ( io_http IS NOT BOUND ).
      cl_http_client=>create_by_url(
        EXPORTING
          url = 'http://app.chairat.me:8888/npl'
        IMPORTING
          client = lo_http ).
    ELSE.
      lo_http = io_http.
    ENDIF.

    lo_http->request->set_version( if_http_request=>co_protocol_version_1_1 ).
    lo_http->request->set_content_type( if_rest_media_type=>gc_appl_json ).

    ao_rest = NEW #( lo_http ).

  ENDMETHOD.

  METHOD log.

    DATA(lo_request) = ao_rest->if_rest_client~create_request_entity( ).
    data(lv_data) = /ui2/cl_json=>serialize( is_data ).
    lo_request->set_string_data( lv_data ).

    ao_rest->if_rest_client~post( lo_request ).

    data(lv_status) = ao_rest->if_rest_client~get_status( ).
    data(lv_response) = ao_rest->if_rest_client~get_response_entity( )->get_string_data( ).

  ENDMETHOD.

ENDCLASS.
