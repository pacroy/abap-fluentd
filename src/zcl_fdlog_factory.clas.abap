CLASS zcl_fdlog_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_fdlog_inject.

  PUBLIC SECTION.
    CLASS-METHODS:
      http
        RETURNING VALUE(ro_http) TYPE REF TO if_http_client
        RAISING
                  zcx_fdlog,
      rest
        IMPORTING io_http        TYPE REF TO if_http_client
        RETURNING VALUE(ro_rest) TYPE REF TO if_rest_client.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS c_rfc_dest TYPE rfcdest VALUE 'FLUENTD' ##NO_TEXT.

    CLASS-DATA: ao_http TYPE REF TO if_http_client.
    CLASS-DATA: ao_rest TYPE REF TO if_rest_client.
ENDCLASS.



CLASS zcl_fdlog_factory IMPLEMENTATION.

  METHOD http.
    IF ( ao_http IS NOT BOUND ).
      cl_http_client=>create_by_destination(
        EXPORTING
          destination = c_rfc_dest
        IMPORTING
          client = ao_http
        EXCEPTIONS
          OTHERS = 1 ).
      IF ( sy-subrc <> 0 ).
        RAISE EXCEPTION TYPE zcx_fdlog
          EXPORTING
            textid = zcx_fdlog=>cx_http_client.
      ENDIF.
    ENDIF.
    ro_http = ao_http.
  ENDMETHOD.

  METHOD rest.
    IF ( ao_rest IS NOT BOUND ).
      ao_rest = NEW cl_rest_http_client( io_http ).
    ENDIF.
    ro_rest = ao_rest.
  ENDMETHOD.

ENDCLASS.
