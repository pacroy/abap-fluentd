CLASS zcl_fdlog_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    CLASS-METHODS:
      http
        RETURNING VALUE(ro_http) TYPE REF TO if_http_client
        RAISING
                  zcx_fdlog,
      rest
        IMPORTING io_http        TYPE REF TO if_http_client
        RETURNING VALUE(ro_rest) TYPE REF TO cl_rest_http_client.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS c_rfc_dest TYPE rfcdest VALUE 'FLUENTD' ##NO_TEXT.

    CLASS-DATA: ao_http TYPE REF TO if_http_client.
    CLASS-DATA: ao_rest TYPE REF TO cl_rest_http_client.
ENDCLASS.



CLASS zcl_fdlog_factory IMPLEMENTATION.

  METHOD http.
    IF ( ro_http IS NOT BOUND ).
      cl_http_client=>create_by_destination(
        EXPORTING
          destination = c_rfc_dest
        IMPORTING
          client = ao_http
        EXCEPTIONS
          OTHERS = 1 ).
      IF ( sy-subrc <> 0 ).
        RAISE EXCEPTION TYPE zcx_fdlog.
      ENDIF.
    ENDIF.
    ro_http = ao_http.
  ENDMETHOD.

  METHOD rest.
    IF ( ro_rest IS NOT BOUND ).
      ao_rest = NEW #( io_http ).
    ENDIF.
    ro_rest = ao_rest.
  ENDMETHOD.

ENDCLASS.
