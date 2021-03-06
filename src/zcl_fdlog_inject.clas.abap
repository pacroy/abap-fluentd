CLASS zcl_fdlog_inject DEFINITION FOR TESTING
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    CLASS-METHODS:
      inject_http
        IMPORTING io_http TYPE REF TO if_http_client,
      inject_rest
        IMPORTING io_rest TYPE REF TO if_rest_client.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_fdlog_inject IMPLEMENTATION.

  METHOD inject_http.
    zcl_fdlog_factory=>ao_http = io_http.
  ENDMETHOD.

  METHOD inject_rest.
    zcl_fdlog_factory=>ao_rest = io_rest.
  ENDMETHOD.

ENDCLASS.
