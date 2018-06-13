CLASS zcl_fdlog_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_fdlog_factory .

  PUBLIC SECTION.
    INTERFACES zif_fdlog_abap.
    ALIASES get_utc_timestamp FOR zif_fdlog_abap~get_utc_timestamp.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_fdlog_abap IMPLEMENTATION.

  METHOD zif_fdlog_abap~get_utc_timestamp.
    GET TIME STAMP FIELD rv_timestamp.
  ENDMETHOD.

ENDCLASS.
