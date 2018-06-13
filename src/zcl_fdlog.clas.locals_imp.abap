CLASS lcl_fdlog_factory DEFINITION DEFERRED.
CLASS lcl_fdlog_inject DEFINITION DEFERRED.

CLASS lcl_fdlog_abap DEFINITION
  FINAL
  CREATE PRIVATE
  FRIENDS lcl_fdlog_factory.

  PUBLIC SECTION.
    INTERFACES lif_fdlog_abap.
    ALIASES get_utc_timestamp FOR lif_fdlog_abap~get_utc_timestamp.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_fdlog_factory DEFINITION
  FINAL
  CREATE PRIVATE FRIENDS lcl_fdlog_inject.
  PUBLIC SECTION.
    CLASS-METHODS:
      abap RETURNING VALUE(ro_abap) TYPE REF TO lif_fdlog_abap.
  PRIVATE SECTION.
    CLASS-DATA: ao_abap TYPE REF TO lif_fdlog_abap.
ENDCLASS.

CLASS lcl_fdlog_inject DEFINITION FOR TESTING
  FINAL
  CREATE PRIVATE .
  PUBLIC SECTION.
    CLASS-METHODS: inject_abap
      IMPORTING io_abap TYPE REF TO lif_fdlog_abap.
ENDCLASS.

CLASS lcl_fdlog_inject IMPLEMENTATION.

  METHOD inject_abap.
    lcl_fdlog_factory=>ao_abap = io_abap.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_fdlog_factory IMPLEMENTATION.

  METHOD abap.
    IF ( ao_abap IS INITIAL ).
      ao_abap = NEW lcl_fdlog_abap( ).
    ENDIF.
    ro_abap = ao_abap.
  ENDMETHOD.

ENDCLASS.


CLASS lcl_fdlog_abap IMPLEMENTATION.

  METHOD lif_fdlog_abap~get_utc_timestamp.
    GET TIME STAMP FIELD rv_timestamp.
  ENDMETHOD.

ENDCLASS.
