CLASS lcl_fdlog_factory DEFINITION DEFERRED.
CLASS lcl_fdlog_inject DEFINITION DEFERRED.

CLASS lcl_fdlog_abap DEFINITION
  FINAL
  CREATE PRIVATE
  FRIENDS lcl_fdlog_factory.

  PUBLIC SECTION.
    INTERFACES lif_fdlog_abap.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_fdlog_shr_area DEFINITION CREATE PRIVATE FRIENDS lcl_fdlog_factory.

  PUBLIC SECTION.
    INTERFACES lif_fdlog_shr_area.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: ao_shr_area TYPE REF TO zcl_fdlog_shr_area .
    DATA: av_read_only TYPE abap_bool VALUE abap_true.
ENDCLASS.

CLASS lcl_fdlog_factory DEFINITION
  FINAL
  CREATE PRIVATE FRIENDS lcl_fdlog_inject.
  PUBLIC SECTION.
    CLASS-METHODS:
      abap RETURNING VALUE(ro_abap) TYPE REF TO lif_fdlog_abap,
      shr_area RETURNING VALUE(ro_shr_area) TYPE REF TO lif_fdlog_shr_area.
  PRIVATE SECTION.
    CLASS-DATA: ao_abap TYPE REF TO lif_fdlog_abap.
    CLASS-DATA: ao_shr_area TYPE REF TO lif_fdlog_shr_area.
ENDCLASS.

CLASS lcl_fdlog_inject DEFINITION FOR TESTING
  FINAL
  CREATE PRIVATE .
  PUBLIC SECTION.
    CLASS-METHODS: inject_abap
      IMPORTING io_abap TYPE REF TO lif_fdlog_abap.
    CLASS-METHODS: inject_shr_area
      IMPORTING io_shr_area TYPE REF TO lif_fdlog_shr_area.
ENDCLASS.

CLASS lcl_fdlog_inject IMPLEMENTATION.

  METHOD inject_abap.
    lcl_fdlog_factory=>ao_abap = io_abap.
  ENDMETHOD.

  METHOD inject_shr_area.
    lcl_fdlog_factory=>ao_shr_area = io_shr_area.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_fdlog_factory IMPLEMENTATION.

  METHOD abap.
    IF ( ao_abap IS INITIAL ).
      ao_abap = NEW lcl_fdlog_abap( ).
    ENDIF.
    ro_abap = ao_abap.
  ENDMETHOD.

  METHOD shr_area.
    IF ( ao_shr_area IS NOT BOUND  ).
      ao_shr_area = NEW lcl_fdlog_shr_area( ).
    ENDIF.
    ro_shr_area = ao_shr_area.
  ENDMETHOD.

ENDCLASS.


CLASS lcl_fdlog_abap IMPLEMENTATION.

  METHOD lif_fdlog_abap~get_utc_timestamp.
    GET TIME STAMP FIELD rv_timestamp.
  ENDMETHOD.

  METHOD lif_fdlog_abap~get_guid.
    CALL FUNCTION 'GUID_CREATE'
      IMPORTING
        ev_guid_16 = rv_guid   " GUID of length 16 (RAW  Format)
*       ev_guid_22 =     " GUID of length 22 (CHAR Format) Upper/Lower Case (!)
*       ev_guid_32 =     " Guid of length 32 (CHAR Format) Uppper Case
      .
  ENDMETHOD.

ENDCLASS.

CLASS lcl_fdlog_shr_area IMPLEMENTATION.

  METHOD lif_fdlog_shr_area~attach_for_read.
    ao_shr_area = zcl_fdlog_shr_area=>attach_for_read( inst_name = inst_name ).
    handle = ao_shr_area.
    av_read_only = abap_true.
  ENDMETHOD.

  METHOD lif_fdlog_shr_area~attach_for_update.
    ao_shr_area = zcl_fdlog_shr_area=>attach_for_update( inst_name = inst_name ).
    handle = ao_shr_area.
    av_read_only = abap_false.
  ENDMETHOD.

  METHOD lif_fdlog_shr_area~get_root.
    root = ao_shr_area->get_root( ).
  ENDMETHOD.

  METHOD lif_fdlog_shr_area~detach.
    IF ( av_read_only = abap_true ).
      ao_shr_area->detach( ).
    ELSE.
      ao_shr_area->detach_commit( ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
