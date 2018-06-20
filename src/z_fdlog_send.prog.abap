*&---------------------------------------------------------------------*
*& Report z_fdlog_send
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_fdlog_send.

CLASS lcl_factory DEFINITION DEFERRED.

CLASS lcl_app DEFINITION CREATE PRIVATE FRIENDS lcl_factory.

  PUBLIC SECTION.
    METHODS:
      initialization,
      start_of_selection,
      end_of_selection.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA ax_fdlog TYPE REF TO zcx_fdlog.

ENDCLASS.

CLASS lcl_factory DEFINITION CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-DATA: ao_inst_name TYPE shm_inst_name VALUE cl_shm_area=>default_instance.

    CLASS-METHODS:
      app RETURNING VALUE(ro_app) TYPE REF TO lcl_app,
      fdlog RETURNING VALUE(ro_fdlog) TYPE REF TO zif_fdlog.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA: ao_app TYPE REF TO lcl_app.
    CLASS-DATA: ao_fdlog TYPE REF TO zif_fdlog.

ENDCLASS.

PARAMETERS: p_insnam TYPE shm_inst_name OBLIGATORY.

INITIALIZATION.
  lcl_factory=>app( )->initialization( ).

START-OF-SELECTION.
  lcl_factory=>app( )->start_of_selection( ).

END-OF-SELECTION.
  lcl_factory=>app( )->end_of_selection( ).


CLASS lcl_app IMPLEMENTATION.

  METHOD end_of_selection.
    TRY.
        data(lv_count) = lcl_factory=>fdlog( )->send( ).
        MESSAGE |{ lv_count } record(s) sent successfully.| TYPE 'S'.
      CATCH zcx_fdlog INTO ax_fdlog.
        IF ( ax_fdlog->textid = ax_fdlog->cx_no_data ).
          MESSAGE ax_fdlog->get_text( ) TYPE 'S'.
        ELSE.
          MESSAGE ax_fdlog->get_text( ) TYPE 'E'.
        ENDIF.
    ENDTRY.
  ENDMETHOD.

  METHOD initialization.

  ENDMETHOD.

  METHOD start_of_selection.
    lcl_factory=>ao_inst_name = p_insnam.
  ENDMETHOD.


ENDCLASS.

CLASS lcl_factory IMPLEMENTATION.

  METHOD app.
    IF ( ao_app IS NOT BOUND ).
      ao_app = NEW lcl_app( ).
    ENDIF.
    ro_app = ao_app.
  ENDMETHOD.

  METHOD fdlog.
    IF ( ao_fdlog IS NOT BOUND ).
      ao_fdlog = NEW zcl_fdlog( iv_inst_name = ao_inst_name iv_upd_task = abap_false ).
    ENDIF.
    ro_fdlog = ao_fdlog.
  ENDMETHOD.

ENDCLASS.
