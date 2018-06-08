*"* use this source file for your ABAP unit test classes

CLASS ltcl_log DEFINITION DEFERRED.
CLASS zcl_fdlog DEFINITION LOCAL FRIENDS ltcl_log.

CLASS ltcl_log DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PUBLIC SECTION.
  PROTECTED SECTION.
    DATA: ao_cut  TYPE REF TO zcl_fdlog.
  PRIVATE SECTION.
    METHODS:
      setup,
      happy_path FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_log IMPLEMENTATION.

  METHOD setup.

    ao_cut = NEW #( ).

  ENDMETHOD.

  METHOD happy_path.

    DATA(ls_data) = zcl_hello_controller=>hello( 'ABAPUnit' ).

    ao_cut->log( ls_data ).

  ENDMETHOD.

ENDCLASS.
