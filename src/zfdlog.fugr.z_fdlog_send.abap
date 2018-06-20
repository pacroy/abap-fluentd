FUNCTION Z_FDLOG_SEND.
*"----------------------------------------------------------------------
*"*"Update Function Module:
*"
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IM_INST_NAME) TYPE  SHM_INST_NAME
*"----------------------------------------------------------------------
  TRY.
      NEW zcl_fdlog( iv_inst_name = im_inst_name iv_upd_task = abap_false )->zif_fdlog~send( ).
    CATCH zcx_fdlog INTO DATA(lx_fdlog).
*     Do nothing
  ENDTRY.

ENDFUNCTION.
