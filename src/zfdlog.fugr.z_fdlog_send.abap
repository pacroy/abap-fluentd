FUNCTION Z_FDLOG_SEND.
*"----------------------------------------------------------------------
*"*"Update Function Module:
*"
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IM_INST_NAME) TYPE  SHM_INST_NAME
*"----------------------------------------------------------------------
  NEW zcl_fdlog( iv_inst_name = im_inst_name iv_upd_task = abap_false )->send( ).

ENDFUNCTION.
