INTERFACE zif_fdlog
  PUBLIC .
  TYPES: tv_unixtime TYPE p LENGTH 16 DECIMALS 0.

  TYPES: BEGIN OF ts_fdlog,
           system  TYPE syst-sysid,
           client  TYPE syst-mandt,
           user    TYPE syst-uname,
           msgtype TYPE syst-msgty,
           message TYPE string,
           msgid   TYPE syst-msgid,
           msgno   TYPE syst-msgno,
           program TYPE syst-cprog,
           host    TYPE syst-host,
           time    TYPE tv_unixtime,
         END OF ts_fdlog,
         tt_fdlog TYPE STANDARD TABLE OF ts_fdlog WITH EMPTY KEY.

  METHODS:
    i IMPORTING iv_message TYPE string,
    s IMPORTING iv_message TYPE string,
    w IMPORTING iv_message TYPE string,
    e IMPORTING iv_message TYPE string,
    a IMPORTING iv_message TYPE string,
    x IMPORTING iv_message TYPE string,
    log IMPORTING is_symsg type symsg OPTIONAL,
    send.
ENDINTERFACE.
