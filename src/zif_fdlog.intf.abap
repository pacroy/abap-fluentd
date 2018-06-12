INTERFACE zif_fdlog
  PUBLIC .
  TYPES: tv_unixtime TYPE string.

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
    i
      IMPORTING message TYPE string.
ENDINTERFACE.
