class ZCX_FDLOG definition
  public
  inheriting from CX_DYNAMIC_CHECK
  final
  create public .

public section.

  constants ZCX_FDLOG type SOTR_CONC value '000D3A282AC01EE89D879A3301D10E6D' ##NO_TEXT.
  constants CX_ATTACH_ERROR type SOTR_CONC value '000D3A282AC01EE89D87B29B06DF8E86' ##NO_TEXT.
  constants CX_HTTP_CLIENT type SOTR_CONC value '000D3A282AC01EE89D87B9774CA00E8C' ##NO_TEXT.
  constants CX_HTTP_FAILED type SOTR_CONC value '000D3A282AC01EE89D87F3C315970ECE' ##NO_TEXT.
  constants CX_NO_DATA type SOTR_CONC value '000D3A282AC01EE89D880A855A09CEE7' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID default ZCX_FDLOG
      !PREVIOUS like PREVIOUS optional .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_FDLOG IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        textid   = textid
        previous = previous.
  ENDMETHOD.
ENDCLASS.
