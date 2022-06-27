CLASS zcx_medapi DEFINITION
  PUBLIC
  INHERITING FROM cx_ish_static_handler
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES if_t100_dyn_msg.
    INTERFACES if_t100_message.

    DATA:
      mv_text1 TYPE string READ-ONLY,
      mv_text2 TYPE string READ-ONLY,
      mv_text3 TYPE string READ-ONLY,
      mv_text4 TYPE string READ-ONLY.

    METHODS constructor
      IMPORTING
        is_textid       LIKE if_t100_message=>t100key OPTIONAL
        iv_text1        TYPE string OPTIONAL
        iv_text2        TYPE string OPTIONAL
        iv_text3        TYPE string OPTIONAL
        iv_text4        TYPE string OPTIONAL
        ix_previous     LIKE previous OPTIONAL
        io_errorhandler TYPE REF TO cl_ishmed_errorhandling OPTIONAL
        iv_message_type TYPE sy-msgty DEFAULT 'E'.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS zcx_medapi IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    super->constructor( previous = ix_previous gr_errorhandler = io_errorhandler gr_msgtyp = iv_message_type ).

    if_t100_message~t100key =
        COND #(
            WHEN is_textid IS NOT INITIAL
                THEN is_textid
                ELSE if_t100_message=>default_textid ).

    mv_text1 = iv_text1.
    mv_text2 = iv_text2.
    mv_text3 = iv_text3.
    mv_text4 = iv_text4.

  ENDMETHOD.


ENDCLASS.
