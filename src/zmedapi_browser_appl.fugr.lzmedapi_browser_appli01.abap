*----------------------------------------------------------------------*
***INCLUDE LZISHMED_API_BROWSER_APPLI01.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  exit_0100  INPUT
*&---------------------------------------------------------------------*
MODULE exit_0100 INPUT.

  IF cl_ish_gui_dynpro_connector=>exit_command( i_repid = sy-repid
                                                i_ucomm = gv_ucomm_0100 ) = abap_true.
    LEAVE TO SCREEN 0.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  before_pai_0100  INPUT
*&---------------------------------------------------------------------*
MODULE before_pai_0100 INPUT.

  cl_ish_gui_dynpro_connector=>before_pai( sy-repid ).

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  pai_0100  INPUT
*&---------------------------------------------------------------------*
MODULE pai_0100 INPUT.

  cl_ish_gui_dynpro_connector=>pai( sy-repid ).

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  after_pai_0100  INPUT
*&---------------------------------------------------------------------*
MODULE after_pai_0100 INPUT.

  cl_ish_gui_dynpro_connector=>after_pai( sy-repid ).

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ucomm_0100  INPUT
*&---------------------------------------------------------------------*
MODULE ucomm_0100 INPUT.

  IF cl_ish_gui_dynpro_connector=>user_command( i_repid = sy-repid
                                                i_ucomm = gv_ucomm_0100 ) = abap_true.
    LEAVE TO SCREEN 0.
  ENDIF.

ENDMODULE.
