*----------------------------------------------------------------------*
***INCLUDE LZISHMED_API_BROWSER_APPLI01.
*----------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Module  exit_0100  INPUT
*&---------------------------------------------------------------------*
MODULE exit_0100 INPUT.

  IF cl_ish_gui_dynpro_connector=>exit_command( i_repid = gc_program_id
                                                i_ucomm = gv_ucomm_0100 ) = abap_true.
    LEAVE TO SCREEN 0.
  ENDIF.

ENDMODULE.                 " exit_0100  INPUT


*&---------------------------------------------------------------------*
*&      Module  before_pai_0100  INPUT
*&---------------------------------------------------------------------*
MODULE before_pai_0100 INPUT.

  cl_ish_gui_dynpro_connector=>before_pai( i_repid = gc_program_id ).

ENDMODULE.                 " before_pai_0100  INPUT


*&---------------------------------------------------------------------*
*&      Module  pai_0100  INPUT
*&---------------------------------------------------------------------*
MODULE pai_0100 INPUT.

  cl_ish_gui_dynpro_connector=>pai( i_repid = gc_program_id ).

ENDMODULE.                 " pai_0100  INPUT


*&---------------------------------------------------------------------*
*&      Module  after_pai_0100  INPUT
*&---------------------------------------------------------------------*
MODULE after_pai_0100 INPUT.

  cl_ish_gui_dynpro_connector=>after_pai( i_repid = gc_program_id ).

ENDMODULE.                 " after_pai_0100  INPUT


*&---------------------------------------------------------------------*
*&      Module  ucomm_0100  INPUT
*&---------------------------------------------------------------------*
MODULE ucomm_0100 INPUT.

  IF cl_ish_gui_dynpro_connector=>user_command( i_repid = gc_program_id
                                                i_ucomm = gv_ucomm_0100 ) = abap_true.
    LEAVE TO SCREEN 0.
  ENDIF.

ENDMODULE.                 " ucomm_0100  INPUT