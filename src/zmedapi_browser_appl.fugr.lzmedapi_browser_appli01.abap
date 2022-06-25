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
module before_pai_0100 input.

  cl_ish_gui_dynpro_connector=>before_pai( i_repid = gc_program_id ).

endmodule.                 " before_pai_0100  INPUT


*&---------------------------------------------------------------------*
*&      Module  pai_0100  INPUT
*&---------------------------------------------------------------------*
module pai_0100 input.

  cl_ish_gui_dynpro_connector=>pai( i_repid = gc_program_id ).

endmodule.                 " pai_0100  INPUT


*&---------------------------------------------------------------------*
*&      Module  after_pai_0100  INPUT
*&---------------------------------------------------------------------*
module after_pai_0100 input.

  cl_ish_gui_dynpro_connector=>after_pai( i_repid = gc_program_id ).

endmodule.                 " after_pai_0100  INPUT


*&---------------------------------------------------------------------*
*&      Module  ucomm_0100  INPUT
*&---------------------------------------------------------------------*
module ucomm_0100 input.

  IF cl_ish_gui_dynpro_connector=>user_command( i_repid = gc_program_id
                                                i_ucomm = gv_ucomm_0100 ) = abap_true.
    LEAVE TO SCREEN 0.
  ENDIF.

endmodule.                 " ucomm_0100  INPUT
