CLASS zcl_medapi_model DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_ish_gui_model.
    INTERFACES if_ish_gui_structure_model.
    INTERFACES if_ish_gui_treenode_model.

    METHODS constructor
      IMPORTING
        is_data TYPE zmedapi_s_list
      RAISING
        cx_ish_static_handler.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA:
      gr_structdescr TYPE REF TO cl_abap_structdescr,
      gs_data        TYPE zmedapi_s_list.

ENDCLASS.



CLASS zcl_medapi_model IMPLEMENTATION.


  METHOD constructor.

    cl_abap_typedescr=>describe_by_data( EXPORTING  p_data      = gs_data
                                         RECEIVING  p_descr_ref = DATA(lr_typedescr)
                                         EXCEPTIONS OTHERS      = 1 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_ish_static_handler.
    ENDIF.

    TRY.
        gr_structdescr ?= lr_typedescr.
      CATCH cx_sy_move_cast_error INTO DATA(lx_move_cast_error).
        RAISE EXCEPTION TYPE cx_ish_static_handler
          EXPORTING
            previous = lx_move_cast_error.
    ENDTRY.

    gs_data = is_data.

  ENDMETHOD.


  METHOD if_ish_gui_structure_model~get_field_content.

    ASSIGN COMPONENT i_fieldname OF STRUCTURE gs_data TO FIELD-SYMBOL(<lg_data>).
    IF sy-subrc <> 0 OR <lg_data> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

    c_content = <lg_data>.

  ENDMETHOD.


  METHOD if_ish_gui_structure_model~get_supported_fields.

    rt_supported_fieldname = VALUE #( FOR ls_component IN gr_structdescr->components ( ls_component-name ) ).

  ENDMETHOD.


  METHOD if_ish_gui_structure_model~is_field_supported.

    r_supported = xsdbool( line_exists( gr_structdescr->components[ name = i_fieldname ] ) ).

  ENDMETHOD.


  METHOD if_ish_gui_structure_model~set_field_content.

    ASSIGN COMPONENT i_fieldname OF STRUCTURE gs_data TO FIELD-SYMBOL(<lg_data>).
    IF sy-subrc <> 0 OR <lg_data> IS NOT ASSIGNED OR <lg_data> = i_content.
      RETURN.
    ENDIF.

    <lg_data> = i_content.

    DATA(lt_changed_field) = VALUE ish_t_fieldname( ( i_fieldname ) ).
    RAISE EVENT if_ish_gui_structure_model~ev_changed
      EXPORTING
        et_changed_field = lt_changed_field.

    r_changed = abap_true.

  ENDMETHOD.


  METHOD if_ish_gui_treenode_model~get_node_icon.

    r_node_icon = `ICON_WORKFLOW_ACTIVITY`.

  ENDMETHOD.


  METHOD if_ish_gui_treenode_model~get_node_text.

    r_node_text = gs_data-description.

  ENDMETHOD.


ENDCLASS.
