CLASS zcl_medapi_tree_model DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_ish_gui_table_model.

    METHODS constructor
      RAISING
        cx_ish_static_handler.

  PROTECTED SECTION.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_namespace_model,
        namespace TYPE namespace,
        model     TYPE REF TO if_ish_gui_table_model,
      END OF ty_namespace_model,
      ty_namespace_models TYPE SORTED TABLE OF ty_namespace_model
        WITH UNIQUE KEY namespace.

    DATA:
      mt_models TYPE ty_namespace_models.

    METHODS load_apis
      RAISING
        cx_ish_static_handler.

    METHODS add_class
      IMPORTING
        iv_class_name TYPE seoclsname
      RAISING
        cx_ish_static_handler.

    METHODS build_namespace_model
      IMPORTING
        iv_namespace     TYPE namespace
        iv_class_name    TYPE seoclsname OPTIONAL
      RETURNING
        VALUE(ri_result) TYPE REF TO if_ish_gui_table_model.

    METHODS on_entry_added
        FOR EVENT ev_entry_added OF if_ish_gui_table_model
      IMPORTING
        er_entry
        sender.

    METHODS on_entry_removed
        FOR EVENT ev_entry_removed OF if_ish_gui_table_model
      IMPORTING
        er_entry
        sender.

ENDCLASS.



CLASS zcl_medapi_tree_model IMPLEMENTATION.


  METHOD constructor.

    load_apis( ).

  ENDMETHOD.


  METHOD if_ish_gui_table_model~add_entry.

    CLEAR r_added.

  ENDMETHOD.


  METHOD if_ish_gui_table_model~get_entries.

    rt_entry = VALUE #( FOR ls_model IN mt_models ( ls_model-model ) ).

  ENDMETHOD.


  METHOD if_ish_gui_table_model~remove_entry.

    CLEAR r_removed.

  ENDMETHOD.


  METHOD load_apis.

    TRY.
        DATA(lo_interface) = CAST cl_oo_interface( cl_oo_interface=>get_instance( `IF_ISHMED_API` ) ).
      CATCH cx_class_not_existent.
        RETURN.
    ENDTRY.

    LOOP AT lo_interface->get_implementing_classes( ) INTO DATA(ls_class).
      add_class( ls_class-clsname ).
    ENDLOOP.

  ENDMETHOD.


  METHOD add_class.

    DATA:
      lv_namespace TYPE namespace.

    CALL FUNCTION 'TRINT_GET_NAMESPACE'
      EXPORTING
        iv_pgmid            = 'R3TR'
        iv_object           = 'CLAS'
        iv_obj_name         = CONV sobj_name( iv_class_name )
      IMPORTING
        ev_namespace        = lv_namespace
      EXCEPTIONS
        invalid_prefix      = 1
        invalid_object_type = 2
        OTHERS              = 3.
    IF sy-subrc <> 0.
      CLEAR lv_namespace.
    ENDIF.

    TRY.
        ASSIGN mt_models[ namespace = lv_namespace ] TO FIELD-SYMBOL(<ls_model>).
      CATCH cx_sy_itab_line_not_found.
        IF <ls_model> IS ASSIGNED.
          UNASSIGN <ls_model>.
        ENDIF.
    ENDTRY.

    IF <ls_model> IS NOT ASSIGNED.
      INSERT VALUE #( namespace = lv_namespace
                      model     = build_namespace_model( iv_namespace = lv_namespace iv_class_name = iv_class_name ) )
          INTO TABLE mt_models ASSIGNING <ls_model>.
    ENDIF.

    DATA(ls_api_list) =
        VALUE zmedapi_s_list(
            LET li_api = cl_ishmed_api_factory=>get_instance( iv_class_name ) IN
            class_name   = iv_class_name
            description  = li_api->if_ishmed_api_documentation~get_description( )
            api_type     = li_api->get_type( )
            api_version  = li_api->get_version( )
            is_singleton = li_api->if_ishmed_api_descriptor~is_singleton( )
            api          = li_api ).

    <ls_model>-model->add_entry( NEW zcl_medapi_model( ls_api_list ) ).

  ENDMETHOD.


  METHOD build_namespace_model.

    INCLUDE rddkorri.

    CASE iv_namespace.
      WHEN gc_gns_cust OR space.
        DATA(lv_node_text) = CONV lvc_value( 'Customer'(002) ).

      WHEN gc_gns_sap_all.
        lv_node_text = COND #( WHEN iv_class_name CS `ISHMED` THEN |i.s.h.med ({ 'By'(001) } Cerner)|
                                                              ELSE |IS-H ({ 'By'(001) } SAP)| ).

      WHEN OTHERS.

        SELECT SINGLE descriptn,owner
          FROM trnspacett
          WHERE namespace = @iv_namespace
            AND spras = @sy-langu
            INTO @DATA(ls_text).
        IF sy-subrc <> 0.
          CLEAR ls_text.
        ENDIF.

        lv_node_text =
            COND #(
                LET l_description = COND #( WHEN ls_text-descriptn IS NOT INITIAL THEN ls_text-descriptn
                                                                                  ELSE iv_namespace ) IN
                WHEN ls_text-owner IS INITIAL AND iv_namespace <> gc_gns_cust
                    THEN l_description
                    ELSE |{ l_description } ({ 'By'(001) } { ls_text-owner })| ).

    ENDCASE.

    DATA(lv_node_icon) = SWITCH tv_image( iv_namespace WHEN gc_gns_sap_all THEN `ICON_SAP`
                                                       WHEN gc_gns_cust
                                                         OR space          THEN `ICON_CUSTOMER`
                                                                          ELSE `ICON_PARTNER` ).

    ri_result = cl_ish_gm_table_simple=>create( i_node_text = lv_node_text i_node_icon = lv_node_icon ).

    SET HANDLER on_entry_added FOR ri_result ACTIVATION abap_true.
    SET HANDLER on_entry_removed FOR ri_result ACTIVATION abap_true.

  ENDMETHOD.


  METHOD on_entry_added.

    RAISE EVENT if_ish_gui_table_model~ev_entry_added
      EXPORTING
        er_entry = er_entry.

  ENDMETHOD.


  METHOD on_entry_removed.

    RAISE EVENT if_ish_gui_table_model~ev_entry_removed
      EXPORTING
        er_entry = er_entry.

  ENDMETHOD.


ENDCLASS.
