CLASS zcl_medapi_gv_html_document DEFINITION
  PUBLIC
  INHERITING FROM cl_ish_gui_control_view
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS create_and_init_by_contview
      IMPORTING iv_element_name    TYPE n1gui_element_name
                ii_cb_destroyable  TYPE REF TO if_ish_cb_destroyable     OPTIONAL
                iv_document_object TYPE doku_obj                         OPTIONAL
                iv_document_class  TYPE doku_id                          OPTIONAL
                io_layout          TYPE REF TO cl_ish_gui_control_layout OPTIONAL
                iv_processing_mode TYPE ish_vcode                        DEFAULT if_ish_gui_view=>co_vcode_display
                iv_ctrname         TYPE n1gui_element_name
                ii_parent_view     TYPE REF TO if_ish_gui_container_view
      RETURNING VALUE(ro_result)   TYPE REF TO zcl_medapi_gv_html_document
      RAISING   cx_ish_static_handler.

    CLASS-METHODS create_and_init_by_dynpview
      IMPORTING iv_element_name    TYPE n1gui_element_name
                ii_cb_destroyable  TYPE REF TO if_ish_cb_destroyable     OPTIONAL
                iv_document_object TYPE doku_obj                         OPTIONAL
                iv_document_class  TYPE doku_id                          OPTIONAL
                io_layout          TYPE REF TO cl_ish_gui_control_layout OPTIONAL
                iv_processing_mode TYPE ish_vcode                        DEFAULT if_ish_gui_view=>co_vcode_display
                iv_ctrname         TYPE n1gui_element_name
                ii_parent_view     TYPE REF TO if_ish_gui_dynpro_view
                iv_sdy_ctrname     TYPE n1gui_element_name
                iv_sdy_viewname    TYPE n1gui_element_name
      RETURNING VALUE(ro_result)   TYPE REF TO zcl_medapi_gv_html_document
      RAISING   cx_ish_static_handler.

    METHODS set_document
      IMPORTING iv_document_object TYPE doku_obj
                iv_document_class  TYPE doku_id DEFAULT `TX`
      RAISING   cx_ish_static_handler.

  PROTECTED SECTION.
    METHODS initialize
      IMPORTING iv_document_object TYPE doku_obj                         OPTIONAL
                iv_document_class  TYPE doku_id                          OPTIONAL
                ii_controller      TYPE REF TO if_ish_gui_controller
                ii_parent_view     TYPE REF TO if_ish_gui_container_view
                io_layout          TYPE REF TO cl_ish_gui_control_layout OPTIONAL
                iv_processing_mode TYPE ish_vcode                        DEFAULT if_ish_gui_view=>co_vcode_display
      RAISING   cx_ish_static_handler.

    METHODS load_document
      IMPORTING iv_document_object TYPE doku_obj DEFAULT ``
                iv_document_class  TYPE doku_id  DEFAULT `TX`
      RAISING   cx_ish_static_handler.

    METHODS _create_control  REDEFINITION.
    METHODS _first_display   REDEFINITION.
    METHODS _refresh_display REDEFINITION.

  PRIVATE SECTION.
    DATA mv_document_object TYPE doku_obj.
    DATA mv_document_class  TYPE doku_id.
    DATA mo_html_viewer     TYPE REF TO cl_epss_html_viewer.

ENDCLASS.



CLASS zcl_medapi_gv_html_document IMPLEMENTATION.


  METHOD create_and_init_by_contview.

    DATA(lo_controller) = cl_ish_gc_simple=>create( i_element_name    = iv_ctrname
                                                    ir_cb_destroyable = ii_cb_destroyable ).

    ro_result = NEW #( i_element_name    = iv_element_name
                       ir_cb_destroyable = ii_cb_destroyable ).

    lo_controller->initialize(
        ir_parent_controller = COND #( WHEN ii_parent_view IS BOUND THEN ii_parent_view->get_controller( ) )
        ir_view              = ro_result
        i_vcode              = iv_processing_mode ).

    ro_result->initialize( ii_controller      = lo_controller
                           io_layout          = io_layout
                           iv_document_object = iv_document_object
                           iv_document_class  = iv_document_class
                           iv_processing_mode = iv_processing_mode
                           ii_parent_view     = ii_parent_view ).

  ENDMETHOD.


  METHOD create_and_init_by_dynpview.

    DATA(lr_sdy_custcont_ctr) =
        cl_ish_gc_sdy_custcont=>create_and_initialize(
            i_element_name          = iv_sdy_ctrname
            ir_cb_destroyable       = ii_cb_destroyable
            ir_parent_controller    = COND #( WHEN ii_parent_view IS BOUND THEN ii_parent_view->get_controller( ) )
            i_vcode                 = iv_processing_mode
            i_viewname_sdy_custcont = iv_sdy_viewname ).

    ro_result = create_and_init_by_contview( iv_element_name    = iv_element_name
                                             iv_document_object = iv_document_object
                                             iv_document_class  = iv_document_class
                                             ii_cb_destroyable  = ii_cb_destroyable
                                             io_layout          = io_layout
                                             iv_processing_mode = iv_processing_mode
                                             iv_ctrname         = iv_ctrname
                                             ii_parent_view     = lr_sdy_custcont_ctr->get_custcont_view( ) ).

  ENDMETHOD.


  METHOD set_document.

    load_document( iv_document_object = iv_document_object
                   iv_document_class  = iv_document_class ).

    mv_document_object = iv_document_object.
    mv_document_class  = iv_document_class.

  ENDMETHOD.


  METHOD load_document.

    DATA ls_head            TYPE thead.
    DATA lt_itflines        TYPE tlinetab.
    DATA lt_conv_parformats TYPE tlinetab.
    DATA lt_html            TYPE htmltable.
    DATA lv_url             TYPE c LENGTH 5000.

    TRY.
        DATA(lo_html_viewer) = CAST cl_gui_html_viewer( _create_control_on_demand( ) ).
      CATCH cx_sy_move_cast_error.
        CLEAR lo_html_viewer.
    ENDTRY.
    IF lo_html_viewer IS NOT BOUND.
      RAISE EXCEPTION TYPE zcx_medapi
        EXPORTING
          is_textid = zcx_medapi=>msg_method_error
          iv_text1  = '1'
          iv_text2  = 'DISPLAY_DOCUMENT'
          iv_text3  = 'ZCL_MEDAPI_GV_HTML_DOCUMENT'.
    ENDIF.

    lo_html_viewer->stop( EXCEPTIONS cntl_error = 1
                                     OTHERS     = 2 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_medapi
        EXPORTING
          is_textid = zcx_medapi=>get_system_textid( )
          iv_text1  = CONV #( sy-msgv1 )
          iv_text2  = CONV #( sy-msgv2 )
          iv_text3  = CONV #( sy-msgv3 )
          iv_text4  = CONV #( sy-msgv4 ).
    ENDIF.

    IF iv_document_class IS NOT INITIAL AND iv_document_object IS NOT INITIAL.
      DATA(lv_document_object) = iv_document_object.
      DATA(lv_document_class)  = iv_document_class.
    ELSE.
      lv_document_object = mv_document_object.
      lv_document_class  = mv_document_class.
    ENDIF.

    CALL FUNCTION 'DOCU_GET'
      EXPORTING
        id                = lv_document_class
        langu             = sy-langu
        object            = lv_document_object
      IMPORTING
        head              = ls_head
      TABLES
        line              = lt_itflines
      EXCEPTIONS
        no_docu_on_screen = 1
        no_docu_self_def  = 2
        no_docu_temp      = 3
        ret_code          = 4
        OTHERS            = 5.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_medapi
        EXPORTING
          is_textid = zcx_medapi=>get_system_textid( )
          iv_text1  = CONV #( sy-msgv1 )
          iv_text2  = CONV #( sy-msgv2 )
          iv_text3  = CONV #( sy-msgv3 )
          iv_text4  = CONV #( sy-msgv4 ).
    ENDIF.

    INSERT VALUE #( tdformat = 'K6'
                    tdline   = '<TR CLASS="tableheader"><TH>' )
           INTO TABLE lt_conv_parformats.

    CALL FUNCTION 'SE_CONVERT_ITF_TO_HTML'
      EXPORTING
        is_header          = ls_head
        i_funcname         = 'EPSS_CONVERT_ITF_TO_HTML_LINK'
        i_title            = ls_head-tdname
      TABLES
        it_itf_text        = lt_itflines
        it_html_text       = lt_html
        it_conv_parformats = lt_conv_parformats
      EXCEPTIONS
        syntax_check       = 1
        replace            = 2
        illegal_header     = 3
        document_not_found = 4.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_medapi
        EXPORTING
          is_textid = zcx_medapi=>get_system_textid( )
          iv_text1  = CONV #( sy-msgv1 )
          iv_text2  = CONV #( sy-msgv2 )
          iv_text3  = CONV #( sy-msgv3 )
          iv_text4  = CONV #( sy-msgv4 ).
    ENDIF.

    lo_html_viewer->load_data( EXPORTING  encoding             = 'UTF-8'
                               IMPORTING  assigned_url         = lv_url
                               CHANGING   data_table           = lt_html
                               EXCEPTIONS dp_invalid_parameter = 1
                                          dp_error_general     = 2
                                          cntl_error           = 3
                                          OTHERS               = 4 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_medapi
        EXPORTING
          is_textid = zcx_medapi=>get_system_textid( )
          iv_text1  = CONV #( sy-msgv1 )
          iv_text2  = CONV #( sy-msgv2 )
          iv_text3  = CONV #( sy-msgv3 )
          iv_text4  = CONV #( sy-msgv4 ).
    ENDIF.

    lo_html_viewer->show_data( EXPORTING  url                    = lv_url
                               EXCEPTIONS cntl_error             = 1
                                          cnht_error_not_allowed = 2
                                          cnht_error_parameter   = 3
                                          dp_error_general       = 4
                                          OTHERS                 = 5 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_medapi
        EXPORTING
          is_textid = zcx_medapi=>get_system_textid( )
          iv_text1  = CONV #( sy-msgv1 )
          iv_text2  = CONV #( sy-msgv2 )
          iv_text3  = CONV #( sy-msgv3 )
          iv_text4  = CONV #( sy-msgv4 ).
    ENDIF.

  ENDMETHOD.


  METHOD initialize.

    IF is_initialized( ) OR is_in_initialization_mode( ).
      RAISE EXCEPTION TYPE zcx_medapi
        EXPORTING
          is_textid = zcx_medapi=>msg_method_error
          iv_text1  = '1'
          iv_text2  = 'INITIALIZE'
          iv_text3  = 'ZCL_MEDAPI_GV_HTML_DOCUMENT'.
    ENDIF.

    IF ii_controller IS NOT BOUND.
      RAISE EXCEPTION TYPE zcx_medapi
        EXPORTING
          is_textid = zcx_medapi=>msg_method_error
          iv_text1  = '2'
          iv_text2  = 'INITIALIZE'
          iv_text3  = 'ZCL_MEDAPI_GV_HTML_DOCUMENT'.
    ENDIF.

    IF iv_document_class IS INITIAL OR iv_document_object IS INITIAL.
      mv_document_object = `ZMEDAPI_BROWSER`.
      mv_document_class  = `TX`.
    ELSE.
      mv_document_object = iv_document_object.
      mv_document_class  = iv_document_class.
    ENDIF.

    g_initialization_mode = abap_true.

    TRY.
        _init_control_view( ir_controller  = ii_controller
                            ir_parent_view = ii_parent_view
                            ir_layout      = io_layout
                            i_vcode        = iv_processing_mode ).
      CLEANUP.
        g_initialization_mode = abap_false.
    ENDTRY.

    g_initialization_mode = abap_false.
    g_initialized         = abap_true.

  ENDMETHOD.


  METHOD _create_control.

    TRY.
        DATA(li_parent_container_view) = CAST if_ish_gui_container_view( get_parent_view( ) ).
        DATA(lo_parent_container) = li_parent_container_view->get_container_for_child_view( me ).
      CATCH cx_sy_move_cast_error.
        CLEAR lo_parent_container.
    ENDTRY.

    IF lo_parent_container IS NOT BOUND.
      RAISE EXCEPTION TYPE zcx_medapi
        EXPORTING
          is_textid = zcx_medapi=>msg_method_error
          iv_text1  = '1'
          iv_text2  = '_CREATE_CONTROL'
          iv_text3  = 'ZCL_MEDAPI_GV_HTML_DOCUMENT'.
    ENDIF.

    mo_html_viewer = NEW #( ).

    mo_html_viewer->start_application( lo_parent_container ).

    rr_control = mo_html_viewer->epss_html_viewer.

  ENDMETHOD.


  METHOD _first_display.

    load_document( ).

  ENDMETHOD.


  METHOD _refresh_display.

    load_document( ).

  ENDMETHOD.


ENDCLASS.
