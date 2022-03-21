*&---------------------------------------------------------------------*
*& Report ZAPP_SETTINGS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zapp_settings.

*&---------------------------------------------------------------------*
*& Class lcl_settings
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
CLASS lcl_settings DEFINITION FINAL.


  PUBLIC SECTION.
    METHODS constructor
      IMPORTING iv_uname TYPE sy-uname.

    METHODS lock
      RAISING zcx_app_exception.

    METHODS unlock.

    METHODS get_settings
      RETURNING VALUE(rs_settings) TYPE zapp_settings.

    METHODS handle_exit
      IMPORTING is_settings TYPE zapp_settings
      RAISING   zcx_app_exception.

    METHODS save
      IMPORTING is_settings TYPE zapp_settings.

    METHODS delete.

    METHODS exists_settings
      RETURNING VALUE(rv_result) TYPE abap_bool.

  PRIVATE SECTION.
    DATA mv_uname TYPE sy-uname.

ENDCLASS.

CLASS lcl_settings IMPLEMENTATION.
  METHOD constructor.
    mv_uname = iv_uname.
  ENDMETHOD.
  METHOD lock.

    CALL FUNCTION 'ENQUEUE_EZAPP_SETTINGS'
      EXPORTING
*       MODE_ZAPP_SETTINGS       = 'E'
        uname          = mv_uname
*       X_UNAME        = ' '
        _scope         = '1'
*       _WAIT          = ' '
*       _COLLECT       = ' '
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_app_exception
        MESSAGE ID sy-msgid
        TYPE sy-msgty
        NUMBER sy-msgno
        WITH
        sy-msgv1
        sy-msgv2
        sy-msgv3
        sy-msgv4.
    ENDIF.

  ENDMETHOD.

  METHOD unlock.
    CALL FUNCTION 'DEQUEUE_EZAPP_SETTINGS'
      EXPORTING
*       MODE_ZAPP_SETTINGS       = 'E'
        uname = mv_uname
*       X_UNAME                  = ' '
*       _SCOPE                   = '3'
*       _SYNCHRON                = ' '
*       _COLLECT                 = ' '
      .


  ENDMETHOD.

  METHOD get_settings.
    SELECT SINGLE * FROM zapp_settings INTO rs_settings
  WHERE uname = mv_uname.
    IF sy-subrc <> 0.
      CLEAR rs_settings.
      rs_settings-uname = mv_uname.
    ENDIF.
  ENDMETHOD.

  METHOD handle_exit.
    DATA ls_settings_old TYPE zapp_settings.
    DATA lv_answer TYPE c.
    ls_settings_old = get_settings( ).
    IF ls_settings_old <> is_settings.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar       = TEXT-110
*         DIAGNOSE_OBJECT             = ' '
          text_question  = TEXT-100
*         TEXT_BUTTON_1  = 'Ja'(001)
*         ICON_BUTTON_1  = ' '
*         TEXT_BUTTON_2  = 'Nein'(002)
*         ICON_BUTTON_2  = ' '
*         DEFAULT_BUTTON = '1'
*         DISPLAY_CANCEL_BUTTON       = 'X'
*         USERDEFINED_F1_HELP         = ' '
*         START_COLUMN   = 25
*         START_ROW      = 6
*         POPUP_TYPE     =
*         IV_QUICKINFO_BUTTON_1       = ' '
*         IV_QUICKINFO_BUTTON_2       = ' '
        IMPORTING
          answer         = lv_answer
*       TABLES
*         PARAMETER      =
        EXCEPTIONS
          text_not_found = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_app_exception
          MESSAGE ID sy-msgid
          TYPE sy-msgty
          NUMBER sy-msgno
          WITH
          sy-msgv1
          sy-msgv2
          sy-msgv3
          sy-msgv4.
      ENDIF.

      CASE lv_answer.
        WHEN '1'.
          save( is_settings ).
          LEAVE TO SCREEN 0.
        WHEN '2'.
          LEAVE TO SCREEN 0.
        WHEN OTHERS.
          RETURN.
      ENDCASE.
    ELSE.
      LEAVE TO SCREEN 0.
    ENDIF.
  ENDMETHOD.

  METHOD save.
    DATA lv_subrc TYPE sy-subrc.
    IF exists_settings( ).
      UPDATE zapp_settings FROM is_settings.
    ELSE.
      INSERT zapp_settings FROM is_settings.
    ENDIF.
    lv_subrc = sy-subrc.
    COMMIT WORK AND WAIT.
    IF lv_subrc = 0.
      MESSAGE s016(zapp_mc_pretty_print).
    ELSE.
      MESSAGE e017(zapp_mc_pretty_print).
    ENDIF.

    LEAVE TO SCREEN 0.
  ENDMETHOD.

  METHOD delete.
    DELETE FROM zapp_settings WHERE uname = mv_uname.
    COMMIT WORK AND WAIT.
    MESSAGE s018(zapp_mc_pretty_print).
    LEAVE TO SCREEN 0.
  ENDMETHOD.

  METHOD exists_settings.
    ##NEEDED DATA lv_uname TYPE sy-uname.
    SELECT SINGLE uname INTO lv_uname
    FROM zapp_settings
    WHERE uname =  mv_uname.
    IF sy-subrc = 0.
      rv_result = abap_true.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

TABLES zapp_settings.

PARAMETERS p_set_g RADIOBUTTON GROUP g1 DEFAULT 'X'.
PARAMETERS p_set_u RADIOBUTTON GROUP g1.
DATA gr_settings TYPE REF TO lcl_settings.
DATA gr_ex TYPE REF TO zcx_app_exception.
DATA gv_okcode TYPE sy-ucomm.

AT SELECTION-SCREEN.

  IF sy-ucomm = 'ONLI'.
    IF p_set_g = abap_true.
      CREATE OBJECT gr_settings
        EXPORTING
          iv_uname = '*'.
    ELSE.
      CREATE OBJECT gr_settings
        EXPORTING
          iv_uname = sy-uname.
    ENDIF.
    TRY.
        gr_settings->lock( ).
      CATCH zcx_app_exception INTO gr_ex. " Pretty Printer Exception
        MESSAGE gr_ex.
    ENDTRY.
  ENDIF.


START-OF-SELECTION.

  zapp_settings = gr_settings->get_settings( ).

  CALL SCREEN 0100.
  gr_settings->unlock( ).

  INCLUDE zapp_settings_canceli01.

  INCLUDE zapp_settings_status_0100o01.

  INCLUDE zapp_settings_user_command_i01.
