prompt --application/set_environment
set define off verify off feedback off
whenever sqlerror exit sql.sqlcode rollback
--------------------------------------------------------------------------------
--
-- ORACLE Application Express (APEX) export file
--
-- You should run the script connected to SQL*Plus as the Oracle user
-- APEX_210200 or as the owner (parsing schema) of the application.
--
-- NOTE: Calls to apex_application_install override the defaults below.
--
--------------------------------------------------------------------------------
begin
wwv_flow_api.import_begin (
 p_version_yyyy_mm_dd=>'2021.10.15'
,p_release=>'21.2.4'
,p_default_workspace_id=>38134676337898140197
,p_default_application_id=>48032
,p_default_id_offset=>0
,p_default_owner=>'WKSP_DMETI'
);
end;
/
 
prompt APPLICATION 48032 - Oracle APEX Development
--
-- Application Export:
--   Application:     48032
--   Name:            Oracle APEX Development
--   Date and Time:   13:36 Domingo Março 6, 2022
--   Exported By:     DANIEL@APEX.COM
--   Flashback:       0
--   Export Type:     Page Export
--   Manifest
--     PAGE: 44
--   Manifest End
--   Version:         21.2.4
--   Instance ID:     63113759365424
--

begin
null;
end;
/
prompt --application/pages/delete_00044
begin
wwv_flow_api.remove_page (p_flow_id=>wwv_flow.g_flow_id, p_page_id=>44);
end;
/
prompt --application/pages/page_00044
begin
wwv_flow_api.create_page(
 p_id=>44
,p_user_interface_id=>wwv_flow_api.id(38135772577540198928)
,p_name=>'pg_mapamundi'
,p_alias=>'MAPAMUNDI'
,p_step_title=>unistr('Mapa-m\00FAndi')
,p_autocomplete_on_off=>'OFF'
,p_page_template_options=>'#DEFAULT#'
,p_required_role=>wwv_flow_api.id(38137761775886382934)
,p_protection_level=>'C'
,p_last_updated_by=>'DANIEL@APEX.COM'
,p_last_upd_yyyymmddhh24miss=>'20220306133031'
);
wwv_flow_api.create_page_plug(
 p_id=>wwv_flow_api.id(30363532857664073801)
,p_plug_name=>unistr('Mapa-m\00FAndi')
,p_region_template_options=>'#DEFAULT#:t-Region--noPadding:t-Region--noBorder:t-Region--scrollBody'
,p_component_template_options=>'#DEFAULT#'
,p_plug_template=>wwv_flow_api.id(38135382911602198865)
,p_plug_display_sequence=>20
,p_include_in_reg_disp_sel_yn=>'Y'
,p_plug_source=>wwv_flow_string.join(wwv_flow_t_varchar2(
'DECLARE',
'  l_abertura   VARCHAR2(1000);',
'  l_fechamento VARCHAR2(100);',
'  l_fundo      VARCHAR2(32767);',
'  l_n_america  VARCHAR2(32767);',
'  l_s_america  VARCHAR2(32767);',
'  l_europa     VARCHAR2(32767);',
'  l_africa     VARCHAR2(32767);',
'  l_asia       VARCHAR2(32767);',
'  l_oceania    VARCHAR2(32767);',
'  l_setemares  VARCHAR2(32767);',
'  l_antartida  VARCHAR2(32767);',
'  l_cidades    VARCHAR2(32767);',
'  l_grade      VARCHAR2(32767);',
'  l_largura    NUMBER           := :P44_LARGURA;',
'  l_altura     NUMBER           := :P44_ALTURA;',
'  l_projecao   VARCHAR2(1)      := :P44_PROJECAO;',
'  l_moldura    BOOLEAN          := false;',
'',
'BEGIN',
'  l_abertura := world_map_projections.exibir_abertura_svg ( p_largura  => l_largura',
'                                                          , p_altura   => l_altura',
'                                                          , p_projecao => l_projecao',
'                                                          , p_moldura => l_moldura',
'                                                          );',
'                                                          ',
'  l_fundo := world_map_projections.exibir_fundo_azul ( p_largura  => l_largura',
'                                                     , p_altura   => l_altura',
'                                                     , p_projecao => l_projecao',
'                                                     );',
'                                                     ',
'  l_n_america := world_map_projections.exibir_continente ( p_continente => ''North America''',
'                                                         , p_largura    => l_largura',
'                                                         , p_altura     => l_altura',
'                                                         , p_projecao   => l_projecao',
'                                                         );',
'                                                           ',
'  l_oceania := world_map_projections.exibir_continente ( p_continente => ''Oceania''',
'                                                       , p_largura    => l_largura',
'                                                       , p_altura     => l_altura',
'                                                       , p_projecao   => l_projecao',
'                                                       );',
'                                                    ',
'  l_setemares := world_map_projections.exibir_continente ( p_continente => ''Seven seas (open ocean)''',
'                                                         , p_largura    => l_largura',
'                                                         , p_altura     => l_altura',
'                                                         , p_projecao   => l_projecao',
'                                                         );',
'                                                         ',
'  l_antartida := world_map_projections.exibir_continente ( p_continente => ''Antarctica''',
'                                                         , p_largura    => l_largura',
'                                                         , p_altura     => l_altura',
'                                                         , p_projecao   => l_projecao',
'                                                         );',
'                                                         ',
'  l_s_america := world_map_projections.exibir_continente ( p_continente => ''South America''',
'                                                         , p_largura    => l_largura',
'                                                         , p_altura     => l_altura',
'                                                         , p_projecao   => l_projecao',
'                                                         );',
'                                                            ',
'  l_europa := world_map_projections.exibir_continente ( p_continente => ''Europe''',
'                                                      , p_largura    => l_largura',
'                                                      , p_altura     => l_altura',
'                                                      , p_projecao   => l_projecao',
'                                                      );',
'                                                  ',
'  l_africa := world_map_projections.exibir_continente ( p_continente => ''Africa''',
'                                                      , p_largura    => l_largura',
'                                                      , p_altura     => l_altura',
'                                                      , p_projecao   => l_projecao',
'                                                      );',
'                                                  ',
'  l_asia := world_map_projections.exibir_continente ( p_continente => ''Asia''',
'                                                    , p_largura    => l_largura',
'                                                    , p_altura     => l_altura',
'                                                    , p_projecao   => l_projecao',
'                                                    );',
'                                              ',
'  l_cidades := world_map_projections.exibir_cidades ( p_largura  => l_largura',
'                                                    , p_altura   => l_altura',
'                                                    , p_projecao => l_projecao',
'                                                    );',
'                                                    ',
'  l_grade := world_map_projections.exibir_grade ( p_largura  => l_largura',
'                                                , p_altura   => l_altura',
'                                                , p_projecao => l_projecao',
'                                                );',
'                                                ',
'  l_fechamento := world_map_projections.exibir_fechamento_svg;',
'  ',
'  htp.p (l_abertura);',
'  htp.p (l_fundo);',
'  htp.p (l_n_america);',
'  htp.p (l_oceania);',
'  htp.p (l_setemares);',
'  htp.p (l_antartida);',
'  htp.p (l_s_america);',
'  htp.p (l_europa);',
'  htp.p (l_africa);',
'  htp.p (l_asia);',
'  htp.p (l_cidades);',
'  htp.p (l_grade);',
'  htp.p (l_fechamento);',
'  ',
'END;'))
,p_plug_source_type=>'NATIVE_PLSQL'
,p_plug_query_options=>'DERIVED_REPORT_COLUMNS'
);
wwv_flow_api.create_page_plug(
 p_id=>wwv_flow_api.id(30635190181611372801)
,p_plug_name=>unistr('Par\00E2metros')
,p_region_template_options=>'#DEFAULT#:t-Region--noPadding:t-Region--noBorder:t-Region--scrollBody'
,p_plug_template=>wwv_flow_api.id(38135382911602198865)
,p_plug_display_sequence=>10
,p_include_in_reg_disp_sel_yn=>'Y'
,p_query_type=>'SQL'
,p_plug_source=>wwv_flow_string.join(wwv_flow_t_varchar2(
'select null AS id',
'     , null AS largura',
'     , null AS altura',
'     , null AS projecao',
'  FROM dual;'))
,p_is_editable=>false
,p_plug_source_type=>'NATIVE_FORM'
,p_plug_query_options=>'DERIVED_REPORT_COLUMNS'
);
wwv_flow_api.create_page_button(
 p_id=>wwv_flow_api.id(30635192090976372820)
,p_button_sequence=>50
,p_button_plug_id=>wwv_flow_api.id(30635190181611372801)
,p_button_name=>'Alterar'
,p_button_action=>'SUBMIT'
,p_button_template_options=>'#DEFAULT#:t-Button--large:t-Button--gapTop'
,p_button_template_id=>wwv_flow_api.id(38135748027198198903)
,p_button_is_hot=>'Y'
,p_button_image_alt=>'Alterar'
,p_grid_new_row=>'N'
,p_grid_new_column=>'Y'
,p_grid_column_span=>2
);
wwv_flow_api.create_page_branch(
 p_id=>wwv_flow_api.id(30635192208587372822)
,p_branch_name=>'Go to page'
,p_branch_action=>'f?p=&APP_ID.:44:&SESSION.::&DEBUG.::P44_LARGURA,P44_ALTURA,P44_PROJECAO:&P44_LARGURA.,&P44_ALTURA.,''&P44_PROJECAO.''&success_msg=#SUCCESS_MSG#'
,p_branch_point=>'AFTER_PROCESSING'
,p_branch_type=>'REDIRECT_URL'
,p_branch_sequence=>10
);
wwv_flow_api.create_page_item(
 p_id=>wwv_flow_api.id(30635190826609372808)
,p_name=>'P44_ID'
,p_source_data_type=>'VARCHAR2'
,p_is_primary_key=>true
,p_item_sequence=>10
,p_item_plug_id=>wwv_flow_api.id(30635190181611372801)
,p_item_source_plug_id=>wwv_flow_api.id(30635190181611372801)
,p_source=>'ID'
,p_source_type=>'REGION_SOURCE_COLUMN'
,p_display_as=>'NATIVE_HIDDEN'
,p_is_persistent=>'N'
,p_protection_level=>'S'
,p_attribute_01=>'Y'
);
wwv_flow_api.create_page_item(
 p_id=>wwv_flow_api.id(30635190907857372809)
,p_name=>'P44_LARGURA'
,p_source_data_type=>'VARCHAR2'
,p_item_sequence=>20
,p_item_plug_id=>wwv_flow_api.id(30635190181611372801)
,p_item_source_plug_id=>wwv_flow_api.id(30635190181611372801)
,p_item_default=>wwv_flow_string.join(wwv_flow_t_varchar2(
'DECLARE',
'  l_largura NUMBER := &P44_LARGURA.;',
'BEGIN',
'  IF l_largura IS NULL THEN',
'    RETURN 800;',
'  ELSE',
'    RETURN l_largura;',
'  END IF;',
'END;'))
,p_item_default_type=>'FUNCTION_BODY'
,p_item_default_language=>'PLSQL'
,p_prompt=>'Largura'
,p_source=>'LARGURA'
,p_source_type=>'REGION_SOURCE_COLUMN'
,p_display_as=>'NATIVE_TEXT_FIELD'
,p_cSize=>30
,p_colspan=>2
,p_field_template=>wwv_flow_api.id(38135745552562198901)
,p_item_template_options=>'#DEFAULT#'
,p_is_persistent=>'N'
,p_attribute_01=>'N'
,p_attribute_02=>'N'
,p_attribute_04=>'TEXT'
,p_attribute_05=>'BOTH'
);
wwv_flow_api.create_page_item(
 p_id=>wwv_flow_api.id(30635191026576372810)
,p_name=>'P44_ALTURA'
,p_source_data_type=>'VARCHAR2'
,p_item_sequence=>30
,p_item_plug_id=>wwv_flow_api.id(30635190181611372801)
,p_item_source_plug_id=>wwv_flow_api.id(30635190181611372801)
,p_item_default=>wwv_flow_string.join(wwv_flow_t_varchar2(
'DECLARE',
'  l_altura NUMBER := &P44_ALTURA.;',
'BEGIN',
'  IF l_altura IS NULL THEN',
'    RETURN 400;',
'  ELSE',
'    RETURN l_altura;',
'  END IF;',
'END;'))
,p_item_default_type=>'FUNCTION_BODY'
,p_item_default_language=>'PLSQL'
,p_prompt=>'Altura'
,p_source=>'ALTURA'
,p_source_type=>'REGION_SOURCE_COLUMN'
,p_display_as=>'NATIVE_TEXT_FIELD'
,p_cSize=>30
,p_begin_on_new_line=>'N'
,p_colspan=>2
,p_field_template=>wwv_flow_api.id(38135745552562198901)
,p_item_template_options=>'#DEFAULT#'
,p_is_persistent=>'N'
,p_attribute_01=>'N'
,p_attribute_02=>'N'
,p_attribute_04=>'TEXT'
,p_attribute_05=>'BOTH'
);
wwv_flow_api.create_page_item(
 p_id=>wwv_flow_api.id(30635191129415372811)
,p_name=>'P44_PROJECAO'
,p_source_data_type=>'VARCHAR2'
,p_item_sequence=>40
,p_item_plug_id=>wwv_flow_api.id(30635190181611372801)
,p_item_source_plug_id=>wwv_flow_api.id(30635190181611372801)
,p_item_default=>wwv_flow_string.join(wwv_flow_t_varchar2(
'DECLARE',
'  l_projecao VARCHAR(1) := &P44_PROJECAO.;',
'BEGIN',
'  IF l_projecao IS NULL THEN',
'    RETURN ''c'';',
'  ELSE',
'    RETURN l_projecao;',
'  END IF;',
'END;'))
,p_item_default_type=>'FUNCTION_BODY'
,p_item_default_language=>'PLSQL'
,p_prompt=>unistr('Proje\00E7\00E3o')
,p_source=>'PROJECAO'
,p_source_type=>'REGION_SOURCE_COLUMN'
,p_display_as=>'NATIVE_SELECT_LIST'
,p_named_lov=>'LOV_PROJECOES'
,p_lov=>'.'||wwv_flow_api.id(30638358904324468141)||'.'
,p_lov_display_null=>'YES'
,p_cHeight=>1
,p_begin_on_new_line=>'N'
,p_colspan=>4
,p_field_template=>wwv_flow_api.id(38135745552562198901)
,p_item_template_options=>'#DEFAULT#'
,p_is_persistent=>'N'
,p_lov_display_extra=>'NO'
,p_attribute_01=>'NONE'
,p_attribute_02=>'N'
);
wwv_flow_api.create_page_process(
 p_id=>wwv_flow_api.id(30635192136582372821)
,p_process_sequence=>10
,p_process_point=>'AFTER_SUBMIT'
,p_region_id=>wwv_flow_api.id(30635190181611372801)
,p_process_type=>'NATIVE_FORM_DML'
,p_process_name=>'Novo'
,p_attribute_01=>'REGION_SOURCE'
,p_attribute_05=>'Y'
,p_attribute_06=>'Y'
,p_attribute_08=>'Y'
,p_error_display_location=>'INLINE_IN_NOTIFICATION'
);
wwv_flow_api.create_page_process(
 p_id=>wwv_flow_api.id(30635190739946372807)
,p_process_sequence=>10
,p_process_point=>'BEFORE_HEADER'
,p_region_id=>wwv_flow_api.id(30635190181611372801)
,p_process_type=>'NATIVE_FORM_INIT'
,p_process_name=>'Inicializar o form pg_mapamundi'
,p_error_display_location=>'INLINE_IN_NOTIFICATION'
);
end;
/
prompt --application/end_environment
begin
wwv_flow_api.import_end(p_auto_install_sup_obj => nvl(wwv_flow_application_install.get_auto_install_sup_obj, false));
commit;
end;
/
set verify on feedback on define on
prompt  ...done
