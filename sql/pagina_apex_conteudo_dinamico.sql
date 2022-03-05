-- Página APEX - Tipo = Conteúdo Dinâmico PL/SQL
-- Origem - Código PL/SQL =

DECLARE
  l_abertura   VARCHAR2(1000);
  l_fechamento VARCHAR2(100);
  l_fundo      VARCHAR2(32767);
  l_n_america  VARCHAR2(32767);
  l_s_america  VARCHAR2(32767);
  l_europa     VARCHAR2(32767);
  l_africa     VARCHAR2(32767);
  l_asia       VARCHAR2(32767);
  l_oceania    VARCHAR2(32767);
  l_setemares  VARCHAR2(32767);
  l_antartida  VARCHAR2(32767);
  l_cidades    VARCHAR2(32767);
  l_grade      VARCHAR2(32767);
  l_largura    NUMBER           := :P44_LARGURA;
  l_altura     NUMBER           := :P44_ALTURA;
  l_projecao   VARCHAR2(1)      := :P44_PROJECAO;
  l_moldura    BOOLEAN          := false;

BEGIN
  l_abertura := world_map_projections.exibir_abertura_svg ( p_largura  => l_largura
                                                          , p_altura   => l_altura
                                                          , p_projecao => l_projecao
                                                          , p_moldura => l_moldura);
                                                          
  l_fundo := world_map_projections.exibir_fundo_azul ( p_largura  => l_largura
                                                     , p_altura   => l_altura
                                                     , p_projecao => l_projecao);
                                                     
  l_n_america := world_map_projections.exibir_north_america ( p_largura  => l_largura
                                                            , p_altura   => l_altura
                                                           , p_projecao => l_projecao);
                                                           
  l_oceania := world_map_projections.exibir_oceania ( p_largura  => l_largura
                                                    , p_altura   => l_altura
                                                    , p_projecao => l_projecao);
                                                    
  l_setemares := world_map_projections.exibir_seven_seas ( p_largura  => l_largura
                                                         , p_altura   => l_altura
                                                         , p_projecao => l_projecao);
                                                         
  l_antartida := world_map_projections.exibir_antarctica ( p_largura  => l_largura
                                                         , p_altura   => l_altura
                                                         , p_projecao => l_projecao);
                                                         
  l_s_america := world_map_projections.exibir_south_america ( p_largura  => l_largura
                                                            , p_altura   => l_altura
                                                            , p_projecao => l_projecao);
                                                            
  l_europa := world_map_projections.exibir_europe ( p_largura  => l_largura
                                                  , p_altura   => l_altura
                                                  , p_projecao => l_projecao);
                                                  
  l_africa := world_map_projections.exibir_africa ( p_largura  => l_largura
                                                  , p_altura   => l_altura
                                                  , p_projecao => l_projecao);
                                                  
  l_asia := world_map_projections.exibir_asia ( p_largura  => l_largura
                                              , p_altura   => l_altura
                                              , p_projecao => l_projecao);
                                              
  l_cidades := world_map_projections.exibir_cidades ( p_largura  => l_largura
                                                    , p_altura   => l_altura
                                                    , p_projecao => l_projecao);
                                                    
  l_grade := world_map_projections.exibir_grade ( p_largura  => l_largura
                                                , p_altura   => l_altura
                                                , p_projecao => l_projecao);
                                                
  l_fechamento := world_map_projections.exibir_fechamento_svg;
  
  htp.p (l_abertura);
  htp.p (l_fundo);
  htp.p (l_n_america);
  htp.p (l_oceania);
  htp.p (l_setemares);
  htp.p (l_antartida);
  htp.p (l_s_america);
  htp.p (l_europa);
  htp.p (l_africa);
  htp.p (l_asia);
  htp.p (l_cidades);
  htp.p (l_grade);
  htp.p (l_fechamento);
  
END;
