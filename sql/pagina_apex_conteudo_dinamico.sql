-- Página APEX - Tipo = Conteúdo Dinâmico PL/SQL
-- Origem - Código PL/SQL =
--
-- O procedimento exibir_mapa monta o SVG completo (fundo, continentes, capitais e grade) e o
-- envia ao buffer HTP em blocos, sem o limite de 32767 bytes do VARCHAR2.
-- Valores nulos usam os padrões 800 x 400 e projeção 'c' (equirretangular).

BEGIN
  world_map_projections.exibir_mapa ( p_largura  => :P44_LARGURA
                                    , p_altura   => :P44_ALTURA
                                    , p_projecao => :P44_PROJECAO
                                    , p_moldura  => false
                                    );
END;
