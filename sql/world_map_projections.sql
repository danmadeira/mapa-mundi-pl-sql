CREATE OR REPLACE PACKAGE world_map_projections
-- =================================================================================================
-- Author:      Daniel Madeira
-- Create date: 15/01/2022
-- Description: Um pacote para construir uma imagem em SVG do mapa da superfície total da Terra,
--              baseando-se em alguns sistemas de projeções.
--
-- Change History:
--   15/01/22 Daniel Madeira: Versão inicial
--   06/03/22 Daniel Madeira: Função unificada para os continentes
--   06/09/26 Daniel Madeira: Projeção Equal Earth
--   29/10/26 Daniel Madeira: Retornos em CLOB (evita ORA-06502 em imagens grandes),
--                            procedimentos imprimir_clob e exibir_mapa
--   02/11/26 Daniel Madeira: Alinhamento matemático com a versão em C (constante PI, proteções
--                            numéricas, tratamento de antípodas, limites de escala e round)
--   
-- =================================================================================================
AS
  FUNCTION exibir_abertura_svg (
    p_largura  IN NUMBER   DEFAULT 800
  , p_altura   IN NUMBER   DEFAULT 400
  , p_projecao IN VARCHAR2 DEFAULT 'c'
  , p_moldura  IN BOOLEAN  DEFAULT false
  )
  RETURN VARCHAR2;
  
  FUNCTION exibir_fechamento_svg
  RETURN VARCHAR2;
  
  FUNCTION exibir_fundo_azul (
    p_largura  IN NUMBER   DEFAULT 800
  , p_altura   IN NUMBER   DEFAULT 400
  , p_projecao IN VARCHAR2 DEFAULT 'c'
  )
  RETURN CLOB;
  
  FUNCTION exibir_continente (
    p_continente IN VARCHAR2
  , p_largura    IN NUMBER   DEFAULT 800
  , p_altura     IN NUMBER   DEFAULT 400
  , p_projecao   IN VARCHAR2 DEFAULT 'c'
  )
  RETURN CLOB;
  
  FUNCTION exibir_cidades (
    p_largura  IN NUMBER   DEFAULT 800
  , p_altura   IN NUMBER   DEFAULT 400
  , p_projecao IN VARCHAR2 DEFAULT 'c'
  , p_raio     IN NUMBER   DEFAULT NULL
  )
  RETURN CLOB;
  
  FUNCTION exibir_grade (
    p_largura  IN NUMBER   DEFAULT 800
  , p_altura   IN NUMBER   DEFAULT 400
  , p_projecao IN VARCHAR2 DEFAULT 'c'
  )
  RETURN CLOB;
  
  PROCEDURE imprimir_clob (
    p_clob IN CLOB
  );
  
  PROCEDURE exibir_mapa (
    p_largura  IN NUMBER   DEFAULT 800
  , p_altura   IN NUMBER   DEFAULT 400
  , p_projecao IN VARCHAR2 DEFAULT 'c'
  , p_moldura  IN BOOLEAN  DEFAULT false
  , p_raio     IN NUMBER   DEFAULT NULL
  );
  
END world_map_projections;
/


CREATE OR REPLACE PACKAGE BODY world_map_projections
AS

  c_pi CONSTANT NUMBER := 3.14159265358979323846;

  g_ultima_projecao VARCHAR2(1) := NULL;
  g_cache_x_max     NUMBER      := 0;
  g_cache_y_max     NUMBER      := 0;

  TYPE coordenadas_xy_t IS TABLE OF NUMBER INDEX BY VARCHAR2(1);

  -- Acumulador de texto SVG: bufferiza em VARCHAR2 e descarrega no CLOB quando o buffer enche,
  -- evitando o limite de 32767 bytes do VARCHAR2 e o custo de concatenar CLOB a cada trecho.
  TYPE svg_t IS RECORD (
    conteudo CLOB
  , buffer   VARCHAR2(32767 BYTE)
  );

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 29/10/2026
  -- Description: Inicializa o acumulador de texto SVG com um CLOB temporário.
  --
  -- Parameters:
  --   @p_svg = Acumulador a ser inicializado
  --
  -- Change History:
  --   29/10/26 Daniel Madeira: Versão inicial
  --
  -- =================================================================================================
  PROCEDURE svg_iniciar (
    p_svg IN OUT NOCOPY svg_t
  )
  IS
  BEGIN
    dbms_lob.createtemporary (p_svg.conteudo, true, dbms_lob.call);
    p_svg.buffer := NULL;

  END svg_iniciar;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 29/10/2026
  -- Description: Descarrega o buffer no CLOB.
  --
  -- Parameters:
  --   @p_svg = Acumulador
  --
  -- Change History:
  --   29/10/26 Daniel Madeira: Versão inicial
  --
  -- =================================================================================================
  PROCEDURE svg_descarregar (
    p_svg IN OUT NOCOPY svg_t
  )
  IS
  BEGIN
    IF p_svg.buffer IS NOT NULL THEN
      dbms_lob.writeappend (p_svg.conteudo, length(p_svg.buffer), p_svg.buffer);
      p_svg.buffer := NULL;
    END IF;

  END svg_descarregar;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 29/10/2026
  -- Description: Anexa um trecho de texto ao acumulador SVG.
  --
  -- Parameters:
  --   @p_svg   = Acumulador
  --   @p_texto = Trecho a ser anexado
  --
  -- Change History:
  --   29/10/26 Daniel Madeira: Versão inicial
  --
  -- =================================================================================================
  PROCEDURE svg_anexar (
    p_svg   IN OUT NOCOPY svg_t
  , p_texto IN VARCHAR2
  )
  IS
  BEGIN
    IF p_texto IS NULL THEN
      RETURN;
    END IF;
    IF nvl(lengthb(p_svg.buffer), 0) + lengthb(p_texto) > 32767 THEN
      svg_descarregar (p_svg);
    END IF;
    p_svg.buffer := p_svg.buffer || p_texto;

  END svg_anexar;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 29/10/2026
  -- Description: Finaliza o acumulador SVG.
  --
  -- Parameters:
  --   @p_svg = Acumulador
  -- Returns:
  --   O CLOB com todo o texto acumulado
  --
  -- Change History:
  --   29/10/26 Daniel Madeira: Versão inicial
  --
  -- =================================================================================================
  FUNCTION svg_finalizar (
    p_svg IN OUT NOCOPY svg_t
  )
  RETURN CLOB
  IS
  BEGIN
    svg_descarregar (p_svg);
    RETURN p_svg.conteudo;

  END svg_finalizar;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 29/10/2026
  -- Description: Normaliza o código da projeção, removendo aspas e espaços que possam vir da URL
  --              (ex.: 'e' com aspas simples) e mantendo apenas o primeiro caractere.
  --
  -- Parameters:
  --   @p_projecao = Caractere que identifica o sistema de projeção do mapa terrestre
  -- Returns:
  --   O código normalizado (padrão 'c')
  --
  -- Change History:
  --   29/10/26 Daniel Madeira: Versão inicial
  --
  -- =================================================================================================
  FUNCTION normalizar_projecao (
    p_projecao IN VARCHAR2
  )
  RETURN VARCHAR2
  IS
  BEGIN
    RETURN nvl(substr(trim(replace(replace(p_projecao, ''''), '"')), 1, 1), 'c');

  END normalizar_projecao;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Calcula as coordenadas cartesianas X e Y do centro da imagem em SVG.
  --
  -- Parameters:
  --   @p_largura = Tamanho horizontal da imagem do mapa em pontos
  --   @p_altura  = Tamanho vertical da imagem do mapa em pontos
  -- Returns:
  --   As coordenadas do centro da imagem
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   
  -- =================================================================================================
  FUNCTION coordenar_centro (
    p_largura IN NUMBER
  , p_altura  IN NUMBER
  )
  RETURN coordenadas_xy_t
  IS
    l_centro coordenadas_xy_t;
    
  BEGIN
    l_centro('x') := p_largura / 2;
    l_centro('y') := p_altura / 2;
    RETURN l_centro;
    
  END coordenar_centro;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 06/09/2026
  -- Description: Converte a latitude geográfica em graus para latitude autálica também em graus.
  --              * SNYDER, J. P. Map Projections - A Working Manual. p 16
  --
  -- Parameters:
  --   @p_latitude  = Coordenada geográfica da distância ao Equador em graus em notação decimal
  -- Returns:
  --   A latitude autálica
  --
  -- Change History:
  --   06/09/26 Daniel Madeira: Versão inicial
  --   02/11/26 Daniel Madeira: Proteção de intervalo [-1, 1] para asin(l_q / l_qp)
  --
  -- =================================================================================================
  FUNCTION converter_geografica_para_autalica (
    p_latitude IN NUMBER
  )
  RETURN NUMBER
  IS
    l_phi               NUMBER;
    l_latitude_autalica NUMBER;
    l_q                 NUMBER;
    l_qp                NUMBER;
    l_ratio_q           NUMBER;
    l_e                 CONSTANT NUMBER := 0.0818191908426;
    l_e2                CONSTANT NUMBER := 0.00669437999014;

  BEGIN
    IF (abs(p_latitude) >= 90) THEN
      RETURN p_latitude;
    END IF;

    l_phi := p_latitude * c_pi / 180;

    l_q := (1 - l_e2)
         * (
             (sin(l_phi) / (1 - l_e2 * power(sin(l_phi), 2)))
           - (1 / (2 * l_e))
           * ln((1 - l_e * sin(l_phi)) / (1 + l_e * sin(l_phi)))
           );

    l_qp := (1 - l_e2)
          * (
              (1 / (1 - l_e2))
            - (1 / (2 * l_e))
            * ln((1 - l_e) / (1 + l_e))
            );

    l_ratio_q := l_q / l_qp;
    IF (l_ratio_q > 1) THEN
      l_ratio_q := 1;
    ELSIF (l_ratio_q < -1) THEN
      l_ratio_q := -1;
    END IF;

    l_latitude_autalica := asin(l_ratio_q);

    RETURN (l_latitude_autalica * 180 / c_pi);

  END converter_geografica_para_autalica;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Calcula o ângulo paramétrico theta.
  --              * SNYDER, J. P. Map Projections - A Working Manual. p 256
  --              * SNYDER, J. P.; VOXLAND, P. M. An Album of Map Projections. p 221
  --
  -- Parameters:
  --   @p_latitude  = Coordenada geográfica da distância ao Equador em graus em notação decimal
  -- Returns:
  --   O ângulo paramétrico
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   02/11/26 Daniel Madeira: Retorno direto nos polos e proteção contra divisão por zero
  --   
  -- =================================================================================================
  FUNCTION calcular_eckert_iv_theta (
    p_latitude IN NUMBER
  )
  RETURN NUMBER
  IS
    l_theta NUMBER;
    l_k     NUMBER;
    l_sint  NUMBER;
    l_cost  NUMBER;
    l_denom NUMBER;
    l_v     NUMBER;
    
  BEGIN
    IF (abs(p_latitude) >= 90) THEN
      IF (p_latitude >= 0) THEN
        RETURN (c_pi / 2);
      ELSE
        RETURN (-c_pi / 2);
      END IF;
    END IF;
    l_theta := p_latitude * (c_pi / 180);
    l_k := (2 + (c_pi / 2)) * sin(l_theta);
    l_theta := l_theta / 2;
    FOR i IN REVERSE 1 .. 10 LOOP
      l_sint := sin(l_theta);
      l_cost := cos(l_theta);
      l_denom := 2 * l_cost * (1 + l_cost);
      EXIT WHEN (abs(l_denom) < 1E-12);
      l_v := (l_theta + l_sint * l_cost + 2 * l_sint - l_k) / l_denom;
      l_theta := l_theta - l_v;
      EXIT WHEN (abs(l_v) < 1E-7);
    END LOOP;
    RETURN l_theta;
    
  END calcular_eckert_iv_theta;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Calcula a coordenada cartesiana X referente a projeção da longitude.
  --              * SNYDER, J. P. Map Projections - A Working Manual. p 256
  --              * SNYDER, J. P.; VOXLAND, P. M. An Album of Map Projections. p 221
  --
  -- Parameters:
  --   @p_theta     = Ângulo paramétrico, visto do centro do mapa entre o Equador e a posição da latitude no 90º círculo meridiano.
  --   @p_longitude = Coordenada geográfica da distância ao meridiano de Greenwich em graus em notação decimal
  -- Returns:
  --   A coordenada X no plano cartesiano
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   
  -- =================================================================================================
  FUNCTION calcular_eckert_iv_x (
    p_theta     IN NUMBER
  , p_longitude IN NUMBER
  )
  RETURN NUMBER
  IS
    l_longitude NUMBER;
    
  BEGIN
    l_longitude := p_longitude * (c_pi / 180);
    RETURN ((2 / (sqrt(c_pi * (4 + c_pi)))) * l_longitude * (1 + cos(p_theta)));
    
  END calcular_eckert_iv_x;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Calcula a coordenada cartesiana Y referente a projeção da latitude.
  --              * SNYDER, J. P. Map Projections - A Working Manual. p 256
  --              * SNYDER, J. P.; VOXLAND, P. M. An Album of Map Projections. p 221
  --
  -- Parameters:
  --   @p_theta = Ângulo paramétrico, visto do centro do mapa entre o Equador e a posição da latitude no 90º círculo meridiano.
  -- Returns:
  --   A coordenada Y no plano cartesiano
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   
  -- =================================================================================================
  FUNCTION calcular_eckert_iv_y (
    p_theta IN NUMBER
  )
  RETURN NUMBER
  IS
  BEGIN
    RETURN (2 * sqrt(c_pi / (4 + c_pi)) * sin(p_theta));
    
  END calcular_eckert_iv_y;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Calcula o ângulo paramétrico theta.
  --              * SNYDER, J. P. Map Projections - A Working Manual. p 257
  --              * SNYDER, J. P.; VOXLAND, P. M. An Album of Map Projections. p 220
  --
  -- Parameters:
  --   @p_latitude  = Coordenada geográfica da distância ao Equador em graus em notação decimal
  -- Returns:
  --   O ângulo paramétrico
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   02/11/26 Daniel Madeira: Proteção contra divisão por zero no denominador
  --   
  -- =================================================================================================
  FUNCTION calcular_eckert_vi_theta (
    p_latitude IN NUMBER
  )
  RETURN NUMBER
  IS
    l_theta NUMBER;
    l_k     NUMBER;
    l_denom NUMBER;
    l_v     NUMBER;
    
  BEGIN
    l_theta := p_latitude * (c_pi / 180);
    l_k := (1 + (c_pi / 2)) * sin(l_theta);
    FOR i IN REVERSE 1 .. 10 LOOP
      l_denom := 1 + cos(l_theta);
      EXIT WHEN (abs(l_denom) < 1E-12);
      l_v := (l_theta + sin(l_theta) - l_k) / l_denom;
      l_theta := l_theta - l_v;
      EXIT WHEN (abs(l_v) < 1E-7);
    END LOOP;
    RETURN l_theta;
    
  END calcular_eckert_vi_theta;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Calcula a coordenada cartesiana X referente a projeção da longitude.
  --              * SNYDER, J. P. Map Projections - A Working Manual. p 257
  --              * SNYDER, J. P.; VOXLAND, P. M. An Album of Map Projections. p 220
  --
  -- Parameters:
  --   @p_theta     = Ângulo paramétrico, visto do centro do mapa entre o Equador e a posição da latitude no 90º círculo meridiano.
  --   @p_longitude = Coordenada geográfica da distância ao meridiano de Greenwich em graus em notação decimal
  -- Returns:
  --   A coordenada X no plano cartesiano
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   
  -- =================================================================================================
  FUNCTION calcular_eckert_vi_x (
    p_theta     IN NUMBER
  , p_longitude IN NUMBER
  )
  RETURN NUMBER
  IS
    l_longitude NUMBER;
    
  BEGIN
    l_longitude := p_longitude * (c_pi / 180);
    RETURN (l_longitude * (1 + cos(p_theta)) / sqrt(2 + c_pi));
    
  END calcular_eckert_vi_x;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Calcula a coordenada cartesiana Y referente a projeção da latitude.
  --              * SNYDER, J. P. Map Projections - A Working Manual. p 257
  --              * SNYDER, J. P.; VOXLAND, P. M. An Album of Map Projections. p 220
  --
  -- Parameters:
  --   @p_theta = Ângulo paramétrico, visto do centro do mapa entre o Equador e a posição da latitude no 90º círculo meridiano.
  -- Returns:
  --   A coordenada Y no plano cartesiano
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   
  -- =================================================================================================
  FUNCTION calcular_eckert_vi_y (
    p_theta IN NUMBER
  )
  RETURN NUMBER
  IS
  BEGIN
    RETURN (2 * p_theta / sqrt(2 + c_pi));
    
  END calcular_eckert_vi_y;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Calcula a coordenada cartesiana X referente a projeção da longitude.
  --              * GOTT, J. R.; MUGNOLO, C.; COLLEY, W. N. Map Projections Minimizing Distance Errors. p 4
  --
  -- Parameters:
  --   @p_latitude  = Coordenada geográfica da distância ao Equador em graus em notação decimal
  --   @p_longitude = Coordenada geográfica da distância ao meridiano de Greenwich em graus em notação decimal
  -- Returns:
  --   A coordenada X no plano cartesiano
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   29/10/26 Daniel Madeira: Corrigida a detecção de não convergência na última iteração
  --   02/11/26 Daniel Madeira: Inicialização com phi, sinal de phi na não convergência e proteções numéricas
  --   
  -- =================================================================================================
  FUNCTION calcular_gott_equal_area_elliptical_x (
    p_latitude  IN NUMBER
  , p_longitude IN NUMBER
  )
  RETURN NUMBER
  IS
    l_longitude NUMBER;
    l_lat_rad   NUMBER;
    l_sin_phi   NUMBER;
    l_phi       NUMBER;
    l_theta     NUMBER;
    l_k         NUMBER;
    l_denom     NUMBER;
    l_v         NUMBER;
    l_convergiu BOOLEAN := false;
    
  BEGIN
    l_longitude := p_longitude * (c_pi / 180);
    l_lat_rad := p_latitude * (c_pi / 180);
    l_sin_phi := cos(l_lat_rad) * sin(l_longitude / 2);
    IF (l_sin_phi > 1) THEN
      l_sin_phi := 1;
    ELSIF (l_sin_phi < -1) THEN
      l_sin_phi := -1;
    END IF;
    l_phi := asin(l_sin_phi);
    l_theta := l_phi;
    l_k := c_pi * l_sin_phi;
    FOR i IN 1 .. 10 LOOP
      l_denom := 1 + cos(l_theta);
      EXIT WHEN (abs(l_denom) < 1E-12);
      l_v := (l_theta + sin(l_theta) - l_k) / l_denom;
      l_theta := l_theta - l_v;
      IF (abs(l_v) < 1E-7) THEN
        l_convergiu := true;
        EXIT;
      END IF;
    END LOOP;
    IF NOT l_convergiu THEN
      IF (l_phi < 0) THEN
        l_theta := (-c_pi / 2);
      ELSE
        l_theta := (c_pi / 2);
      END IF;
    ELSE
      l_theta := l_theta * 0.5;
    END IF;
    RETURN (sqrt(2) * sin(l_theta));
    
  END calcular_gott_equal_area_elliptical_x;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Calcula a coordenada cartesiana Y referente a projeção da latitude.
  --              * GOTT, J. R.; MUGNOLO, C.; COLLEY, W. N. Map Projections Minimizing Distance Errors. p 4
  --
  -- Parameters:
  --   @p_latitude  = Coordenada geográfica da distância ao Equador em graus em notação decimal
  --   @p_longitude = Coordenada geográfica da distância ao meridiano de Greenwich em graus em notação decimal
  -- Returns:
  --   A coordenada Y no plano cartesiano
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   29/10/26 Daniel Madeira: Corrigida a detecção de não convergência na última iteração
  --   02/11/26 Daniel Madeira: Tratamento exato de cos(phi) = 0 (sem remendo de +-180),
  --                            inicialização com phi e sinal de phi na não convergência
  --   
  -- =================================================================================================
  FUNCTION calcular_gott_equal_area_elliptical_y (
    p_latitude  IN NUMBER
  , p_longitude IN NUMBER
  )
  RETURN NUMBER
  IS
    l_longitude NUMBER;
    l_lat_rad   NUMBER;
    l_sin_phi   NUMBER;
    l_phi       NUMBER;
    l_cos_phi   NUMBER;
    l_ratio_s   NUMBER;
    l_lambda    NUMBER;
    l_theta     NUMBER;
    l_k         NUMBER;
    l_denom     NUMBER;
    l_v         NUMBER;
    l_convergiu BOOLEAN := false;
    
  BEGIN
    l_longitude := p_longitude * (c_pi / 180);
    l_lat_rad := p_latitude * (c_pi / 180);
    l_sin_phi := cos(l_lat_rad) * sin(l_longitude / 2);
    IF (l_sin_phi > 1) THEN
      l_sin_phi := 1;
    ELSIF (l_sin_phi < -1) THEN
      l_sin_phi := -1;
    END IF;
    l_phi := asin(l_sin_phi);
    l_cos_phi := cos(l_phi);
    IF (abs(l_cos_phi) < 1E-12) THEN
      l_lambda := 0;
    ELSE
      l_ratio_s := sin(l_lat_rad) / l_cos_phi;
      IF (l_ratio_s > 1) THEN
        l_ratio_s := 1;
      ELSIF (l_ratio_s < -1) THEN
        l_ratio_s := -1;
      END IF;
      l_lambda := 0.5 * asin(l_ratio_s);
    END IF;
    l_theta := l_phi;
    l_k := c_pi * l_sin_phi;
    FOR i IN 1 .. 10 LOOP
      l_denom := 1 + cos(l_theta);
      EXIT WHEN (abs(l_denom) < 1E-12);
      l_v := (l_theta + sin(l_theta) - l_k) / l_denom;
      l_theta := l_theta - l_v;
      IF (abs(l_v) < 1E-7) THEN
        l_convergiu := true;
        EXIT;
      END IF;
    END LOOP;
    IF NOT l_convergiu THEN
      IF (l_phi < 0) THEN
        l_theta := (-c_pi / 2);
      ELSE
        l_theta := (c_pi / 2);
      END IF;
    ELSE
      l_theta := l_theta * 0.5;
    END IF;
    RETURN ((c_pi / (2 * sqrt(2))) * l_lambda * cos(l_theta));
    
  END calcular_gott_equal_area_elliptical_y;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 02/11/2026
  -- Description: Calcula simultaneamente as coordenadas cartesianas X e Y da projeção elíptica de
  --              igual área de Gott, executando o método de Newton-Raphson apenas uma vez por ponto.
  --              * GOTT, J. R.; MUGNOLO, C.; COLLEY, W. N. Map Projections Minimizing Distance Errors. p 4
  --
  -- Parameters:
  --   @p_latitude  = Coordenada geográfica da distância ao Equador em graus em notação decimal
  --   @p_longitude = Coordenada geográfica da distância ao meridiano de Greenwich em graus em notação decimal
  --   @p_x         = Coordenada X no plano cartesiano (saída)
  --   @p_y         = Coordenada Y no plano cartesiano (saída)
  --
  -- Change History:
  --   02/11/26 Daniel Madeira: Versão inicial (alinhada com calcular_gott_eliptica em C)
  --
  -- =================================================================================================
  PROCEDURE calcular_gott_equal_area_elliptical (
    p_latitude  IN  NUMBER
  , p_longitude IN  NUMBER
  , p_x         OUT NUMBER
  , p_y         OUT NUMBER
  )
  IS
    l_longitude NUMBER;
    l_lat_rad   NUMBER;
    l_sin_phi   NUMBER;
    l_phi       NUMBER;
    l_cos_phi   NUMBER;
    l_ratio_s   NUMBER;
    l_lambda    NUMBER;
    l_theta     NUMBER;
    l_k         NUMBER;
    l_denom     NUMBER;
    l_v         NUMBER;
    l_convergiu BOOLEAN := false;

  BEGIN
    l_longitude := p_longitude * (c_pi / 180);
    l_lat_rad := p_latitude * (c_pi / 180);
    l_sin_phi := cos(l_lat_rad) * sin(l_longitude / 2);
    IF (l_sin_phi > 1) THEN
      l_sin_phi := 1;
    ELSIF (l_sin_phi < -1) THEN
      l_sin_phi := -1;
    END IF;
    l_phi := asin(l_sin_phi);
    l_cos_phi := cos(l_phi);
    IF (abs(l_cos_phi) < 1E-12) THEN
      l_lambda := 0;
    ELSE
      l_ratio_s := sin(l_lat_rad) / l_cos_phi;
      IF (l_ratio_s > 1) THEN
        l_ratio_s := 1;
      ELSIF (l_ratio_s < -1) THEN
        l_ratio_s := -1;
      END IF;
      l_lambda := 0.5 * asin(l_ratio_s);
    END IF;
    l_theta := l_phi;
    l_k := c_pi * l_sin_phi;
    FOR i IN 1 .. 10 LOOP
      l_denom := 1 + cos(l_theta);
      EXIT WHEN (abs(l_denom) < 1E-12);
      l_v := (l_theta + sin(l_theta) - l_k) / l_denom;
      l_theta := l_theta - l_v;
      IF (abs(l_v) < 1E-7) THEN
        l_convergiu := true;
        EXIT;
      END IF;
    END LOOP;
    IF NOT l_convergiu THEN
      IF (l_phi < 0) THEN
        l_theta := (-c_pi / 2);
      ELSE
        l_theta := (c_pi / 2);
      END IF;
    ELSE
      l_theta := l_theta * 0.5;
    END IF;
    p_x := sqrt(2) * sin(l_theta);
    p_y := (c_pi / (2 * sqrt(2))) * l_lambda * cos(l_theta);

  END calcular_gott_equal_area_elliptical;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Calcula a coordenada cartesiana X referente a projeção da longitude.
  --              * GOTT, J. R.; MUGNOLO, C.; COLLEY, W. N. Map Projections Minimizing Distance Errors. p 8
  --
  -- Parameters:
  --   @p_latitude  = Coordenada geográfica da distância ao Equador em graus em notação decimal
  --   @p_longitude = Coordenada geográfica da distância ao meridiano de Greenwich em graus em notação decimal
  -- Returns:
  --   A coordenada X no plano cartesiano
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial (necessita ajuste pois está centrada no Polo Norte)
  --   20/09/26 Daniel Madeira: Projeção centralizada no Equador (Lat 0, Lon 0)
  --   02/11/26 Daniel Madeira: Tratamento explícito do antípoda (0, +-180) e proteção de acos
  --   
  -- =================================================================================================
  FUNCTION calcular_gott_mugnolo_azimuthal_x (
    p_latitude  IN NUMBER
  , p_longitude IN NUMBER
  )
  RETURN NUMBER
  IS
    l_latitude  NUMBER;
    l_longitude NUMBER;
    l_prod      NUMBER;
    l_c         NUMBER;
    l_k         NUMBER;
    
  BEGIN
    IF (abs(p_latitude) < 1E-12 AND abs(abs(p_longitude) - 180) < 1E-9) THEN
      IF (p_longitude >= 0) THEN
        RETURN sin(0.446 * c_pi);
      ELSE
        RETURN -sin(0.446 * c_pi);
      END IF;
    END IF;

    l_latitude := p_latitude * (c_pi / 180);
    l_longitude := p_longitude * (c_pi / 180);

    l_prod := cos(l_latitude) * cos(l_longitude);
    IF (l_prod > 1) THEN
      l_prod := 1;
    ELSIF (l_prod < -1) THEN
      l_prod := -1;
    END IF;
    l_c := acos(l_prod);

    IF (l_c < 1E-10) THEN
      RETURN 0;
    ELSE
      IF (abs(sin(l_c)) < 1E-10) THEN
        l_k := sin(0.446 * l_c) / 1E-10;
      ELSE
        l_k := sin(0.446 * l_c) / sin(l_c);
      END IF;

      RETURN (l_k * cos(l_latitude) * sin(l_longitude));
    END IF;

  END calcular_gott_mugnolo_azimuthal_x;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Calcula a coordenada cartesiana Y referente a projeção da latitude.
  --              * GOTT, J. R.; MUGNOLO, C.; COLLEY, W. N. Map Projections Minimizing Distance Errors. p 8
  --
  -- Parameters:
  --   @p_latitude  = Coordenada geográfica da distância ao Equador em graus em notação decimal
  --   @p_longitude = Coordenada geográfica da distância ao meridiano de Greenwich em graus em notação decimal
  -- Returns:
  --   A coordenada Y no plano cartesiano
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial (necessita ajuste pois está centrada no Polo Norte)
  --   20/09/26 Daniel Madeira: Projeção centralizada no Equador (Lat 0, Lon 0)
  --   02/11/26 Daniel Madeira: Tratamento explícito do antípoda (0, +-180) e proteção de acos
  --   
  -- =================================================================================================
  FUNCTION calcular_gott_mugnolo_azimuthal_y (
    p_latitude  IN NUMBER
  , p_longitude IN NUMBER
  )
  RETURN NUMBER
  IS
    l_latitude  NUMBER;
    l_longitude NUMBER;
    l_prod      NUMBER;
    l_c         NUMBER;
    l_k         NUMBER;
    
  BEGIN
    IF (abs(p_latitude) < 1E-12 AND abs(abs(p_longitude) - 180) < 1E-9) THEN
      RETURN 0;
    END IF;

    l_latitude := p_latitude * (c_pi / 180);
    l_longitude := p_longitude * (c_pi / 180);

    l_prod := cos(l_latitude) * cos(l_longitude);
    IF (l_prod > 1) THEN
      l_prod := 1;
    ELSIF (l_prod < -1) THEN
      l_prod := -1;
    END IF;
    l_c := acos(l_prod);

    IF (l_c < 1E-10) THEN
      RETURN 0;
    ELSE
      IF (abs(sin(l_c)) < 1E-10) THEN
        l_k := sin(0.446 * l_c) / 1E-10;
      ELSE
        l_k := sin(0.446 * l_c) / sin(l_c);
      END IF;

      RETURN (l_k * sin(l_latitude));
    END IF;

  END calcular_gott_mugnolo_azimuthal_y;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 02/11/2026
  -- Description: Calcula simultaneamente as coordenadas cartesianas X e Y da projeção azimutal de
  --              Gott-Mugnolo, compartilhando a distância angular c e o fator k.
  --              * GOTT, J. R.; MUGNOLO, C.; COLLEY, W. N. Map Projections Minimizing Distance Errors. p 8
  --
  -- Parameters:
  --   @p_latitude  = Coordenada geográfica da distância ao Equador em graus em notação decimal
  --   @p_longitude = Coordenada geográfica da distância ao meridiano de Greenwich em graus em notação decimal
  --   @p_x         = Coordenada X no plano cartesiano (saída)
  --   @p_y         = Coordenada Y no plano cartesiano (saída)
  --
  -- Change History:
  --   02/11/26 Daniel Madeira: Versão inicial (alinhada com calcular_gott_azimutal em C)
  --
  -- =================================================================================================
  PROCEDURE calcular_gott_mugnolo_azimuthal (
    p_latitude  IN  NUMBER
  , p_longitude IN  NUMBER
  , p_x         OUT NUMBER
  , p_y         OUT NUMBER
  )
  IS
    l_latitude  NUMBER;
    l_longitude NUMBER;
    l_prod      NUMBER;
    l_c         NUMBER;
    l_k         NUMBER;

  BEGIN
    IF (abs(p_latitude) < 1E-12 AND abs(abs(p_longitude) - 180) < 1E-9) THEN
      IF (p_longitude >= 0) THEN
        p_x := sin(0.446 * c_pi);
      ELSE
        p_x := -sin(0.446 * c_pi);
      END IF;
      p_y := 0;
      RETURN;
    END IF;

    l_latitude := p_latitude * (c_pi / 180);
    l_longitude := p_longitude * (c_pi / 180);

    l_prod := cos(l_latitude) * cos(l_longitude);
    IF (l_prod > 1) THEN
      l_prod := 1;
    ELSIF (l_prod < -1) THEN
      l_prod := -1;
    END IF;
    l_c := acos(l_prod);

    IF (l_c < 1E-10) THEN
      p_x := 0;
      p_y := 0;
    ELSE
      IF (abs(sin(l_c)) < 1E-10) THEN
        l_k := sin(0.446 * l_c) / 1E-10;
      ELSE
        l_k := sin(0.446 * l_c) / sin(l_c);
      END IF;

      p_x := l_k * cos(l_latitude) * sin(l_longitude);
      p_y := l_k * sin(l_latitude);
    END IF;

  END calcular_gott_mugnolo_azimuthal;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Calcula a coordenada cartesiana X referente a projeção da longitude.
  --              * BUGAYEVSKIY, L. M.; SNYDER, J. P. *Map Projections A Reference Manual. p 176
  --              * JENNY, B. Adaptive Composite Map Projections. p 3
  --              * SNYDER, J. P.; VOXLAND, P. M. An Album of Map Projections. p 232
  --
  -- Parameters:
  --   @p_latitude  = Coordenada geográfica da distância ao Equador em graus em notação decimal
  --   @p_longitude = Coordenada geográfica da distância ao meridiano de Greenwich em graus em notação decimal
  -- Returns:
  --   A coordenada X no plano cartesiano
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   02/11/26 Daniel Madeira: Proteção do radicando para evitar divisão por zero
  --   
  -- =================================================================================================
  FUNCTION calcular_hammer_x (
    p_latitude  IN NUMBER
  , p_longitude IN NUMBER
  )
  RETURN NUMBER
  IS
    l_latitude  NUMBER;
    l_longitude NUMBER;
    l_radicando NUMBER;
    l_eta       NUMBER;
    
  BEGIN
    l_latitude := p_latitude * (c_pi / 180);
    l_longitude := p_longitude * (c_pi / 180);
    l_radicando := 1 + cos(l_latitude) * cos(l_longitude / 2);
    IF (l_radicando < 1E-12) THEN
      l_radicando := 1E-12;
    END IF;
    l_eta := sqrt(l_radicando);
    RETURN ((2 * sqrt(2) * cos(l_latitude) * sin(l_longitude / 2)) / l_eta);
    
  END calcular_hammer_x;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Calcula a coordenada cartesiana Y referente a projeção da latitude.
  --              * BUGAYEVSKIY, L. M.; SNYDER, J. P. *Map Projections A Reference Manual. p 176
  --              * JENNY, B. Adaptive Composite Map Projections. p 3
  --              * SNYDER, J. P.; VOXLAND, P. M. An Album of Map Projections. p 232
  --
  -- Parameters:
  --   @p_latitude  = Coordenada geográfica da distância ao Equador em graus em notação decimal
  --   @p_longitude = Coordenada geográfica da distância ao meridiano de Greenwich em graus em notação decimal
  -- Returns:
  --   A coordenada Y no plano cartesiano
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   02/11/26 Daniel Madeira: Proteção do radicando para evitar divisão por zero
  --   
  -- =================================================================================================
  FUNCTION calcular_hammer_y (
    p_latitude  IN NUMBER
  , p_longitude IN NUMBER
  )
  RETURN NUMBER
  IS
    l_latitude  NUMBER;
    l_longitude NUMBER;
    l_radicando NUMBER;
    l_eta       NUMBER;
    
  BEGIN
    l_latitude := p_latitude * (c_pi / 180);
    l_longitude := p_longitude * (c_pi / 180);
    l_radicando := 1 + cos(l_latitude) * cos(l_longitude / 2);
    IF (l_radicando < 1E-12) THEN
      l_radicando := 1E-12;
    END IF;
    l_eta := sqrt(l_radicando);
    RETURN ((sqrt(2) * sin(l_latitude)) / l_eta);
    
  END calcular_hammer_y;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 02/11/2026
  -- Description: Calcula simultaneamente as coordenadas cartesianas X e Y da projeção de Hammer,
  --              compartilhando o denominador radical eta.
  --              * BUGAYEVSKIY, L. M.; SNYDER, J. P. *Map Projections A Reference Manual. p 176
  --              * JENNY, B. Adaptive Composite Map Projections. p 3
  --              * SNYDER, J. P.; VOXLAND, P. M. An Album of Map Projections. p 232
  --
  -- Parameters:
  --   @p_latitude  = Coordenada geográfica da distância ao Equador em graus em notação decimal
  --   @p_longitude = Coordenada geográfica da distância ao meridiano de Greenwich em graus em notação decimal
  --   @p_x         = Coordenada X no plano cartesiano (saída)
  --   @p_y         = Coordenada Y no plano cartesiano (saída)
  --
  -- Change History:
  --   02/11/26 Daniel Madeira: Versão inicial (alinhada com calcular_hammer em C)
  --
  -- =================================================================================================
  PROCEDURE calcular_hammer (
    p_latitude  IN  NUMBER
  , p_longitude IN  NUMBER
  , p_x         OUT NUMBER
  , p_y         OUT NUMBER
  )
  IS
    l_latitude  NUMBER;
    l_longitude NUMBER;
    l_radicando NUMBER;
    l_eta       NUMBER;

  BEGIN
    l_latitude := p_latitude * (c_pi / 180);
    l_longitude := p_longitude * (c_pi / 180);
    l_radicando := 1 + cos(l_latitude) * cos(l_longitude / 2);
    IF (l_radicando < 1E-12) THEN
      l_radicando := 1E-12;
    END IF;
    l_eta := sqrt(l_radicando);
    p_x := (2 * sqrt(2) * cos(l_latitude) * sin(l_longitude / 2)) / l_eta;
    p_y := (sqrt(2) * sin(l_latitude)) / l_eta;

  END calcular_hammer;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Calcula a coordenada cartesiana X referente a projeção da longitude.
  --              * SNYDER, J. P. Flattening the Earth: Two thousand years of map projections. p 202
  --
  -- Parameters:
  --   @p_latitude  = Coordenada geográfica da distância ao Equador em graus em notação decimal
  --   @p_longitude = Coordenada geográfica da distância ao meridiano de Greenwich em graus em notação decimal
  -- Returns:
  --   A coordenada X no plano cartesiano
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   02/11/26 Daniel Madeira: Proteção contra radicando negativo
  --   
  -- =================================================================================================
  FUNCTION calcular_kavrayskiy_vii_x (
    p_latitude  IN NUMBER
  , p_longitude IN NUMBER
  )
  RETURN NUMBER
  IS
    l_latitude  NUMBER;
    l_longitude NUMBER;
    l_radicando NUMBER;
    
  BEGIN
    l_latitude := p_latitude * (c_pi / 180);
    l_longitude := p_longitude * (c_pi / 180);
    l_radicando := 1 / 3 - power(l_latitude / c_pi, 2);
    IF (l_radicando < 0) THEN
      l_radicando := 0;
    END IF;
    RETURN (3 * l_longitude / 2) * sqrt(l_radicando);
    
  END calcular_kavrayskiy_vii_x;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Calcula a coordenada cartesiana Y referente a projeção da latitude.
  --              * SNYDER, J. P. Flattening the Earth: Two thousand years of map projections. p 202
  --
  -- Parameters:
  --   @p_latitude = Coordenada geográfica da distância ao Equador em graus em notação decimal
  -- Returns:
  --   A coordenada Y no plano cartesiano
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   
  -- =================================================================================================
  FUNCTION calcular_kavrayskiy_vii_y (
    p_latitude IN NUMBER
  )
  RETURN NUMBER
  IS
    l_latitude NUMBER;
    
  BEGIN
    l_latitude := p_latitude * (c_pi / 180);
    RETURN l_latitude;
    
  END calcular_kavrayskiy_vii_y;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Calcula a coordenada cartesiana X referente a projeção da longitude.
  --              * SNYDER, J. P. Map Projections - A Working Manual. p 185
  --
  -- Parameters:
  --   @p_latitude  = Coordenada geográfica da distância ao Equador em graus em notação decimal
  --   @p_longitude = Coordenada geográfica da distância ao meridiano de Greenwich em graus em notação decimal
  -- Returns:
  --   A coordenada X no plano cartesiano
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   20/09/26 Daniel Madeira: Checagem no denominador para evitar divisão por zero (NaN) no antípoda
  --   02/11/26 Daniel Madeira: Tratamento explícito do antípoda (0, +-180)
  --   
  -- =================================================================================================
  FUNCTION calcular_lambert_azimuthal_equal_area_x (
    p_latitude  IN NUMBER
  , p_longitude IN NUMBER
  )
  RETURN NUMBER
  IS
    l_latitude    NUMBER;
    l_longitude   NUMBER;
    l_denominador NUMBER;
    l_k           NUMBER;
    
  BEGIN
    IF (abs(p_latitude) < 1E-12 AND abs(abs(p_longitude) - 180) < 1E-9) THEN
      IF (p_longitude >= 0) THEN
        RETURN 2;
      ELSE
        RETURN -2;
      END IF;
    END IF;
    l_latitude := p_latitude * (c_pi / 180);
    l_longitude := p_longitude * (c_pi / 180);
    l_denominador := 1 + cos(l_latitude) * cos(l_longitude);
    IF (l_denominador < 1E-10) THEN
      l_denominador := 1E-10;
    END IF;
    l_k := sqrt(2 / l_denominador);
    RETURN (l_k * cos(l_latitude) * sin(l_longitude));
    
  END calcular_lambert_azimuthal_equal_area_x;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Calcula a coordenada cartesiana Y referente a projeção da latitude.
  --              * SNYDER, J. P. Map Projections - A Working Manual. p 185
  --
  -- Parameters:
  --   @p_latitude  = Coordenada geográfica da distância ao Equador em graus em notação decimal
  --   @p_longitude = Coordenada geográfica da distância ao meridiano de Greenwich em graus em notação decimal
  -- Returns:
  --   A coordenada Y no plano cartesiano
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   20/09/26 Daniel Madeira: Checagem no denominador para evitar divisão por zero (NaN) no antípoda
  --   02/11/26 Daniel Madeira: Tratamento explícito do antípoda (0, +-180)
  --   
  -- =================================================================================================
  FUNCTION calcular_lambert_azimuthal_equal_area_y (
    p_latitude  IN NUMBER
  , p_longitude IN NUMBER
  )
  RETURN NUMBER
  IS
    l_latitude    NUMBER;
    l_longitude   NUMBER;
    l_denominador NUMBER;
    l_k           NUMBER;
    
  BEGIN
    IF (abs(p_latitude) < 1E-12 AND abs(abs(p_longitude) - 180) < 1E-9) THEN
      RETURN 0;
    END IF;
    l_latitude := p_latitude * (c_pi / 180);
    l_longitude := p_longitude * (c_pi / 180);
    l_denominador := 1 + cos(l_latitude) * cos(l_longitude);
    IF (l_denominador < 1E-10) THEN
      l_denominador := 1E-10;
    END IF;
    l_k := sqrt(2 / l_denominador);
    RETURN (l_k * sin(l_latitude));
    
  END calcular_lambert_azimuthal_equal_area_y;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 02/11/2026
  -- Description: Calcula simultaneamente as coordenadas cartesianas X e Y da projeção azimutal de
  --              igual área de Lambert, compartilhando o fator k.
  --              * SNYDER, J. P. Map Projections - A Working Manual. p 185
  --
  -- Parameters:
  --   @p_latitude  = Coordenada geográfica da distância ao Equador em graus em notação decimal
  --   @p_longitude = Coordenada geográfica da distância ao meridiano de Greenwich em graus em notação decimal
  --   @p_x         = Coordenada X no plano cartesiano (saída)
  --   @p_y         = Coordenada Y no plano cartesiano (saída)
  --
  -- Change History:
  --   02/11/26 Daniel Madeira: Versão inicial (alinhada com calcular_lambert_azimutal em C)
  --
  -- =================================================================================================
  PROCEDURE calcular_lambert_azimuthal_equal_area (
    p_latitude  IN  NUMBER
  , p_longitude IN  NUMBER
  , p_x         OUT NUMBER
  , p_y         OUT NUMBER
  )
  IS
    l_latitude    NUMBER;
    l_longitude   NUMBER;
    l_denominador NUMBER;
    l_k           NUMBER;

  BEGIN
    IF (abs(p_latitude) < 1E-12 AND abs(abs(p_longitude) - 180) < 1E-9) THEN
      IF (p_longitude >= 0) THEN
        p_x := 2;
      ELSE
        p_x := -2;
      END IF;
      p_y := 0;
      RETURN;
    END IF;
    l_latitude := p_latitude * (c_pi / 180);
    l_longitude := p_longitude * (c_pi / 180);
    l_denominador := 1 + cos(l_latitude) * cos(l_longitude);
    IF (l_denominador < 1E-10) THEN
      l_denominador := 1E-10;
    END IF;
    l_k := sqrt(2 / l_denominador);
    p_x := l_k * cos(l_latitude) * sin(l_longitude);
    p_y := l_k * sin(l_latitude);

  END calcular_lambert_azimuthal_equal_area;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Calcula a coordenada cartesiana X referente a projeção da longitude.
  --              * SNYDER, J. P. Map Projections - A Working Manual. p 41
  --              * PEARSON, F. Map Projections: Theory and Applications. pp 190-191
  --              * GOLDBERG, D. M.; GOTT, J. R. Flexion and Skewness in Map Projections of the Earth. p 8
  --              * IOGP Coordinate Conversions and Transformations including Formulas. p 45
  --              * SNYDER, J. P.; VOXLAND, P. M. An Album of Map Projections. p 218
  --
  -- Parameters:
  --   @p_longitude = Coordenada geográfica da distância ao meridiano de Greenwich em graus em notação decimal
  -- Returns:
  --   A coordenada X no plano cartesiano
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   
  -- =================================================================================================
  FUNCTION calcular_mercator_x (
    p_longitude IN NUMBER
  )
  RETURN NUMBER
  IS
    l_longitude NUMBER;
    
  BEGIN
    l_longitude := p_longitude * (c_pi / 180);
    RETURN l_longitude;
    
  END calcular_mercator_x;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Calcula a coordenada cartesiana Y referente a projeção da latitude.
  --              * SNYDER, J. P. Map Projections - A Working Manual. p 41
  --              * PEARSON, F. Map Projections: Theory and Applications. pp 190-191
  --              * GOLDBERG, D. M.; GOTT, J. R. Flexion and Skewness in Map Projections of the Earth. p 8
  --              * IOGP Coordinate Conversions and Transformations including Formulas. p 45
  --              * SNYDER, J. P.; VOXLAND, P. M. An Album of Map Projections. p 218
  --
  -- Parameters:
  --   @p_latitude = Coordenada geográfica da distância ao Equador em graus em notação decimal
  -- Returns:
  --   A coordenada Y no plano cartesiano
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   
  -- =================================================================================================
  FUNCTION calcular_mercator_y (
    p_latitude IN NUMBER
  )
  RETURN NUMBER
  IS
    l_latitude NUMBER;
    
  BEGIN
    l_latitude := p_latitude * (c_pi / 180);
    
    IF (l_latitude > 1.4844222297453322) THEN
      RETURN c_pi;
    ELSIF (l_latitude < -1.4844222297453322) THEN
      RETURN -c_pi;
    ELSE
      RETURN (ln(tan(c_pi/4 + l_latitude/2)));
    END IF;
    
  END calcular_mercator_y;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Calcula a coordenada cartesiana X referente a projeção da longitude.
  --              * WEISSTEIN, E. W. Miller Cylindrical Projection
  --
  -- Parameters:
  --   @p_longitude = Coordenada geográfica da distância ao meridiano de Greenwich em graus em notação decimal
  -- Returns:
  --   A coordenada X no plano cartesiano
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   
  -- =================================================================================================
  FUNCTION calcular_miller_x (
    p_longitude IN NUMBER
  )
  RETURN NUMBER
  IS
    l_longitude NUMBER;
    
  BEGIN
    l_longitude := p_longitude * (c_pi / 180);
    RETURN l_longitude;
    
  END calcular_miller_x;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Calcula a coordenada cartesiana Y referente a projeção da latitude.
  --              * WEISSTEIN, E. W. Miller Cylindrical Projection
  --
  -- Parameters:
  --   @p_latitude = Coordenada geográfica da distância ao Equador em graus em notação decimal
  -- Returns:
  --   A coordenada Y no plano cartesiano
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   
  -- =================================================================================================
  FUNCTION calcular_miller_y (
    p_latitude IN NUMBER
  )
  RETURN NUMBER
  IS
    l_latitude NUMBER;
    
  BEGIN
    l_latitude := p_latitude * (c_pi / 180);
    RETURN ((5/4) * ln(tan(c_pi/4 + ((2 * l_latitude)/5))));
    
  END calcular_miller_y;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Calcula o ângulo paramétrico theta.
  --              * WEISSTEIN, E. W. Mollweide Projection.
  --              * SNYDER, J. P. Map Projections - A Working Manual. p 251
  --              * SNYDER, J. P.; VOXLAND, P. M. An Album of Map Projections. p 220
  --
  -- Parameters:
  --   @p_latitude = Coordenada geográfica da distância ao Equador em graus em notação decimal
  -- Returns:
  --   O ângulo paramétrico
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   29/10/26 Daniel Madeira: Corrigida a detecção de não convergência na última iteração
  --   02/11/26 Daniel Madeira: Retorno direto nos polos e proteção contra divisão por zero
  --   
  -- =================================================================================================
  FUNCTION calcular_mollweide_theta (
    p_latitude IN NUMBER
  )
  RETURN NUMBER
  IS
    l_theta     NUMBER;
    l_k         NUMBER;
    l_denom     NUMBER;
    l_v         NUMBER;
    l_convergiu BOOLEAN := false;
    
  BEGIN
    IF (abs(p_latitude) >= 90) THEN
      IF (p_latitude < 0) THEN
        RETURN (-c_pi / 2);
      ELSE
        RETURN (c_pi / 2);
      END IF;
    END IF;
    l_theta := p_latitude * (c_pi / 180);
    l_k := c_pi * sin(l_theta);
    FOR i IN 1 .. 10 LOOP
      l_denom := 1 + cos(l_theta);
      EXIT WHEN (abs(l_denom) < 1E-12);
      l_v := (l_theta + sin(l_theta) - l_k) / l_denom;
      l_theta := l_theta - l_v;
      IF (abs(l_v) < 1E-7) THEN
        l_convergiu := true;
        EXIT;
      END IF;
    END LOOP;
    IF NOT l_convergiu THEN
      IF (p_latitude < 0) THEN
        l_theta := (-c_pi / 2);
      ELSE
        l_theta := (c_pi / 2);
      END IF;
    ELSE
      l_theta := l_theta * 0.5;
    END IF;
    RETURN l_theta;
    
  END calcular_mollweide_theta;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Calcula a coordenada cartesiana X referente a projeção da longitude.
  --              * WEISSTEIN, E. W. Mollweide Projection.
  --              * SNYDER, J. P. Map Projections - A Working Manual. p 251
  --              * SNYDER, J. P.; VOXLAND, P. M. An Album of Map Projections. p 220
  --
  -- Parameters:
  --   @p_theta     = Ângulo paramétrico, visto do centro do mapa entre o Equador e a posição da latitude no 90º círculo meridiano.
  --   @p_longitude = Coordenada geográfica da distância ao meridiano de Greenwich em graus em notação decimal
  -- Returns:
  --   A coordenada X no plano cartesiano
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   
  -- =================================================================================================
  FUNCTION calcular_mollweide_x (
    p_theta     IN NUMBER
  , p_longitude IN NUMBER
  )
  RETURN NUMBER
  IS
    l_longitude NUMBER;
    l_cx        NUMBER;
    
  BEGIN
    l_longitude := p_longitude * (c_pi / 180);
    l_cx := 2 * sqrt(2) / c_pi;
    RETURN (l_cx * l_longitude * cos(p_theta));
    
  END calcular_mollweide_x;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Calcula a coordenada cartesiana Y referente a projeção da latitude.
  --              * WEISSTEIN, E. W. Mollweide Projection.
  --              * SNYDER, J. P. Map Projections - A Working Manual. p 251
  --              * SNYDER, J. P.; VOXLAND, P. M. An Album of Map Projections. p 220
  --
  -- Parameters:
  --   @p_theta = Ângulo paramétrico, visto do centro do mapa entre o Equador e a posição da latitude no 90º círculo meridiano.
  -- Returns:
  --   A coordenada Y no plano cartesiano
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   
  -- =================================================================================================
  FUNCTION calcular_mollweide_y (
    p_theta IN NUMBER
  )
  RETURN NUMBER
  IS
    l_cy NUMBER;
    
  BEGIN
    l_cy := sqrt(2);
    RETURN (l_cy * sin(p_theta));
    
  END calcular_mollweide_y;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Calcula a coordenada cartesiana X referente a projeção da longitude.
  --              * ŠAVRIČ, B.; JENNY, B.; PATTERSON, T.; PETROVIČ, D.; HURNI, L. A Polynomial Equation for the Natural Earth Projection. p 366
  --
  -- Parameters:
  --   @p_latitude  = Coordenada geográfica da distância ao Equador em graus em notação decimal
  --   @p_longitude = Coordenada geográfica da distância ao meridiano de Greenwich em graus em notação decimal
  -- Returns:
  --   A coordenada X no plano cartesiano
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   
  -- =================================================================================================
  FUNCTION calcular_natural_earth_x (
    p_latitude  IN NUMBER
  , p_longitude IN NUMBER
  )
  RETURN NUMBER
  IS
    l_latitude  NUMBER;
    l_longitude NUMBER;
    
  BEGIN
    l_latitude := p_latitude * (c_pi / 180);
    l_longitude := p_longitude * (c_pi / 180);
    RETURN (l_longitude * (0.870700 - 0.131979 * power(l_latitude, 2) - 0.013791 * power(l_latitude, 4) + 0.003971 * power(l_latitude, 10) - 0.001529 * power(l_latitude, 12)));
    
  END calcular_natural_earth_x;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Calcula a coordenada cartesiana Y referente a projeção da latitude.
  --              * ŠAVRIČ, B.; JENNY, B.; PATTERSON, T.; PETROVIČ, D.; HURNI, L. A Polynomial Equation for the Natural Earth Projection. p 366
  --
  -- Parameters:
  --   @p_latitude = Coordenada geográfica da distância ao Equador em graus em notação decimal
  -- Returns:
  --   A coordenada Y no plano cartesiano
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   
  -- =================================================================================================
  FUNCTION calcular_natural_earth_y (
    p_latitude IN NUMBER
  )
  RETURN NUMBER
  IS
    l_latitude  NUMBER;
    
  BEGIN
    l_latitude := p_latitude * (c_pi / 180);
    RETURN (1.007226 * l_latitude + 0.015085 * power(l_latitude, 3) - 0.044475 * power(l_latitude, 7) + 0.028874 * power(l_latitude, 9) - 0.005916 * power(l_latitude, 11));
    
  END calcular_natural_earth_y;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Calcula a coordenada cartesiana X referente a projeção da longitude.
  --              * ŠAVRIČ, B.; PATTERSON, T.; JENNY, B. The Natural Earth II world map projection. p 125
  --
  -- Parameters:
  --   @p_latitude  = Coordenada geográfica da distância ao Equador em graus em notação decimal
  --   @p_longitude = Coordenada geográfica da distância ao meridiano de Greenwich em graus em notação decimal
  -- Returns:
  --   A coordenada X no plano cartesiano
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   
  -- =================================================================================================
  FUNCTION calcular_natural_earth_ii_x (
    p_latitude  IN NUMBER
  , p_longitude IN NUMBER
  )
  RETURN NUMBER
  IS
    l_latitude  NUMBER;
    l_longitude NUMBER;
    l_latitude2 NUMBER;
    l_latitude4 NUMBER;
    l_latitude6 NUMBER;
    
  BEGIN
    l_latitude := p_latitude * (c_pi / 180);
    l_longitude := p_longitude * (c_pi / 180);
    l_latitude2 := l_latitude * l_latitude;
    l_latitude4 := l_latitude2 * l_latitude2;
    l_latitude6 := l_latitude2 * l_latitude4;
    RETURN (l_longitude * (0.84719 - 0.13063 * l_latitude2 + l_latitude6 * l_latitude6 * (-0.04515 + 0.05494 * l_latitude2 - 0.02326 * l_latitude4 + 0.00331 * l_latitude6)));
    
  END calcular_natural_earth_ii_x;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Calcula a coordenada cartesiana Y referente a projeção da latitude.
  --              * ŠAVRIČ, B.; PATTERSON, T.; JENNY, B. The Natural Earth II world map projection. p 125
  --
  -- Parameters:
  --   @p_latitude = Coordenada geográfica da distância ao Equador em graus em notação decimal
  -- Returns:
  --   A coordenada Y no plano cartesiano
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   
  -- =================================================================================================
  FUNCTION calcular_natural_earth_ii_y (
    p_latitude IN NUMBER
  )
  RETURN NUMBER
  IS
    l_latitude  NUMBER;
    l_latitude2 NUMBER;
    l_latitude4 NUMBER;
    
  BEGIN
    l_latitude := p_latitude * (c_pi / 180);
    l_latitude2 := l_latitude * l_latitude;
    l_latitude4 := l_latitude2 * l_latitude2;
    RETURN (l_latitude * (1.01183 + l_latitude4 * l_latitude4 * (0.01926 * l_latitude2 - 0.00396 * l_latitude4 - 0.02625)));
    
  END calcular_natural_earth_ii_y;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Calcula a coordenada cartesiana X referente a projeção da longitude.
  --              * PATTERSON, T.; ŠAVRIČ, B.; JENNY, B. Introducing the Patterson Cylindrical Projection. p 80
  --
  -- Parameters:
  --   @p_longitude = Coordenada geográfica da distância ao meridiano de Greenwich em graus em notação decimal
  -- Returns:
  --   A coordenada X no plano cartesiano
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   
  -- =================================================================================================
  FUNCTION calcular_patterson_x (
    p_longitude IN NUMBER
  )
  RETURN NUMBER
  IS
    l_longitude NUMBER;
    
  BEGIN
    l_longitude := p_longitude * (c_pi / 180);
    RETURN l_longitude;
    
  END calcular_patterson_x;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Calcula a coordenada cartesiana Y referente a projeção da latitude.
  --              * PATTERSON, T.; ŠAVRIČ, B.; JENNY, B. Introducing the Patterson Cylindrical Projection. p 80
  --
  -- Parameters:
  --   @p_latitude = Coordenada geográfica da distância ao Equador em graus em notação decimal
  -- Returns:
  --   A coordenada Y no plano cartesiano
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   
  -- =================================================================================================
  FUNCTION calcular_patterson_y (
    p_latitude IN NUMBER
  )
  RETURN NUMBER
  IS
    l_latitude  NUMBER;
    l_latitude2 NUMBER;
    
  BEGIN
    l_latitude := p_latitude * (c_pi / 180);
    l_latitude2 := l_latitude * l_latitude;
    RETURN (l_latitude * (1.0148 + l_latitude2 * l_latitude2 * (0.23185 + l_latitude2 * (-0.14499 + l_latitude2 * 0.02406))));
    
  END calcular_patterson_y;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Calcula a coordenada cartesiana X referente a projeção da longitude.
  --              * IPBUKER, C. A computational approach to the Robinson projection. p 207
  --
  -- Parameters:
  --   @p_latitude  = Coordenada geográfica da distância ao Equador em graus em notação decimal
  --   @p_longitude = Coordenada geográfica da distância ao meridiano de Greenwich em graus em notação decimal
  -- Returns:
  --   A coordenada X no plano cartesiano
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   
  -- =================================================================================================
  FUNCTION calcular_robinson_x (
    p_latitude  IN NUMBER
  , p_longitude IN NUMBER
  )
  RETURN NUMBER
  IS
    l_latitude  NUMBER;
    l_longitude NUMBER;
    
  BEGIN
    l_latitude := p_latitude * (c_pi / 180);
    l_longitude := p_longitude * (c_pi / 180);
    RETURN ((2.6666 - 0.367 * power(l_latitude, 2) - 0.150 * power(l_latitude, 4) + 0.0379 * power(l_latitude, 6)) * l_longitude / c_pi);
    
  END calcular_robinson_x;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Calcula a coordenada cartesiana Y referente a projeção da latitude.
  --              * IPBUKER, C. A computational approach to the Robinson projection. p 207
  --
  -- Parameters:
  --   @p_latitude = Coordenada geográfica da distância ao Equador em graus em notação decimal
  -- Returns:
  --   A coordenada Y no plano cartesiano
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   
  -- =================================================================================================
  FUNCTION calcular_robinson_y (
    p_latitude IN NUMBER
  )
  RETURN NUMBER
  IS
    l_latitude NUMBER;
    l_s        NUMBER;
    
  BEGIN
    l_latitude := p_latitude * (c_pi / 180);
    IF (l_latitude >= 0) THEN
      l_s := 1;
    ELSE
      l_s := -1;
    END IF;
    RETURN (0.96047 * l_latitude - 0.00857 * l_s * power(abs(l_latitude), 6.41));
    
  END calcular_robinson_y;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Calcula a coordenada cartesiana X referente a projeção da longitude.
  --              * SNYDER, J. P. Map Projections - A Working Manual. p 247
  --              * SNYDER, J. P.; VOXLAND, P. M. An Album of Map Projections. p 220
  --
  -- Parameters:
  --   @p_latitude  = Coordenada geográfica da distância ao Equador em graus em notação decimal
  --   @p_longitude = Coordenada geográfica da distância ao meridiano de Greenwich em graus em notação decimal
  -- Returns:
  --   A coordenada X no plano cartesiano
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   02/11/26 Daniel Madeira: Conversão de longitude para radianos (alinhado com versão C)
  --   
  -- =================================================================================================
  FUNCTION calcular_sinusoidal_x (
    p_latitude  IN NUMBER
  , p_longitude IN NUMBER
  )
  RETURN NUMBER
  IS
    l_latitude  NUMBER;
    l_longitude NUMBER;
    
  BEGIN
    l_latitude := p_latitude * (c_pi / 180);
    l_longitude := p_longitude * (c_pi / 180);
    RETURN l_longitude * cos(l_latitude);
    
  END calcular_sinusoidal_x;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Calcula a coordenada cartesiana Y referente a projeção da latitude.
  --              * SNYDER, J. P. Map Projections - A Working Manual. p 247
  --              * SNYDER, J. P.; VOXLAND, P. M. An Album of Map Projections. p 220
  --
  -- Parameters:
  --   @p_latitude = Coordenada geográfica da distância ao Equador em graus em notação decimal
  -- Returns:
  --   A coordenada Y no plano cartesiano
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   02/11/26 Daniel Madeira: Conversão de latitude para radianos (alinhado com versão C)
  --   
  -- =================================================================================================
  FUNCTION calcular_sinusoidal_y (
    p_latitude IN NUMBER
  )
  RETURN NUMBER
  IS
  BEGIN
    RETURN p_latitude * (c_pi / 180);
    
  END calcular_sinusoidal_y;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Calcula a coordenada cartesiana X referente a projeção da longitude.
  --              * SNYDER, J. P. Flattening the Earth: Two thousand years of map projections. p 205
  --
  -- Parameters:
  --   @p_latitude  = Coordenada geográfica da distância ao Equador em graus em notação decimal
  --   @p_longitude = Coordenada geográfica da distância ao meridiano de Greenwich em graus em notação decimal
  -- Returns:
  --   A coordenada X no plano cartesiano
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   02/11/26 Daniel Madeira: Proteção contra radicando negativo
  --   
  -- =================================================================================================
  FUNCTION calcular_wagner_vi_x (
    p_latitude  IN NUMBER
  , p_longitude IN NUMBER
  )
  RETURN NUMBER
  IS
    l_latitude  NUMBER;
    l_longitude NUMBER;
    l_radicando NUMBER;
    
  BEGIN
    l_latitude := p_latitude * (c_pi / 180);
    l_longitude := p_longitude * (c_pi / 180);
    l_radicando := 1 - 3 * power(l_latitude / c_pi, 2);
    IF (l_radicando < 0) THEN
      l_radicando := 0;
    END IF;
    RETURN (l_longitude * sqrt(l_radicando));
    
  END calcular_wagner_vi_x;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Calcula a coordenada cartesiana Y referente a projeção da latitude.
  --              * SNYDER, J. P. Flattening the Earth: Two thousand years of map projections. p 205
  --
  -- Parameters:
  --   @p_latitude = Coordenada geográfica da distância ao Equador em graus em notação decimal
  -- Returns:
  --   A coordenada Y no plano cartesiano
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   
  -- =================================================================================================
  FUNCTION calcular_wagner_vi_y (
    p_latitude IN NUMBER
  )
  RETURN NUMBER
  IS
    l_latitude NUMBER;
    
  BEGIN
    l_latitude := p_latitude * (c_pi / 180);
    RETURN l_latitude;
    
  END calcular_wagner_vi_y;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Calcula a coordenada cartesiana X referente a projeção da longitude.
  --              * SNYDER, J. P.; VOXLAND, P. M. An Album of Map Projections. p 233
  --
  -- Parameters:
  --   @p_latitude  = Coordenada geográfica da distância ao Equador em graus em notação decimal
  --   @p_longitude = Coordenada geográfica da distância ao meridiano de Greenwich em graus em notação decimal
  -- Returns:
  --   A coordenada X no plano cartesiano
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   29/10/26 Daniel Madeira: sinc(0) = 1 para evitar divisão por zero (ORA-01476) em (0, 0)
  --   02/11/26 Daniel Madeira: Proteção de intervalo [-1, 1] para acos
  --   
  -- =================================================================================================
  FUNCTION calcular_winkel_iii_x (
    p_latitude  IN NUMBER
  , p_longitude IN NUMBER
  )
  RETURN NUMBER
  IS
    l_latitude  NUMBER;
    l_longitude NUMBER;
    l_prod      NUMBER;
    l_alpha     NUMBER;
    l_sinc      NUMBER;
    
  BEGIN
    l_latitude := p_latitude * (c_pi / 180);
    l_longitude := p_longitude * (c_pi / 180);
    l_prod := cos(l_latitude) * cos(l_longitude / 2);
    IF (l_prod > 1) THEN
      l_prod := 1;
    ELSIF (l_prod < -1) THEN
      l_prod := -1;
    END IF;
    l_alpha := acos(l_prod);
    IF (l_alpha < 1E-10) THEN -- limite de sin(a)/a quando a -> 0
      l_sinc := 1;
    ELSE
      l_sinc := sin(l_alpha) / l_alpha;
    END IF;
    RETURN ((1 / 2) * (l_longitude * cos(acos(2 / c_pi)) + ((2 * cos(l_latitude) * sin(l_longitude / 2)) / (l_sinc))));
    
  END calcular_winkel_iii_x;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Calcula a coordenada cartesiana Y referente a projeção da latitude.
  --              * SNYDER, J. P.; VOXLAND, P. M. An Album of Map Projections. p 233
  --
  -- Parameters:
  --   @p_latitude  = Coordenada geográfica da distância ao Equador em graus em notação decimal
  --   @p_longitude = Coordenada geográfica da distância ao meridiano de Greenwich em graus em notação decimal
  -- Returns:
  --   A coordenada Y no plano cartesiano
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   29/10/26 Daniel Madeira: sinc(0) = 1 para evitar divisão por zero (ORA-01476) em (0, 0)
  --   02/11/26 Daniel Madeira: Proteção de intervalo [-1, 1] para acos
  --   
  -- =================================================================================================
  FUNCTION calcular_winkel_iii_y (
    p_latitude  IN NUMBER
  , p_longitude IN NUMBER
  )
  RETURN NUMBER
  IS
    l_latitude  NUMBER;
    l_longitude NUMBER;
    l_prod      NUMBER;
    l_alpha     NUMBER;
    l_sinc      NUMBER;
    
  BEGIN
    l_latitude := p_latitude * (c_pi / 180);
    l_longitude := p_longitude * (c_pi / 180);
    l_prod := cos(l_latitude) * cos(l_longitude / 2);
    IF (l_prod > 1) THEN
      l_prod := 1;
    ELSIF (l_prod < -1) THEN
      l_prod := -1;
    END IF;
    l_alpha := acos(l_prod);
    IF (l_alpha < 1E-10) THEN -- limite de sin(a)/a quando a -> 0
      l_sinc := 1;
    ELSE
      l_sinc := sin(l_alpha) / l_alpha;
    END IF;
    RETURN ((1 / 2) * (l_latitude + (sin(l_latitude) / (l_sinc))));
    
  END calcular_winkel_iii_y;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 02/11/2026
  -- Description: Calcula simultaneamente as coordenadas cartesianas X e Y da projeção de Winkel
  --              Tripel, compartilhando os termos intermediários alpha e sinc(alpha).
  --              * SNYDER, J. P.; VOXLAND, P. M. An Album of Map Projections. p 233
  --
  -- Parameters:
  --   @p_latitude  = Coordenada geográfica da distância ao Equador em graus em notação decimal
  --   @p_longitude = Coordenada geográfica da distância ao meridiano de Greenwich em graus em notação decimal
  --   @p_x         = Coordenada X no plano cartesiano (saída)
  --   @p_y         = Coordenada Y no plano cartesiano (saída)
  --
  -- Change History:
  --   02/11/26 Daniel Madeira: Versão inicial (alinhada com calcular_winkel_tripel em C)
  --
  -- =================================================================================================
  PROCEDURE calcular_winkel_iii (
    p_latitude  IN  NUMBER
  , p_longitude IN  NUMBER
  , p_x         OUT NUMBER
  , p_y         OUT NUMBER
  )
  IS
    l_latitude  NUMBER;
    l_longitude NUMBER;
    l_prod      NUMBER;
    l_alpha     NUMBER;
    l_sinc      NUMBER;

  BEGIN
    l_latitude := p_latitude * (c_pi / 180);
    l_longitude := p_longitude * (c_pi / 180);
    l_prod := cos(l_latitude) * cos(l_longitude / 2);
    IF (l_prod > 1) THEN
      l_prod := 1;
    ELSIF (l_prod < -1) THEN
      l_prod := -1;
    END IF;
    l_alpha := acos(l_prod);
    IF (l_alpha < 1E-10) THEN
      l_sinc := 1;
    ELSE
      l_sinc := sin(l_alpha) / l_alpha;
    END IF;
    p_x := (1 / 2) * (l_longitude * (2 / c_pi) + ((2 * cos(l_latitude) * sin(l_longitude / 2)) / l_sinc));
    p_y := (1 / 2) * (l_latitude + (sin(l_latitude) / l_sinc));

  END calcular_winkel_iii;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 06/09/2026
  -- Description: Calcula a coordenada cartesiana X referente a projeção da longitude.
  --              * ŠAVRIČ, B.; PATTERSON, T.; JENNY, B. The Equal Earth map projection. p 456
  --
  -- Parameters:
  --   @p_latitude  = Coordenada geográfica da distância ao Equador em graus em notação decimal
  --   @p_longitude = Coordenada geográfica da distância ao meridiano de Greenwich em graus em notação decimal
  --   @p_raio      = Raio
  -- Returns:
  --   A coordenada X no plano cartesiano
  --
  -- Change History:
  --   06/09/26 Daniel Madeira: Versão inicial
  --   02/11/26 Daniel Madeira: Proteção de intervalo [-1, 1] para asin
  --
  -- =================================================================================================
  FUNCTION calcular_equal_earth_x (
    p_latitude  IN NUMBER
  , p_longitude IN NUMBER
  , p_raio      IN NUMBER DEFAULT 1
  )
  RETURN NUMBER
  IS
    l_latitude  NUMBER;
    l_longitude NUMBER;
    l_arg       NUMBER;
    l_theta     NUMBER;
    l_t2        NUMBER;
    l_t6        NUMBER;
    l_sqrt3     CONSTANT NUMBER := SQRT(3);
    l_A1        CONSTANT NUMBER :=  1.340264;
    l_A2        CONSTANT NUMBER := -0.081106;
    l_A3        CONSTANT NUMBER :=  0.000893;
    l_A4        CONSTANT NUMBER :=  0.003796;

  BEGIN
    l_latitude := p_latitude * (c_pi / 180);
    l_longitude := p_longitude * (c_pi / 180);

    l_arg := (l_sqrt3 / 2) * sin(l_latitude);
    IF (l_arg > 1) THEN
      l_arg := 1;
    ELSIF (l_arg < -1) THEN
      l_arg := -1;
    END IF;
    l_theta := asin(l_arg);
    l_t2 := power(l_theta, 2);
    l_t6 := power(l_theta, 6);

    RETURN ((p_raio * 2 * l_sqrt3 * l_longitude * cos(l_theta)) /
            (3 * (l_A1 + 3 * l_A2 * l_t2 + l_t6 * (7 * l_A3 + 9 * l_A4 * l_t2))));

  END calcular_equal_earth_x;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 06/09/2026
  -- Description: Calcula a coordenada cartesiana Y referente a projeção da latitude.
  --              * ŠAVRIČ, B.; PATTERSON, T.; JENNY, B. The Equal Earth map projection. p 456
  --
  -- Parameters:
  --   @p_latitude  = Coordenada geográfica da distância ao Equador em graus em notação decimal
  --   @p_raio      = Raio
  -- Returns:
  --   A coordenada Y no plano cartesiano
  --
  -- Change History:
  --   06/09/26 Daniel Madeira: Versão inicial
  --   02/11/26 Daniel Madeira: Proteção de intervalo [-1, 1] para asin
  --
  -- =================================================================================================
  FUNCTION calcular_equal_earth_y (
    p_latitude IN NUMBER
  , p_raio     IN NUMBER DEFAULT 1
  )
  RETURN NUMBER
  IS
    l_latitude NUMBER;
    l_arg      NUMBER;
    l_theta    NUMBER;
    l_t2       NUMBER;
    l_t6       NUMBER;
    l_sqrt3    CONSTANT NUMBER := SQRT(3);
    l_A1       CONSTANT NUMBER :=  1.340264;
    l_A2       CONSTANT NUMBER := -0.081106;
    l_A3       CONSTANT NUMBER :=  0.000893;
    l_A4       CONSTANT NUMBER :=  0.003796;

  BEGIN
    l_latitude := p_latitude * (c_pi / 180);

    l_arg := (l_sqrt3 / 2) * sin(l_latitude);
    IF (l_arg > 1) THEN
      l_arg := 1;
    ELSIF (l_arg < -1) THEN
      l_arg := -1;
    END IF;
    l_theta := asin(l_arg);
    l_t2 := power(l_theta, 2);
    l_t6 := power(l_theta, 6);

    RETURN (p_raio * l_theta * (l_A1 + l_A2 * l_t2 + l_t6 * (l_A3 + l_A4 * l_t2)));

  END calcular_equal_earth_y;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 02/11/2026
  -- Description: Calcula simultaneamente as coordenadas cartesianas X e Y da projeção Equal Earth,
  --              compartilhando o ângulo paramétrico theta e suas potências.
  --              * ŠAVRIČ, B.; PATTERSON, T.; JENNY, B. The Equal Earth map projection. p 456
  --
  -- Parameters:
  --   @p_latitude  = Coordenada geográfica (ou autálica) da distância ao Equador em graus em notação decimal
  --   @p_longitude = Coordenada geográfica da distância ao meridiano de Greenwich em graus em notação decimal
  --   @p_x         = Coordenada X no plano cartesiano (saída)
  --   @p_y         = Coordenada Y no plano cartesiano (saída)
  --   @p_raio      = Raio
  --
  -- Change History:
  --   02/11/26 Daniel Madeira: Versão inicial (alinhada com calcular_equal_earth em C)
  --
  -- =================================================================================================
  PROCEDURE calcular_equal_earth (
    p_latitude  IN  NUMBER
  , p_longitude IN  NUMBER
  , p_x         OUT NUMBER
  , p_y         OUT NUMBER
  , p_raio      IN  NUMBER DEFAULT 1
  )
  IS
    l_latitude  NUMBER;
    l_longitude NUMBER;
    l_arg       NUMBER;
    l_theta     NUMBER;
    l_t2        NUMBER;
    l_t6        NUMBER;
    l_sqrt3     CONSTANT NUMBER := SQRT(3);
    l_A1        CONSTANT NUMBER :=  1.340264;
    l_A2        CONSTANT NUMBER := -0.081106;
    l_A3        CONSTANT NUMBER :=  0.000893;
    l_A4        CONSTANT NUMBER :=  0.003796;

  BEGIN
    l_latitude := p_latitude * (c_pi / 180);
    l_longitude := p_longitude * (c_pi / 180);

    l_arg := (l_sqrt3 / 2) * sin(l_latitude);
    IF (l_arg > 1) THEN
      l_arg := 1;
    ELSIF (l_arg < -1) THEN
      l_arg := -1;
    END IF;
    l_theta := asin(l_arg);
    l_t2 := l_theta * l_theta;
    l_t6 := l_t2 * l_t2 * l_t2;

    p_x := (p_raio * 2 * l_sqrt3 * l_longitude * cos(l_theta)) /
           (3 * (l_A1 + 3 * l_A2 * l_t2 + l_t6 * (7 * l_A3 + 9 * l_A4 * l_t2)));
    p_y := p_raio * l_theta * (l_A1 + l_A2 * l_t2 + l_t6 * (l_A3 + l_A4 * l_t2));

  END calcular_equal_earth;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 02/11/2026
  -- Description: Obtém os limites cartesianos máximos (semi-eixos X e Y) da projeção com cache.
  --
  -- Parameters:
  --   @p_projecao = Caractere que identifica o sistema de projeção do mapa terrestre
  --   @p_x_max    = Limite máximo no eixo X (Equador, longitude 180°)
  --   @p_y_max    = Limite máximo no eixo Y (Polo Norte, latitude 90°)
  --
  -- Change History:
  --   02/11/26 Daniel Madeira: Versão inicial (alinhada com obter_limites_projecao em C)
  --
  -- =================================================================================================
  PROCEDURE obter_limites_projecao (
    p_projecao IN  VARCHAR2
  , p_x_max    OUT NUMBER
  , p_y_max    OUT NUMBER
  )
  IS
  BEGIN
    IF (p_projecao = g_ultima_projecao AND g_ultima_projecao IS NOT NULL) THEN
      p_x_max := g_cache_x_max;
      p_y_max := g_cache_y_max;
      RETURN;
    END IF;

    CASE p_projecao
      WHEN 'e' THEN
        p_x_max := calcular_eckert_iv_x (calcular_eckert_iv_theta (0), 180);
        p_y_max := calcular_eckert_iv_y (calcular_eckert_iv_theta (90));
      WHEN 'E' THEN
        p_x_max := calcular_eckert_vi_x (calcular_eckert_vi_theta (0), 180);
        p_y_max := calcular_eckert_vi_y (calcular_eckert_vi_theta (90));
      WHEN 'g' THEN
        p_x_max := calcular_gott_equal_area_elliptical_x (0, 180);
        p_y_max := calcular_gott_equal_area_elliptical_y (90, 0);
      WHEN 'G' THEN
        p_x_max := sin(0.446 * c_pi);
        p_y_max := sin(0.446 * c_pi);
      WHEN 'h' THEN
        p_x_max := calcular_hammer_x (0, 180);
        p_y_max := calcular_hammer_y (90, 0);
      WHEN 'i' THEN
        p_x_max := calcular_equal_earth_x (converter_geografica_para_autalica (0), 180);
        p_y_max := calcular_equal_earth_y (converter_geografica_para_autalica (90));
      WHEN 'k' THEN
        p_x_max := calcular_kavrayskiy_vii_x (0, 180);
        p_y_max := calcular_kavrayskiy_vii_y (90);
      WHEN 'l' THEN
        p_x_max := 2;
        p_y_max := 2;
      WHEN 'm' THEN
        p_x_max := calcular_miller_x (180);
        p_y_max := calcular_miller_y (90);
      WHEN 'M' THEN
        p_x_max := calcular_mollweide_x (calcular_mollweide_theta (0), 180);
        p_y_max := calcular_mollweide_y (calcular_mollweide_theta (90));
      WHEN 'n' THEN
        p_x_max := calcular_natural_earth_x (0, 180);
        p_y_max := calcular_natural_earth_y (90);
      WHEN 'N' THEN
        p_x_max := calcular_natural_earth_ii_x (0, 180);
        p_y_max := calcular_natural_earth_ii_y (90);
      WHEN 'p' THEN
        p_x_max := calcular_patterson_x (180);
        p_y_max := calcular_patterson_y (90);
      WHEN 'r' THEN
        p_x_max := calcular_robinson_x (0, 180);
        p_y_max := calcular_robinson_y (90);
      WHEN 's' THEN
        p_x_max := calcular_sinusoidal_x (0, 180);
        p_y_max := calcular_sinusoidal_y (90);
      WHEN 't' THEN
        p_x_max := calcular_mercator_x (180);
        p_y_max := calcular_mercator_y (90);
      WHEN 'w' THEN
        p_x_max := calcular_wagner_vi_x (0, 180);
        p_y_max := calcular_wagner_vi_y (90);
      WHEN 'W' THEN
        p_x_max := calcular_winkel_iii_x (0, 180);
        p_y_max := calcular_winkel_iii_y (90, 0);
      ELSE
        p_x_max := 180;
        p_y_max := 90;
    END CASE;

    g_ultima_projecao := p_projecao;
    g_cache_x_max     := p_x_max;
    g_cache_y_max     := p_y_max;

  END obter_limites_projecao;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 02/11/2026
  -- Description: Ajusta a largura e a altura da imagem para a dimensão útil da projeção,
  --              eliminando bordas vazias excedentes no arquivo SVG final.
  --
  -- Parameters:
  --   @p_largura  = Tamanho horizontal da imagem do mapa em pontos (entrada/saída)
  --   @p_altura   = Tamanho vertical da imagem do mapa em pontos (entrada/saída)
  --   @p_projecao = Caractere que identifica o sistema de projeção do mapa terrestre
  --
  -- Change History:
  --   02/11/26 Daniel Madeira: Versão inicial (alinhada com ajustar_dimensoes_uteis em C)
  --
  -- =================================================================================================
  PROCEDURE ajustar_dimensoes_uteis (
    p_largura  IN OUT NUMBER
  , p_altura   IN OUT NUMBER
  , p_projecao IN     VARCHAR2
  )
  IS
    l_x_max    NUMBER;
    l_y_max    NUMBER;
    l_modulo_x NUMBER;
    l_modulo_y NUMBER;

  BEGIN
    p_largura := nvl(p_largura, 800);
    p_altura  := nvl(p_altura, 400);

    obter_limites_projecao (p_projecao, l_x_max, l_y_max);

    l_modulo_x := p_largura / (l_x_max * 2);
    l_modulo_y := p_altura / (l_y_max * 2);

    IF (l_modulo_x < l_modulo_y) THEN
      p_altura := round(l_y_max * 2 * l_modulo_x);
    ELSE
      p_largura := round(l_x_max * 2 * l_modulo_y);
    END IF;

    IF (p_largura < 1) THEN
      p_largura := 1;
    END IF;
    IF (p_altura < 1) THEN
      p_altura := 1;
    END IF;

  END ajustar_dimensoes_uteis;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Calcula a coordenada cartesiana X referente a projeção da longitude.
  --
  -- Parameters:
  --   @p_latitude  = Coordenada geográfica da distância ao Equador em graus em notação decimal
  --   @p_longitude = Coordenada geográfica da distância ao meridiano de Greenwich em graus em notação decimal
  --   @p_largura   = Tamanho horizontal da imagem do mapa em pontos
  --   @p_altura    = Tamanho vertical da imagem do mapa em pontos
  --   @p_projecao  = Caractere que identifica o sistema de projeção do mapa terrestre
  -- Returns:
  --   A coordenada X e Y no plano cartesiano
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   06/09/26 Daniel Madeira: Projeção Equal Earth
  --   20/09/26 Daniel Madeira: Projeção Gott–Mugnolo azimuthal centralizada no Equador
  --   20/09/26 Daniel Madeira: Projeção Lambert azimuthal equal-area com diâmetro máximo igual a 4
  --   02/11/26 Daniel Madeira: Escala via obter_limites_projecao e arredondamento com round
  --   
  -- =================================================================================================
  FUNCTION converter_geo_pixel (
    p_latitude  IN NUMBER
  , p_longitude IN NUMBER
  , p_largura   IN NUMBER
  , p_altura    IN NUMBER
  , p_projecao  IN VARCHAR2
  )
  RETURN coordenadas_xy_t
  IS
    l_centro       coordenadas_xy_t;
    l_xy           coordenadas_xy_t;
    l_x_proj       NUMBER;
    l_y_proj       NUMBER;
    l_x_max        NUMBER;
    l_y_max        NUMBER;
    l_modulo_x     NUMBER;
    l_modulo_y     NUMBER;
    l_modulo       NUMBER;
    l_theta        NUMBER;
    l_lat_autalica NUMBER;
  
  BEGIN
    l_centro := coordenar_centro (p_largura, p_altura);
    
    CASE
      WHEN (p_projecao = 'e') THEN -- Eckert IV projection
        l_theta  := calcular_eckert_iv_theta (p_latitude);
        l_x_proj := calcular_eckert_iv_x (l_theta, p_longitude);
        l_y_proj := calcular_eckert_iv_y (l_theta);
            
      WHEN (p_projecao = 'E') THEN -- Eckert VI projection
        l_theta  := calcular_eckert_vi_theta (p_latitude);
        l_x_proj := calcular_eckert_vi_x (l_theta, p_longitude);
        l_y_proj := calcular_eckert_vi_y (l_theta);
           
      WHEN (p_projecao = 'g') THEN -- Gott equal-area elliptical projection
        calcular_gott_equal_area_elliptical (p_latitude, p_longitude, l_x_proj, l_y_proj);
         
      WHEN (p_projecao = 'G') THEN -- Gott–Mugnolo azimuthal projection
        calcular_gott_mugnolo_azimuthal (p_latitude, p_longitude, l_x_proj, l_y_proj);
            
      WHEN (p_projecao = 'h') THEN -- Hammer projection
        calcular_hammer (p_latitude, p_longitude, l_x_proj, l_y_proj);

      WHEN (p_projecao = 'i') THEN -- Equal Earth projection
        l_lat_autalica := converter_geografica_para_autalica (p_latitude);
        calcular_equal_earth (l_lat_autalica, p_longitude, l_x_proj, l_y_proj);

      WHEN (p_projecao = 'k') THEN -- Kavrayskiy VII projection
        l_x_proj := calcular_kavrayskiy_vii_x (p_latitude, p_longitude);
        l_y_proj := calcular_kavrayskiy_vii_y (p_latitude);

      WHEN (p_projecao = 'l') THEN -- Lambert azimuthal equal-area projection
        calcular_lambert_azimuthal_equal_area (p_latitude, p_longitude, l_x_proj, l_y_proj);
            
      WHEN (p_projecao = 'm') THEN -- Miller cylindrical projection
        l_x_proj := calcular_miller_x (p_longitude);
        l_y_proj := calcular_miller_y (p_latitude);
            
      WHEN (p_projecao = 'M') THEN -- Mollweide projection
        l_theta  := calcular_mollweide_theta (p_latitude);
        l_x_proj := calcular_mollweide_x (l_theta, p_longitude);
        l_y_proj := calcular_mollweide_y (l_theta);
            
      WHEN (p_projecao = 'n') THEN -- Natural Earth projection
        l_x_proj := calcular_natural_earth_x (p_latitude, p_longitude);
        l_y_proj := calcular_natural_earth_y (p_latitude);
            
      WHEN (p_projecao = 'N') THEN -- Natural Earth II projection
        l_x_proj := calcular_natural_earth_ii_x (p_latitude, p_longitude);
        l_y_proj := calcular_natural_earth_ii_y (p_latitude);
            
      WHEN (p_projecao = 'p') THEN -- Patterson projection
        l_x_proj := calcular_patterson_x (p_longitude);
        l_y_proj := calcular_patterson_y (p_latitude);
            
      WHEN (p_projecao = 'r') THEN -- Robinson projection
        l_x_proj := calcular_robinson_x (p_latitude, p_longitude);
        l_y_proj := calcular_robinson_y (p_latitude);
            
      WHEN (p_projecao = 's') THEN -- sinusoidal projection
        l_x_proj := calcular_sinusoidal_x (p_latitude, p_longitude);
        l_y_proj := calcular_sinusoidal_y (p_latitude);
            
      WHEN (p_projecao = 't') THEN -- Mercator projection
        l_x_proj := calcular_mercator_x (p_longitude);
        l_y_proj := calcular_mercator_y (p_latitude);
          
      WHEN (p_projecao = 'w') THEN -- Wagner VI projection
        l_x_proj := calcular_wagner_vi_x (p_latitude, p_longitude);
        l_y_proj := calcular_wagner_vi_y (p_latitude);
            
      WHEN (p_projecao = 'W') THEN -- Winkel tripel projection
        calcular_winkel_iii (p_latitude, p_longitude, l_x_proj, l_y_proj);
            
      ELSE -- c: equirectangular projection -> plate carrée
           -- SNYDER, J. P.; VOXLAND, P. M. An Album of Map Projections. p 219
        l_x_proj := p_longitude;
        l_y_proj := p_latitude;
    END CASE;

    obter_limites_projecao (p_projecao, l_x_max, l_y_max);
    l_modulo_x := p_largura / (l_x_max * 2);
    l_modulo_y := p_altura / (l_y_max * 2);
    IF (l_modulo_x < l_modulo_y) THEN
      IF (round(l_x_max * 2 * l_modulo_y) <= p_largura) THEN
        l_modulo := l_modulo_y;
      ELSE
        l_modulo := l_modulo_x;
      END IF;
    ELSE
      IF (round(l_y_max * 2 * l_modulo_x) <= p_altura) THEN
        l_modulo := l_modulo_x;
      ELSE
        l_modulo := l_modulo_y;
      END IF;
    END IF;

    l_xy('x') := round(l_centro('x') + (l_x_proj * l_modulo));
    l_xy('y') := round(l_centro('y') - (l_y_proj * l_modulo));
    RETURN l_xy;
    
  END converter_geo_pixel;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Constrói o caminho de um paralelo.
  --
  -- Parameters:
  --   @p_latitude = Coordenada geográfica da distância ao Equador em graus em notação decimal
  --   @p_largura  = Tamanho horizontal da imagem do mapa em pontos
  --   @p_altura   = Tamanho vertical da imagem do mapa em pontos
  --   @p_projecao = Caractere que identifica o sistema de projeção do mapa terrestre
  -- Returns:
  --   O elemento <path> em SVG para um paralelo
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   16/01/22 Daniel Madeira: Comparação com ponto anterior
  --   02/11/26 Daniel Madeira: Resolução de amostragem ampliada de 10° para 2°
  --   
  -- =================================================================================================
  FUNCTION montar_caminho_paralelo (
    p_latitude IN NUMBER
  , p_largura  IN NUMBER
  , p_altura   IN NUMBER
  , p_projecao IN VARCHAR2
  )
  RETURN VARCHAR2
  IS
    l_svg       VARCHAR2(32767);
    l_ponto     coordenadas_xy_t;
    l_anterior  coordenadas_xy_t;
    l_longitude NUMBER;
    
  BEGIN
    l_ponto := converter_geo_pixel (p_latitude, -180, p_largura, p_altura, p_projecao);
    l_svg := '<path d="M' || l_ponto('x') || ',' || l_ponto('y');
    l_anterior('x') := l_ponto('x');
    l_anterior('y') := l_ponto('y');
    FOR lon IN -90 .. 90 LOOP
      l_longitude := lon * 2;
      l_ponto := converter_geo_pixel (p_latitude, l_longitude, p_largura, p_altura, p_projecao);
      IF (l_ponto('x') != l_anterior('x')) OR (l_ponto('y') != l_anterior('y')) THEN
        l_svg := l_svg || ' L' || l_ponto('x') || ',' || l_ponto('y');
        l_anterior('x') := l_ponto('x');
        l_anterior('y') := l_ponto('y');
      END IF;
    END LOOP;
    l_svg := l_svg || '" />' || chr(13) || chr(10);
    RETURN l_svg;
    
  END montar_caminho_paralelo;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Constrói a linha de um paralelo.
  --
  -- Parameters:
  --   @p_latitude = Coordenada geográfica da distância ao Equador em graus em notação decimal
  --   @p_largura  = Tamanho horizontal da imagem do mapa em pontos
  --   @p_altura   = Tamanho vertical da imagem do mapa em pontos
  --   @p_projecao = Caractere que identifica o sistema de projeção do mapa terrestre
  -- Returns:
  --   O elemento <line> em SVG para um paralelo
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   
  -- =================================================================================================
  FUNCTION montar_linha_paralelo (
    p_latitude IN NUMBER
  , p_largura  IN NUMBER
  , p_altura   IN NUMBER
  , p_projecao IN VARCHAR2
  )
  RETURN VARCHAR2
  IS
    l_svg      VARCHAR2(4000);
    l_ocidente coordenadas_xy_t;
    l_oriente  coordenadas_xy_t;
    
  BEGIN
    l_ocidente := converter_geo_pixel (p_latitude, -180, p_largura, p_altura, p_projecao);
    l_oriente := converter_geo_pixel (p_latitude, 180, p_largura, p_altura, p_projecao);
    l_svg := '<line x1="' || l_ocidente('x') || '" y1="' || l_ocidente('y') || '" x2="' || l_oriente('x') || '" y2="' || l_oriente('y') || '" />' || chr(13) || chr(10);
    RETURN l_svg;
    
  END montar_linha_paralelo;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Invoca a construção dos paralelos.
  --
  -- Parameters:
  --   @p_largura  = Tamanho horizontal da imagem do mapa em pontos
  --   @p_altura   = Tamanho vertical da imagem do mapa em pontos
  --   @p_projecao = Caractere que identifica o sistema de projeção do mapa terrestre
  --   @p_svg      = Acumulador onde os elementos <path> ou <line> em SVG dos paralelos são anexados
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   20/09/26 Daniel Madeira: Projeção Gott-Mugnolo (G) incluída na montagem por caminho
  --   29/10/26 Daniel Madeira: Convertida em procedimento que anexa ao acumulador (sem limite de 32767)
  --   02/11/26 Daniel Madeira: Remoção dos paralelos degenerados de +-90° (polos)
  --   
  -- =================================================================================================
  PROCEDURE exibir_paralelos (
    p_largura  IN NUMBER
  , p_altura   IN NUMBER
  , p_projecao IN VARCHAR2
  , p_svg      IN OUT NOCOPY svg_t
  )
  IS
    TYPE lat_type IS TABLE OF NUMBER;
    l_lat lat_type := lat_type (15, 30, 45, 60, 75);
    
  BEGIN
    FOR i IN 1 .. l_lat.count LOOP
      IF p_projecao IN ('W', 'h', 'g', 'G', 'l') THEN
        svg_anexar (p_svg, montar_caminho_paralelo (l_lat(i), p_largura, p_altura, p_projecao));
        svg_anexar (p_svg, montar_caminho_paralelo ((-1 * l_lat(i)), p_largura, p_altura, p_projecao));
      ELSE
        svg_anexar (p_svg, montar_linha_paralelo (l_lat(i), p_largura, p_altura, p_projecao));
        svg_anexar (p_svg, montar_linha_paralelo ((-1 * l_lat(i)), p_largura, p_altura, p_projecao));
      END IF;
    END LOOP;
    
  END exibir_paralelos;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Constrói o caminho de um meridiano.
  --
  -- Parameters:
  --   @p_longitude = Coordenada geográfica da distância ao meridiano de Greenwich em graus em notação decimal
  --   @p_largura   = Tamanho horizontal da imagem do mapa em pontos
  --   @p_altura    = Tamanho vertical da imagem do mapa em pontos
  --   @p_projecao  = Caractere que identifica o sistema de projeção do mapa terrestre
  -- Returns:
  --   O elemento <path> em SVG para um meridiano
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   16/01/22 Daniel Madeira: Comparação com ponto anterior
  --   02/11/26 Daniel Madeira: Salto em lat = 0 no meridiano +-180 das projeções azimutais (l e G)
  --   
  -- =================================================================================================
  FUNCTION montar_caminho_meridiano (
    p_longitude IN NUMBER
  , p_largura   IN NUMBER
  , p_altura    IN NUMBER
  , p_projecao  IN VARCHAR2
  )
  RETURN VARCHAR2
  IS
    l_svg      VARCHAR2(32767);
    l_ponto    coordenadas_xy_t;
    l_anterior coordenadas_xy_t;
    l_latitude NUMBER;
    
  BEGIN
    l_ponto := converter_geo_pixel (-90, p_longitude, p_largura, p_altura, p_projecao);
    l_svg := '<path d="M' || l_ponto('x') || ',' || l_ponto('y');
    l_anterior('x') := l_ponto('x');
    l_anterior('y') := l_ponto('y');
    FOR lat IN -45 .. 45 LOOP
      l_latitude := lat * 2;
      IF (p_projecao IN ('l', 'G') AND abs(p_longitude) = 180 AND l_latitude = 0) THEN
        CONTINUE;
      END IF;
      l_ponto := converter_geo_pixel (l_latitude, p_longitude, p_largura, p_altura, p_projecao);
      IF (p_projecao IN ('l', 'G') AND abs(p_longitude) = 180 AND l_latitude = 2) THEN
        l_svg := l_svg || ' M' || l_ponto('x') || ',' || l_ponto('y');
        l_anterior('x') := l_ponto('x');
        l_anterior('y') := l_ponto('y');
      ELSIF (l_ponto('x') != l_anterior('x')) OR (l_ponto('y') != l_anterior('y')) THEN
        l_svg := l_svg || ' L' || l_ponto('x') || ',' || l_ponto('y');
        l_anterior('x') := l_ponto('x');
        l_anterior('y') := l_ponto('y');
      END IF;
    END LOOP;
    l_svg := l_svg || '" />' || chr(13) || chr(10);
    RETURN l_svg;
    
  END montar_caminho_meridiano;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Invoca a construção dos meridianos.
  --
  -- Parameters:
  --   @p_largura  = Tamanho horizontal da imagem do mapa em pontos
  --   @p_altura   = Tamanho vertical da imagem do mapa em pontos
  --   @p_projecao = Caractere que identifica o sistema de projeção do mapa terrestre
  --   @p_svg      = Acumulador onde os elementos <path> em SVG dos meridianos são anexados
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   29/10/26 Daniel Madeira: Convertida em procedimento que anexa ao acumulador (sem limite de 32767)
  --   
  -- =================================================================================================
  PROCEDURE exibir_meridianos (
    p_largura  IN NUMBER
  , p_altura   IN NUMBER
  , p_projecao IN VARCHAR2
  , p_svg      IN OUT NOCOPY svg_t
  )
  IS
    TYPE lon_type IS TABLE OF NUMBER;
    l_lon lon_type := lon_type (15, 30, 45, 60, 75, 90, 105, 120, 135, 150, 165, 180);
    
  BEGIN
    FOR i IN 1 .. l_lon.count LOOP
      svg_anexar (p_svg, montar_caminho_meridiano ((-1 * l_lon(i)), p_largura, p_altura, p_projecao));
      svg_anexar (p_svg, montar_caminho_meridiano (l_lon(i), p_largura, p_altura, p_projecao));
    END LOOP;
    
  END exibir_meridianos;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Constrói o caminho de um círculo.
  --
  -- Parameters:
  --   @p_latitude = Coordenada geográfica da distância ao Equador em graus em notação decimal
  --   @p_largura  = Tamanho horizontal da imagem do mapa em pontos
  --   @p_altura   = Tamanho vertical da imagem do mapa em pontos
  --   @p_projecao = Caractere que identifica o sistema de projeção do mapa terrestre
  -- Returns:
  --   O elemento <path> em SVG para um círculo
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   16/01/22 Daniel Madeira: Comparação com ponto anterior
  --   02/11/26 Daniel Madeira: Resolução de amostragem ampliada de 10° para 2°
  --   
  -- =================================================================================================
  FUNCTION montar_caminho_circulo (
    p_latitude IN NUMBER
  , p_largura  IN NUMBER
  , p_altura   IN NUMBER
  , p_projecao IN VARCHAR2
  )
  RETURN VARCHAR2
  IS
    l_svg       VARCHAR2(32767);
    l_ponto     coordenadas_xy_t;
    l_anterior  coordenadas_xy_t;
    l_longitude NUMBER;
    
  BEGIN
    l_ponto := converter_geo_pixel (p_latitude, -180, p_largura, p_altura, p_projecao);
    l_svg := '<path d="M' || l_ponto('x') || ',' || l_ponto('y');
    l_anterior('x') := l_ponto('x');
    l_anterior('y') := l_ponto('y');
    FOR lon IN -90 .. 90 LOOP
      l_longitude := lon * 2;
      l_ponto := converter_geo_pixel (p_latitude, l_longitude, p_largura, p_altura, p_projecao);
      IF (l_ponto('x') != l_anterior('x')) OR (l_ponto('y') != l_anterior('y')) THEN
        l_svg := l_svg || ' L' || l_ponto('x') || ',' || l_ponto('y');
        l_anterior('x') := l_ponto('x');
        l_anterior('y') := l_ponto('y');
      END IF;
    END LOOP;
    l_svg := l_svg || '" stroke-dasharray="10,10" />' || chr(13) || chr(10);
    RETURN l_svg;
    
  END montar_caminho_circulo;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Constrói a linha de um círculo.
  --
  -- Parameters:
  --   @p_latitude = Coordenada geográfica da distância ao Equador em graus em notação decimal
  --   @p_largura  = Tamanho horizontal da imagem do mapa em pontos
  --   @p_altura   = Tamanho vertical da imagem do mapa em pontos
  --   @p_projecao = Caractere que identifica o sistema de projeção do mapa terrestre
  -- Returns:
  --   O elemento <line> em SVG para um círculo
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   
  -- =================================================================================================
  FUNCTION montar_linha_circulo (
    p_latitude IN NUMBER
  , p_largura  IN NUMBER
  , p_altura   IN NUMBER
  , p_projecao IN VARCHAR2
  )
  RETURN VARCHAR2
  IS
    l_svg      VARCHAR2(4000);
    l_ocidente coordenadas_xy_t;
    l_oriente  coordenadas_xy_t;
  
  BEGIN
    l_ocidente := converter_geo_pixel (p_latitude, -180, p_largura, p_altura, p_projecao);
    l_oriente := converter_geo_pixel (p_latitude, 180, p_largura, p_altura, p_projecao);
    l_svg := '<line x1="' || l_ocidente('x') || '" y1="' || l_ocidente('y') || '" x2="' || l_oriente('x') || '" y2="' || l_oriente('y') || '" stroke-dasharray="10,10" />' || chr(13) || chr(10);
    RETURN l_svg;
    
  END montar_linha_circulo;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Invoca a construção dos círculos e trópicos.
  --
  -- Parameters:
  --   @p_largura  = Tamanho horizontal da imagem do mapa em pontos
  --   @p_altura   = Tamanho vertical da imagem do mapa em pontos
  --   @p_projecao = Caractere que identifica o sistema de projeção do mapa terrestre
  --   @p_svg      = Acumulador onde os elementos <path> ou <line> em SVG dos círculos e trópicos são anexados
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   20/09/26 Daniel Madeira: Projeção Gott-Mugnolo (G) incluída na montagem por caminho
  --   29/10/26 Daniel Madeira: Convertida em procedimento que anexa ao acumulador (sem limite de 32767)
  --   
  -- =================================================================================================
  PROCEDURE exibir_circulos (
    p_largura  IN NUMBER
  , p_altura   IN NUMBER
  , p_projecao IN VARCHAR2
  , p_svg      IN OUT NOCOPY svg_t
  )
  IS
    l_cancer      NUMBER := 23.43656;
    l_capricornio NUMBER := -23.43656;
    l_artico      NUMBER := 66.5622;
    l_antartico   NUMBER := -66.5622;
    
  BEGIN
    IF p_projecao IN ('W', 'h', 'g', 'G', 'l') THEN
      svg_anexar (p_svg, montar_caminho_circulo (l_cancer, p_largura, p_altura, p_projecao));
      svg_anexar (p_svg, montar_caminho_circulo (l_capricornio, p_largura, p_altura, p_projecao));
      svg_anexar (p_svg, montar_caminho_circulo (l_artico, p_largura, p_altura, p_projecao));
      svg_anexar (p_svg, montar_caminho_circulo (l_antartico, p_largura, p_altura, p_projecao));
    ELSE
      svg_anexar (p_svg, montar_linha_circulo (l_cancer, p_largura, p_altura, p_projecao));
      svg_anexar (p_svg, montar_linha_circulo (l_capricornio, p_largura, p_altura, p_projecao));
      svg_anexar (p_svg, montar_linha_circulo (l_artico, p_largura, p_altura, p_projecao));
      svg_anexar (p_svg, montar_linha_circulo (l_antartico, p_largura, p_altura, p_projecao));
    END IF;
    
  END exibir_circulos;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Constrói a linha do Equador.
  --
  -- Parameters:
  --   @p_largura  = Tamanho horizontal da imagem do mapa em pontos
  --   @p_altura   = Tamanho vertical da imagem do mapa em pontos
  --   @p_projecao = Caractere que identifica o sistema de projeção do mapa terrestre
  -- Returns:
  --   O elemento <line> em SVG para o Equador
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   20/09/26 Daniel Madeira: Projeção Gott-Mugnolo (G) incluída no W/E fixo
  --   02/11/26 Daniel Madeira: Uso de -180 e 180 exatos com o tratamento de antípoda em l e G
  --   
  -- =================================================================================================
  FUNCTION exibir_equador (
    p_largura  IN NUMBER
  , p_altura   IN NUMBER
  , p_projecao IN VARCHAR2
  )
  RETURN VARCHAR2
  IS
    l_ocidente coordenadas_xy_t;
    l_oriente  coordenadas_xy_t;
    l_svg      VARCHAR2(32767);
  
  BEGIN
    l_ocidente := converter_geo_pixel (0, -180, p_largura, p_altura, p_projecao);
    l_oriente := converter_geo_pixel (0, 180, p_largura, p_altura, p_projecao);
    l_svg := '<line x1="' || l_ocidente('x') || '" y1="' || l_ocidente('y') || '" x2="' || l_oriente('x') || '" y2="' || l_oriente('y') || '" />' || chr(13) || chr(10);
    RETURN l_svg;
    
  END exibir_equador;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Constrói a linha do meridiano de Greenwich.
  --
  -- Parameters:
  --   @p_largura  = Tamanho horizontal da imagem do mapa em pontos
  --   @p_altura   = Tamanho vertical da imagem do mapa em pontos
  --   @p_projecao = Caractere que identifica o sistema de projeção do mapa terrestre
  -- Returns:
  --   O elemento <line> em SVG para o meridiano de Greenwich
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   
  -- =================================================================================================
  FUNCTION exibir_greenwich (
    p_largura  IN NUMBER
  , p_altura   IN NUMBER
  , p_projecao IN VARCHAR2
  )
  RETURN VARCHAR2
  IS
    l_n   coordenadas_xy_t;
    l_s   coordenadas_xy_t;
    l_svg VARCHAR2(32767);
    
  BEGIN
    l_n := converter_geo_pixel (90, 0, p_largura, p_altura, p_projecao);
    l_s := converter_geo_pixel (-90, 0, p_largura, p_altura, p_projecao);
    l_svg := '<line x1="' || l_n('x') || '" y1="' || l_n('y') || '" x2="' || l_s('x') || '" y2="' || l_s('y') || '" />' || chr(13) || chr(10);
    RETURN l_svg;
    
  END exibir_greenwich;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Invoca a construção dos paralelos, meridianos, círculos e Equador.
  --
  -- Parameters:
  --   @p_largura  = Tamanho horizontal da imagem do mapa em pontos
  --   @p_altura   = Tamanho vertical da imagem do mapa em pontos
  --   @p_projecao = Caractere que identifica o sistema de projeção do mapa terrestre
  -- Returns:
  --   Os elementos em SVG para as linhas de paralelos, meridianos, círculos e Equador
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   29/10/26 Daniel Madeira: Retorno em CLOB
  --   02/11/26 Daniel Madeira: Ajuste para as dimensões úteis da projeção
  --   
  -- =================================================================================================
  FUNCTION exibir_grade (
    p_largura  IN NUMBER   DEFAULT 800
  , p_altura   IN NUMBER   DEFAULT 400
  , p_projecao IN VARCHAR2 DEFAULT 'c'
  )
  RETURN CLOB
  IS
    l_svg      svg_t;
    l_largura  NUMBER      := p_largura;
    l_altura   NUMBER      := p_altura;
    l_projecao VARCHAR2(1) := normalizar_projecao (p_projecao);
    
  BEGIN
    ajustar_dimensoes_uteis (l_largura, l_altura, l_projecao);
    svg_iniciar (l_svg);
    svg_anexar (l_svg, '<g fill="none" stroke="rgb(240,240,240)" stroke-width="1">' || chr(13) || chr(10));
    
    exibir_paralelos (l_largura, l_altura, l_projecao, l_svg);
    exibir_meridianos (l_largura, l_altura, l_projecao, l_svg);
    exibir_circulos (l_largura, l_altura, l_projecao, l_svg);
    svg_anexar (l_svg, exibir_equador (l_largura, l_altura, l_projecao));
    svg_anexar (l_svg, exibir_greenwich (l_largura, l_altura, l_projecao));
    
    svg_anexar (l_svg, '</g>' || chr(13) || chr(10));
    RETURN svg_finalizar (l_svg);
    
  END exibir_grade;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Constrói os pontos das cidades capitais.
  --
  -- Parameters:
  --   @p_largura  = Tamanho horizontal da imagem do mapa em pontos
  --   @p_altura   = Tamanho vertical da imagem do mapa em pontos
  --   @p_projecao = Caractere que identifica o sistema de projeção do mapa terrestre
  --   @p_raio     = Tamanho (raio) em pontos do marcador das cidades (opcional; padrão proporcional à largura)
  -- Returns:
  --   Os elementos <circle> em SVG para as posições das cidades
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   29/10/26 Daniel Madeira: Retorno em CLOB
  --   02/11/26 Daniel Madeira: Ajuste para as dimensões úteis da projeção, raio mínimo >= 1
  --                            (fator 0.003), parâmetro opcional p_raio e validação dos intervalos geográficos
  --   
  -- =================================================================================================
  FUNCTION exibir_cidades (
    p_largura  IN NUMBER   DEFAULT 800
  , p_altura   IN NUMBER   DEFAULT 400
  , p_projecao IN VARCHAR2 DEFAULT 'c'
  , p_raio     IN NUMBER   DEFAULT NULL
  )
  RETURN CLOB
  IS
    CURSOR c_cidades
    IS
      SELECT city, lat, lng, country, id
        FROM worldcities
       WHERE lat IS NOT NULL
         AND lng IS NOT NULL
         AND lat BETWEEN -90 AND 90
         AND lng BETWEEN -180 AND 180
         AND capital = 'primary'
    ORDER BY population;
    
    l_svg       svg_t;
    l_largura   NUMBER      := p_largura;
    l_altura    NUMBER      := p_altura;
    l_projecao  VARCHAR2(1) := normalizar_projecao (p_projecao);
    l_r         NUMBER;
    l_latitude  NUMBER;
    l_longitude NUMBER;
    l_ponto     coordenadas_xy_t;
    l_titulo    VARCHAR2(200);
    
  BEGIN
    ajustar_dimensoes_uteis (l_largura, l_altura, l_projecao);
    svg_iniciar (l_svg);
    svg_anexar (l_svg, '<g fill="rgb(33,42,116)" stroke-width="0" onmouseover="evt.target.setAttribute(''fill'', ''blue'');" onmouseout="evt.target.setAttribute(''fill'',''red'');">' || chr(13) || chr(10));
    IF (p_raio IS NOT NULL AND p_raio > 0) THEN
      l_r := round(p_raio);
    ELSE
      l_r := round(l_largura * 0.003);
    END IF;
    IF (l_r < 1) THEN
      l_r := 1;
    END IF;
    FOR cidade IN c_cidades LOOP
      l_latitude := cidade.lat;
      l_longitude := cidade.lng;
      IF (l_latitude >= -90 AND l_latitude <= 90 AND l_longitude >= -180 AND l_longitude <= 180) THEN
        l_ponto := converter_geo_pixel (l_latitude, l_longitude, l_largura, l_altura, l_projecao);
        l_titulo := cidade.city || ' (' || cidade.country || ')';
        svg_anexar (l_svg, '<circle id="' || cidade.id || '" cx="' || l_ponto('x') || '" cy="' || l_ponto('y') || '" r="' || l_r || '"><title>' || l_titulo || '</title></circle>' || chr(13) || chr(10));
      END IF;
    END LOOP;
    svg_anexar (l_svg, '</g>' || chr(13) || chr(10));
    
    RETURN svg_finalizar (l_svg);
    
  END exibir_cidades;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Converte o polígono de coordenadas geográficas para pontos cartesianos.
  --
  -- Parameters:
  --   @p_poligono = Coordenadas geográficas do polígono que representa o país
  --   @p_largura  = Tamanho horizontal da imagem do mapa em pontos
  --   @p_altura   = Tamanho vertical da imagem do mapa em pontos
  --   @p_projecao = Caractere que identifica o sistema de projeção do mapa terrestre
  --   @p_svg      = Acumulador onde o elemento <polygon> em SVG é anexado
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   29/10/26 Daniel Madeira: Convertida em procedimento que anexa o <polygon> ao acumulador
  --                            (remove o limite de 8000 bytes por anel) e não descarta mais um
  --                            primeiro ponto que caia exatamente no pixel (0, 0)
  --   02/11/26 Daniel Madeira: Validação de intervalos geográficos (lat em [-90, 90], lon em [-180, 180])
  --   
  -- =================================================================================================
  PROCEDURE extrair_poligono (
    p_poligono IN json_array_t
  , p_largura  IN NUMBER
  , p_altura   IN NUMBER
  , p_projecao IN VARCHAR2
  , p_svg      IN OUT NOCOPY svg_t
  )
  IS
    l_coordenadas json_array_t;
    l_latitude    NUMBER;
    l_longitude   NUMBER;
    l_ponto       coordenadas_xy_t;
    l_anterior    coordenadas_xy_t;
    l_aberto      BOOLEAN := false;
    
  BEGIN
    IF (p_poligono IS NULL) OR (p_poligono.get_size = 0) THEN
      RETURN;
    END IF;
    FOR i IN 0 .. p_poligono.get_size - 1 LOOP
      l_coordenadas := treat(p_poligono.get(i) AS json_array_t);
      l_latitude := l_coordenadas.get_number(1);
      l_longitude := l_coordenadas.get_number(0);
      IF (l_latitude >= -90 AND l_latitude <= 90 AND l_longitude >= -180 AND l_longitude <= 180) THEN
        l_ponto := converter_geo_pixel (l_latitude, l_longitude, p_largura, p_altura, p_projecao);
        IF NOT l_aberto THEN
          svg_anexar (p_svg, '<polygon points="' || l_ponto('x') || ',' || l_ponto('y'));
          l_aberto := true;
          l_anterior('x') := l_ponto('x');
          l_anterior('y') := l_ponto('y');
        ELSIF (l_ponto('x') != l_anterior('x')) OR (l_ponto('y') != l_anterior('y')) THEN
          svg_anexar (p_svg, ' ' || l_ponto('x') || ',' || l_ponto('y'));
          l_anterior('x') := l_ponto('x');
          l_anterior('y') := l_ponto('y');
        END IF;
      END IF;
    END LOOP;
    IF l_aberto THEN
      svg_anexar (p_svg, '" />' || chr(13) || chr(10));
    END IF;
    
  END extrair_poligono;
  
  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Carrega o arquivo JSON.
  --
  -- Returns:
  --   O arquivo JSON com as coordenadas geográficas
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   
  -- =================================================================================================
  FUNCTION carregar_arquivo_json
  RETURN BLOB
  IS
    CURSOR c_json
    IS
    SELECT blob_content
      FROM repositorio
     WHERE repositorio_id = 1;
    
    l_json BLOB;
  
  BEGIN
    OPEN c_json;
    FETCH c_json INTO l_json;
    CLOSE c_json;
    RETURN l_json;
    
  END carregar_arquivo_json;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 06/03/2022
  -- Description: Constrói os polígonos dos países.
  --
  -- Parameters:
  --   @p_continente = Nome do continente
  --                   ('Asia' 'Africa' 'Europe' 'South America' 'Antarctica' 'Seven seas (open ocean)' 'Oceania' 'North America')
  --   @p_largura    = Tamanho horizontal da imagem do mapa em pontos
  --   @p_altura     = Tamanho vertical da imagem do mapa em pontos
  --   @p_projecao   = Caractere que identifica o sistema de projeção do mapa terrestre
  -- Returns:
  --   Os elementos <polygon> em SVG para os países
  --
  -- Change History:
  --   06/03/22 Daniel Madeira: Versão inicial
  --   29/10/26 Daniel Madeira: Retorno em CLOB
  --   02/11/26 Daniel Madeira: Ajuste para as dimensões úteis da projeção
  --   
  -- =================================================================================================
  FUNCTION exibir_continente (
    p_continente IN VARCHAR2
  , p_largura    IN NUMBER   DEFAULT 800
  , p_altura     IN NUMBER   DEFAULT 400
  , p_projecao   IN VARCHAR2 DEFAULT 'c'
  )
  RETURN CLOB
  IS
    l_svg         svg_t;
    l_largura     NUMBER      := p_largura;
    l_altura      NUMBER      := p_altura;
    l_projecao    VARCHAR2(1) := normalizar_projecao (p_projecao);
    l_blob        BLOB;
    l_object      json_object_t;
    l_features    json_array_t;
    l_pais        json_object_t;
    l_geometry    json_object_t;
    l_type        VARCHAR2(200);
    l_properties  json_object_t;
    l_continent   VARCHAR2(200);
    l_coordinates json_array_t;
    l_nested_1    json_array_t;
    l_nested_2    json_array_t;
  BEGIN
    ajustar_dimensoes_uteis (l_largura, l_altura, l_projecao);
    svg_iniciar (l_svg);
    svg_anexar (l_svg, '<g fill="rgb(200,200,200)" fill-rule="nonzero" stroke="rgb(152,152,152)" stroke-width="1">' || chr(13) || chr(10));
    
    l_blob := carregar_arquivo_json;
    l_object := new json_object_t (l_blob);
    l_features := treat(l_object.get('features') AS json_array_t);
    
    FOR i IN 0 .. l_features.get_size - 1 LOOP
      l_pais := treat(l_features.get(i) AS json_object_t);
      l_properties := treat(l_pais.get('properties') AS json_object_t);
      l_continent := l_properties.get_string('CONTINENT');
      IF (l_continent = p_continente) THEN
        l_geometry := treat(l_pais.get('geometry') AS json_object_t);
        l_type := l_geometry.get_string('type');
        l_coordinates := treat(l_geometry.get('coordinates') AS json_array_t);
        IF (l_type = 'Polygon') THEN
          FOR j IN 0 .. l_coordinates.get_size - 1 LOOP
            l_nested_1 := treat(l_coordinates.get(j) AS json_array_t);
            extrair_poligono (l_nested_1, l_largura, l_altura, l_projecao, l_svg);
          END LOOP;
        ELSIF (l_type = 'MultiPolygon') THEN
          FOR j IN 0 .. l_coordinates.get_size - 1 LOOP
            l_nested_1 := treat(l_coordinates.get(j) AS json_array_t);
            FOR k IN 0 .. l_nested_1.get_size - 1 LOOP
              l_nested_2 := treat(l_nested_1.get(k) AS json_array_t);
              extrair_poligono (l_nested_2, l_largura, l_altura, l_projecao, l_svg);
            END LOOP;
          END LOOP;
        END IF;
      END IF;
    END LOOP;

    svg_anexar (l_svg, '</g>' || chr(13) || chr(10));
    RETURN svg_finalizar (l_svg);

  END exibir_continente;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Constrói a área que representa os oceanos.
  --
  -- Parameters:
  --   @p_largura  = Tamanho horizontal da imagem do mapa em pontos
  --   @p_altura   = Tamanho vertical da imagem do mapa em pontos
  --   @p_projecao = Caractere que identifica o sistema de projeção do mapa terrestre
  -- Returns:
  --   Um elemento <circle> ou <path> em SVG com preenchimento em azul
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   16/01/22 Daniel Madeira: Comparação com ponto anterior
  --   20/09/26 Daniel Madeira: Fundo circular na projeção Gott-Mugnolo (G)
  --   20/09/26 Daniel Madeira: Projeção Lambert azimuthal equal-area com diâmetro máximo igual a 4
  --   29/10/26 Daniel Madeira: Retorno em CLOB e normalização do código da projeção
  --   02/11/26 Daniel Madeira: Ajuste para as dimensões úteis da projeção e resolução de 2° nas bordas polares
  --   
  -- =================================================================================================
  FUNCTION exibir_fundo_azul (
    p_largura  IN NUMBER   DEFAULT 800
  , p_altura   IN NUMBER   DEFAULT 400
  , p_projecao IN VARCHAR2 DEFAULT 'c'
  )
  RETURN CLOB
  IS
    l_svg       VARCHAR2(32767);
    l_largura   NUMBER      := p_largura;
    l_altura    NUMBER      := p_altura;
    l_projecao  VARCHAR2(1) := normalizar_projecao (p_projecao);
    l_centro    coordenadas_xy_t;
    l_raio      NUMBER;
    l_ponto     coordenadas_xy_t;
    l_anterior  coordenadas_xy_t;
    l_latitude  NUMBER;
    l_longitude NUMBER;
    
  BEGIN
    ajustar_dimensoes_uteis (l_largura, l_altura, l_projecao);
    IF (l_projecao = 'l' OR l_projecao = 'G') THEN
      l_centro := coordenar_centro (l_largura, l_altura);
      IF ((l_largura / l_altura) < 1) THEN
        l_raio := round (l_largura / 2);
      ELSE
        l_raio := round (l_altura / 2);
      END IF;
      l_svg := '<circle cx="' || l_centro('x') || '" cy="' || l_centro('y') || '" r="' || l_raio || '" fill="rgb(174,214,241)" />' || chr(13) || chr(10);
    ELSE
      l_ponto := converter_geo_pixel (90, -180, l_largura, l_altura, l_projecao);
      l_svg := '<path d="M' || l_ponto('x') || ',' || l_ponto('y');
      l_anterior('x') := l_ponto('x');
      l_anterior('y') := l_ponto('y');
      FOR lon IN -90 .. 90 LOOP
        l_longitude := lon * 2;
        l_ponto := converter_geo_pixel (90, l_longitude, l_largura, l_altura, l_projecao);
        IF (l_ponto('x') != l_anterior('x')) OR (l_ponto('y') != l_anterior('y')) THEN
          l_svg := l_svg || ' L' || l_ponto('x') || ',' || l_ponto('y');
          l_anterior('x') := l_ponto('x');
          l_anterior('y') := l_ponto('y');
        END IF;
      END LOOP;
      FOR lat IN REVERSE -45 .. 45 LOOP
        l_latitude := lat * 2;
        l_ponto := converter_geo_pixel (l_latitude, 180, l_largura, l_altura, l_projecao);
        IF (l_ponto('x') != l_anterior('x')) OR (l_ponto('y') != l_anterior('y')) THEN
          l_svg := l_svg || ' L' || l_ponto('x') || ',' || l_ponto('y');
          l_anterior('x') := l_ponto('x');
          l_anterior('y') := l_ponto('y');
        END IF;
      END LOOP;
      FOR lon IN REVERSE -90 .. 90 LOOP
        l_longitude := lon * 2;
        l_ponto := converter_geo_pixel (-90, l_longitude, l_largura, l_altura, l_projecao);
        IF (l_ponto('x') != l_anterior('x')) OR (l_ponto('y') != l_anterior('y')) THEN
          l_svg := l_svg || ' L' || l_ponto('x') || ',' || l_ponto('y');
          l_anterior('x') := l_ponto('x');
          l_anterior('y') := l_ponto('y');
        END IF;
      END LOOP;
      FOR lat IN -45 .. 45 LOOP
        l_latitude := lat * 2;
        l_ponto := converter_geo_pixel (l_latitude, -180, l_largura, l_altura, l_projecao);
        IF (l_ponto('x') != l_anterior('x')) OR (l_ponto('y') != l_anterior('y')) THEN
          l_svg := l_svg || ' L' || l_ponto('x') || ',' || l_ponto('y');
          l_anterior('x') := l_ponto('x');
          l_anterior('y') := l_ponto('y');
        END IF;
      END LOOP;
      l_svg := l_svg || ' Z" fill="rgb(174,214,241)" />' || chr(13) || chr(10);
    END IF;
    RETURN l_svg;
    
  END exibir_fundo_azul;

  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Inicia o gráfico vetorial escalável.
  --
  -- Parameters:
  --   @p_largura  = Tamanho horizontal da imagem do mapa em pontos
  --   @p_altura   = Tamanho vertical da imagem do mapa em pontos
  --   @p_projecao = Caractere que identifica o sistema de projeção do mapa terrestre
  --   @p_moldura  = Verdadeiro ou falso para construir um contorno na imagem
  -- Returns:
  --   O elemento <svg>
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   02/11/26 Daniel Madeira: Ajuste para as dimensões úteis da projeção
  --   
  -- =================================================================================================
  FUNCTION exibir_abertura_svg (
    p_largura  IN NUMBER   DEFAULT 800
  , p_altura   IN NUMBER   DEFAULT 400
  , p_projecao IN VARCHAR2 DEFAULT 'c'
  , p_moldura  IN BOOLEAN  DEFAULT false
  )
  RETURN VARCHAR2
  IS
    l_svg      VARCHAR2(1000);
    l_largura  NUMBER      := p_largura;
    l_altura   NUMBER      := p_altura;
    l_projecao VARCHAR2(1) := normalizar_projecao (p_projecao);
    
  BEGIN
    ajustar_dimensoes_uteis (l_largura, l_altura, l_projecao);
    l_svg := '<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="' || l_largura || '" height="' || l_altura || '">' || chr(13) || chr(10);
    IF p_moldura THEN
      l_svg := l_svg || '<rect x="0" y="0" width="' || l_largura || '" height="' || l_altura || '" fill="none" stroke="rgb(150,150,150)" stroke-width="1" />' || chr(13) || chr(10);
    END IF;
    
    RETURN l_svg;
    
  END exibir_abertura_svg;
  
  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 15/01/2022
  -- Description: Encerra o gráfico vetorial escalável.
  --
  -- Returns:
  --   O elemento </svg>
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   
  -- =================================================================================================
  FUNCTION exibir_fechamento_svg
  RETURN VARCHAR2
  IS
    l_svg VARCHAR2(100);
    
  BEGIN
    l_svg := '</svg>' || chr(13) || chr(10);
    
    RETURN l_svg;
    
  END exibir_fechamento_svg;
  
  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 29/10/2026
  -- Description: Envia um CLOB de qualquer tamanho para o buffer HTP em blocos, pois htp.p e
  --              htp.prn aceitam no máximo 32767 bytes por chamada.
  --
  -- Parameters:
  --   @p_clob = Conteúdo a ser enviado
  --
  -- Change History:
  --   29/10/26 Daniel Madeira: Versão inicial
  --
  -- =================================================================================================
  PROCEDURE imprimir_clob (
    p_clob IN CLOB
  )
  IS
    c_bloco   CONSTANT PLS_INTEGER := 8000; -- caracteres (até 4 bytes cada em AL32UTF8)
    l_tamanho PLS_INTEGER;
    l_posicao PLS_INTEGER := 1;
    
  BEGIN
    IF p_clob IS NULL THEN
      RETURN;
    END IF;
    l_tamanho := dbms_lob.getlength (p_clob);
    WHILE l_posicao <= l_tamanho LOOP
      htp.prn (dbms_lob.substr (p_clob, c_bloco, l_posicao));
      l_posicao := l_posicao + c_bloco;
    END LOOP;
    
  END imprimir_clob;
  
  -- =================================================================================================
  -- Author:      Daniel Madeira
  -- Create date: 29/10/2026
  -- Description: Constrói e envia ao buffer HTP o mapa completo em SVG (fundo, continentes,
  --              capitais e grade).
  --
  -- Parameters:
  --   @p_largura  = Tamanho horizontal da imagem do mapa em pontos
  --   @p_altura   = Tamanho vertical da imagem do mapa em pontos
  --   @p_projecao = Caractere que identifica o sistema de projeção do mapa terrestre
  --   @p_moldura  = Verdadeiro ou falso para construir um contorno na imagem
  --   @p_raio     = Tamanho (raio) em pontos do marcador das cidades (opcional; padrão proporcional à largura)
  --
  -- Change History:
  --   29/10/26 Daniel Madeira: Versão inicial
  --   02/11/26 Daniel Madeira: Ajuste para as dimensões úteis da projeção e parâmetro opcional p_raio
  --
  -- =================================================================================================
  PROCEDURE exibir_mapa (
    p_largura  IN NUMBER   DEFAULT 800
  , p_altura   IN NUMBER   DEFAULT 400
  , p_projecao IN VARCHAR2 DEFAULT 'c'
  , p_moldura  IN BOOLEAN  DEFAULT false
  , p_raio     IN NUMBER   DEFAULT NULL
  )
  IS
    TYPE continentes_t IS TABLE OF VARCHAR2(30);
    l_continentes continentes_t := continentes_t ( 'North America'
                                                 , 'Oceania'
                                                 , 'Seven seas (open ocean)'
                                                 , 'Antarctica'
                                                 , 'South America'
                                                 , 'Europe'
                                                 , 'Africa'
                                                 , 'Asia'
                                                 );
    l_largura  NUMBER      := nvl(p_largura, 800);
    l_altura   NUMBER      := nvl(p_altura, 400);
    l_projecao VARCHAR2(1) := normalizar_projecao (p_projecao);
    l_clob     CLOB;
    
    PROCEDURE imprimir_e_liberar (
      p_conteudo IN OUT NOCOPY CLOB
    )
    IS
    BEGIN
      imprimir_clob (p_conteudo);
      IF dbms_lob.istemporary (p_conteudo) = 1 THEN
        dbms_lob.freetemporary (p_conteudo);
      END IF;
    END imprimir_e_liberar;
    
  BEGIN
    ajustar_dimensoes_uteis (l_largura, l_altura, l_projecao);

    htp.prn (exibir_abertura_svg (l_largura, l_altura, l_projecao, nvl(p_moldura, false)));
    
    l_clob := exibir_fundo_azul (l_largura, l_altura, l_projecao);
    imprimir_e_liberar (l_clob);
    
    FOR i IN 1 .. l_continentes.count LOOP
      l_clob := exibir_continente (l_continentes(i), l_largura, l_altura, l_projecao);
      imprimir_e_liberar (l_clob);
    END LOOP;
    
    l_clob := exibir_cidades (l_largura, l_altura, l_projecao, p_raio);
    imprimir_e_liberar (l_clob);
    
    l_clob := exibir_grade (l_largura, l_altura, l_projecao);
    imprimir_e_liberar (l_clob);
    
    htp.prn (exibir_fechamento_svg);
    
  END exibir_mapa;
  
END world_map_projections;
/
