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
  RETURN VARCHAR2;
  
  FUNCTION exibir_continente (
    p_continente IN VARCHAR2
  , p_largura    IN NUMBER   DEFAULT 800
  , p_altura     IN NUMBER   DEFAULT 400
  , p_projecao   IN VARCHAR2 DEFAULT 'c'
  )
  RETURN VARCHAR2;
  
  FUNCTION exibir_cidades (
    p_largura  IN NUMBER   DEFAULT 800
  , p_altura   IN NUMBER   DEFAULT 400
  , p_projecao IN VARCHAR2 DEFAULT 'c'
  )
  RETURN VARCHAR2;
  
  FUNCTION exibir_grade (
    p_largura  IN NUMBER   DEFAULT 800
  , p_altura   IN NUMBER   DEFAULT 400
  , p_projecao IN VARCHAR2 DEFAULT 'c'
  )
  RETURN VARCHAR2;
  
END world_map_projections;
/


CREATE OR REPLACE PACKAGE BODY world_map_projections
AS

  TYPE coordenadas_xy_t IS TABLE OF NUMBER INDEX BY VARCHAR2(1);

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
    l_v     NUMBER;
    
  BEGIN
    l_theta := p_latitude * (3.14159265359 / 180);
    l_k := (2 + (3.14159265359 / 2)) * sin(l_theta);
    l_theta := l_theta / 2;
    FOR i IN REVERSE 1 .. 10 LOOP
      l_sint := sin(l_theta);
      l_cost := cos(l_theta);
      l_v := (l_theta + l_sint * l_cost + 2 * l_sint - l_k) / (2 * l_cost * (1 + l_cost));
      l_theta := l_theta - l_v;
      EXIT WHEN (abs(l_v) < '1E-7');
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
    l_longitude := p_longitude * (3.14159265359 / 180);
    RETURN ((2 / (sqrt(3.14159265359 * (4 + 3.14159265359)))) * l_longitude * (1 + cos(p_theta)));
    
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
    RETURN (2 * sqrt(3.14159265359 / (4 + 3.14159265359)) * sin(p_theta));
    
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
  --   
  -- =================================================================================================
  FUNCTION calcular_eckert_vi_theta (
    p_latitude IN NUMBER
  )
  RETURN NUMBER
  IS
    l_theta NUMBER;
    l_k     NUMBER;
    l_v     NUMBER;
    
  BEGIN
    l_theta := p_latitude * (3.14159265359 / 180);
    l_k := (1 + (3.14159265359 / 2)) * sin(l_theta);
    FOR i IN REVERSE 1 .. 10 LOOP
      l_v := (l_theta + sin(l_theta) - l_k) / (1 + cos(l_theta));
      l_theta := l_theta - l_v;
      EXIT WHEN (abs(l_v) < '1E-7');
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
    l_longitude := p_longitude * (3.14159265359 / 180);
    RETURN (l_longitude * (1 + cos(p_theta)) / sqrt(2 + 3.14159265359));
    
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
    RETURN (2 * p_theta / sqrt(2 + 3.14159265359));
    
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
  --   
  -- =================================================================================================
  FUNCTION calcular_gott_equal_area_elliptical_x (
    p_latitude  IN NUMBER
  , p_longitude IN NUMBER
  )
  RETURN NUMBER
  IS
    l_longitude NUMBER;
    l_theta     NUMBER;
    l_phi       NUMBER;
    l_k         NUMBER;
    l_v         NUMBER;
    l_fim       BOOLEAN := false;
    
  BEGIN
    l_longitude := p_longitude * (3.14159265359 / 180);
    l_theta := p_latitude * (3.14159265359 / 180);
    l_phi := asin(cos(l_theta) * sin(l_longitude / 2));
    l_k := 3.14159265359 * sin(l_phi);
    FOR i IN REVERSE 1 .. 10 LOOP
      l_v := (l_theta + sin(l_theta) - l_k) / (1 + cos(l_theta));
      l_theta := l_theta - l_v;
      IF (i = 1) THEN
        l_fim := true;
      END IF;
      EXIT WHEN (abs(l_v) < '1E-7');
    END LOOP;
    IF l_fim THEN
      IF (l_theta < 0) THEN
        l_theta := (-3.14159265359 / 2);
      ELSE
        l_theta := (3.14159265359 / 2);
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
  --   
  -- =================================================================================================
  FUNCTION calcular_gott_equal_area_elliptical_y (
    p_latitude  IN NUMBER
  , p_longitude IN NUMBER
  )
  RETURN NUMBER
  IS
    l_longitude NUMBER;
    l_theta     NUMBER;
    l_phi       NUMBER;
    l_k         NUMBER;
    l_lambda    NUMBER;
    l_v         NUMBER;
    l_fim       BOOLEAN := false;
    
  BEGIN
    IF (p_longitude = 180) THEN -- remendo para limitar em 179
      l_longitude := p_longitude - 1;
    ELSIF (p_longitude = -180) THEN
      l_longitude := p_longitude + 1;
    ELSE
      l_longitude := p_longitude;
    END IF;
    l_longitude := l_longitude * (3.14159265359 / 180);
    l_theta := p_latitude * (3.14159265359 / 180);
    l_phi := asin(cos(l_theta) * sin(l_longitude / 2));
    l_k := 3.14159265359 * sin(l_phi);
    l_lambda := 0.5 * asin(sin(l_theta) / cos(l_phi));
    FOR i IN REVERSE 1 .. 10 LOOP
      l_v := (l_theta + sin(l_theta) - l_k) / (1 + cos(l_theta));
      l_theta := l_theta - l_v;
      IF (i = 1) THEN
        l_fim := true;
      END IF;
      EXIT WHEN (abs(l_v) < '1E-7');
    END LOOP;
    IF l_fim THEN
      IF (l_theta < 0) THEN
        l_theta := (-3.14159265359 / 2);
      ELSE
        l_theta := (3.14159265359 / 2);
      END IF;
    ELSE
      l_theta := l_theta * 0.5;
    END IF;
    RETURN ((3.14159265359 / (2 * sqrt(2))) * l_lambda * cos(l_theta));
    
  END calcular_gott_equal_area_elliptical_y;

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
    
  BEGIN
    l_latitude := p_latitude * (3.14159265359 / 180);
    l_longitude := p_longitude * (3.14159265359 / 180);
    RETURN (cos(l_longitude) * sin(0.446 * (3.14159265359 / 2 - l_latitude)));
    
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
    
  BEGIN
    l_latitude := p_latitude * (3.14159265359 / 180);
    l_longitude := p_longitude * (3.14159265359 / 180);
    RETURN (sin(l_longitude) * sin(0.446 * (3.14159265359 / 2 - l_latitude)));
    
  END calcular_gott_mugnolo_azimuthal_y;

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
    l_eta       NUMBER;
    
  BEGIN
    l_latitude := p_latitude * (3.14159265359 / 180);
    l_longitude := p_longitude * (3.14159265359 / 180);
    l_eta := sqrt(1 + cos(l_latitude) * cos(l_longitude / 2));
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
    l_eta       NUMBER;
    
  BEGIN
    l_latitude := p_latitude * (3.14159265359 / 180);
    l_longitude := p_longitude * (3.14159265359 / 180);
    l_eta := sqrt(1 + cos(l_latitude) * cos(l_longitude / 2));
    RETURN ((sqrt(2) * sin(l_latitude)) / l_eta);
    
  END calcular_hammer_y;

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
    
  BEGIN
    l_latitude := p_latitude * (3.14159265359 / 180);
    l_longitude := p_longitude * (3.14159265359 / 180);
    RETURN (3 * l_longitude / 2) * sqrt(1 / 3 - power(l_latitude / 3.14159265359, 2));
    
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
    l_latitude := p_latitude * (3.14159265359 / 180);
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
  --   
  -- =================================================================================================
  FUNCTION calcular_lambert_azimuthal_equal_area_x (
    p_latitude  IN NUMBER
  , p_longitude IN NUMBER
  )
  RETURN NUMBER
  IS
    l_latitude  NUMBER;
    l_longitude NUMBER;
    l_k         NUMBER;
    
  BEGIN
    IF (p_latitude = 0 AND (p_longitude = 180 OR p_longitude = -180)) THEN -- estes dois pontos retornam NaN
      l_latitude := 0.001;
    ELSE
      l_latitude := p_latitude;
    END IF;
    l_latitude := l_latitude * (3.14159265359 / 180);
    l_longitude := p_longitude * (3.14159265359 / 180);
    l_k := sqrt(2 / (1 + cos(l_latitude) * cos(l_longitude)));
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
  --   
  -- =================================================================================================
  FUNCTION calcular_lambert_azimuthal_equal_area_y (
    p_latitude  IN NUMBER
  , p_longitude IN NUMBER
  )
  RETURN NUMBER
  IS
    l_latitude  NUMBER;
    l_longitude NUMBER;
    l_k         NUMBER;
    
  BEGIN
    IF (p_latitude = 0 AND (p_longitude = 180 OR p_longitude = -180)) THEN -- estes dois pontos retornam NaN
      l_latitude := 0.001;
    ELSE
      l_latitude := p_latitude;
    END IF;
    l_latitude := l_latitude * (3.14159265359 / 180);
    l_longitude := p_longitude * (3.14159265359 / 180);
    l_k := sqrt(2 / (1 + cos(l_latitude) * cos(l_longitude)));
    RETURN (l_k * sin(l_latitude));
    
  END calcular_lambert_azimuthal_equal_area_y;

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
    l_longitude := p_longitude * (3.14159265359 / 180);
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
    l_latitude := p_latitude * (3.14159265359 / 180);
    
    IF (l_latitude > 1.4844222297453322) THEN
      RETURN 3.14159265359;
    ELSIF (l_latitude < -1.4844222297453322) THEN
      RETURN -3.14159265359;
    ELSE
      RETURN (ln(tan(3.14159265359/4 + l_latitude/2)));
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
    l_longitude := p_longitude * (3.14159265359 / 180);
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
    l_latitude := p_latitude * (3.14159265359 / 180);
    RETURN ((5/4) * ln(tan(3.14159265359/4 + ((2 * l_latitude)/5))));
    
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
  --   
  -- =================================================================================================
  FUNCTION calcular_mollweide_theta (
    p_latitude IN NUMBER
  )
  RETURN NUMBER
  IS
    l_theta NUMBER;
    l_k     NUMBER;
    l_v     NUMBER;
    l_fim   BOOLEAN := false;
    
  BEGIN
    l_theta := p_latitude * (3.14159265359 / 180);
    l_k := 3.14159265359 * sin(l_theta);
    FOR i IN REVERSE 1 .. 10 LOOP
      l_v := (l_theta + sin(l_theta) - l_k) / (1 + cos(l_theta));
      l_theta := l_theta - l_v;
      IF (i = 1) THEN
        l_fim := true;
      END IF;
      EXIT WHEN (abs(l_v) < '1E-7');
    END LOOP;
    IF l_fim THEN
      IF (l_theta < 0) THEN
        l_theta := (-3.14159265359 / 2);
      ELSE
        l_theta := (3.14159265359 / 2);
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
    l_longitude := p_longitude * (3.14159265359 / 180);
    l_cx := 2 * sqrt(2) / 3.14159265359;
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
    l_latitude := p_latitude * (3.14159265359 / 180);
    l_longitude := p_longitude * (3.14159265359 / 180);
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
    l_latitude := p_latitude * (3.14159265359 / 180);
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
    l_latitude := p_latitude * (3.14159265359 / 180);
    l_longitude := p_longitude * (3.14159265359 / 180);
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
    l_latitude := p_latitude * (3.14159265359 / 180);
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
    l_longitude := p_longitude * (3.14159265359 / 180);
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
    l_latitude := p_latitude * (3.14159265359 / 180);
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
    l_latitude := p_latitude * (3.14159265359 / 180);
    l_longitude := p_longitude * (3.14159265359 / 180);
    RETURN ((2.6666 - 0.367 * power(l_latitude, 2) - 0.150 * power(l_latitude, 4) + 0.0379 * power(l_latitude, 6)) * l_longitude / 3.14159265359);
    
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
    l_latitude := p_latitude * (3.14159265359 / 180);
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
  --   
  -- =================================================================================================
  FUNCTION calcular_sinusoidal_x (
    p_latitude  IN NUMBER
  , p_longitude IN NUMBER
  )
  RETURN NUMBER
  IS
    l_latitude NUMBER;
    
  BEGIN
    l_latitude := p_latitude * (3.14159265359 / 180);
    RETURN p_longitude * cos(l_latitude);
    
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
  --   
  -- =================================================================================================
  FUNCTION calcular_sinusoidal_y (
    p_latitude IN NUMBER
  )
  RETURN NUMBER
  IS
  BEGIN
    RETURN p_latitude;
    
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
    
  BEGIN
    l_latitude := p_latitude * (3.14159265359 / 180);
    l_longitude := p_longitude * (3.14159265359 / 180);
    RETURN (l_longitude * sqrt(1 - 3 * power(l_latitude / 3.14159265359, 2)));
    
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
    l_latitude := p_latitude * (3.14159265359 / 180);
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
    l_alpha     NUMBER;
    l_sinc      NUMBER;
    
  BEGIN
    l_latitude := p_latitude * (3.14159265359 / 180);
    l_longitude := p_longitude * (3.14159265359 / 180);
    l_alpha := acos(cos(l_latitude) * cos(l_longitude / 2));
    l_sinc := sin(l_alpha) / l_alpha;
    RETURN ((1 / 2) * (l_longitude * cos(acos(2 / 3.14159265359)) + ((2 * cos(l_latitude) * sin(l_longitude / 2)) / (l_sinc))));
    
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
    l_alpha     NUMBER;
    l_sinc      NUMBER;
    
  BEGIN
    l_latitude := p_latitude * (3.14159265359 / 180);
    l_longitude := p_longitude * (3.14159265359 / 180);
    l_alpha := acos(cos(l_latitude) * cos(l_longitude / 2));
    l_sinc := sin(l_alpha) / l_alpha;
    RETURN ((1 / 2) * (l_latitude + (sin(l_latitude) / (l_sinc))));
    
  END calcular_winkel_iii_y;

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
    l_centro coordenadas_xy_t;
    l_xy     coordenadas_xy_t;
    l_modulo NUMBER;
    l_theta  NUMBER;
  
  BEGIN
    l_centro := coordenar_centro (p_largura, p_altura);
    
    CASE
      WHEN (p_projecao = 'e') THEN -- Eckert IV projection
        IF (p_largura / p_altura < 2.002) THEN
          l_modulo := p_largura / (calcular_eckert_iv_x (0, 180) * 2);
        ELSE
          l_modulo := p_altura / (calcular_eckert_iv_y (1.5697757215205) * 2);
        END IF;
        l_theta := calcular_eckert_iv_theta(p_latitude);
        l_xy('x') := floor(l_centro('x') + (calcular_eckert_iv_x (l_theta, p_longitude) * l_modulo));
        l_xy('y') := floor(l_centro('y') - (calcular_eckert_iv_y (l_theta) * l_modulo));
            
      WHEN (p_projecao = 'E') THEN -- Eckert VI projection
        IF (p_largura / p_altura < 2) THEN
          l_modulo := p_largura / (calcular_eckert_vi_x (0, 180) * 2);
        ELSE
          l_modulo := p_altura / (calcular_eckert_vi_y (1.570796326795) * 2);
        END IF;
        l_theta := calcular_eckert_vi_theta (p_latitude);
        l_xy('x') := floor(l_centro('x') + (calcular_eckert_vi_x (l_theta, p_longitude) * l_modulo));
        l_xy('y') := floor(l_centro('y') - (calcular_eckert_vi_y (l_theta) * l_modulo));
           
      WHEN (p_projecao = 'g') THEN -- Gott equal-area elliptical projection
        IF (p_largura / p_altura < 1.621) THEN
          l_modulo := p_largura / (calcular_gott_equal_area_elliptical_x (0, 180) * 2);
        ELSE
          l_modulo := p_altura / (calcular_gott_equal_area_elliptical_y (90, 0) * 2);
        END IF;
        l_xy('x') := floor(l_centro('x') + (calcular_gott_equal_area_elliptical_x (p_latitude, p_longitude) * l_modulo));
        l_xy('y') := floor(l_centro('y') - (calcular_gott_equal_area_elliptical_y (p_latitude, p_longitude) * l_modulo));
         
      WHEN (p_projecao = 'G') THEN -- Gott–Mugnolo azimuthal projection // (necessita ajuste pois está centrada no Polo Norte)
        IF (p_largura / p_altura < 2) THEN
          l_modulo := p_largura / (calcular_gott_mugnolo_azimuthal_x (0, 180) * 2);
        ELSE
          l_modulo := p_altura / (calcular_gott_mugnolo_azimuthal_y (90, 0) * 2);
        END IF;
        l_xy('x') := floor(l_centro('x') + (calcular_gott_mugnolo_azimuthal_x (p_latitude, p_longitude) * l_modulo));
        l_xy('y') := floor(l_centro('y') - (calcular_gott_mugnolo_azimuthal_y (p_latitude, p_longitude) * l_modulo));
            
      WHEN (p_projecao = 'h') THEN -- Hammer projection
        IF (p_largura / p_altura < 2) THEN
          l_modulo := p_largura / (calcular_hammer_x (0, 180) * 2);
        ELSE
          l_modulo := p_altura / (calcular_hammer_y (90, 0) * 2);
        END IF;
        l_xy('x') := floor(l_centro('x') + (calcular_hammer_x (p_latitude, p_longitude) * l_modulo));
        l_xy('y') := floor(l_centro('y') - (calcular_hammer_y (p_latitude, p_longitude) * l_modulo));
            
      WHEN (p_projecao = 'k') THEN -- Kavrayskiy VII projection
        IF (p_largura / p_altura < 1.733) THEN
          l_modulo := p_largura / (calcular_kavrayskiy_vii_x (0, 180) * 2);
        ELSE
          l_modulo := p_altura / (calcular_kavrayskiy_vii_y (90) * 2);
        END IF;
        l_xy('x') := floor(l_centro('x') + (calcular_kavrayskiy_vii_x (p_latitude, p_longitude) * l_modulo));
        l_xy('y') := floor(l_centro('y') - (calcular_kavrayskiy_vii_y (p_latitude) * l_modulo));
            
      WHEN (p_projecao = 'l') THEN -- Lambert azimuthal equal-area projection // (necessita ajuste)
        IF (p_largura / p_altura < 1) THEN
          l_modulo := p_largura / (calcular_lambert_azimuthal_equal_area_x (0, 180) * 3.14159265359);
        ELSE
          l_modulo := p_altura / (calcular_lambert_azimuthal_equal_area_y (90, 0) * 3.14159265359);
        END IF;
        l_xy('x') := floor(l_centro('x') + (calcular_lambert_azimuthal_equal_area_x (p_latitude, p_longitude) * l_modulo));
        l_xy('y') := floor(l_centro('y') - (calcular_lambert_azimuthal_equal_area_y (p_latitude, p_longitude) * l_modulo));
            
      WHEN (p_projecao = 'm') THEN -- Miller cylindrical projection
        IF (p_largura / p_altura < 1.363) THEN
          l_modulo := p_largura / (calcular_miller_x (180) * 2);
        ELSE
          l_modulo := p_altura / (calcular_miller_y (90) * 2);
        END IF;
        l_xy('x') := floor(l_centro('x') + (calcular_miller_x (p_longitude) * l_modulo));
        l_xy('y') := floor(l_centro('y') - (calcular_miller_y (p_latitude) * l_modulo));
            
      WHEN (p_projecao = 'M') THEN -- Mollweide projection
        IF (p_largura / p_altura < 2) THEN
          l_modulo := p_largura / (calcular_mollweide_x (0, 180) * 2);
        ELSE
          l_modulo := p_altura / (calcular_mollweide_y (1.570796326795) * 2);
        END IF;
        l_theta := calcular_mollweide_theta (p_latitude);
        l_xy('x') := floor(l_centro('x') + (calcular_mollweide_x (l_theta, p_longitude) * l_modulo));
        l_xy('y') := floor(l_centro('y') - (calcular_mollweide_y (l_theta) * l_modulo));
            
      WHEN (p_projecao = 'n') THEN -- Natural Earth projection
        IF (p_largura / p_altura < 1.923) THEN
          l_modulo := p_largura / (calcular_natural_earth_x (0, 180) * 2);
        ELSE
          l_modulo := p_altura / (calcular_natural_earth_y (90) * 2);
        END IF;
        l_xy('x') := floor(l_centro('x') + (calcular_natural_earth_x (p_latitude, p_longitude) * l_modulo));
        l_xy('y') := floor(l_centro('y') - (calcular_natural_earth_y (p_latitude) * l_modulo));
            
      WHEN (p_projecao = 'N') THEN -- Natural Earth II projection
        IF (p_largura / p_altura < 1.869) THEN
          l_modulo := p_largura / (calcular_natural_earth_ii_x (0, 180) * 2);
        ELSE
          l_modulo := p_altura / (calcular_natural_earth_ii_y (90) * 2);
        END IF;
        l_xy('x') := floor(l_centro('x') + (calcular_natural_earth_ii_x (p_latitude, p_longitude) * l_modulo));
        l_xy('y') := floor(l_centro('y') - (calcular_natural_earth_ii_y (p_latitude) * l_modulo));
            
      WHEN (p_projecao = 'p') THEN -- Patterson projection
        IF (p_largura / p_altura < 1.755) THEN
          l_modulo := p_largura / (calcular_patterson_x (180) * 2);
        ELSE
          l_modulo := p_altura / (calcular_patterson_y (90) * 2);
        END IF;
        l_xy('x') := floor(l_centro('x') + (calcular_patterson_x (p_longitude) * l_modulo));
        l_xy('y') := floor(l_centro('y') - (calcular_patterson_y (p_latitude) * l_modulo));
            
      WHEN (p_projecao = 'r') THEN -- Robinson projection
        IF (p_largura / p_altura < 1.969) THEN
          l_modulo := p_largura / (calcular_robinson_x (0, 180) * 2);
        ELSE
          l_modulo := p_altura / (calcular_robinson_y (90) * 2);
        END IF;
        l_xy('x') := floor(l_centro('x') + (calcular_robinson_x (p_latitude, p_longitude) * l_modulo));
        l_xy('y') := floor(l_centro('y') - (calcular_robinson_y (p_latitude) * l_modulo));
            
      WHEN (p_projecao = 's') THEN -- sinusoidal projection
        IF (p_largura / p_altura < 2) THEN
          l_modulo := p_largura / (calcular_sinusoidal_x (0, 180) * 2);
        ELSE
          l_modulo := p_altura / (calcular_sinusoidal_y (90) * 2);
        END IF;
        l_xy('x') := floor(l_centro('x') + (calcular_sinusoidal_x (p_latitude, p_longitude) * l_modulo));
        l_xy('y') := floor(l_centro('y') - (calcular_sinusoidal_y (p_latitude) * l_modulo));
            
      WHEN (p_projecao = 't') THEN -- Mercator projection
        IF (p_largura / p_altura < 1) THEN
          l_modulo := p_largura / (calcular_mercator_x (180) * 2);
        ELSE
          l_modulo := p_altura / (calcular_mercator_y (90) * 2);
        END IF;
        l_xy('x') := floor(l_centro('x') + (calcular_mercator_x (p_longitude) * l_modulo));
        l_xy('y') := floor(l_centro('y') - (calcular_mercator_y (p_latitude) * l_modulo));
          
      WHEN (p_projecao = 'w') THEN -- Wagner VI projection
        IF (p_largura / p_altura < 2) THEN
          l_modulo := p_largura / (calcular_wagner_vi_x (0, 180) * 2);
        ELSE
          l_modulo := p_altura / (calcular_wagner_vi_y (90) * 2);
        END IF;
        l_xy('x') := floor(l_centro('x') + (calcular_wagner_vi_x (p_latitude, p_longitude) * l_modulo));
        l_xy('y') := floor(l_centro('y') - (calcular_wagner_vi_y (p_latitude) * l_modulo));
            
      WHEN (p_projecao = 'W') THEN -- Winkel tripel projection
        IF (p_largura / p_altura < 1.637) THEN
          l_modulo := p_largura / (calcular_winkel_iii_x (0, 180) * 2);
        ELSE
          l_modulo := p_altura / (calcular_winkel_iii_y (90, 0) * 2);
        END IF;
        l_xy('x') := floor(l_centro('x') + (calcular_winkel_iii_x (p_latitude, p_longitude) * l_modulo));
        l_xy('y') := floor(l_centro('y') - (calcular_winkel_iii_y (p_latitude, p_longitude) * l_modulo));
            
      ELSE -- c: equirectangular projection -> plate carrée
           -- SNYDER, J. P.; VOXLAND, P. M. An Album of Map Projections. p 219
        IF (p_largura / p_altura < 2) THEN
          l_modulo := p_largura / 360;
        ELSE
          l_modulo := p_altura / 180;
        END IF;
        l_xy('x') := floor(l_centro('x') + (p_longitude * l_modulo));
        l_xy('y') := floor(l_centro('y') - (p_latitude * l_modulo));
    END CASE;
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
    FOR lon IN -18 .. 18 LOOP
      l_longitude := lon * 10;
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
  -- Returns:
  --   Os elementos <path> ou <line> em SVG para os paralelos
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   
  -- =================================================================================================
  FUNCTION exibir_paralelos (
    p_largura  IN NUMBER
  , p_altura   IN NUMBER
  , p_projecao IN VARCHAR2
  )
  RETURN VARCHAR2
  IS
    TYPE lat_type IS TABLE OF NUMBER;
    l_lat lat_type := lat_type (15, 30, 45, 60, 75, 90);
    l_svg VARCHAR2(32767);
    
  BEGIN
    FOR i IN 1 .. l_lat.count LOOP
      IF p_projecao IN ('W', 'h', 'g', 'l') THEN
        l_svg := l_svg || montar_caminho_paralelo (l_lat(i), p_largura, p_altura, p_projecao);
        l_svg := l_svg || montar_caminho_paralelo ((-1 * l_lat(i)), p_largura, p_altura, p_projecao);
      ELSE
        l_svg := l_svg || montar_linha_paralelo (l_lat(i), p_largura, p_altura, p_projecao);
        l_svg := l_svg || montar_linha_paralelo ((-1 * l_lat(i)), p_largura, p_altura, p_projecao);
      END IF;
    END LOOP;
    RETURN l_svg;
    
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
      l_ponto := converter_geo_pixel (l_latitude, p_longitude, p_largura, p_altura, p_projecao);
      IF (l_ponto('x') != l_anterior('x')) OR (l_ponto('y') != l_anterior('y')) THEN
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
  -- Returns:
  --   Os elementos <path> em SVG para os meridianos
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   
  -- =================================================================================================
  FUNCTION exibir_meridianos (
    p_largura  IN NUMBER
  , p_altura   IN NUMBER
  , p_projecao IN VARCHAR2
  )
  RETURN VARCHAR2
  IS
    TYPE lon_type IS TABLE OF NUMBER;
    l_lon lon_type := lon_type (15, 30, 45, 60, 75, 90, 105, 120, 135, 150, 165, 180);
    l_svg VARCHAR2(32767);
    
  BEGIN
    FOR i IN 1 .. l_lon.count LOOP
      l_svg := l_svg || montar_caminho_meridiano ((-1 * l_lon(i)), p_largura, p_altura, p_projecao);
      l_svg := l_svg || montar_caminho_meridiano (l_lon(i), p_largura, p_altura, p_projecao);
    END LOOP;
    RETURN l_svg;
    
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
    FOR lon IN -18 .. 18 LOOP
      l_longitude := lon * 10;
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
  -- Returns:
  --   Os elementos <path> ou <line> em SVG para os círculos e trópicos
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   
  -- =================================================================================================
  FUNCTION exibir_circulos (
    p_largura  IN NUMBER
  , p_altura   IN NUMBER
  , p_projecao IN VARCHAR2
  )
  RETURN VARCHAR2
  IS
    l_svg         VARCHAR2(32767);
    l_cancer      NUMBER := 23.43656;
    l_capricornio NUMBER := -23.43656;
    l_artico      NUMBER := 66.5622;
    l_antartico   NUMBER := -66.5622;
    
  BEGIN
    IF p_projecao IN ('W', 'h', 'g', 'l') THEN
      l_svg := l_svg || montar_caminho_circulo (l_cancer, p_largura, p_altura, p_projecao);
      l_svg := l_svg || montar_caminho_circulo (l_capricornio, p_largura, p_altura, p_projecao);
      l_svg := l_svg || montar_caminho_circulo (l_artico, p_largura, p_altura, p_projecao);
      l_svg := l_svg || montar_caminho_circulo (l_antartico, p_largura, p_altura, p_projecao);
    ELSE
      l_svg := l_svg || montar_linha_circulo (l_cancer, p_largura, p_altura, p_projecao);
      l_svg := l_svg || montar_linha_circulo (l_capricornio, p_largura, p_altura, p_projecao);
      l_svg := l_svg || montar_linha_circulo (l_artico, p_largura, p_altura, p_projecao);
      l_svg := l_svg || montar_linha_circulo (l_antartico, p_largura, p_altura, p_projecao);
    END IF;
    RETURN l_svg;
    
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
    l_w        NUMBER;
    l_e        NUMBER;
    l_svg      VARCHAR2(32767);
  
  BEGIN
    l_w := -180;
    l_e := 180;
    IF (p_projecao = 'l') THEN
      l_w := l_w + 1;
      l_e := l_e - 1;
    END IF;
    l_ocidente := converter_geo_pixel (0, l_w, p_largura, p_altura, p_projecao);
    l_oriente := converter_geo_pixel (0, l_e, p_largura, p_altura, p_projecao);
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
  --   
  -- =================================================================================================
  FUNCTION exibir_grade (
    p_largura  IN NUMBER   DEFAULT 800
  , p_altura   IN NUMBER   DEFAULT 400
  , p_projecao IN VARCHAR2 DEFAULT 'c'
  )
  RETURN VARCHAR2
  IS
    l_svg VARCHAR2(32767);
    
  BEGIN
    l_svg := '<g fill="none" stroke="rgb(240,240,240)" stroke-width="1">' || chr(13) || chr(10);
    
    l_svg := l_svg || exibir_paralelos (p_largura, p_altura, p_projecao);
    l_svg := l_svg || exibir_meridianos (p_largura, p_altura, p_projecao);
    l_svg := l_svg || exibir_circulos (p_largura, p_altura, p_projecao);
    l_svg := l_svg || exibir_equador (p_largura, p_altura, p_projecao);
    l_svg := l_svg || exibir_greenwich (p_largura, p_altura, p_projecao);
    
    l_svg := l_svg || '</g>' || chr(13) || chr(10);
    RETURN l_svg;
    
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
  -- Returns:
  --   Os elementos <circle> em SVG para as posições das cidades
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   
  -- =================================================================================================
  FUNCTION exibir_cidades (
    p_largura  IN NUMBER   DEFAULT 800
  , p_altura   IN NUMBER   DEFAULT 400
  , p_projecao IN VARCHAR2 DEFAULT 'c'
  )
  RETURN VARCHAR2
  IS
    CURSOR c_cidades
    IS
      SELECT city, lat, lng, country, id
        FROM worldcities
       WHERE lat IS NOT NULL
         AND lng IS NOT NULL
         AND capital = 'primary'
    ORDER BY population;
    
    l_svg       VARCHAR2(32767);
    l_r         NUMBER;
    l_latitude  NUMBER;
    l_longitude NUMBER;
    l_ponto     coordenadas_xy_t;
    l_titulo    VARCHAR2(200);
    
  BEGIN
    l_svg := '<g fill="rgb(33,42,116)" stroke-width="0" onmouseover="evt.target.setAttribute(''fill'', ''blue'');" onmouseout="evt.target.setAttribute(''fill'',''red'');">' || chr(13) || chr(10);
    l_r := round(p_largura * 0.002);
    FOR cidade IN c_cidades LOOP
      l_latitude := cidade.lat;
      l_longitude := cidade.lng;
      l_ponto := converter_geo_pixel (l_latitude, l_longitude, p_largura, p_altura, p_projecao);
      l_titulo := cidade.city || ' (' || cidade.country || ')';
      l_svg := l_svg || '<circle id="' || cidade.id || '" cx="' || l_ponto('x') || '" cy="' || l_ponto('y') || '" r="' || l_r || '"><title>' || l_titulo || '</title></circle>' || chr(13) || chr(10);
    END LOOP;
    l_svg := l_svg || '</g>' || chr(13) || chr(10);
    
    RETURN l_svg;
    
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
  -- Returns:
  --   Os respectivos pontos no plano cartesiano
  --
  -- Change History:
  --   15/01/22 Daniel Madeira: Versão inicial
  --   
  -- =================================================================================================
  FUNCTION extrair_poligono (
    p_poligono IN json_array_t
  , p_largura  IN NUMBER
  , p_altura   IN NUMBER
  , p_projecao IN VARCHAR2
  )
  RETURN VARCHAR2
  IS
    l_coordenadas json_array_t;
    l_latitude    NUMBER;
    l_longitude   NUMBER;
    l_ponto       coordenadas_xy_t;
    l_anterior    coordenadas_xy_t;
    l_pontos      VARCHAR2(8000);
    
  BEGIN
    l_anterior('x') := 0;
    l_anterior('y') := 0;
    FOR i IN 0 .. p_poligono.get_size - 1 LOOP
      l_coordenadas := treat(p_poligono.get(i) AS json_array_t);
      l_latitude := l_coordenadas.get_number(1);
      l_longitude := l_coordenadas.get_number(0);
      l_ponto := converter_geo_pixel (l_latitude, l_longitude, p_largura, p_altura, p_projecao);
      IF (l_ponto('x') != l_anterior('x')) OR (l_ponto('y') != l_anterior('y')) THEN
        IF l_pontos IS NOT NULL THEN
          l_pontos := l_pontos || ' ';
        END IF;
        l_pontos := l_pontos || l_ponto('x') || ',' || l_ponto('y');
        l_anterior('x') := l_ponto('x');
        l_anterior('y') := l_ponto('y');
      END IF;
    END LOOP;
    RETURN l_pontos;
    
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
  --   
  -- =================================================================================================
  FUNCTION exibir_continente (
    p_continente IN VARCHAR2
  , p_largura    IN NUMBER   DEFAULT 800
  , p_altura     IN NUMBER   DEFAULT 400
  , p_projecao   IN VARCHAR2 DEFAULT 'c'
  )
  RETURN VARCHAR2
  IS
    l_svg         VARCHAR2(32767);
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
    l_coordenadas json_array_t;
    l_latitude    NUMBER;
    l_longitude   NUMBER;
    l_pontos      VARCHAR2(8000);
  BEGIN
    l_svg := '<g fill="rgb(200,200,200)" fill-rule="nonzero" stroke="rgb(152,152,152)" stroke-width="1">' || chr(13) || chr(10);
    
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
            l_pontos := extrair_poligono (l_nested_1, p_largura, p_altura, p_projecao);
            IF l_pontos IS NOT NULL THEN
              l_svg := l_svg || '<polygon points="' || l_pontos || '" />' || chr(13) || chr(10);
            END IF;
          END LOOP;
        ELSIF (l_type = 'MultiPolygon') THEN
          FOR j IN 0 .. l_coordinates.get_size - 1 LOOP
            l_nested_1 := treat(l_coordinates.get(j) AS json_array_t);
            FOR k IN 0 .. l_nested_1.get_size - 1 LOOP
              l_nested_2 := treat(l_nested_1.get(k) AS json_array_t);
              l_pontos := extrair_poligono (l_nested_2, p_largura, p_altura, p_projecao);
              IF l_pontos IS NOT NULL THEN
                l_svg := l_svg || '<polygon points="' || l_pontos || '" />' || chr(13) || chr(10);
              END IF;
            END LOOP;
          END LOOP;
        END IF;
      END IF;
    END LOOP;

    l_svg := l_svg || '</g>' || chr(13) || chr(10);
    RETURN l_svg;

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
  --   
  -- =================================================================================================
  FUNCTION exibir_fundo_azul (
    p_largura  IN NUMBER   DEFAULT 800
  , p_altura   IN NUMBER   DEFAULT 400
  , p_projecao IN VARCHAR2 DEFAULT 'c'
  )
  RETURN VARCHAR2
  IS
    l_svg       VARCHAR2(32767);
    l_centro    coordenadas_xy_t;
    l_raio      NUMBER;
    l_ponto     coordenadas_xy_t;
    l_anterior  coordenadas_xy_t;
    l_latitude  NUMBER;
    l_longitude NUMBER;
    
  BEGIN
    IF (p_projecao = 'l') THEN
      l_centro := coordenar_centro (p_largura, p_altura);
      IF ((p_largura / p_altura) < 1) THEN
        l_raio := round (p_largura / calcular_lambert_azimuthal_equal_area_x (0, 180) / 3.14159265359 * 2);
      ELSE
        l_raio := round (p_altura / calcular_lambert_azimuthal_equal_area_y (90, 0) / 3.14159265359 * 2);
      END IF;
      l_svg := '<circle cx="' || l_centro('x') || '" cy="' || l_centro('y') || '" r="' || l_raio || '" fill="rgb(174,214,241)" />' || chr(13) || chr(10);
    ELSE
      l_ponto := converter_geo_pixel (90, -180, p_largura, p_altura, p_projecao);
      l_svg := '<path d="M' || l_ponto('x') || ',' || l_ponto('y');
      l_anterior('x') := l_ponto('x');
      l_anterior('y') := l_ponto('y');
      FOR lon IN -18 .. 18 LOOP
        l_longitude := lon * 10;
        l_ponto := converter_geo_pixel (90, l_longitude, p_largura, p_altura, p_projecao);
        IF (l_ponto('x') != l_anterior('x')) OR (l_ponto('y') != l_anterior('y')) THEN
          l_svg := l_svg || ' L' || l_ponto('x') || ',' || l_ponto('y');
          l_anterior('x') := l_ponto('x');
          l_anterior('y') := l_ponto('y');
        END IF;
      END LOOP;
      FOR lat IN REVERSE -45 .. 45 LOOP
        l_latitude := lat * 2;
        l_ponto := converter_geo_pixel (l_latitude, 180, p_largura, p_altura, p_projecao);
        IF (l_ponto('x') != l_anterior('x')) OR (l_ponto('y') != l_anterior('y')) THEN
          l_svg := l_svg || ' L' || l_ponto('x') || ',' || l_ponto('y');
          l_anterior('x') := l_ponto('x');
          l_anterior('y') := l_ponto('y');
        END IF;
      END LOOP;
      FOR lon IN REVERSE -18 .. 18 LOOP
        l_longitude := lon * 10;
        l_ponto := converter_geo_pixel (-90, l_longitude, p_largura, p_altura, p_projecao);
        IF (l_ponto('x') != l_anterior('x')) OR (l_ponto('y') != l_anterior('y')) THEN
          l_svg := l_svg || ' L' || l_ponto('x') || ',' || l_ponto('y');
          l_anterior('x') := l_ponto('x');
          l_anterior('y') := l_ponto('y');
        END IF;
      END LOOP;
      FOR lat IN -45 .. 45 LOOP
        l_latitude := lat * 2;
        l_ponto := converter_geo_pixel (l_latitude, -180, p_largura, p_altura, p_projecao);
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
    l_svg VARCHAR2(1000);
    
  BEGIN
    l_svg := '<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="' || p_largura || '" height="' || p_altura || '">' || chr(13) || chr(10);
    IF p_moldura THEN
      l_svg := l_svg || '<rect x="0" y="0" width="' || p_largura || '" height="' || p_altura || '" fill="none" stroke="rgb(150,150,150)" stroke-width="1" />' || chr(13) || chr(10);
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
  
END world_map_projections;
/
