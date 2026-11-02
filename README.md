## Mapa-múndi com cidades capitais, em PL/SQL

Um mapa da superfície total da Terra, construído por uma *package* em PL/SQL, projetado a partir dos dados fornecidos por [Natural Earth](https://www.naturalearthdata.com/) e da base de dados, com 26.569 cidades do mundo, fornecida gratuitamente por [SimpleMaps.com](https://simplemaps.com/data/world-cities).

O Natural Earth fornece arquivos, no formato GeoJSON, com as coordenadas geográficas de todo o perímetro de cada país. Esta *package* em PL/SQL, converte estas coordenadas geográficas em pontos para a construção dos polígonos em SVG. A base do SimpleMaps foi importada para o banco de dados Oracle, para consulta pela *package*, e assim, pontuar as cidades capitais.

Esta *package* em PL/SQL foi desenvolvida de uma forma simples para facilitar o entendimento dos algoritmos, e aceita parâmetros para especificar a largura e a altura do mapa, como também o algoritmo de projeção. Incluem-se aqui, também, o código de uma página em APEX para demostrar a utilização desta *package*.

:eyes: Esta *package* foi baseada no *script* em PHP publicado em [Mapa-múndi com cidades capitais](https://github.com/danmadeira/mapa-mundi)

### Sistemas de projeções

- Eckert IV;
- Eckert VI;
- Equal Earth;
- Equidistante;
- Gott equal-area elliptical;
- Gott–Mugnolo azimuthal;
- Hammer;
- Kavrayskiy VII;
- Lambert azimuthal equal-area;
- Mercator;
- Miller cylindrical;
- Mollweide;
- Natural Earth;
- Natural Earth II;
- Patterson;
- Robinson;
- Sinusoidal;
- Wagner VI;
- Winkel Tripel.

### Cálculo da conversão

Para elaborar um mapa é necessário determinar um método no qual cada ponto na superfície terrestre corresponda a um ponto no plano e vice-versa. Existem diversos métodos e estes métodos são chamados sistemas de projeções. Um sistema de projeção adota um modelo matemático do planeta e relaciona os pontos estabelecendo uma escala e um sistema de coordenadas.

O sistema de coordenadas geográficas expressa a posição de um ponto na superfície do globo terrestre. O globo é dividido em latitudes que vão de 0 a 90 graus, a partir da linha do Equador, separando os hemisférios Norte e Sul, e longitudes que vão de 0 a 180 graus, a partir do meridiano de Greenwich, separando os hemisférios Leste e Oeste. A latitude e a longitude, na notação decimal, são indicadas com valor negativo quando representam uma latitude Sul ou uma longitude Oeste. Enquanto a maioria das projeções desta *package* adota o modelo esférico da Terra, a projeção *Equal Earth* considera o modelo elipsoidal (WGS84), convertendo previamente a latitude geográfica em **latitude autálica** (latitude equivalente em uma esfera de mesma área superficial) para compensar o achatamento polar do planeta.

O sistema de coordenadas cartesiano serve para especificar pontos num determinado espaço plano. Tem uma semelhança com o sistema de coordenadas geográficas pois a origem, a coordenada `(0,0)`, está no centro. No entanto, em computação gráfica e no formato SVG, as imagens possuem um sistema de coordenadas de pixel cuja origem `(0,0)` está no canto superior esquerdo, com o eixo horizontal (`x`) crescendo para a direita e o eixo vertical (`y`) crescendo para baixo, assumindo apenas valores positivos limitados pela largura e pela altura da imagem.

Para adaptar o sistema cartesiano (que admite valores negativos e tem o eixo vertical orientado para cima) ao sistema de pixels da imagem, utilizam-se um falso leste e um falso norte implementados pelas coordenadas do centro da imagem $(\text{centro}_x, \text{centro}_y)$. Na conversão final, o deslocamento horizontal projetado é somado ao centro ($\text{centro}_x + x \cdot \text{módulo}$), o deslocamento vertical é subtraído do centro para inverter a orientação do eixo $y$ ($\text{centro}_y - y \cdot \text{módulo}$), e ambos os valores são arredondados para o inteiro mais próximo por meio de `round`, evitando desvios sistemáticos de truncamento.

Para regular as proporções entre os sistemas de coordenadas e eliminar espaços vazios excedentes ao redor do mapa, a *package* calcula dinamicamente os limites cartesianos máximos exatos de cada projeção: o semieixo horizontal máximo $x_{\max}$ (no Equador, $\text{longitude} = 180^\circ$) e o semieixo vertical máximo $y_{\max}$ (no Polo Norte, $\text{latitude} = 90^\circ$). A partir das dimensões máximas solicitadas pelo usuário, obtêm-se os fatores de escala candidatos $\text{módulo}_x = \frac{\text{largura}}{2 \cdot x_{\max}}$ e $\text{módulo}_y = \frac{\text{altura}}{2 \cdot y_{\max}}$. A *package* adota o menor módulo para garantir que toda a projeção caiba nos limites informados e, em seguida, recalcula a dimensão excedente ($\text{largura útil} = \text{round}(2 \cdot x_{\max} \cdot \text{módulo}_y)$ ou $\text{altura útil} = \text{round}(2 \cdot y_{\max} \cdot \text{módulo}_x)$), fazendo com que a imagem SVG final tenha exatamente a dimensão útil da projeção escolhida.

Tudo na imagem é construído de acordo com a projeção escolhida e proporcionalmente à área útil da imagem. Esta *package* constrói os polígonos dos países, que juntos formam os continentes, as linhas imaginárias dos paralelos, meridianos, trópicos e círculos polares, os pontos das cidades capitais e o fundo azul dentro dos limites da projeção.

### O mapa executado no Oracle APEX

![Mapa-múndi](img/mapamundi.png?raw=true)

<p align="center">projeção Natural Earth II</p>

### Estrutura e funcionamento da package

A *package* `world_map_projections` ([`sql/world_map_projections.sql`](sql/world_map_projections.sql)) está estruturada em cinco camadas internas que trabalham em conjunto para transformar os dados geográficos armazenados no banco de dados Oracle em um documento SVG completo:

#### 1. Acumulador de texto SVG em memória (`svg_t`)
Como um mapa-múndi completo em alta resolução ultrapassa facilmente o limite de `32.767` bytes do tipo `VARCHAR2` no PL/SQL, a *package* utiliza um registro acumulador interno (`svg_t`) composto por um buffer `VARCHAR2(32767 BYTE)` e um `CLOB` temporário:
- `svg_iniciar`: aloca um `CLOB` temporário com duração de chamada (`dbms_lob.call`).
- `svg_anexar`: concatena trechos de SVG no buffer em memória rápida (`VARCHAR2`) e só descarrega no `CLOB` (`svg_descarregar`) quando o buffer se aproxima do limite de `32.767` bytes, evitando o custo de I/O de concatenar no `CLOB` a cada coordenada.
- `svg_finalizar`: descarrega o conteúdo remanescente do buffer e retorna o `CLOB` final.

#### 2. Núcleo matemático e sistemas de projeção
Cada projeção possui subprogramas específicos para transformar coordenadas geográficas $(\phi, \lambda)$ em coordenadas cartesianas planas $(x, y)$:
- **Conversão autálica (`converter_geografica_para_autalica`)**: converte a latitude geodésica (elipsoide WGS84) em latitude autálica (esfera de mesma área superficial) para uso na projeção *Equal Earth*.
- **Equações paramétricas e iterativas**: projeções como *Eckert IV* (`e`), *Eckert VI* (`E`), *Mollweide* (`M`) e *Gott equal-area elliptical* (`g`) resolvem equações transcendentais pelo método numérico de Newton-Raphson (até 10 iterações com tolerância de $10^{-7}$ e proteção contra derivada nula nos polos).
- **Cálculo acoplado de $(x, y)$**: projeções cujas fórmulas de $x$ e $y$ compartilham termos intermediários ou iterações (*Gott elíptica*, *Gott–Mugnolo azimutal*, *Hammer*, *Lambert azimutal*, *Winkel Tripel* e *Equal Earth*) dispõem de procedimentos unificados que calculam ambas as coordenadas simultaneamente, evitando retrabalho trigonométrico.
- **Códigos identificadores (`p_projecao`)**:

| Código | Projeção | Tipo / Propriedade principal |
| :---: | :--- | :--- |
| `c` | Plate Carrée (Equidistante Cilíndrica) | Cilíndrica equidistante (padrão) |
| `e` | Eckert IV | Pseudocilíndrica equivalente (igual área) |
| `E` | Eckert VI | Pseudocilíndrica equivalente (igual área) |
| `g` | Gott Equal-Area Elliptical | Elíptica equivalente (igual área) |
| `G` | Gott–Mugnolo Azimuthal | Azimutal equatorial |
| `h` | Hammer | Azimutal modificada equivalente |
| `i` | Equal Earth | Pseudocilíndrica equivalente (com latitude autálica) |
| `k` | Kavrayskiy VII | Pseudocilíndrica de compromisso |
| `l` | Lambert Azimuthal Equal-Area | Azimutal equatorial equivalente |
| `m` | Miller Cylindrical | Cilíndrica modificada |
| `M` | Mollweide | Pseudocilíndrica equivalente (igual área) |
| `n` | Natural Earth | Pseudocilíndrica de compromisso |
| `N` | Natural Earth II | Pseudocilíndrica de compromisso |
| `p` | Patterson | Cilíndrica de compromisso |
| `r` | Robinson | Pseudocilíndrica de compromisso |
| `s` | Sinusoidal | Pseudocilíndrica equivalente (igual área) |
| `t` | Mercator | Cilíndrica conforme |
| `w` | Wagner VI | Pseudocilíndrica de compromisso |
| `W` | Winkel Tripel (Winkel III) | Azimutal modificada de compromisso |

#### 3. Ajuste da área útil e conversão para pixels
- `obter_limites_projecao`: calcula os limites cartesianos máximos da projeção — $x_{\max}$ no Equador ($\text{lon} = 180^\circ$) e $y_{\max}$ no Polo Norte ($\text{lat} = 90^\circ$) — e os mantém em cache de pacote (`g_ultima_projecao`, `g_cache_x_max`, `g_cache_y_max`) para evitar recálculos a cada ponto.
- `ajustar_dimensoes_uteis`: a partir das dimensões máximas solicitadas (`p_largura` e `p_altura`), calcula os módulos horizontal e vertical e reduz a dimensão excedente para que o elemento `<svg>` tenha exatamente o tamanho ocupado pela projeção, sem faixas vazias ao redor do mapa.
- `converter_geo_pixel`: obtém o ponto projetado $(x, y)$, aplica o fator de escala (`l_modulo`) determinado pelos limites da projeção e soma o deslocamento do centro da imagem (`coordenar_centro`), arredondando o resultado final com `round`.

#### 4. Construção das camadas vetoriais do mapa
Cada camada do mapa pode ser gerada individualmente ou de forma integrada:
- **Estrutura raiz (`exibir_abertura_svg` e `exibir_fechamento_svg`)**: abre a tag `<svg>` com `width` e `height` já ajustados à área útil da projeção (incluindo opcionalmente um `<rect>` de moldura quando `p_moldura = true`) e encerra com `</svg>`.
- **Oceano (`exibir_fundo_azul`)**: desenha a silhueta da superfície terrestre preenchida em azul — usando `<circle>` nas projeções azimutais (`l` e `G`) ou `<path>` percorrendo o perímetro polar e os meridianos extremos ($\pm 180^\circ$ e $\pm 90^\circ$) a cada $2^\circ$ nas demais projeções.
- **Continentes (`exibir_continente`, `extrair_poligono` e `carregar_arquivo_json`)**: carrega o arquivo GeoJSON armazenado em `BLOB` na tabela `repositorio`, filtra as feições pelo continente (`CONTINENT`), percorre anéis de geometrias `Polygon` e `MultiPolygon`, valida se $(\text{lat}, \text{lon}) \in [-90, 90] \times [-180, 180]$ e gera elementos `<polygon>`, omitindo vértices consecutivos que resultem no mesmo pixel.
- **Cidades capitais (`exibir_cidades`)**: consulta as capitais (`capital = 'primary'`) na tabela `worldcities`, ordenadas por população, e renderiza elementos `<circle>` com `<title>` informativo e raio customizado (`p_raio`) ou proporcional à largura útil da imagem (`round(largura * 0.003)`, com mínimo de `1` pixel).
- **Grade cartográfica (`exibir_grade`)**: reúne os paralelos de $15^\circ$ a $75^\circ$ N/S (`exibir_paralelos`), os meridianos a cada $15^\circ$ (`exibir_meridianos`, com descontinuidade tratada no antípoda $(0^\circ, \pm 180^\circ)$ das projeções azimutais), os trópicos de Câncer/Capricórnio e círculos polares tracejados (`exibir_circulos`), a linha do Equador (`exibir_equador`) e o meridiano de Greenwich (`exibir_greenwich`). Paralelos curvos são amostrados a cada $2^\circ$ (`<path>`), enquanto paralelos retos utilizam `<line>`.

#### 5. Renderização e envio ao Oracle APEX (`exibir_mapa` e `imprimir_clob`)
- `imprimir_clob`: fatia qualquer `CLOB` em blocos de `8.000` caracteres (seguros para codificação multibyte `AL32UTF8` dentro do limite de `32.767` bytes do `htp.prn`) e os envia sequencialmente ao buffer HTTP.
- `exibir_mapa`: procedimento de alto nível utilizado em regiões de conteúdo dinâmico do Oracle APEX. Ajusta as dimensões úteis uma única vez e emite em sequência a abertura do SVG, o fundo azul, os oito grupos continentais, as cidades capitais, a grade cartográfica e o fechamento do SVG, liberando os `CLOB`s temporários ao término de cada camada.

### Referências

- BUGAYEVSKIY, L. M.; SNYDER, J. P. *Map Projections A Reference Manual*. Taylor & Francis Ltd, 1995.

- BUTLER, H. et al. *The GeoJSON Format*. IETF RFC 7946. August 2016. Disponível em: <https://tools.ietf.org/html/rfc7946>

- CHANG, K. *Introduction to Geographic Information Systems, Ninth Edition*. McGraw-Hill Education, 2018.

- COWBURN, P. e col. *Manual do PHP*. PHP Documentation Group. 25 de Dezembro de 2020. Disponível em: <https://www.php.net/manual/pt_BR/index.php>

- DEAKIN, R. E. *A Guide to the Mathematics of Map Projections*. Victorian Tasmanian Survey Conference: Across the Strait, Launceston Tasmania. April 15-17, 2004.

- DMA WGS 84 Development Committee *Supplement to Department of Defense World Geodetic System 1984 Technical Report: Part II - Parameters, Formulas, and Graphics for the Practical Application of WGS 84*. Technical Report 8350.2-B, Second Printing. Defense Mapping Agency, December 1st, 1987. Disponível em: <https://earth-info.nga.mil/GandG/publications/tr8350.2/TR8350.2-b/DMA%20TR8350.pdf>

- ESRI *Understanding Map Projections*. GIS by ESRI. Environmental Systems Research Institute, 2000. Disponível em: <http://downloads2.esri.com/support/documentation/ao_/710Understanding_Map_Projections.pdf>

- FENNA, D. *Cartographic Science: A Compendium of Map Projections, with Derivations*. CRC Press, Taylor & Francis Group, 2007.

- GOLDBERG, D. M.; GOTT, J. R. *Flexion and Skewness in Map Projections of the Earth*. Cartographica: The International Journal for Geographic Information and Geovisualization. Volume 42 Issue 4, pp. 297-318, December 18, 2007. Disponível em: <https://www.physics.drexel.edu/~goldberg/projections/goldberg_gott.pdf>

- GOTT, J. R.; GOLDBERG, D. M.; VANDERBEI, R. J. *Flat Maps that improve on the Winkel Tripel*. arXiv Astrophysics, Cornell University, February 2021. Disponível em: <https://arxiv.org/abs/2102.08176>

- GOTT, J. R.; MUGNOLO, C.; COLLEY, W. N. *Map Projections Minimizing Distance Errors*. Cartographica: The International Journal for Geographic Information and Geovisualization, Volume 42 Issue 3, pp. 219-234, Fall 2007. Disponível em: <https://arxiv.org/pdf/astro-ph/0608500.pdf>

- HOOIJBERG, M. *Practical Geodesy: Using Computers*. Springer-Verlag, Berlin, Heidelberg, 1997.

- IOGP *Coordinate Conversions and Transformations including Formulas*. IOGP Publication 373-7-2, Geomatics Guidance Note number 7, part 2, October 2020. Disponível em: <https://epsg.org/guidance-notes.html>

- IPBUKER, C. *A computational approach to the Robinson projection*. Survey Review. Volume 38, Issue 297, pp. 204-217, July 2005.

- JENNY, B. *Adaptive Composite Map Projections*. IEEE Transactions on Visualization and Computer Graphics, Volume 18, Issue 12, p. 2575-2582, December 2012.

- JENNY, B. *Java Map Projection Library*. Cartography and Geovisualization Group, Oregon State University. June 12, 2019. Disponível em: <https://github.com/OSUCartography/JMapProjLib>

- JENNY, B.; ŠAVRIČ, B.; PATTERSON, T. *A compromise aspect-adaptive cylindrical projection for world maps*. International Journal of Geographical Information Science, Vol. 29, No. 6, pp. 935-952, 2015.

- JENNY, B.; ŠAVRIČ, B.; STREBE, D. R. *A computational method for the Hufnagel pseudocylindric map projection family*. Cartography and Geographic Information Science, Vol. 44, No. 1, pp. 86-94, 2017.

- KELSO, N. V. *Natural Earth vector*. Disponível em: <https://github.com/nvkelso/natural-earth-vector>

- KELSO, N. V.; PATTERSON, T. *Natural Earth*. Disponível em: <https://www.naturalearthdata.com/>

- KENNEDY, M.; KOPP, S. *Understanding Map Projections*. GIS by ESRI. Esri Press, 2000. Disponível em: <https://giscourses.cfans.umn.edu/sites/giscourses.cfans.umn.edu/files/understanding_map_projections.pdf>

- MOON, P.; SPENCER, D. E. *Field Theory Handbook: Including Coordinate Systems, Differential Equations and Their Solutions*. Corrected 3rd Printing, Springer-Verlag, 1988.

- NGA *Map Projections for GEOINT Content, Products, and Applications*. Standardization Implementation Guidance 28, Version 1.0. National Geospatial-Intelligence Agency Standardization Document. U.S.A., December 13, 2017. Disponível em: <ftp://ftp.nga.mil/pub2/gandg/website/coordsys/resources/NGA.SIG.0028_1.0_MAPPROJ.PDF>

- NIMA *Technical Report 8350.2, Department of Defense World Geodetic System 1984: Its Definition and Relationships with Local Geodetic Systems, Third Edition, Amendment 1*. Geodesy and Geophysics Department, National Imagery and Mapping Agency. January 3, 2000. Disponível em: <https://earth-info.nga.mil/GandG/publications/tr8350.2/wgs84fin.pdf>

- OSGeo Project. *GeoTools*, Release 24.2. Open Source Geospatial Foundation, January 20, 2021. Disponível em: <https://geotools.org/>

- PATTERSON, T.; ŠAVRIČ, B.; JENNY, B. *Introducing the Patterson Cylindrical Projection*. Cartographic Perspectives, Number 78, pp. 77-81, December 2014. Disponível em: <https://cartographicperspectives.org/index.php/journal/article/view/cp78-patterson-et-al/1361>

- PEARSON, F. *Map Projections: Theory and Applications*. CRC Press, Taylor & Francis Group, 1990.

- PROJ contributors *PROJ coordinate transformation software library*, Release 7.2.1. Open Source Geospatial Foundation, January 1st 2021. Disponível em: <https://proj.org/>

- ŠAVRIČ, B.; JENNY, B.; PATTERSON, T.; PETROVIČ, D.; HURNI, L. *A Polynomial Equation for the Natural Earth Projection*. Cartography and Geographic Information Science, Vol. 38, No. 4, pp. 363-372, 2011.

- ŠAVRIČ, B.; PATTERSON, T.; JENNY, B. *The Equal Earth map projection*. International Journal of Geographical Information Science, Volume 33, Issue 3, pp. 454-465, 2019.

- ŠAVRIČ, B.; PATTERSON, T.; JENNY, B. *The Natural Earth II world map projection*. International Journal of Cartography, 
Volume 1, Issue 2, pp. 123–133, 2015.

- SimpleMaps.com *How to import a CSV file into MySQL*. Tutorials and Articles. Disponível em: <https://simplemaps.com/resources/import-csv-mysql>

- SimpleMaps.com *World Cities Database*. SimpleMaps Worldcities Basic v1.73. Disponível em: <https://simplemaps.com/data/world-cities>

- SNYDER, J. P.; VOXLAND, P. M. *An Album of Map Projections*, 2nd Printing. U.S. Geological Survey Professional Paper 1453, United States Government Printing Office, Washington, 1989. Disponível em: <https://pubs.usgs.gov/pp/1453/report.pdf>

- SNYDER, J. P. *Flattening the Earth: Two thousand years of map projections*. University of Chicago Press, Chicago, 1993.

- SNYDER, J. P. *Map Projections - A Working Manual*, U.S. Geological Survey Professional Paper 1395, Supersedes USGS Bulletin 1532, United States Government Printing Office, Washington, 1987. Disponível em: <https://pubs.usgs.gov/pp/1395/report.pdf>

- W3C *Scalable Vector Graphics (SVG) 1.1 (Second Edition)*, W3C Recommendation 16 August 2011. Disponível em: <https://www.w3.org/TR/SVG11/>

- WEISSTEIN, E. W. *Lambert Azimuthal Equal-Area Projection*. MathWorld: A Wolfram Web Resource. Disponível em: <https://mathworld.wolfram.com/LambertAzimuthalEqual-AreaProjection.html>

- WEISSTEIN, E. W. *Miller Cylindrical Projection*. MathWorld: A Wolfram Web Resource. Disponível em: <https://mathworld.wolfram.com/MillerCylindricalProjection.html>

- WEISSTEIN, E. W. *Mollweide Projection*. MathWorld: A Wolfram Web Resource. Disponível em: <https://mathworld.wolfram.com/MollweideProjection.html>
