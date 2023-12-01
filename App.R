# Bibliotecas Shiny
library(shinydashboard)
library(leaflet)
library(shiny)
library(here)
library(fresh)
library(shinythemes)
library(magrittr)
library(rvest)
library(readxl)
library(maps)
library(ggplot2)
library(reshape2)
library(ggiraph)
library(RColorBrewer)
library(plotly)
library(geojsonio)
library(shinyWidgets)
library(stringr)

# Bibliotecas operacionais
library(forecast)
library(dplyr)
library(fpp3)
library(tsibble)
library(zoo)
library(readr)
library(imputeTS) # biblioteca que faz a interpolação das observações


# Leitura dos dados
est_nomes <- read.csv("nomes_codigos_estacoes.csv", sep=",", header = TRUE)

cidades <- read.csv("CatalogoEstaçõesAutomáticas.csv", sep=";", header = TRUE)

# Criando uma coluna com o nome da cidade e estado
cidades_mod <- data.frame(coluna1 = cidades$DC_NOME,coluna2 = cidades$SG_ESTADO, codigo = cidades$CD_ESTACAO)
cidades_mod$coluna1 <- str_to_title(tolower(cidades_mod$coluna1))
cidades_mod$estacao <- paste(cidades_mod$coluna1, cidades_mod$coluna2, sep = "-")

cidades_mod = cidades_mod[,c("codigo", "estacao")]

# Manipulação dos nomes das estações
epc = function(e) filter(est_nomes, estacao == e)$codigo # converte estacao para codigo
cpe = function(c) filter(est_nomes, codigo == c)$estacao # converte codigo para estacao

# Manipulação dos nomes das variáveis
var_nomes = data.frame(variavel = c("Tair_mean..c.", "Tair_min..c.", "Tair_max..c.", "Dew_tmean..c.", "Dew_tmin..c.", "Dew_tmax..c.", "Dry_bulb_t..c.", "Rainfall..mm.", "Rh_mean..porc.", "Rh_max..porc.", "Rh_min..porc.", "Ws_10..m.s.1.", "Ws_gust..m.s.1.", "Wd..degrees.", "Sr..Mj.m.2.day.1."),
                       titulo = c("Temperatura média do ar", "Temperatura mínima do ar", "Temperatura máxima do ar", "Temperatura do ponto de orvalho média", "Temperatura do ponto de orvalho mínima", "Temperatura do ponto de orvalho máxima", "Temperatura de bulbo seco", "Precipitação total", "Umidade relativa do ar média", "Umidade relativa do ar máxima", "Umidade relativa do ar mínima", "Velocidade do vento a 10 metros de altura", "Rajada de vento", "Direção do vento", "Radiação solar"),
                       legenda = c('Temperatura (°C)', 'Temperatura (°C)', 'Temperatura (°C)', 'Temperatura (°C)', 'Temperatura (°C)', 'Temperatura (°C)', 'Temperatura (°C)', 'Precipitação (mm)', 'Umidade (%)', 'Umidade (%)', 'Umidade (%)', 'Velocidade do vento (m/s)', 'Velocidade do vento (m/s)', 'Direção do vento (°)', "Radiação solar (MJ/m^2)"))


vpt = function(v) filter(var_nomes, variavel == v)$titulo # converte variável para título
vpl = function(v) filter(var_nomes, variavel == v)$legenda # converte variável para legenda
tpv = function(t) filter(var_nomes, titulo == t)$variavel # converte título para variável

carrega_estacao = function(cod_estacao){
  dados = read.csv(paste("estacoes/", cod_estacao, ".csv", sep=""), sep=",", header = TRUE)
  dados$Date = as.Date(dados$Date)
  
  # adiciona as datas faltantes
  menor_dia = min(dados$Date, na.rm=TRUE)
  maior_dia = max(dados$Date, na.rm=TRUE)
  
  ts = seq.Date(menor_dia, maior_dia, by="day")
  df = data.frame(Date=ts)
  dados = merge(df, dados, by='Date', all.x = TRUE, all.y = F)
  
  dados[is.na(dados$Station_code),"Station_code"] = unique(na.omit(dados$Station_code))
  dados[is.na(dados$Station),"Station"] = unique(na.omit(dados$Station))
  dados[is.na(dados$UF),"UF"] = unique(na.omit(dados$UF))
  
  # imputa os dados
  dados <- na_interpolation(dados, option = "stine")
  # dados <- na_seadec(dados, algorithm = "interpolation")
  
  return(dados)
}

medias_por_ano <- read_csv("medias_por_ano.csv", col_types = cols(...1 = col_skip()))
medias_por_ano = na.omit(medias_por_ano)


# UI
ui <- fluidPage(
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">Meteorologia INMET</a>'), id="nav",
             windowTitle = "Meteorologia INMET",
             
             ## Análise geográfica
             tabPanel("Análise geográfica",
                      div(class="outer",
                          tags$head(includeCSS("styles.css")),
                          leafletOutput("map", width="100%", height="100%"),
                          
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 140, left = 55, width = 400, fixed=TRUE,
                                        draggable = F, height = "auto", h3("Análise geográfica"),
                                        
                                        selectInput("var","Var",label = h4("Selecione a variável:"), choices = var_nomes$titulo),
                                        sliderInput("ano", label = h4("Selecione o ano"), min = 2000, max = 2022, value = 2019, sep = ""), hr(),
                                        fluidRow(column(4, verbatimTextOutput("value"))),
                                        tags$div(id="cite", h6('Dados retirados do portal INMET.'))))
             ),
             
             ## Análise temporal
             tabPanel("Análise Temporal",
                      navlistPanel(widths=c(2, 10),
                                   tabPanel("Média móvel", icon = icon("chart-line"),
                                            sidebarLayout(
                                              sidebarPanel(width = 3,
                                                           numericInput("mediamovel_k", h5("Número de observações utilizadas na média:"), value = 30, min = 0),
                                                           selectInput("mediamovel_var", h5("Selecione a variável:"), var_nomes$titulo),
                                                           selectInput("mediamovel_est", h5("Selecione a estação:"), est_nomes$estacao),
                                                           dateInput("mediamovel_data_i", h5("Data de início"), "2015-01-01"),
                                                           dateInput("mediamovel_data_f", h5("Data de fim"), "2016-01-01"),
                                                           tags$div(id = "cite", h6('Dados retirados do portal INMET.'))
                                              ),
                                              mainPanel(plotOutput("graph_media_movel"),
                                                        helpText("Gráficos de médias móveis são úteis para identificar tendências na série temporal. A linha das médias móveis representa o valor médio dos pontos de dados dentro da janela de tempo especificada que você pode selecionar.",tags$br(),
                                                                 "Se a linha estiver subindo, há uma indicação de uma tendência ascendente nos dados. Se a linha estiver em declínio, há uma indicação de uma tendência descendente.",tags$br(),
                                                                 "Observe casos em que a média móvel e os dados brutos divergem significativamente. A divergência pode indicar potenciais pontos de viragem ou reversões na tendência."))
                                            )
                                   ),
                                   tabPanel("Gráfico de Sazonalidade", icon = icon("chart-line"), 
                                            sidebarLayout(
                                              sidebarPanel(width = 3,
                                                           selectInput("sazonalidade_periodo", h5("Selecione o período:"), c("week","month","year"), selected = "year"),
                                                           selectInput("sazonalidade_var", h5("Selecione a variável:"), var_nomes$titulo),
                                                           selectInput("sazonalidade_est", h5("Selecione a estação:"), est_nomes$estacao),
                                                           dateInput("sazonalidade_data_i", h5("Data de início"), "2013-01-01"),
                                                           dateInput("sazonalidade_data_f", h5("Data de fim"), "2020-01-01"),
                                                           tags$div(id = "cite", h6('Dados retirados do portal INMET.'))
                                              ),
                                              mainPanel(plotOutput("graph_sazonalidade"),
                                                        helpText("Gráficos sazonais são úteis para identificar padrões e tendências regulares que se repetem em intervalos aproximadamente fixos, ou identificar pontos de virada e o período em que ocorreu. A interpretação de um gráfico sazonal envolve a análise de padrões e flutuações nos dados em intervalos de tempo."))
                                            )
                                   ),
                                   tabPanel("Gráfico de Defasagens", icon = icon("chart-line"), 
                                            sidebarLayout(
                                              sidebarPanel(width = 3,
                                                           selectInput("lag_var", h5("Selecione a variável:"), var_nomes$titulo),
                                                           selectInput("lag_est", h5("Selecione a estação:"), est_nomes$estacao),
                                                           dateInput("lag_data_i", h5("Data de início"), "2013-01-01"),
                                                           dateInput("lag_data_f", h5("Data de fim"), "2020-01-01"),
                                                           tags$div(id = "cite", h6('Dados retirados do portal INMET.'))
                                              ),
                                              mainPanel(plotOutput("graph_lag"),
                                                        helpText(HTML("Gráficos de defasagens são úteis para a avaliar autocorrelação, ou seja, verificam se uma série temporal é aleatória ou não.<br>",
                                                                      "<ul>
                                                                 <li> O eixo horizontal mostra o valor da variável. </li>
                                                                 <li> O eixo vertical mostra o valor da variável para k = 6 meses (primeiro plot) e k = 12 meses (segundo plot). </li>
                                                                 </ul>",
                                                                      "As cores representam cada mês do ano.<br>",
                                                                      "Se os pontos no gráfico de defasagens se agruparem em torno da linha diagonal tracejada em cor cinza, há indicação  de autocorrelação positiva. Ou seja, a variável está positivamente correlacionada com seus valores passados.<br>",
                                                                      "Se os pontos se agruparem em torno de uma linha diagonal do canto superior esquerdo ao canto inferior direito, isso sugere autocorrelação negativa. Neste caso, a variável está negativamente correlacionada com seus valores passados.<br>",
                                                                      "Se os pontos estiverem espalhados aleatoriamente sem formar um padrão claro, há indicação de que não há autocorrelação significativa.")))
                                            )
                                   ),
                                   tabPanel("Gráfico de Sub-séries", icon = icon("chart-line"), 
                                            sidebarLayout(
                                              sidebarPanel(width = 3,
                                                           selectInput("subserie_var", h5("Selecione a variável:"), var_nomes$titulo),
                                                           selectInput("subserie_est", h5("Selecione a estação:"), est_nomes$estacao),
                                                           dateInput("subserie_data_i", h5("Data de início"), "2013-01-01"),
                                                           dateInput("subserie_data_f", h5("Data de fim"), "2020-01-01"),
                                                           tags$div(id = "cite", h6('Dados retirados do portal INMET.'))
                                              ),
                                              mainPanel(plotOutput("graph_subserie"),
                                                        helpText(HTML("Gráficos de sub-séries são úteis para identificar mudanças em períodos específicos e padrões sazonais. Neste gráfico, os dados de cada mês são coletados de forma conjunta e separados em mini plots. Esta forma de gráfico permite que o padrão sazonal subjacente seja visualizado de forma mais clara.<br>",
                                                                      "<ul>
                                                                 <li> Eixo vertical: variável resposta. </li>
                                                                 <li> Eixo horizontal: Tempo ordenado por mês do ano. Por exemplo, todos os valores de janeiro são plotados (em ordem cronológica), depois todos os valores de fevereiro, e assim por diante. </li>
                                                                 </ul>",
                                                                      "As linhas em azul representam as médias dos meses conforme os anos escolhidos.<br>",
                                                                      "Compare as alturas dos picos e vales em diferentes meses. Esta comparação ajuda a identificar os meses com maior impacto na variável resposta.<br>",
                                                                      "Observe se há um padrão dentro do mês (por exemplo, janeiro e dezembro apresentam padrões semelhantes?).<br>",
                                                                      "Procure mudanças nos padrões sazonais em diferentes meses. Uma modificação pode indicar uma mudança no início ou no final de uma temporada específica.")))
                                            )
                                   ),
                                   tabPanel("Wetbulb", icon = icon("chart-line"), 
                                            sidebarLayout(
                                              sidebarPanel(width = 3,
                                                           selectInput("wetbulb_est", h5("Selecione a estação:"), est_nomes$estacao),
                                                           dateInput("wetbulb_data_i", h5("Data de início"), "2019-01-01"),
                                                           dateInput("wetbulb_data_f", h5("Data de fim"), "2020-01-01"),
                                                           tags$div(id = "cite", h6('Dados retirados do portal INMET.'))
                                              ),
                                              mainPanel(plotOutput("graph_wetbulb"),
                                                        helpText(HTML("O gráfico visa visualizar como as diferentes combinações de temperatura e umidade afetam a habitabilidade humana. As zonas de conforto são identificadas com cores diferentes para indicar diferentes níveis de conforto, risco, criticidade ou perigo, dependendo das condições de Wet Bulb.<br>",
                                                                      "<br>",
                                                                      "Eixo X (Temperatura Ambiente (°C)): Este eixo representa a temperatura ambiente em graus Celsius.<br>",
                                                                      "Eixo Y (Umidade Relativa (%)): Este eixo representa a umidade relativa em percentagem.<br>",
                                                                      "Legenda (Zonas): Esta legenda fornece uma chave para as diferentes zonas identificadas no gráfico. As zonas são caracterizadas por,<br>",
                                                                      "<ul>
                                                                 <li> Confortável: Uma cor verde pálido. </li>
                                                                 <li> Risco: Uma cor laranja claro. </li>
                                                                 <li> Crítico: Uma cor vermelha claro. </li>
                                                                 <li> Perigoso: Uma cor vermelha mais escura. </li>
                                                                 </ul>",
                                                                      "Os pontos azuis representam os dados observados de temperatura e umidade.")))
                                            )
                                   ),
                                   tabPanel("Gráfico de Autocorrelação", icon = icon("chart-line"), 
                                            sidebarLayout(
                                              sidebarPanel(width = 3,
                                                           selectInput("autocorr_var", h5("Selecione a variável:"), var_nomes$titulo),
                                                           selectInput("autocorr_est", h5("Selecione a estação:"), est_nomes$estacao),
                                                           dateInput("autocorr_data_i", h5("Data de início"), "2013-01-01"),
                                                           dateInput("autocorr_data_f", h5("Data de fim"), "2020-01-01"),
                                                           tags$div(id = "cite", h6('Dados retirados do portal INMET.'))
                                              ),
                                              mainPanel(plotOutput("graph_autocorr"),
                                                        br(), br(),
                                                        verbatimTextOutput("stats_autocor"),
                                                        helpText(HTML("Este gráfico é útil para identificar padrões temporais na série de dados. Por exemplo, se houver uma autocorrelação significativa em uma determinada defasagem, isso pode indicar a presença de padrões sazonais ou tendências temporais nos dados.<br>",
                                                                      "<br>",
                                                                      "<ul>
                                                                      <li> Eixo X (Defasagem): A defasagem (lag) representa o número de períodos anteriores que estão sendo usados para calcular a correlação com o período atual. Por exemplo, uma defasagem de 1 indica a correlação entre os dados no momento atual e os dados de um período anterior.</li>
                                                                      <li> Eixo Y (Autocorrelação): A autocorrelação é uma medida estatística que indica o grau de correlação entre uma série temporal e uma versão deslocada (defasada) de si mesma. Varia de -1 a 1, onde 1 indica uma correlação perfeita, -1 indica uma correlação inversa perfeita e 0 indica ausência de correlação.</li>
                                                                      <li> Linhas verticais: As linhas azuis no gráfico representam os valores de autocorrelação para diferentes defasagens. Cada ponto no gráfico indica a autocorrelação para uma determinada defasagem.</li>
                                                                      <li> Área entre as linhas pontilhadas azuis: A área sombreada em torno de zero indica o intervalo de confiança para a autocorrelação. Pontos fora desta área podem ser estatisticamente significativos.</li>
                                                                      </ul>",
                                                                      "<br>",
                                                                      "Ao analisar o gráfico de autocorrelação, podem ser procurados cortes abruptos ou quedas significativas, que podem sugerir o valor de ordem do termo de médias móveis no modelo.<br>",
                                                                      "Abaixo do gráfico encontra-se o valor calculado, utilizando como critério o AIC (Critério de informação de Akaike), da estimativa do valor de ordem do termo de médias móveis (q).")))
                                            )
                                   ),
                                   tabPanel("Gráfico de Autocorrelação Parcial", icon = icon("chart-line"),
                                            sidebarLayout(
                                              sidebarPanel(width = 3,
                                                           selectInput("autocorr_var_parcial", h5("Selecione a variável:"), var_nomes$titulo),
                                                           selectInput("autocorr_est_parcial", h5("Selecione a estação:"), est_nomes$estacao),
                                                           dateInput("autocorr_data_i_parcial", h5("Data de início"), "2013-01-01"),
                                                           dateInput("autocorr_data_f_parcial", h5("Data de fim"), "2020-01-01"),
                                                           tags$div(id = "cite", h6('Dados retirados do portal INMET.'))
                                              ),
                                              mainPanel(plotOutput("graph_autocorr_parcial"),
                                                        br(), br(),
                                                        verbatimTextOutput("stats_autocorpar"),
                                                        helpText(HTML("Ao analisar o gráfico de autocorrelação parcial, podem ser procurados cortes abruptos ou quedas significativas, que podem sugerir o número de termos autorregressivos (p) no modelo.<br>",
                                                                      "<br>",
                                                                      "<ul>
                                                                      <li> Eixo X (Defasagem): A defasagem (lag) representa o número de períodos anteriores que estão sendo usados para calcular a correlação com o período atual. Por exemplo, uma defasagem de 1 indica a correlação entre os dados no momento atual e os dados de um período anterior.</li>
                                                                      <li> Eixo Y (Autocorrelação): A autocorrelação é uma medida estatística que indica o grau de correlação entre uma série temporal e uma versão deslocada (defasada) de si mesma. Varia de -1 a 1, onde 1 indica uma correlação perfeita, -1 indica uma correlação inversa perfeita e 0 indica ausência de correlação.</li>
                                                                      <li> Linhas verticais: As linhas azuis no gráfico representam os valores de autocorrelação para diferentes defasagens. Cada ponto no gráfico indica a autocorrelação para uma determinada defasagem.</li>
                                                                      <li> Área entre as linhas pontilhadas azuis: A área sombreada em torno de zero indica o intervalo de confiança para a autocorrelação. Pontos fora desta área podem ser estatisticamente significativos.</li>
                                                                      </ul>",
                                                                      "<br>",
                                                                      "Abaixo do gráfico encontra-se o valor calculado, utilizando como critério o AIC (Critério de informação de Akaike), da estimativa do número de termos autorregressivos (p) no modelo.")))
                                            )
                                   ),
                                   tabPanel("Gráfico de Decomposição", icon = icon("chart-line"), 
                                            sidebarLayout(
                                              sidebarPanel(width = 3,
                                                           selectInput("decomp_var", h5("Selecione a variável:"), var_nomes$titulo),
                                                           selectInput("decomp_est", h5("Selecione a estação:"), est_nomes$estacao),
                                                           dateInput("decomp_data_i", h5("Data de início"), "2013-01-01"),
                                                           dateInput("decomp_data_f", h5("Data de fim"), "2020-01-01"),
                                                           tags$div(id = "cite", h6('Dados retirados do portal INMET.'))
                                              ),
                                              mainPanel(plotOutput("graph_decomp"),
                                                        helpText(HTML("<ul>
                                                        <li> Série Temporal Original: A linha no gráfico representa a série temporal original, que são os dados de uma variável meteorológica (como temperatura, precipitação, etc.) ao longo do tempo.</li>
                                                        <li> Componente de Tendência (Trend): A linha mais suave no gráfico representa a tendência da série temporal. É uma estimativa de como a variável muda ao longo do tempo, removendo os efeitos sazonais e irregulares.</li>
                                                        <li> Componente Sazonal (Seasonal): As flutuações que ocorrem em padrões regulares dentro da série temporal são representadas pela componente sazonal. Por exemplo, se os dados exibirem padrões sazonais, como variações anuais de temperatura, essas variações serão capturadas por esta componente.</li>
                                                        <li> Componente de Irregularidade (Residuals): Esta componente representa os resíduos ou erros que não podem ser explicados pela tendência ou pela sazonalidade. São as variações imprevisíveis ou aleatórias nos dados.</li>
                                                        </ul>",
                                                                      "Este tipo de gráfico é útil para entender a estrutura subjacente de uma série temporal. Permite separar os diferentes componentes que contribuem para as variações nos dados ao longo do tempo. A decomposição facilita a identificação de padrões sazonais, tendências de longo prazo e flutuações irregulares.<br>",
                                                                      "No contexto meteorológico, por exemplo, a tendência pode representar uma mudança gradual nas temperaturas ao longo dos anos, a componente sazonal pode indicar variações sazonais previsíveis (como as estações do ano), e os resíduos podem representar variações imprevisíveis de curto prazo. Essa informação é valiosa para a interpretação e modelagem de séries temporais.")))
                                            )
                                   ),
                                   tabPanel("Gráfico de Diferenciação", icon = icon("chart-line"),
                                            sidebarLayout(
                                              sidebarPanel(width = 3,
                                                           selectInput("dif_var", h5("Selecione a variável:"), var_nomes$titulo),
                                                           selectInput("dif_est", h5("Selecione a estação:"), est_nomes$estacao),
                                                           numericInput("dif_defasagem", h5("Selecione a defasagem:"), value = 12, min = 1),
                                                           dateInput("dif_data_i", h5("Data de início"), "2013-01-01"),
                                                           dateInput("dif_data_f", h5("Data de fim"), "2020-01-01"),
                                                           tags$div(id = "cite", h6('Dados retirados do portal INMET.'))
                                              ),
                                              mainPanel(plotOutput("graph_dif"),
                                                        helpText(HTML("O gráfico de diferenciação exibe a diferença entre os valores de uma variável em relação a um número específico de períodos de tempo (defasagem). Vamos analisar a interpretação do gráfico:<br>",
                                                                      "<ul>
                                                                      <li> Eixo X (horizontal): Representa o tempo ou os períodos de observação. Cada ponto ao longo do eixo representa uma observação em um determinado intervalo de tempo. </li>
                                                                      <li> Eixo Y (vertical): Indica a diferença entre os valores da variável selecionada em relação à defasagem especificada. A defasagem determina quantos períodos de tempo são subtraídos entre os valores. </li>
                                                                      <li> Linha no Gráfico: A linha conecta os pontos, mostrando como a diferença sazonal varia ao longo do tempo. Uma linha ascendente indica que a diferença está aumentando, enquanto uma linha descendente indica que está diminuindo. </li>
                                                                      </ul>")))
                                            )
                                   ),
                                   tabPanel("Teste de Cox-Stuart", icon = icon("chart-line"),
                                            sidebarLayout(
                                              sidebarPanel(width = 3,
                                                           selectInput("cox_stuart_var", h5("Selecione a variável:"), var_nomes$titulo),
                                                           selectInput("cox_stuart_est", h5("Selecione a estação:"), est_nomes$estacao),
                                                           numericInput("cox_stuart_defasagem", h5("Selecione a defasagem:"), value = 12, min = 1),
                                                           dateInput("cox_stuart_data_i", h5("Data de início"), "2013-01-01"),
                                                           dateInput("cox_stuart_data_f", h5("Data de fim"), "2020-01-01"),
                                                           tags$div(id = "cite", h6('Dados retirados do portal INMET.'))
                                              ),
                                              mainPanel(plotOutput("graph_cox_stuart"),
                                                        br(), br(),
                                                        verbatimTextOutput("stats"),
                                                        helpText("O gráfico mostra a série temporal ajustada, que é a série original após a remoção da componente sazonal. Isso ajuda a visualizar os padrões de variação que não são explicados pela sazonalidade. Por exemplo, se houver uma tendência de longo prazo ou padrões de curto prazo, eles serão mais visíveis na série ajustada.",tags$br(),
                                                                 tags$br(),
                                                                 "O teste de Cox-Stuart é uma ferramenta estatística utilizada para verificar se existe uma tendência significativa em uma série temporal. Ele avalia se há uma mudança sistemática na direção dos valores ao longo do tempo. A estatística de teste é comparada a uma distribuição de probabilidade para determinar se a tendência é estatisticamente significativa. O valor-p indica a probabilidade de observar uma estatística de teste tão extrema quanto a observada, se não houver tendência na série. Se o valor-p for pequeno (geralmente abaixo de 0.05), podemos rejeitar a hipótese nula de ausência de tendência."))
                                            )
                                   )
                      )
             ),
             
             ## Modelagem preditiva
             tabPanel("Modelagem preditiva",
                      navlistPanel(widths=c(2, 10),
                                   tabPanel("Predição mensal", icon = icon("chart-line"),
                                            sidebarLayout(
                                              sidebarPanel(width = 3,
                                                           selectInput("modelagem_var", h5("Selecione a variável:"), var_nomes$titulo),
                                                           selectInput("modelagem_est", h5("Selecione a(s) estação(ões) meteorológica(s)"), cidades_mod$estacao),
                                                           dateInput("modelagem_data_i", h5("Data de início"), "2013-01-01"),
                                                           dateInput("modelagem_data_f", h5("Data de fim"), "2020-01-01"),
                                                           tags$div(id = "cite", h6('Dados retirados do portal INMET.'))
                                              ),
                                              mainPanel(plotOutput("graph_modelagem_preditiva"),
                                                        br(), br(),
                                                        verbatimTextOutput("stats_modelagem_preditiva"),
                                                        helpText("O gráfico exibe as previsões do modelo para um horizonte de previsão de 10 anos (120 meses) com um intervalo de confiança de 95%. As previsões podem incluir tendências e padrões identificados automaticamente pelo modelo. O eixo x representa o tempo, enquanto o eixo y representa os valores previstos. O intervalo sombreado ao redor das previsões destaca a incerteza associada às previsões, refletindo a variabilidade esperada nos dados futuros. Este gráfico é útil para avaliar a confiança nas previsões e identificar possíveis padrões de tendência.",tags$br(),
                                                                 tags$br(),
                                                                 "Abaixo do gráfico encontram-se os valores estimados para os parâmetros, e com base neles o modelo mais apropriado para o ajuste dos dados."))
                                            )        
                                   ),
                                   tabPanel("Predição diária", icon = icon("chart-line"),
                                            sidebarLayout(
                                              sidebarPanel(width = 3,
                                                           selectInput("modelagem_var2", h5("Selecione a variável:"), var_nomes$titulo),
                                                           selectInput("modelagem_est2", h5("Selecione a(s) estação(ões) meteorológica(s)"), cidades_mod$estacao),
                                                           dateInput("modelagem_data_i2", h5("Data de início"), "2013-01-01"),
                                                           dateInput("modelagem_data_f2", h5("Data de fim"), "2020-01-01"),
                                                           tags$div(id = "cite", h6('Dados retirados do portal INMET.'))
                                              ),
                                              mainPanel(plotOutput("graph_modelagem_preditiva2"),
                                                        br(), br(),
                                                        verbatimTextOutput("stats_modelagem_preditiva2"),
                                                        helpText("O gráfico exibe as previsões do modelo para um horizonte de previsão de 10 anos (120 meses) com um intervalo de confiança de 95%. As previsões podem incluir tendências e padrões identificados automaticamente pelo modelo. O eixo x representa o tempo, enquanto o eixo y representa os valores previstos. O intervalo sombreado ao redor das previsões destaca a incerteza associada às previsões, refletindo a variabilidade esperada nos dados futuros. Este gráfico é útil para avaliar a confiança nas previsões e identificar possíveis padrões de tendência.",tags$br(),
                                                                 tags$br(),
                                                                 "Abaixo do gráfico encontram-se os valores estimados para os parâmetros, e com base neles o modelo mais apropriado para o ajuste dos dados."))
                                            )        
                                   )
             )),
             
             ## Sobre o site
             tabPanel("Sobre o site",
                      tags$div(
                        tags$h4("Introdução"),
                        "No âmbito de um programa de aprendizado contínuo ao longo de um semestre acadêmico, empreenderemos uma jornada composta por quatro fases fundamentais, culminando na concepção de uma plataforma digital interativa voltada para a análise e exploração de dados meteorológicos provenientes da base de dados do Instituto Nacional de Meteorologia (INMET). Por meio deste percurso, iremos transpor o conhecimento teórico para a esfera prática.",
                        
                        tags$br(),tags$br(),tags$h4("Exploração e Compreensão dos Dados Meteorológicos"),
                        "Na etapa inaugural, nos dedicaremos a uma imersão meticulosa no vasto repositório de dados meteorológicos de acesso público disponibilizados pelo INMET. Durante essa incursão, desvelaremos a infraestrutura que sustenta tais registros, consolidando uma compreensão detalhada das metodologias subjacentes à coleta e disseminação dos mesmos. Esta exploração abrangente não apenas expandirá o horizonte de compreensão dos dados, mas também proporcionará discernimento sobre o contexto subjacente e as limitações inerentes.",
                        
                        tags$br(),tags$br(),tags$h4("Da Curiosidade à Formulação de Questões Relevantes"),
                        "Com os questionamentos delineados, nos aprofundaremos na exploração das teorias matemáticas, estatísticas e computacionais que fundamentam a análise dos dados meteorológicos. Esta etapa nos dotará das habilidades necessárias para conduzir investigações mais abrangentes, utilizando técnicas estatísticas robustas e métodos avançados de análise computacional. Neste processo, padrões, tendências e correlações latentes nos dados serão revelados, reforçando sua capacidade de sustentar decisões informadas.",
                        
                        tags$br(),tags$br(),tags$h4("Alicerces Teóricos para Análise Profunda"),
                        "Aprofundando-se nos questionamentos delineados, exploraremos as bases teóricas das teorias matemáticas, estatísticas e computacionais que fundamentam a análise dos dados meteorológicos. Esta etapa nos habilitará a conduzir investigações mais abrangentes, utilizando técnicas estatísticas robustas e métodos avançados de análise computacional. Ao fazê-lo, padrões, tendências e correlações latentes nos dados serão revelados, reforçando sua habilidade para sustentar decisões informadas.",
                        
                        tags$br(),tags$br(),tags$h4("Desenvolvimento de uma Plataforma Web Interativa"),
                        "O ponto culminante deste programa educacional será concretizado na construção de uma plataforma digital interativa. Esta plataforma se configurará como um portal, possibilitando a análise de dados meteorológicos por um público diversificado, abrangendo diversas disciplinas e níveis de expertise. Através da implementação de uma abordagem intuitiva e customizada, a plataforma capacitará os usuários a explorar insights de maneira eficaz. Desta forma, a teoria será materializada por meio de uma ferramenta funcional, oferecendo uma experiência envolvente tanto para os pesquisadores quanto para os interessados em compreender os padrões climáticos característicos do período considerado.",
                        
                        tags$br(),tags$br(),tags$h4("Referências"),
                        tags$b("Pacote ‘BrazilMet’: "), tags$a(href="https://github.com/nytimes/covid-19-data", "Package ‘BrazilMet’"),tags$br(),
                        tags$b("GitHub dos autores da biblioteca ‘BrazilMet’: "), tags$a(href="https://github.com/FilgueirasR/BrazilMet", "GitHub - FilgueirasR / BrazilMet"),tags$br(),
                        tags$b("Portal do INMET: "), tags$a(href="https://portal.inmet.gov.br/", "Instituto Nacional de Meteorologia (INMET)"),tags$br(),
                        
                        tags$br(),tags$br(),tags$h4("Professor"),
                        "Oilson Alberto Gonzatto Junior",
                        
                        tags$br(),tags$br(),tags$h4("Equipe Discente"),
                        "Aime Gomes da Nobrega",tags$br(),
                        "Alice Guimarães Perez",tags$br(),
                        "André Dylan Andrade",tags$br(),
                        "Carolina Spera Braga",tags$br(),
                        "Daniel Gregório Chagas",tags$br(),
                        "Matheus Vinicius Barreto de Farias",tags$br(),
                        "Thaís Parron Alves",tags$br(),tags$br(),
                        
                        tags$img(src = "usp-university-of-sao-paulo7715.jpg", width = "120px", height = "65px"), tags$img(src = "logo-icmc.png", width = "120px", height = "65px")
                      )
             )
  )
)


# Server
server <- function(input, output){
  
  ## Análise temporal
  output$graph_media_movel <- renderPlot({
    data_i = input$mediamovel_data_i
    data_f = input$mediamovel_data_f
    estacao = epc(input$mediamovel_est)
    
    dados = carrega_estacao(estacao)
    
    k = input$mediamovel_k
    variavel = tpv(input$mediamovel_var)
    
    mascara = (dados$Date >= as.Date(data_i)) & (dados$Date <= as.Date(data_f)) & (dados$Station_code == estacao)
    
    d <- dados[mascara,]
    d <- data.frame(x=d$Date, y=d[,variavel])
    
    ggplot(d, aes(x = x, y = y)) +
      geom_line() +
      geom_line(aes(y = rollmean(y, k = k, na.pad=TRUE), color="#FF0000")) +
      labs(title = paste(vpt(variavel), ", em dias sucessivos"),
           x = "Data",
           y = vpl(variavel),
           color="Média móvel") +
      theme(legend.position = "none")
  })
  
  output$graph_sazonalidade <- renderPlot({
    estacao = epc(input$sazonalidade_est)
    base = carrega_estacao(estacao)
    variavel = tpv(input$sazonalidade_var)
    Data_ini = input$sazonalidade_data_i
    Data_fim = input$sazonalidade_data_f
    periodo = input$sazonalidade_periodo
    
    filtro <- filter(base, Station_code == toString(estacao) & Date >= toString(Data_ini) & Date <= toString(Data_fim) )
    
    dados = tsibble(
      data = ymd(filtro$Date),
      y = filtro[[variavel]],
      index = data
    )
    G2 =
      dados %>%
      fill_gaps(data,y = mean(y),.full=TRUE) %>%
      gg_season(y, labels = 'both',period=periodo) +
      labs(
        y = vpl(variavel),
        x = 'Tempo',
        title = paste(vpt(variavel), ", em dias sucessivos")
      );
    G2
  })
  
  output$graph_lag <- renderPlot({
    estacao = epc(input$lag_est)
    base = carrega_estacao(estacao)
    variavel = tpv(input$lag_var)
    Data_ini = input$lag_data_i
    Data_fim = input$lag_data_f
    
    base$months <- yearmonth(base$Date) # Passando pra formato ano/mês
    filtro <- filter(base, Station_code == toString(estacao) & Date >= toString(Data_ini) & Date <= toString(Data_fim) )
    
    filtro$y <- filtro[[variavel]]
    medias_T <- aggregate( y ~ months, data = filtro , FUN="mean" )
    
    dados_mensais = tsibble(
      data = medias_T$months,
      y = medias_T$y,
      index = data
    )
    G3= 
      dados_mensais %>% 
      fill_gaps(data,y = mean(y),.full=TRUE) %>%
      gg_lag(y, geom='point', period='year',lags = c(6,12)) +
      #gg_season(fill_gaps(dados,y = mean(y),.full=TRUE), labels = 'both') + 
      labs(
        y = vpl(variavel), 
        x = 'Tempo (em meses)', 
        title = paste(vpt(variavel),', em anos sucessivos com lag = 6 e 12 meses')
      ); G3
  })
  
  output$graph_subserie <- renderPlot({
    estacao = epc(input$subserie_est)
    base = carrega_estacao(estacao)
    variavel = tpv(input$subserie_var)
    Data_ini = input$subserie_data_i
    Data_fim = input$subserie_data_f
    
    base$months <- yearmonth(base$Date) # Passando pra formato ano/mês
    filtro <- filter(base, Station_code == toString(estacao) & Date >= toString(Data_ini) & Date <= toString(Data_fim) )
    
    filtro$y <- filtro[[variavel]]
    medias_T <- aggregate( y ~ months, data = filtro , FUN="mean" )
    # Convertendo pra Tsibble
    dados_mensais = tsibble(
      data = medias_T$months,
      y = medias_T$y,
      index = data
    )
    
    G4 = 
      dados_mensais %>% 
      gg_subseries(y) + 
      labs(
        y = vpl(variavel), 
        x = 'Tempo (em anos)', 
        title = paste(vpt(variavel), "em meses sucessivos")
      ); G4
  })
  
  output$graph_wetbulb <- renderPlot({
    estacao = epc(input$wetbulb_est)
    base = carrega_estacao(estacao)
    Data_ini = input$wetbulb_data_i
    Data_fim = input$wetbulb_data_f
    
    dados_corte = filter(base, Station_code == toString(estacao) & Date >= toString(Data_ini) & Date <= toString(Data_fim))
    
    temperatures <- dados_corte$Tair_mean..c.
    humidities <- dados_corte$Rh_mean..porc.
    
    dados_corte = data.frame(T1=temperatures, H1=humidities)
    
    temperatures <- seq(0, 50, length.out=50)
    humidities <- seq(0, 100, length.out=50)
    
    grid <- expand.grid(T=temperatures, H=humidities)
    grid$W <- grid$T - (0.55 * (1 - grid$H/100) * (grid$T - 14.5))
    
    # Definindo as zonas
    W_amarela <- 24
    W_laranja <- 26
    W_vermelha <- 28
    
    grid$Z <- ifelse(grid$W <= W_amarela, "Confortável",
                     ifelse(grid$W <= W_laranja, "Risco",
                            ifelse(grid$W <= W_vermelha, "Crítico", "Perigoso")))
    
    # Cores
    colors_light <- c("Confortável"="#a8e6cf",
                      "Risco"="#ffd3b6",
                      "Crítico"="#ffaaa5",
                      "Perigoso"="#ff8b94")
    
    # Plot
    ggplot(grid, aes(x=T, y=H, fill=Z)) +
      geom_tile() +
      scale_fill_manual(values=colors_light, breaks=names(colors_light)) +  # Definindo a escala de preenchimento como discreta
      labs(title="Efeito de Wet Bulb na Habitabilidade Humana", x="Temperatura Ambiente (°C)", y="Umidade Relativa (%)", fill="Zonas") +
      theme_minimal() +
      geom_point(data=dados_corte, aes(x=T1, y=H1), color="blue", size=2, inherit.aes=F)
  })
  
  
  output$graph_autocorr <- renderPlot({
    estacao = epc(input$autocorr_est)
    base = carrega_estacao(estacao)
    variavel = tpv(input$autocorr_var)
    Data_ini = input$autocorr_data_i
    Data_fim = input$autocorr_data_f
    
    base$months <- yearmonth(base$Date) # Passando pra formato ano/mês
    filtro <- filter(base, Station_code == toString(estacao) & Date >= toString(Data_ini) & Date <= toString(Data_fim) )
    filtro$y <- filtro[[variavel]]
    medias_T <- aggregate( y ~ months, data = filtro , FUN="mean" )
    
    dados_mensais = tsibble(
      data = medias_T$months,
      y = medias_T$y,
      index = data
    )
    
    decomposicao = dados_mensais %>%
      fill_gaps(data,y = mean(y),.full=TRUE) %>%
      model(STL(y ~ season(window = 12))) %>%
      components()
    
    # Obter a série sem tendência e sazonalidade (componente remainder)
    serie_sem_tendencia_sazonalidade <- decomposicao %>%
      pull(remainder)
    
    G1 <- dados_mensais %>%
      fill_gaps(data, y = mean(serie_sem_tendencia_sazonalidade), .full = TRUE) %>%
      ACF(y = .$y, lag_max = 20) %>%
      autoplot() +
      labs(
        x = 'Defasagem',
        y = 'Autocorrelação'
      ) +
      coord_cartesian(ylim = c(-1, 1)) +
      theme_minimal()
    G1
  })
  
  output$stats_autocor <- renderPrint({
    estacao = epc(input$autocorr_est)
    base = carrega_estacao(estacao)
    variavel = tpv(input$autocorr_var)
    Data_ini = input$autocorr_data_i
    Data_fim = input$autocorr_data_f
    
    base$months <- yearmonth(base$Date) # Passando pra formato ano/mês
    filtro <- filter(base, Station_code == toString(estacao) & Date >= toString(Data_ini) & Date <= toString(Data_fim) )
    dados = tsibble(
      data = ymd(filtro$Date),
      y = filtro[[variavel]],
      index = data
    )
    
    modelo_auto_arima <- auto.arima(dados$y) # Encontra a ordem do processo de médias móveis
    q_valor <- arimaorder(modelo_auto_arima)[3]
    
    return(paste("A estimativa do valor da ordem do termo de médias móveis (q), calculada utilizando como critério o AIC, é:", q_valor))
  })
  
  output$graph_autocorr_parcial <- renderPlot({
    estacao = epc(input$autocorr_est_parcial)
    base = carrega_estacao(estacao)
    variavel = tpv(input$autocorr_var_parcial)
    Data_ini = input$autocorr_data_i_parcial
    Data_fim = input$autocorr_data_f_parcial
    
    base$months <- yearmonth(base$Date) # Passando pra formato ano/mês
    filtro <- filter(base, Station_code == toString(estacao) & Date >= toString(Data_ini) & Date <= toString(Data_fim) )
    filtro$y <- filtro[[variavel]]
    medias_T <- aggregate( y ~ months, data = filtro , FUN="mean" )
    
    dados_mensais = tsibble(
      data = medias_T$months,
      y = medias_T$y,
      index = data
    )
    
    decomposicao = dados_mensais %>%
      fill_gaps(data, y = mean(y), .full = TRUE) %>%
      model(STL(y ~ season(window = 12))) %>%
      components()
    
    # Obter a série sem tendência e sazonalidade (componente de erro)
    serie_sem_tendencia_sazonalidade <- decomposicao %>%
      pull(remainder)
    
    G1 <- dados_mensais %>%
      fill_gaps(data, y = mean(serie_sem_tendencia_sazonalidade), .full = TRUE) %>%
      {pacf(.$y, lag.max = 20)} %>%
      autoplot() +
      labs(
        x = 'Defasagem',
        y = 'Autocorrelação parcial'
      ) +
      coord_cartesian(ylim = c(-1, 1)) +
      theme_minimal()
    G1
  })
  
  output$stats_autocorpar <- renderPrint({
    estacao <- epc(input$autocorr_est_parcial)
    base <- carrega_estacao(estacao)
    variavel <- tpv(input$autocorr_var_parcial)
    Data_ini <- input$autocorr_data_i_parcial
    Data_fim <- input$autocorr_data_f_parcial
    
    base$months <- yearmonth(base$Date) # Passando para o formato ano/mês
    filtro <- filter(base, Station_code == toString(estacao) & Date >= toString(Data_ini) & Date <= toString(Data_fim) )
    dados <- tsibble(
      data = ymd(filtro$Date),
      y = filtro[[variavel]],
      index = data
    )
    
    modelo_auto_arima <- auto.arima(dados$y) # Encontra o número de termos autorregressivos (p) no modelo
    p_valor <- arimaorder(modelo_auto_arima)[1]
    
    return(paste("A estimativa do número de termos autorregressivos (p) no modelo, calculada utilizando como critério o AIC, é:", p_valor))
  })
  
  output$graph_decomp <- renderPlot({
    estacao = epc(input$decomp_est)
    base = carrega_estacao(estacao)
    variavel = tpv(input$decomp_var)
    Data_ini = input$decomp_data_i
    Data_fim = input$decomp_data_f
    
    base$months <- yearmonth(base$Date) # Passando pra formato ano/mês
    filtro <- filter(base, Station_code == toString(estacao) & Date >= toString(Data_ini) & Date <= toString(Data_fim) )
    filtro$y <- filtro[[variavel]]
    medias_T <- aggregate( y ~ months, data = filtro , FUN="mean" )
    
    dados_mensais = tsibble(
      data = medias_T$months,
      y = medias_T$y,
      index = data
    )
    
    decomposicao = dados_mensais %>%
      fill_gaps(data,y = mean(y),.full=TRUE) %>%
      model(STL(y ~ season(window = 12))) %>%
      components()
    
    autoplot(decomposicao)
  }) 
  
  output$graph_dif <- renderPlot({
    estacao = epc(input$dif_est)
    base = carrega_estacao(estacao)
    variavel = tpv(input$dif_var)
    Data_ini = input$dif_data_i
    Data_fim = input$dif_data_f
    defasagem = input$dif_defasagem
    
    base$months <- yearmonth(base$Date) # Passando pra formato ano/mês
    filtro <- filter(base, Station_code == toString(estacao) & Date >= toString(Data_ini) & Date <= toString(Data_fim) )
    dados = tsibble(
      data = ymd(filtro$Date),
      y = filtro[[variavel]],
      index = data
    )
    diferenca_sazonal = diff(dados$y, lag=defasagem)
    
    plot(diferenca_sazonal, type='l')
  })
  
  output$graph_cox_stuart <- renderPlot({
    estacao = epc(input$cox_stuart_est)
    base = carrega_estacao(estacao)
    variavel = tpv(input$cox_stuart_var)
    Data_ini = input$cox_stuart_data_i
    Data_fim = input$cox_stuart_data_f
    defasagem = input$cox_stuart_defasagem
    
    base$months <- yearmonth(base$Date) # Passando pra formato ano/mês
    filtro <- filter(base, Station_code == toString(estacao) & Date >= toString(Data_ini) & Date <= toString(Data_fim) )
    filtro$y <- filtro[[variavel]]
    medias_T <- aggregate( y ~ months, data = filtro , FUN="mean" )
    
    dados_mensais = tsibble(
      data = medias_T$months,
      y = medias_T$y,
      index = data
    )
    
    decomposicao = dados_mensais %>%
      fill_gaps(data,y = mean(y),.full=TRUE) %>%
      model(STL(y ~ season(window = 12))) %>%
      components()
    
    dados_ajustados = dados_mensais$y - decomposicao$season_year
    plot(dados_ajustados, type='l', sub = 'Série com sazonalidade removida')
    
    n = length(dados_ajustados)
    n2 = n / 2
    first_half = dados_ajustados[1:n2]
    second_half = dados_ajustados[(n2 + 1):n]
    signs = sign(second_half - first_half)
    pos_signs = sum(signs == 1)
    neg_signs = sum(signs == -1)
    test_statistic = abs(pos_signs - neg_signs)
    p_value = 2 * pbinom(min(pos_signs, neg_signs), n2, 0.5)
    return(list(test_statistic = test_statistic, p_value = p_value))
  })
  
  output$stats <- renderPrint({
    estacao = epc(input$cox_stuart_est)
    base = carrega_estacao(estacao)
    variavel = tpv(input$cox_stuart_var)
    Data_ini = input$cox_stuart_data_i
    Data_fim = input$cox_stuart_data_f
    defasagem = input$cox_stuart_defasagem
    
    base$months <- yearmonth(base$Date) # Passando pra formato ano/mês
    filtro <- filter(base, Station_code == toString(estacao) & Date >= toString(Data_ini) & Date <= toString(Data_fim) )
    filtro$y <- filtro[[variavel]]
    medias_T <- aggregate( y ~ months, data = filtro , FUN="mean" )
    
    dados_mensais = tsibble(
      data = medias_T$months,
      y = medias_T$y,
      index = data
    )
    
    decomposicao = dados_mensais %>%
      fill_gaps(data,y = mean(y),.full=TRUE) %>%
      model(STL(y ~ season(window = 12))) %>%
      components()
    
    dados_ajustados = dados_mensais$y - decomposicao$season_year
    
    n = length(dados_ajustados)
    n2 = as.integer(round(n / 2))
    first_half = dados_ajustados[1:n2]
    second_half = dados_ajustados[(n2 + 1):n]
    signs = sign(second_half - first_half)
    pos_signs = sum(signs == 1)
    neg_signs = sum(signs == -1)
    test_statistic = abs(pos_signs - neg_signs)
    p_value = 2 * pbinom(min(pos_signs, neg_signs), n2, 0.5)
    return(list(test_statistic = test_statistic, p_value = p_value))
    
  })
  
  
  ## Modelagem preditiva
  # Predição mensal
  output$graph_modelagem_preditiva <- renderPlot({
    estacao = epc(input$modelagem_est)
    base = carrega_estacao(estacao)
    variavel = tpv(input$modelagem_var)
    Data_ini = input$modelagem_data_i
    Data_fim = input$modelagem_data_f
    
    base$months <- yearmonth(base$Date) # Passando pra formato ano/mês
    filtro <- filter(base, Station_code == toString(estacao) & Date >= toString(Data_ini) & Date <= toString(Data_fim) )
    filtro$y <- filtro[[variavel]]
    medias_T <- aggregate( y ~ months, data = filtro , FUN="mean" )
    
    dados = tsibble(
      data = medias_T$months,
      y = medias_T$y,
      index = data
    )
    
    modelo_auto_arima <- auto.arima(dados) # Estime automaticamente os parâmetros do modelo ARIMA
    
    previsoes <- forecast(modelo_auto_arima, level=c(95), h=10*12)  # Obtenha as previsões do melhor modelo
    plot(previsoes)
  })
  
  output$stats_modelagem_preditiva <- renderPrint({
    estacao = epc(input$modelagem_est)
    base = carrega_estacao(estacao)
    variavel = tpv(input$modelagem_var)
    Data_ini = input$modelagem_data_i
    Data_fim = input$modelagem_data_f
    
    base$months <- yearmonth(base$Date) # Passando pra formato ano/mês
    filtro <- filter(base, Station_code == toString(estacao) & Date >= toString(Data_ini) & Date <= toString(Data_fim) )
    filtro$y <- filtro[[variavel]]
    medias_T <- aggregate( y ~ months, data = filtro , FUN="mean" )
    
    dados = tsibble(
      data = medias_T$months,
      y = medias_T$y,
      index = data
    )
    
    modelo_auto_arima <- auto.arima(dados) # Estime automaticamente os parâmetros do modelo ARIMA
    
    p <- arimaorder(modelo_auto_arima)[1]
    d <- arimaorder(modelo_auto_arima)[2]
    q <- arimaorder(modelo_auto_arima)[3]
    if (p == 0 && q == 0) {
      cat("O modelo é de um processo de médias móveis, com ordem q =", q, "\n")
    } else if (d == 0 && q == 0) {
      cat("O modelo é um processo autoregressivo, com ordem p =", p, "\n")
    } else if (d == 0) {
      cat("O modelo é ARMA, com ordem p =", p, "e ordem q =", q, "\n")
    } else {
      cat("O modelo é ARIMA, com ordem p =", p, ", ordem d =", d, "e ordem q =", q, "\n")
    }
  })
  
  # Predição diária
  output$graph_modelagem_preditiva2 <- renderPlot({
    estacao = epc(input$modelagem_est2)
    base = carrega_estacao(estacao)
    variavel = tpv(input$modelagem_var2)
    Data_ini = input$modelagem_data_i2
    Data_fim = input$modelagem_data_f2
    
    base$months <- yearmonth(base$Date) # Passando pra formato ano/mês
    filtro <- filter(base, Station_code == toString(estacao) & Date >= toString(Data_ini) & Date <= toString(Data_fim) )
    dados = tsibble(
      data = ymd(filtro$Date),
      y = filtro[[variavel]],
      index = data
    )
    
    modelo_auto_arima <- auto.arima(dados) # Estime automaticamente os parâmetros do modelo ARIMA
    
    previsoes <- forecast(modelo_auto_arima, level=c(95), h=10*12)  # Obtenha as previsões do melhor modelo
    plot(previsoes)
  })
  
  output$stats_modelagem_preditiva2 <- renderPrint({
    estacao = epc(input$modelagem_est2)
    base = carrega_estacao(estacao)
    variavel = tpv(input$modelagem_var2)
    Data_ini = input$modelagem_data_i2
    Data_fim = input$modelagem_data_f2

    base$months <- yearmonth(base$Date) # Passando pra formato ano/mês
    filtro <- filter(base, Station_code == toString(estacao) & Date >= toString(Data_ini) & Date <= toString(Data_fim) )
    dados = tsibble(
      data = ymd(filtro$Date),
      y = filtro[[variavel]],
      index = data
    )
    
    modelo_auto_arima <- auto.arima(dados) # Estime automaticamente os parâmetros do modelo ARIMA
    
    p <- arimaorder(modelo_auto_arima)[1]
    d <- arimaorder(modelo_auto_arima)[2]
    q <- arimaorder(modelo_auto_arima)[3]
    if (p == 0 && q == 0) {
      cat("O modelo é de um processo de médias móveis, com ordem q =", q, "\n")
    } else if (d == 0 && q == 0) {
      cat("O modelo é um processo autoregressivo, com ordem p =", p, "\n")
    } else if (d == 0) {
      cat("O modelo é ARMA, com ordem p =", p, "e ordem q =", q, "\n")
    } else {
      cat("O modelo é ARIMA, com ordem p =", p, ", ordem d =", d, "e ordem q =", q, "\n")
    }
  })
  
  
  ## Análise geográfica
  output$map <- renderLeaflet({
    ano <- input$ano
    variavel = input$var
    var_nome = tpv(variavel)
    
    media_variavel <- as.numeric(medias_por_ano[[var_nome]])
    
    data_filtered <- subset(medias_por_ano, Ano == ano)
    m <- data_filtered[[var_nome]]
    
    bins = round(seq(min(m) - sd(m), max(m) + sd(m), by = sd(m)), 2)
    
    pal <- colorBin("YlOrRd", domain = data_filtered$media_variavel, bins = bins)
    labels <- sprintf("<strong>%s</strong><br/>%g anos<sup></sup>",
                      data_filtered$Station, data_filtered$media_variavel) %>% lapply(htmltools::HTML)
    mapa <- leaflet(data = data_filtered) %>% addTiles() %>%
      addCircles(lng = ~as.numeric(Longitude..degrees.), lat = ~as.numeric(Latitude..degrees.), weight = 15,
                 popup = ~ paste0(sep = " ","<b>", Station, "<br>", vpl(var_nome), " média anual: ", round(as.numeric(m),2),"</b><br>"),
                 radius = 30000, color = ~pal(as.numeric(data_filtered[[var_nome]])), fillOpacity = 1) %>%
      addLegend("bottomright", pal = pal, values = ~as.numeric(data_filtered[[var_nome]]), title = variavel, opacity = 1)
    
    mapa
  })
}

shinyApp(ui = ui, server = server)
