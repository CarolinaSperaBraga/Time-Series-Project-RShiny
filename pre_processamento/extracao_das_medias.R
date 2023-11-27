# Função que calcula as médias das variáveis por ano e armazena em um dataframe
calcular_media_por_ano <- function(variaveis, dados) {
  anos <- format(as.Date(dados$Date), "%Y")
  
  # Adicionando os anos como uma nova coluna nos dados
  dados$Ano <- as.integer(anos)
  
  # Inicializando um data frame para armazenar as médias
  medias_por_ano <- data.frame(Ano = integer(0), Station = character(0),
                               Latitude = numeric(0), Longitude = numeric(0))
  
  # Loop sobre os anos e estações para calcular a média de cada variável por ano
  for (ano in unique(dados$Ano)) {
    for (estacao in unique(dados$Station)) {
      dados_filtrados <- subset(dados, Ano == ano & Station == estacao)
      
      if (nrow(dados_filtrados) > 0) {
        media_ano_estacao <- c(Ano = as.numeric(ano),
                               Station = estacao,
                               Latitude = as.numeric(unique(dados_filtrados$Latitude..degrees.)),
                               Longitude = as.numeric(unique(dados_filtrados$Longitude..degrees.)))
        
        for (variavel in variaveis) {
          media_ano_estacao[paste("Media", variavel, sep = "_")] <- mean(dados_filtrados[[variavel]], na.rm = TRUE)
        }
        
        medias_por_ano <- rbind(medias_por_ano, media_ano_estacao)
        colnames(medias_por_ano) <- c("Ano", "Station", "Latitude..degrees.", "Longitude..degrees.",
                                      "Tair_mean..c.", "Tair_min..c.", "Tair_max..c.",
                                      "Dew_tmean..c.", "Dew_tmin..c.", "Dew_tmax..c.", "Dry_bulb_t..c.",
                                      "Rainfall..mm.", "Rh_mean..porc.", "Rh_max..porc.",
                                      "Rh_min..porc.", "Ws_10..m.s.1.", "Ws_gust..m.s.1.",
                                      "Wd..degrees.", "Sr..Mj.m.2.day.1.") #"Ra..Mj.m.2.day.1." e "Patm..mB."
        
      }
    }
  }
  
  return(medias_por_ano)
}

# Leitura dos dados
dados <- read.csv("dados_quase_completos.csv", sep=",", header = TRUE)
dados$Date = as.Date(dados$Date)

variaveis <- c("Tair_mean..c.", "Tair_min..c.", "Tair_max..c.","Dew_tmean..c.", "Dew_tmin..c.", "Dew_tmax..c.", "Dry_bulb_t..c.",
               "Rainfall..mm.", "Rh_mean..porc.", "Rh_max..porc.", "Rh_min..porc.", "Ws_10..m.s.1.", "Ws_gust..m.s.1.",
               "Wd..degrees.", "Sr..Mj.m.2.day.1.") #"Ra..Mj.m.2.day.1." e "Patm..mB."

medias_por_ano <- calcular_media_por_ano(variaveis, dados)

write.csv(medias_por_ano, "medias_por_ano.csv")
