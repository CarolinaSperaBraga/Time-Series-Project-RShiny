library(BrazilMet)
library(tidyverse)

stations <- see_stations_info()

# loop para todas as estações
stations <- see_stations_info()
df2022 <- data.frame()
for (year in 2000:2023) {
  start <- Sys.time()
  df.new <- download_AWS_INMET_daily_adapted(stations = stations$OMM, start_date = paste0(year,"-01-01"), end_date = paste0(year,"-12-31"))
  df2022 <- bind_rows(df2022,df.new)
  end <- Sys.time()
  cat("\n",end-start)
}

df <- df2022

u2 <- (4.868 / (log(67.75 * 10 - 5.42))) * df$`Ws_10 (m s-1)`

df$eto <- daily_eto_FAO56(lat = df$`Latitude (degrees)`,
                          tmin = df$`Tair_min (c)`,
                          tmax = df$`Tair_max (c)`,
                          tmean = df$`Tair_mean (c)`,
                          Rs = df$`Sr (Mj m-2 day-1)`,
                          u2 = u2,
                          Patm = df$`Patm (mB)`,
                          RH_max = df$`Rh_max (porc)`,
                          RH_min = df$`Rh_min (porc)`,
                          z = df$`Altitude (m)`,
                          date = df$Date)

caminho_arquivo <- ("C:/Users/Usuario/Downloads/dados_completos.csv")
write.csv(df, file = caminho_arquivo, row.names = FALSE)
