##### Sttation_code e Station data frame ##################################################################################################
# Leitura dos dados
dados <- read.csv("dados_quase_completos2.csv", sep=",", header = TRUE)
dados$Date = as.Date(dados$Date)

est_nomes <- data.frame(codigo = unique(dados$Station_code))
cidades <- read.csv("CatalogoEstaçõesAutomáticas.csv", sep=";", header = TRUE)

# Criando uma coluna com o nome da cidade e estado
cidades_mod <- data.frame(coluna1 = cidades$DC_NOME,coluna2 = cidades$SG_ESTADO, codigo = cidades$CD_ESTACAO)
cidades_mod$coluna1 <- str_to_title(tolower(cidades_mod$coluna1))
cidades_mod$estacao <- paste(cidades_mod$coluna1, cidades_mod$coluna2, sep = "-")

cidades_mod = cidades_mod[,c("codigo", "estacao")]

est_nomes = merge(est_nomes, cidades_mod, by = "codigo")

write.csv(est_nomes, "nomes_codigos_estacoes.csv", row.names = FALSE)
