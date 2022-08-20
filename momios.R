#install.packages("remotes")
#library(remotes)
#install_github("cran/fbRanks")

library(fbRanks)
library(dplyr)
library(ggplot2)


# Colocar el directorio de trabajo según corresponda

setwd("Path")
getwd()

# Descarga de archivos
# https://www.football-data.co.uk/spainm.php
urls <- c(
  "https://www.football-data.co.uk/mmz4281/1011/SP1.csv", "https://www.football-data.co.uk/mmz4281/1112/SP1.csv",
  "https://www.football-data.co.uk/mmz4281/1213/SP1.csv", "https://www.football-data.co.uk/mmz4281/1314/SP1.csv",
  "https://www.football-data.co.uk/mmz4281/1415/SP1.csv", "https://www.football-data.co.uk/mmz4281/1516/SP1.csv",
  "https://www.football-data.co.uk/mmz4281/1617/SP1.csv", "https://www.football-data.co.uk/mmz4281/1718/SP1.csv",
  "https://www.football-data.co.uk/mmz4281/1819/SP1.csv", "https://www.football-data.co.uk/mmz4281/1920/SP1.csv")

data_directory <- paste0(getwd(),"/raw_data")

for (url in urls){
  file <- paste0(data_directory, "/SP1-", substr(url, 41, 44), ".csv")
  download.file(url = url, destfile = file, mode = "wb")
}

# Lectura de datos
df_list <- lapply(list.files(path = data_directory, full.names = T), read.csv)

# Procesamiento de datos
# Especificamos nombres de columnas para evitar problemas futuros donde columnas están en otro orden
cols <- c("Date", "HomeTeam", "AwayTeam", "FTHG", "FTAG", "BbMx.2.5", "BbAv.2.5",
          "BbMx.2.5.1", "BbAv.2.5.1", "Max.2.5", "Avg.2.5", "Max.2.5.1", "Avg.2.5.1")

# Funcion simple que toma las columnas necesarias en cada df
df_list <- lapply(df_list, function(df){    
  df[, (names(df) %in% cols)]
  }
)

# Tratamiento de los datos
rename_cols <- c(date = "Date", home.team = "HomeTeam", home.score = "FTHG", away.team = "AwayTeam", away.score = "FTAG",
                 Max.2.5.O = "BbMx.2.5", Avg.2.5.O = "BbAv.2.5", Max.2.5.U = "BbMx.2.5.1", Avg.2.5.U = "BbAv.2.5.1",
                 Max.2.5.O = "Max.2.5", Avg.2.5.O = "Avg.2.5", Max.2.5.U = "Max.2.5.1", Avg.2.5.U = "Avg.2.5.1")

data <- df_list %>%
  lapply(mutate, Date = ifelse(nchar(Date) == 8, paste0(substr(Date, 1, 6), "20", substr(Date, 7, 8)), Date)) %>% 
  lapply(mutate, Date = as.Date(Date, format = "%d/%m/%Y")) %>%   # Fechas
  lapply(rename, any_of(rename_cols)) %>%  # Colnames. any_of ignora una variable si no la encuentra, en lugar de tirar error
  bind_rows() %>% # Combinar dataframes
  select(date, home.team, home.score, away.team, away.score:Avg.2.5.U)  # Reordenamos

# Data frames de partidos y equipos
md <- data %>% select(date:away.score)
write.csv(md, "match.data.csv", row.names = FALSE)
df <- create.fbRanks.dataframes(scores.file = "match.data.csv")
teams <- df$teams; scores <- df$scores

head(teams, n = 2L); dim(teams); head(scores, n = 2L); dim(scores)

# Conjuntos iniciales de entrenamiento y de prueba
Ym <- scores$date %>% # Fechas
  format("%Y-%m") %>% # Year-Month
  unique()            # Year-Month unico

places <- which(Ym[15]==format(scores$date, "%Y-%m")) # Consideramos partidos de 15 meses para comenzar a ajustar el modelo
ffe <- scores$date[max(places)] # Fecha final conjunto de entrenamiento

# Consideraremos partidos de 15 meses para comenzar a ajustar el modelo. Así, nuestro primer conjunto de entrenamiento consiste de datos de partidos hasta el `r ffe` 
train <- scores %>% filter(date <= ffe)
test <- scores %>% filter(date > ffe)

head(train, n = 1); tail(train, n = 1)
head(test, n = 1); tail(test, n = 1)

# Primer ajuste del modelo
traindate <- unique(train$date)
testdate <- unique(test$date)

ranks <- rank.teams(scores = scores, teams = teams, 
                    min.date = traindate[1], 
                    max.date = traindate[length(traindate)])

# Primera predicción
pred <- predict(ranks, date = testdate[1])
phs <- pred$scores$pred.home.score # predicted home score
pas <- pred$scores$pred.away.score # predicted away score
pht <- pred$scores$home.team # home team in predictions
pat <- pred$scores$away.team # away team in predictions

# Continuar ajustando y prediciendo
phs <- NULL; pas <- NULL; pht <- NULL; pat <- NULL
for(i in 1:(length(unique(scores$date))-170)){
  ranks <- rank.teams(scores = scores, teams = teams, 
                      min.date = unique(scores$date)[i], 
                      max.date = unique(scores$date)[i+170-1], 
                      silent = TRUE,
                      time.weight.eta = 0.0005)
  pred <- predict(ranks, date = unique(scores$date)[i+170],
                  silent = TRUE)
  
  phs <- c(phs, pred$scores$pred.home.score) # predicted home score
  pas <- c(pas, pred$scores$pred.away.score) # predicted away score
  pht <- c(pht, pred$scores$home.team) # home team in predictions
  pat <- c(pat, pred$scores$away.team) # away team in predictions
}

# Eliminamos NA's
buenos <- !(is.na(phs) | is.na(pas)) # 
phs <- phs[buenos] # predicted home score
pas <- pas[buenos] # predicted away score
pht <- pht[buenos] # home team in predictions
pat <- pat[buenos] # away team in predictions
momio <- data %>% filter(date >= unique(scores$date)[171]) # momios conjunto de prueba
momio <- momio[buenos,]
mean(pht == momio$home.team); mean(pat == momio$away.team)
mean(phs + pas > 2.5 & momio$home.score + momio$away.score > 2.5)
mean(phs + pas < 2.5 & momio$home.score + momio$away.score < 2.5)
hs <- momio$home.score
as <- momio$away.score

# Probabilidades condicionales
mean(phs + pas > 3) # proporción de partidos con más de tres goles según el modelo
mean(phs + pas > 3 & hs + as > 2.5)/mean(phs + pas > 3) 
# probabilidad condicional estimada de ganar en over 2.5
mean(phs + pas < 2.1) # proporción de partidos con menos de 2.1 goles según el modelo
mean(phs + pas < 2.1 & hs + as < 2.5)/mean(phs + pas < 2.1) 
# probabilidad condicional estimada de ganar en under 2.5


# Juegos con momios máximos
cap <- 50000; g <- NULL

for(j in 1:length(phs)){
  if(((phs[j] + pas[j]) > 3) & (0.64/(momio$Max.2.5.O[j]^-1) > 1)){
    if((hs[j] + as[j]) > 2.5) cap <- cap + 1000*(momio$Max.2.5.O[j]-1)
    else cap <- cap - 1000
    g <- c(g, cap)
  }
  
  if(((phs[j] + pas[j]) < 2.1) & (0.58/(momio$Max.2.5.U[j]^-1) > 1)){
    if((hs[j] + as[j]) < 2.5) cap <- cap + 1000*(momio$Max.2.5.U[j]-1)
    else cap <- cap - 1000
    g <- c(g, cap)
  }
}

# Escenario con momios máximos
library(scales)
g <- data.frame(Num_Ap = 1:length(g), Capital = g)
p <- ggplot(g, aes(x=Num_Ap, y=Capital)) + geom_line(color="#003366") + geom_point(color="#003366") +
  geom_hline(aes(yintercept=50000), linetype="dashed", color="#990000", size=1) +
  annotate(geom="text", x=200, y=52000, label="Capital Inicial (50k)", size=5, color="#990000") +
  labs(x = "Número de juego", 
       y = "Capital",
       title = "Secuencia de juegos considerando momios máximos") +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))  +
  theme(axis.text.x = element_text(face = "bold", color="#555555" , size = 12),
        axis.text.y = element_text(face = "bold", color="#555555" , size = 12)) +
  scale_y_continuous(label=dollar_format(), breaks = pretty_breaks(n=5))
png(filename="momios_max.png")
p
dev.off()

# Escenario con momios promedio
cap <- 50000; g <- NULL
for(j in 1:length(phs)){
  if(((phs[j] + pas[j]) > 3) & (0.64/(momio$Avg.2.5.O[j]^-1) > 1)){
    if((hs[j] + as[j]) > 2.5) cap <- cap + 1000*(momio$Avg.2.5.O[j]-1)
    else cap <- cap - 1000
    g <- c(g, cap)
  }
  
  if(((phs[j] + pas[j]) < 2.1) & (0.58/(momio$Avg.2.5.U[j]^-1) > 1)){
    if((hs[j] + as[j]) < 2.5) cap <- cap + 1000*(momio$Avg.2.5.U[j]-1)
    else cap <- cap - 1000
    g <- c(g, cap)
  }
}

g <- data.frame(Num_Ap = 1:length(g), Capital = g)
p <- ggplot(g, aes(x=Num_Ap, y=Capital)) + geom_line(color="#003366") + geom_point(color="#003366") +
  geom_hline(aes(yintercept=50000), linetype="dashed", color="#990000", size=1) +
  annotate(geom="text", x=mean(g$Num_Ap), y=52000, label="Capital Inicial (50k)", size=5, color="#990000") +
  labs(x = "Número de juego", 
       y = "Capital",
       title = "Secuencia de juegos considerando momios promedio") +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))  +
  theme(axis.text.x = element_text(face = "bold", color="#555555" , size = 12),
        axis.text.y = element_text(face = "bold", color="#555555" , size = 12)) +
  scale_y_continuous(label=dollar_format(), breaks = pretty_breaks(n=5))

png(filename="momios_avg.png")
p
dev.off()

