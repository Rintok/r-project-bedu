## Proyecto Team 20

library(ggplot2)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(dplyr)

# Leemos datos y creamos columnas usadas en mutliples tabs
data <- read.csv("match.data.csv", header = T)
teams <- sort(unique(c(data$home.team, data$away.team)))
df <- data %>%
    mutate(FTR = ifelse(home.score > away.score, "H", ifelse(home.score < away.score, "A", "D"))) %>%
    mutate(total.goals = home.score + away.score)


# La funcion utiliza boostrapping para generar un vector de medias (para comprobar TLC)
generar_medias <- function(df, sample_size){
    set.seed(1234)
    medias <- vector("numeric", length=10000) 
    for (i in 1:length(medias)){
        
        # Obtener muestra
        sample <- sample(nrow(df), size = sample_size, replace = T)
        df_new <- df[sample,]
        
        medias[i] <- mean(df_new$total.goals)
    }
    return(medias)
}

ui <- fluidPage(
    dashboardPage(
        dashboardHeader(title = "Prediccion de Resultados"),
        dashboardSidebar(
            sidebarMenu(
                menuItem("Integrantes", tabName = "Integrantes", icon = icon("file-picture-o")),
                menuItem("Graficos de barras", tabName = "Dashboard", icon = icon("dashboard")),
                menuItem("Partidos entre Equipos", tabName = "historic", icon = icon("area-chart")),
                menuItem("Goles casa - visitante", tabName = "goles", icon = icon("area-chart")),
                menuItem("Data Table", tabName = "data_table", icon = icon("table")),
                menuItem("Factores de ganancia", tabName = "momios", icon = icon("file-picture-o")),
                menuItem("Histograma de goles", tabName = "histograma", icon = icon("area-chart")),
                menuItem("Teorema del Limite Central", tabName = "tlc", icon = icon("area-chart")),
                menuItem("Contraste de Hipotesis", tabName = "hipotesis", icon = icon("table"))
            )
        ),
        dashboardBody(tabItems(
            
            # Integrantes
            tabItem(tabName = "Integrantes",
                fluidRow(
                    img(src = "bedu.png", height = 180, width = 180),
                    headerPanel("* * *   Equipo 20    * * *"),  
                    p(h6(".")),
                    titlePanel("- Alamo Diaz, Luis Manuel"),
                    titlePanel("- Caballero Valdez, Roberto"),
                    titlePanel("- Canton Arceo, Abdel Karim"),
                    titlePanel("- Herrera Hernandez, Juan Carlos"),
                    titlePanel("- Neri Armenta, Mayte")
                )
            ),
            
            # Graficos de barras con face wrap
            tabItem(tabName = "Dashboard",
                fluidRow(
                    titlePanel("Goles a favor y en contra por equipo"),
                    selectInput(inputId = "scorer", 
                                label = "Selecciona visitante o local", 
                                list("away.score", "home.score"), 
                                selected = "home.score"),
                    plotOutput("bar", height = 500)
                )
            ),

            # Partidos por equipo
            tabItem(tabName = "historic",
                fluidRow(
                    titlePanel("Partidos Realizados entre Equipos"), 
                    selectInput("historic_home", "Equipo local", choices = teams),
                    selectInput("historic_away", "Equipo Visitante", choices = teams),
                    verbatimTextOutput("historic")
                )
            ),
            
            # Postwork 3
            tabItem(tabName = "goles", 
                fluidRow(
                    titlePanel(h3("Probabilidad de goles en casa y visitante")),
                    img(src = "pw3_home.png", height = 425) ,
                    img(src = "pw3_away.png", height = 425),
                    img(src = "pw3_hm.png", height = 425)
                )
            ),
            
            # Data table
            tabItem(tabName = "data_table",
                fluidRow(        
                    titlePanel(h3("Data Table")),
                    dataTableOutput ("data_table")
                )
            ),
            
            # Factores de ganancia
            tabItem(tabName = "momios",
                fluidRow(
                    titlePanel(h3("Factores de ganancia, según tipo de momio")),
                    column(
                        width=6,
                        h4("Factor de ganancia Maximo"),
                        img(src = "momios_max.png", height = 500)
                    ),
                    column(
                        width=6,
                        h4("Factor de ganancia Promedio"),
                        img(src = "momios_avg.png", height = 500)
                    )
                )
            ),
            
            # Histograma dinÃ¡mico segÃºn equipo y/o year
            tabItem(tabName = "histograma",
                fluidRow(
                    titlePanel('Histograma para el total de goles por partido'), 
                    selectInput("selected_team", "Seleccione un equipo", choices = c("All", teams)),
                    selectInput("selected_year", "Seleccione un year", choices = c("All", 2010:2020)),
                    plotOutput("histograma_goles", height = 400)
                )
            ),
                    
            # ComprobaciÃ³n de TLC
            tabItem(tabName = "tlc",
                fluidRow(
                    titlePanel('Comprobacion del Teorema del Limite Central'),
                    h4("Se utiliza bootstrapping para generar medias de total de goles por partido (10000 medias). Partiendo de ahi confirmamos el teorema del limite central, donde la media de las medias es igual a la media poblacional, y la desviacion estandar de las medias es igual al error estandar de la poblacion"),
                    sliderInput("bins", label = "Number of bins:", min = 1, value = 16, max = 50),
                    plotOutput("histograma_tlc", height = 400)
                )
            ),
                    
            # Contrastes de hipotesis
            tabItem(tabName = "hipotesis",
                fluidRow(
                    titlePanel('Contraste de hipotesis comparando media de goles entre dos equipos'),
                    h4("Uso: Selecciona dos equipos, una hipotesis, y un intervalo de confianza. Se utilizara t.test para determinar si podemos o no rechazar la hipotesis nula y aceptar la hipotesis alternativa, segun sea el caso (utilizando la media de goles por partido para los equipos seleccionados)"),
                    selectInput("h.team.x", "Equipo X", choices = c(teams)),
                    selectInput("h.team.y", "Equipo Y", choices = c(teams)),
                    selectInput("alternative", "Hipotesis Alternativa", 
                                choices = c("Media de X es mayor a media de Y", "Media de X es menor a media de Y")
                    ),
                    selectInput("ci", "Intervalo de Confianza", choices = c("90%", "95%", "99%")),
                    htmlOutput("hipotesis")
                )
            )
        ))
    )
)


server <- function(input, output) {
    
    #GrÃ¡fico de barras
    output$bar <- renderPlot({
        x <- df[,input$scorer]
        g <- ggplot(data=df, aes(x)) +
            geom_bar(color='darkblue', fill="darkblue")
        g + facet_wrap("away.team") + labs(x = input$scorer, y = "Conteo")
    })

    #Data Table
    output$data_table <- renderDataTable({data}, options = list(aLengthMenu = c(10,25,50), iDisplayLength = 10))
    
    # Partidos entre equipos
    output$historic <- renderPrint({
        filtered_df <- df %>% filter(home.team == input$historic_home & away.team == input$historic_away)
        filtered_df
    })
    
    # Histograma dinÃ¡mico segÃºn equipo y/o year
    output$histograma_goles <- renderPlot({ 
        df_filtered <- df %>%
            filter(if (input$selected_team == "All") TRUE else home.team == input$selected_team | away.team == input$selected_team) %>% 
            mutate(year = format(as.Date(date), format="%Y")) %>%
            filter(if (input$selected_year == "All") TRUE else year == input$selected_year)
        
        # Si se seleccionan todos los equipos, tenemos que combinar los scores de home y away en una misma columna        
        if(input$selected_team == "All") {
            df_filtered <- append(df_filtered$home.score, df_filtered$away.score) %>%
                as.data.frame(col.names=c("goles")) %>%
                setNames(c("goles")) %>%
                arrange(goles)
        } else {
        # Si es solo un equipo, solo tomamos el score correspondiente con base en si el equipo era el home o el away
            df_filtered <- df_filtered %>%
                mutate(goles = ifelse(home.team == input$selected_team, home.score, away.score)) %>%
                select(goles) %>%
                arrange(goles)
        }

        # Algunos equipos no estan en todas las temporadas
        validate(
            need(try(nrow(df_filtered) > 0), "Los parametros seleccionados no arrojaron datos")
        )
        
        # Graficamos       
        ggplot(df_filtered, aes(goles)) + 
            geom_histogram(aes(y=..density..),
                           color = 'darkorange', 
                           fill = 'orange',
                           alpha = 0.5, # Intensidad del color fill
                           binwidth = 1) + 
            #geom_density(alpha=0.6, color="darkred") +
            stat_function(fun = dnorm, args = list(mean = mean(df_filtered$goles), sd = sd(df_filtered$goles))) +
            geom_vline(aes(xintercept=mean(goles)),
                       color="darkred",
                       linetype="dashed") +
            labs(x = 'Goles por Partido', y = 'Frecuencia') +
            theme_light() +
            theme(plot.title = element_text(hjust = 0.5, size = 16))
    })
    
    # Comprobacion de TLC
    output$histograma_tlc <- renderPlot({

        # Generamos un vector de medias utilizando muestras de 350 observaciones
        medias <- generar_medias(df, 350)
        
        ggplot() +
            aes(medias) +
            geom_histogram(bins = input$bins, col="black", fill="steelblue") + 
            geom_vline(aes(xintercept = mean(medias), colour="red"), size=2, linetype="dashed", show.legend = F) +
            ggtitle("Histograma de Medias") +
            ylab("Frecuencia") +
            xlab("Medias de Goles por Partido (Home + Away)") +
            theme_light() +
            theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5)) +
            annotate("text", x=2.4, y=2000, size = 6, label = paste0("Media de Medias = ", round(mean(medias), 3)), hjust= 0) +
            annotate("text", x=2.4, y=1875, size = 6,label = paste0("Media Poblacional = ", round(mean(df$total.goals), 3)), hjust= 0) +
            annotate("text", x=2.4, y=1750, size = 6,label = paste0("S.D. de Medias = ", round(sd(medias), 3)), hjust= 0) +
            annotate("text", x=2.4, y=1625, size = 6,label = paste0("E.E Poblacional = ", round(sd(df$total.goals)/sqrt(350), 3)), hjust= 0)
    })
    
    # Contraste de hipotesis
    output$hipotesis <- renderUI({
        df.x <- df %>%
            filter(home.team == input$h.team.x | away.team == input$h.team.x) %>% 
            mutate(goles = ifelse(home.team == input$h.team.x, home.score, away.score))
        
        df.y <- df %>%
            filter(home.team == input$h.team.y | away.team == input$h.team.y) %>% 
            mutate(goles = ifelse(home.team == input$h.team.y, home.score, away.score))
        
        alt = ifelse(input$alternative == "Media de X es mayor a media de Y", "greater", "less")
        
        mean_test <- t.test(x = df.x$goles, y = df.y$goles, 
                            alternative = alt,  # greater or less
                            mu = 0, conf.level = 0.95)
        
        ci <- as.numeric(sub("%", "", input$ci))/100
        
        if(alt == "greater"){
            str1 <- paste0("Ho: media(", input$h.team.x, ") <= media(", input$h.team.y, ")")
            str2 <- paste0("Ha: media(", input$h.team.x, ") > media(", input$h.team.y, ")")
        } else {
            str1 <-paste0("Ho: media(", input$h.team.x, ") >= media(", input$h.team.y, ")")
            str2 <-paste0("Ha: media(", input$h.team.x, ") < media(", input$h.team.y, ")")
        }
        
        if(mean_test$p.value < 1-ci){
            str3 <- paste0("p-value de ", round(mean_test$p.value, 3), ". Se cumple significancia al ", input$ci, ", y por ende se rechaza la hipotesis nula.")
        } else {
            str3 <- paste0("p-value de ", round(mean_test$p.value, 3), ". No se cumple significancia al ", input$ci, ", y por ende no podemos rechazar la hipotesis nula.")
        }
        
        str4 <- paste0("Medias reales:<br>", 
                       input$h.team.x, " = ", round(mean_test$estimate[["mean of x"]], 2), "<br>",
                       input$h.team.y, " = ", round(mean_test$estimate[["mean of y"]], 2))
        HTML(paste("<h4>", str1, str2, str3, str4, "<h4/>", sep = '<br><br>'))
    })
    
}


shinyApp(ui, server)