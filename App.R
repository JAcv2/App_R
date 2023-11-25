# Cargar bibliotecas
library(shiny)
library(shinydashboard)
library(readr)
library(lubridate)
library(plotly)
library(dplyr)
library(forecast)
library(TSstudio)
library(xts)
library(tidyverse)
library(tseries)
library(tsibble)

# Definir la interfaz de usuario (UI)
ui <- dashboardPage(
  dashboardHeader(title = "Data Viz"), # titulo Dash
  dashboardSidebar( # Menu barra izq
    sidebarMenu(
      menuItem("EDA", tabName = "eda", icon = icon("dashboard")), # 1. Item 'EDA'
      menuItem("SERIES DE TIEMPO", tabName = "ts", icon = icon("bar-chart-o")), # 2. Item 'ST'
      menuItem("Modelos", tabName = "Modelos", icon = icon("table"), #  1. Item 'Modelos'
               menuSubItem("Auto.Arima",tabName = "autoarima"), # 1.2 subItem 'AutoArima'
               menuSubItem("Arima",tabName = "arima"), # 1.1 subItem 'Arima'
               menuSubItem("Red Neuronal",tabName = "res_n"), # 1.3 subItem 'RN'
               menuSubItem("Modelo Exponencial"   ,tabName = "mod_exp")) # 1.4 subItem 'ME'
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "eda", # pestaña con el nombre 'eda'
        tabBox(
          width = 12, # ancho del contenedor de la pestaña
          fluidRow(
            column(
              width = 3, # ancho de la columna
              selectInput(inputId = "año" , label = "Año: ", # box 'año'
                          choices = c("Total", "2020","2021","2022","2023"), # lista de años
                          selected = "Total") # por defecto
            ),
            column(
              width = 3,
              selectInput(
                inputId = "grupo", label = "Grupo", # box grupo
                choices = c("Mes" = "mes" , "Semana" = "semana" ,"Fecha" = "FECHA" ), # lista grupo , "Día Semana" = "dia" 
                selected = "Fecha" #por defecto
              )
            ),
            column(
              width = 3,
              checkboxInput( # mod grafico
                inputId = "tipo_grafico" ,label = "Mod Gráfico" ,
                value =  TRUE  
              )
            )
          ), # Panel EDA / serie de tiempo / Grafico densidad / Boxplot
          tabPanel(
            title = "Serie de tiempo",
            plotlyOutput("grafico_ts") # grafico serie de tiempo
          ),
          tabPanel(
            title = "Grafico densidad",
            plotlyOutput("grafico_density") # grafico para densidad
          ),
          tabPanel(
            title = "Boxplots",
            fluidRow(
              column(
                width = 4,
                plotlyOutput("boxplots_general") # boxplot año
              ),
              column(
                width = 8,
                plotlyOutput("boxplots")# boxplot de los grupos
              )
            )
          )
        )
      ),
      
      tabItem(
        # Serie de tiempo ####
        tabName = "ts", # pestaña ts para serie de tiempo
        fluidRow(
          tabBox(
            width = 12,
            title = "Serie de tiempo",
            tabPanel(
              title = "Descomposicion",
              uiOutput("plot_desomposition")
            )
          ),
          box(
            width = 12,
            numericInput(inputId = "diff_number" ,min = 1,max = 5,value = 1,width = "150px",
                         label = "Diferencias"),
            fluidPage(
              plotlyOutput("plot_seriediff"), # plot diferenciacion
              plotOutput("plot_seriediffACF"), # plot ACF
              textOutput("ressultados") # texto para resultados
            )
          )
        )
      ),
      tabItem(
        # UI: ARIMA ####
        tabName = "arima", # pestaña modelo arima
        tableOutput("info_modarima"), # tabla modelo arima
        textOutput("tes_d"), # salida test dicky-fuller
        tabBox(
          # aqui modelos
          title = "Arima",
          width =  12,
          fluidRow(
            column(
              width = 3,
              numericInput(inputId = "p" ,label = "p" ,value = 0) # inouto p (auto regresor) del modelo 
            ),
            column(
              width = 3,
              numericInput(inputId = "d" ,label = "d" ,value = 0) # input d (diferenciacion del modelo)
            ),
            column(
              width = 3,
              numericInput(inputId = "q" ,label = "q" ,value = 0) # inpunt q (promedio) del modelo
            )
          ),
          tabPanel(
            title = "Fitted", # panel de ajuste
            plotlyOutput("model_arima_fitted") # grafica modelo ajustado
          ),
          tabPanel(
            title = "Predicciones", # panel predicciones
            plotlyOutput("model_arima_pred") # grafica predicciones
          )
        )
      ),
      tabItem(
        # UI: AutoArima ####
        tabName = "autoarima", # pestaña modelo autoarima
        tableOutput("info_modauto"),# tabla modelo autoarima
        tabBox(
          # aqui modelos
          title = "Auto Arima",
          width =  12,
          numericInput("n_pred",min = 1,max = 100,label = "N° de días para la predicción: ",
                       value = 30,step = 5,width = "150px"), 
          tabPanel(
            title = "Fitted", # panel de ajuste
            plotlyOutput("auto_arima_fitted") # grafica modelo ajustado
          ),
          tabPanel(
            title = "Predicción", # panel predicciones
            plotlyOutput("predicciones_autoArima") # grafica predicciones
          )
        ),
      ),
      tabItem(
        
        # UI: res_neuronal ####
        tabName = "res_n", # pestaña red neuronal
        tableOutput("info_modneural"), # tabla modelo red neuronal
        tabBox(
          # aqui modelos
          title = "Red Neuronal",
          width =  12,
          tabPanel(
            title = "Fitted", #Panel de ajuste
            plotlyOutput("redes_fitted") # grafica modelo ajustado
          ),
          tabPanel(
            title = "Predicción", # panel de prediccion
            fluidRow(
              column(
                width = 3,
                numericInput("size_red",label = "Capas" , min = 1,max = 10,value = 1) # input de capas
              ),
              column(
                width = 3,
                numericInput("Rep",label = "Epocas" , min = 10,max = 100,value = 10,step = 10) # input de epocas
              )
            ),
            plotlyOutput("red_neuronal_predic") # grafica predicciones
          )
        )
      ),
      tabItem(
        tabName = "mod_exp",
        tableOutput("info_modExpo"),
        tabBox(
          title = "MODELO EXPONENCIAL",
          width = 12,
          tabPanel(
            title = "Fitted",
            plotlyOutput("exponencial_model"),
          ),
          tabPanel(
            title = "Test",
            plotlyOutput("model_exp_pred")
          )
        )
      )
    )
  )
)

# SERVER ####
server <- function(input, output) {
  # Código del servidor (puedes agregar funciones y lógica aquí)
  # Carga de data ####
  data <- reactive({
    data <- read_delim("ts_dash.csv", delim = ";", 
                              escape_double = FALSE, trim_ws = TRUE)
    data$FECHA <- dmy(data$FECHA)
    return(data)
  })

  output$grafico_ts <- renderPlotly({
    df <- data()
    #df <- data
    filtro <- input$año
    
    df <- df %>% mutate(mes = month(FECHA,label = TRUE) ,
                        semana = week(FECHA), año = as.character(year(FECHA))) # columnas año, mes, semana
    if(filtro == "Total"){
      df <- df 
    }else{
      df <- df %>% filter(año == filtro) 
    }
    grupo <- input$grupo
    df <- df %>% group_by(grupo = get(grupo)) %>% 
      summarise(promedio_calls = mean(`REAL CALLS`,na.rm = T)) # agrupacion por grupo seleccionado
    
    if(filtro == "Total"){
      titulo <- "Serie Temporal Calls"   
    }else(
      titulo <- paste0("Serie Temporal calls : ", filtro )
    )
    
    
    if(input$tipo_grafico == TRUE){
      fig1 <- plot_ly(data = df , x = ~ grupo , # grafico scatter (dispersion)
                      y = ~promedio_calls , 
                      type = "scatter" , mode = "lines") %>%
        layout(xaxis = list(title = grupo),
               title = titulo, yaxis = list(title = "Promedio Calls")) 
      
    }else{
      data <- df$promedio_calls
      x <- density(data)
      df_1 <- data.frame(y = x$y , x = x$x)
      fig1 <-  plot_ly(df_1, x = ~ x , y = ~y , type = "scatter", mode= "lines")
    }
    return(fig1)
  })
  
  
  # grafico de densidad
  output$grafico_density <- renderPlotly({
    df <- data()
    #df <- data
    filtro <- input$año
    
    df <- df %>% mutate(mes = month(FECHA,label = TRUE) ,
                        semana = week(FECHA), año = as.character(year(FECHA)))
    if(filtro == "Total"){
      df <- df 
    }else{
      df <- df %>% filter(año == filtro) 
    }
    grupo <- input$grupo
    df <- df %>% group_by(grupo = get(grupo)) %>% 
      summarise(promedio_calls = mean(`REAL CALLS`,na.rm = T))
    
    if(filtro == "Total"){
      titulo <- "Serie Temporal Calls"   
    }else(
      titulo <- paste0("Serie Temporal calls : ", filtro )
    )
    
    

    data <- df$promedio_calls
    x <- density(data)
    df_1 <- data.frame(y = x$y , x = x$x)
    df_1 <- df_1 %>% filter(x >= 0)
    fig1 <-  plot_ly(df_1, x = ~ x , y = ~y , type = "scatter", mode= "lines")
    return(fig1)
  })
  
  # grafico de boxplot
  output$boxplots <- renderPlotly({
    
    df <- data()
    filtro <- input$año
    df <- df %>% mutate(mes = month(FECHA,label = TRUE) ,
                        semana = week(FECHA), año = as.character(year(FECHA)))
    df$dia <- weekdays(df$FECHA)
    if(filtro == "Total"){
      df <- df 
    }else{
      df <- df %>% filter(año == filtro) 
    }

    grupo = input$grupo
    fig1 <- plot_ly(data = df , x = ~ get(grupo) ,
                    y = ~`REAL CALLS` , 
                    type = "box" ) %>% layout(xaxis =  list(title= grupo) , title = "BOXPLOT")
  
  })
  
  output$boxplots_general <- renderPlotly({
    df <- data()
    filtro <- input$año
    if(filtro  == "Total"){
      df <- df
    }else{
      df <- df %>% mutate(mes = month(FECHA,label = TRUE) ,
                          semana = week(FECHA), año = as.character(year(FECHA)))
      df <- df %>% filter(año == filtro)
    }

    fig1 <- plot_ly(data = df  ,x = "REAL CALLS",
                    y = ~`REAL CALLS` , 
                    type = "box" )
  }) 
  
  data_serie <- reactive({ #crea expresion reactiva de actualización automatica 
    df_proyect <- data() # obtengo datos del df reactivo definido
    df_proyect <- df_proyect %>% 
      complete(FECHA = seq(min(df_proyect$FECHA) , max(df_proyect$FECHA) , "days" )) # filtro fila para cada día en el rango de fechas.
    sum(is.na(df_proyect$`REAL CALLS`)) # valores NA
    ts_proyect <- ts(df_proyect$`REAL CALLS`,frequency=7) # transformacion de datos a ts frecuencia 90
  })
  
  
  
  
  
  
  # Serie de tiempo ####
  output$plot_desomposition <- renderUI({
    df <- data_serie()
    ts_decompose(df)
  })
  
  output$plot_seriediff <- renderPlotly({
    df <- data()
    diff.order = input$diff_number
    data.diff <- diff(df$`REAL CALLS`, differences = diff.order)
    data_aux <- data.frame(`REAL CALLS` = data.diff ,
                           FECHA = df$FECHA[diff.order+1:length(data.diff)] )
    title.prefix = diff.order
    # Crear gráfico de la serie de tiempo diferenciada
    fig1 <- plot_ly(data = data_aux, x = ~ FECHA ,
                    y = ~`REAL.CALLS`, 
                    type = "scatter" , mode = "lines") %>%
      layout(xaxis = list(title = "Fecha"),
             title = paste(title.prefix, "Order Differencing ACF"), yaxis = list(title = "Real Calls"))
  })
  
  
  output$plot_seriediffACF <- renderPlot({
    data <- data()
    diff.order = input$diff_number
    data.diff <- diff(data$`REAL CALLS`, differences = diff.order)
    title.prefix <- as.character(diff.order)
    Acf(data.diff, lag.max = 90, main = paste(title.prefix, "Order Differencing ACF"))

  })
  
  output$ressultados <- renderPrint({
    diff.order = input$diff_number
    # Realizar la diferencia de la serie y eliminar valores NA
    df_proyect <- data()
    df_proyect_diff <- na.omit(diff(df_proyect$`REAL CALLS`, differences = diff.order))
    
    # Realizar la prueba de Dickey-Fuller aumentada en la serie diferenciada
    dickey_fuller_result <- adf.test(df_proyect_diff)
    
    # Extracción de los resultados
    test_stat <- dickey_fuller_result$statistic
    p_value <- dickey_fuller_result$p.value
    alpha <- 0.05
    
    #cat("Hipotesis:\n")
    #cat("H0: Serie no estacionaria\n")
    #cat("H1: Serie estacionaria\n")
    cat("Test Dicky-Fuller:  ")
    cat(sprintf("Estadistico de prueba: %.4f\n", test_stat))
    cat(sprintf("p-value: %.4f\n", p_value))
    #cat(sprintf("Alpha: %f\n", alpha))
    if (p_value > alpha) {
      cat("No se rechaza H0, por lo tanto la serie no es estacionaria.\n")
    } else {
      cat("Se rechaza H0, por lo tanto la serie es estacionaria.\n")
    }
  })
  
  # SECCION: MODELOS ####
  
  output$info_modauto <- renderTable({
    df <- data()
    df <- df %>% rename(REAL_CALLS = `REAL CALLS`)
    df <- df %>% select(FECHA,`REAL_CALLS`)
    df <- tsibble(date = df$FECHA , value = df$REAL_CALLS)
    df_list <- ts_split(df, sample.out = input$n_pred)
    train <- df_list$train
    test <-  df_list$test
    p <- input$p
    d <- input$d  
    q <- input$q
    model_train <- auto.arima(train)
    predicciones <- forecast(model_train, h = input$n_pred)
    mse <- accuracy(predicciones$mean,  test$value )
    resmetricas <-  data.frame(t(mse)) 
    AIC <-  data.frame(row.names = "AIC", Test.set = AIC(model_train))  
    def <- rbind(resmetricas,AIC)
    t(def)
  })
  
  
   
   ## Auto arima ####
   output$auto_arima_fitted <- renderPlotly({

     df <- data()
     df <- df %>% rename(REAL_CALLS = `REAL CALLS`)
     df <- df %>% select(FECHA,`REAL_CALLS`)
     
     df <- tsibble(date = df$FECHA , value = df$REAL_CALLS ) # ts(df$x1)
     df_list <- ts_split(df, sample.out = input$n_pred)
     
     train <- df_list$train
     test <-  df_list$test
     

     model_train <- auto.arima(train)
     
     df_residuales <- data.frame(fecha = train$date , 
                                 REAL_CALLS = train$value , Fitted = model_train$fitted )
     
     fig1 <- plot_ly(df_residuales, x = ~ fecha ,
                     y = ~REAL_CALLS,type = "scatter",
                     mode = "lines", name = "Train")
     fig1 <- add_trace(fig1 , x = df_residuales$fecha , 
                       y = df_residuales$Fitted,type = "scatter", mode = "lines",
                       name = "Fitted")%>% layout(title = "ARIMA(1,0,2)(0,1,2)[7]") 
     fig1
     
   })
  
  
  output$predicciones_autoArima <- renderPlotly({
    df <- data()
    df <- df %>% rename(REAL_CALLS = `REAL CALLS`)
    df <- df %>% select(FECHA,`REAL_CALLS`)
    df <- tsibble(date = df$FECHA , value = df$REAL_CALLS )
    df_list <- ts_split(df, sample.out = input$n_pred)
    
    train <- df_list$train
    test <-  df_list$test
    model_train <- auto.arima(train)
    predicciones <- forecast(model_train, h = input$n_pred)
    
    df_pred <- data.frame(fecha = test$date , real_calls = test$value ,
                          prediccion = predicciones$mean  )
    
    fig1 <- plot_ly(df_pred , x = ~ fecha ,
                    y = ~real_calls,type = "scatter",
                    mode = "lines", name = "Test")
    fig1 <- add_trace(fig1 , x = df_pred$fecha , 
                      y = df_pred$prediccion,type = "scatter", mode = "lines",
                      name = "prediccion") %>% layout(title = "ARIMA(1,0,2)(0,1,2)[7]") 
    fig1
    
  })
  
  
  
  
  ## red neuronal ####
  output$info_modneural <- renderTable({
    df <- data()
    df <- df %>% rename(REAL_CALLS = `REAL CALLS`)
    df <- df %>% select(FECHA,`REAL_CALLS`)
    df <- tsibble(date = df$FECHA , value = df$REAL_CALLS)
    df_list <- ts_split(df, sample.out = input$n_pred)
    train <- df_list$train
    test <-  df_list$test
    p <- input$p
    d <- input$d  
    q <- input$q
    model_train <- forecast::nnetar(train$value ,size = input$size_red,repeats = input$Rep)
    prediction <- forecast(model_train , h = input$n_pred)
    mse <- accuracy(prediction$mean,  test$value )
    resmetricas <-  data.frame(t(mse)) 
    #AIC <-  data.frame(row.names = "AIC", Test.set = AIC(model_train))  
    def <- rbind(resmetricas)
    t(def)
  })
  
  
  
  output$redes_fitted <- renderPlotly({
    
    df <- data()
    df <- df %>% rename(REAL_CALLS = `REAL CALLS`)
    df <- df %>% select(FECHA,`REAL_CALLS`)
    df <- tsibble(date = df$FECHA , value = df$REAL_CALLS )
    df_list <- ts_split(df, sample.out = input$n_pred)
    
    train <- df_list$train
    test <-  df_list$test
    
    
    model_train <- forecast::nnetar(train$value ,size = input$size_red,repeats = input$Rep)
    
    df_residuales <- data.frame(fecha = train$date , 
                                REAL_CALLS = train$value , Fitted = model_train$fitted )
    
    fig1 <- plot_ly(df_residuales, x = ~ fecha ,
                    y = ~REAL_CALLS,type = "scatter",
                    mode = "lines", name = "Train")
    fig1 <- add_trace(fig1 , x = df_residuales$fecha , 
                      y = df_residuales$Fitted,type = "scatter", mode = "lines",
                      name = "Fitted")
    fig1
    
  })
  

  
  
  
  
  
  output$red_neuronal_predic <- renderPlotly({
    df <- data()
    df <- df %>% rename(REAL_CALLS = `REAL CALLS`)
    df <- df %>% select(FECHA,`REAL_CALLS`)
    df <- tsibble(date = df$FECHA , value = df$REAL_CALLS )
    df_list <- ts_split(df, sample.out = input$n_pred)
    
    train <- df_list$train
    test <-  df_list$test
    
  
    model_train <- forecast::nnetar(train$value ,size = input$size_red,repeats = input$Rep)
    prediction <- forecast(model_train , h = input$n_pred)
    
    df_pred <- data.frame(fecha = test$date , 
                                REAL_CALLS = test$value , prediction = prediction$mean)
    
    fig1 <- plot_ly(df_pred, x = ~ fecha ,
                    y = ~REAL_CALLS,type = "scatter",
                    mode = "lines", name = "Train")
    fig1 <- add_trace(fig1 , x = df_pred$fecha , 
                      y = df_pred$prediction,type = "scatter", mode = "lines",
                      name = "Fitted")
    fig1
    
  })
  
  
  ## Arima ####   
  
  output$tes_d <- renderPrint({
    df <- data()
    df <- df %>% rename(REAL_CALLS = `REAL CALLS`)
    df <- df %>% select(FECHA,`REAL_CALLS`)
    
    # Realizar la diferencia de la serie y eliminar valores NA
    if(input$d == 0){
      df_proyect_diff <- df$REAL_CALLS
      
    }else{
      df_proyect_diff <- na.omit(diff(df$REAL_CALLS, differences = input$d))
    }
    
    # Realizar la prueba de Dickey-Fuller aumentada en la serie diferenciada
    dickey_fuller_result <- adf.test(df_proyect_diff)
    
    # Extracción de los resultados
    test_stat <- dickey_fuller_result$statistic
    p_value <- dickey_fuller_result$p.value
    alpha <- 0.05
    
    #cat("Hipotesis:\n")
    #cat("H0: Serie no estacionaria\n")
    #cat("H1: Serie estacionaria\n")
    cat("Test Dicky-Fuller:  ")
    cat(sprintf("Estadistico de prueba: %.4f\n", test_stat))
    cat(sprintf("p-value: %.4f\n", p_value))
    #cat(sprintf("Alpha: %f\n", alpha))
    if (p_value > alpha) {
      cat("No se rechaza H0, por lo tanto la serie no es estacionaria.\n")
    } else {
      cat("Se rechaza H0, por lo tanto la serie es estacionaria.\n")
    }
    
  })
  
  
  output$info_modarima <- renderTable({
    df <- data()
    df <- df %>% rename(REAL_CALLS = `REAL CALLS`)
    df <- df %>% select(FECHA,`REAL_CALLS`)
    df <- tsibble(date = df$FECHA , value = df$REAL_CALLS)
    df_list <- ts_split(df, sample.out = input$n_pred)
    train <- df_list$train
    test <-  df_list$test
    p <- input$p
    d <- input$d  
    q <- input$q
    model_train <- forecast::Arima(ts(train$value ,frequency = 7),order = c(p,d,q) )
    prediction <- forecast(model_train , h = input$n_pred)
    mse <- accuracy(prediction$mean,  test$value )
    resmetricas <-  data.frame(t(mse)) 
    AIC <-  data.frame(row.names = "AIC", Test.set = AIC(model_train))  
    def <- rbind(resmetricas,AIC)
    t(def)
  })
  
  
  
  output$model_arima_fitted <- renderPlotly({
    df <- data()
    df <- df %>% rename(REAL_CALLS = `REAL CALLS`)
    df <- df %>% select(FECHA,`REAL_CALLS`)
    df <- tsibble(date = df$FECHA , value = df$REAL_CALLS )
    df_list <- ts_split(df, sample.out = input$n_pred)
    
    train <- df_list$train
    test <-  df_list$test
    
    p <- input$p
    d <- input$d  
    q <- input$q
    P <- input$P
    D <- input$D
    Q <- input$Q
    model_train <- forecast::Arima(ts(train$value ,frequency = 7),
                                   order = c(p,d,q))
    
    #title <- paste0("MOdelo Arima :", "(",p,",", d , ",",q   )
    df_residuales <- data.frame(fecha = train$date , 
                          REAL_CALLS = train$value , Fitted = model_train$fitted )
    
    fig1 <- plot_ly(df_residuales, x = ~ fecha ,
                    y = ~REAL_CALLS,type = "scatter",
                    mode = "lines", name = "Train")
    fig1 <- add_trace(fig1 , x = df_residuales$fecha , 
                      y = df_residuales$Fitted,type = "scatter", mode = "lines",
                      name = "Fitted")
    fig1
    
  })
  
  
  output$model_arima_pred <- renderPlotly({
    df <- data()
    df <- df %>% rename(REAL_CALLS = `REAL CALLS`)
    df <- df %>% select(FECHA,`REAL_CALLS`)
    df <- tsibble(date = df$FECHA , value = df$REAL_CALLS )
    df_list <- ts_split(df, sample.out = input$n_pred)
    
    train <- df_list$train
    test <-  df_list$test
    
    p <- input$p
    d <- input$d  
    q <- input$q
    
    
    model_train <- forecast::Arima(ts(train$value ,frequency = 7),order = c(p,d,q) )
    
    prediction <- forecast(model_train , h = input$n_pred)
    df_pred <- data.frame(fecha = test$date , 
                          REAL_CALLS = test$value , prediction = prediction$mean)
    
    fig1 <- plot_ly(df_pred, x = ~ fecha ,
                    y = ~REAL_CALLS,type = "scatter",
                    mode = "lines", name = "Test")
    fig1 <- add_trace(fig1 , x = df_pred$fecha , 
                      y = df_pred$prediction,type = "scatter", mode = "lines",
                      name = "Prediccion")
    fig1
    
  })
  
  
  output$chekresiduals <- renderPlot({
    
    df <- data()
    df <- df %>% rename(REAL_CALLS = `REAL CALLS`)
    df <- df %>% select(FECHA,`REAL_CALLS`)
    df <- tsibble(date = df$FECHA , value = df$REAL_CALLS )
    df_list <- ts_split(df, sample.out = input$n_pred)
    train <- df_list$train
    test <-  df_list$test
    p <- input$p
    d <- input$d  
    q <- input$q
    model_train <- arima(ts(train$value ,frequency = 7),order = c(p,d,q) )
    checkresiduals(model_train)
  })
  
  
  ## MODELO EXPONENCIAL ####
  
  output$info_modExpo <- renderTable({
    df <- data()
    df <- df %>% rename(REAL_CALLS = `REAL CALLS`)
    df <- df %>% select(FECHA,`REAL_CALLS`)
    df <- tsibble(date = df$FECHA , value = df$REAL_CALLS)
    df_list <- ts_split(df, sample.out = input$n_pred)
    train <- df_list$train
    test <-  df_list$test
    model_train <-ses(ts(train$value, frequency = 7) ,
                      h= input$n_pred, initial = "optimal")
    prediction <- forecast(model_train , h = input$n_pred)
    mse <- accuracy(prediction$mean,  test$value )
    resmetricas <-  data.frame(t(mse)) 
    #AIC <-  data.frame(row.names = "AIC", Test.set = AIC(model_train))  
    def <- rbind(resmetricas)
    t(def)
  })
  
  
  output$exponencial_model <- renderPlotly({
    df <- data()
    df <- df %>% rename(REAL_CALLS = `REAL CALLS`)
    df <- df %>% select(FECHA,`REAL_CALLS`)
    df <- tsibble(date = df$FECHA , value = df$REAL_CALLS )
    df_list <- ts_split(df, sample.out = input$n_pred)
    
    train <- df_list$train
    test <-  df_list$test
    
    model_train <-ses(ts(train$value,frequency = 7) ,
                      h=input$n_pred, initial ="optimal", alpha=0.9)
    
    df_residuales <- data.frame(fecha = train$date , 
                                REAL_CALLS = train$value , Fitted = model_train$fitted )
    fig1 <- plot_ly(df_residuales, x = ~ fecha ,
                    y = ~REAL_CALLS,type = "scatter",
                    mode = "lines", name = "Train")
    fig1 <- add_trace(fig1 , x = df_residuales$fecha , 
                      y = df_residuales$Fitted,type = "scatter", mode = "lines",
                      name = "Fitted")
    
    fig1
  })
  
  
  output$model_exp_pred <- renderPlotly({
    df <- data()
    df <- df %>% rename(REAL_CALLS = `REAL CALLS`)
    df <- df %>% select(FECHA,`REAL_CALLS`)
    df <- tsibble(date = df$FECHA , value = df$REAL_CALLS )
    df_list <- ts_split(df, sample.out = input$n_pred)
    
    train <- df_list$train
    test <-  df_list$test
    
    model_pred <- forecast::ses(ts(train$value ,frequency = 7),h = input$n_pred)
    
    fig1 <- plot_ly(test, x = ~ date ,
                    y = ~value,type = "scatter",
                    mode = "lines", name = "Test")
    fig1 <- add_trace(fig1 , x = test$date , 
                      y = model_pred$mean ,type = "scatter", mode = "lines",
                      name = "Prediccion")
    fig1
    
  })
  
  
  

}

shinyApp(ui, server)