server <- function(input, output){
    
    classe_selecionada <- eventReactive(input$submeter, {
        
        medida_selecionada <- input$medida
        intervalo_tempo <- input$intervalo_tempo
        
        dataframe_selecionado <- dados %>% select("date", medida_selecionada) %>% filter(date >= intervalo_tempo[1] & date <= intervalo_tempo[2])
        
        return(dataframe_selecionado)
    })
    
    output$data <- renderUI({
        
        medida_selecionada <- input$medida
        
        dados <- dados %>% select("date")
        
        tempo_minimo <- min(dados$date)
        tempo_maximo <- max(dados$date)
        
        dateRangeInput("intervalo_tempo", "Periodo de analise:",
                       end = tempo_maximo,
                       start = tempo_minimo,
                       min = tempo_minimo,
                       max = tempo_maximo,
                       format = "dd/mm/yy",
                       separator = " - ",
                       language = "pt-BR")
    })
    
    getmode <- function(x) {
        which.max(tabulate(x))
    }
    
    info_datatable <- eventReactive(input$submeter, {
        df <- classe_selecionada()
        Medida <- input$medida
        
        lista_classe <- pull(df, Medida)
        
        Media <- mean(lista_classe, na.rm = TRUE)
        Moda <- getmode(lista_classe)
        Mediana <- median(lista_classe, na.rm = TRUE)
        DesvioPadrao <- sd(lista_classe, na.rm = TRUE)
        ValorMaximo <- max(lista_classe, na.rm = TRUE)
        ValorMinimo <- min(lista_classe, na.rm = TRUE)
        
        df_tb <- data.frame(Medida, Media, Moda, Mediana, DesvioPadrao, ValorMaximo, ValorMinimo)
        
        df_tb <- as.data.frame(t(df_tb))
        
        return(df_tb)
        
    })
    
    output$info <- renderDT({
        info_datatable() %>%
            as.data.frame() %>%
            DT::datatable(options = list(
                language = list(
                    url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json"
                )
            ))
        
    })
    
    output$sh <- renderPlot({
        df <- classe_selecionada()
        Medida <- input$medida
    
        lista_classe <- pull(df, Medida)
        
        valor_minimo <- min(lista_classe)
        valor_maximo <- max(lista_classe)
        
        df$date <- ymd(df$date)
        grafico <- df %>%
            ggplot(aes(date, mean_temperature, group = 1)) +
            geom_path() +
            ylab("Grafico da Serie") +
            coord_cartesian(ylim = c(valor_minimo, valor_maximo)) +
            theme_bw() +
            scale_x_date(date_labels = "%Y-%m-%d")
        
        grafico
    })
    
    }