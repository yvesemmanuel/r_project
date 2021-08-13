server <- function(input, output){
    
    classe_selecionada <- eventReactive(input$submeter, {
        
        medida_selecionada <- input$medida
        intervalo_tempo <- input$intervalo_tempo
        
        dataframe_selecionado <- dados %>% select("date", medida_selecionada) %>% filter(date >= intervalo_tempo[1] & date <= intervalo_tempo[2])
        
        return (dataframe_selecionado)
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
        medida_selecionada <- input$medida
        lista_classe <- pull(df, medida_selecionada)

        media <- mean(lista_classe, na.rm = TRUE)
        moda <- getmode(lista_classe)
        mediana <- median(lista_classe, na.rm = TRUE)
        desvio_padrao <- sd(lista_classe, na.rm = TRUE)
        valor_maximo <- max(lista_classe, na.rm = TRUE)
        valor_minimo <- min(lista_classe, na.rm = TRUE)

        df_tb <- data.frame(medida_selecionada, media, moda, mediana, desvio_padrao, valor_maximo, valor_minimo)
        df_tb <- as.data.frame(t(df_tb))

        return (df_tb)

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
    
    output$grafico_linha <- renderPlot({
        df <- classe_selecionada()
        medida_selecionada <- input$medida
        lista_classe <- pull(df, medida_selecionada)
        
        valor_minimo <- min(lista_classe)
        valor_maximo <- max(lista_classe)
        
        df$date <- ymd(df$date)
        grafico <- df %>%
            ggplot(aes(date, lista_classe, group = 1)) +
            geom_line() +
            ylab("Gráfico da Série") +
            coord_cartesian(ylim = c(valor_minimo, valor_maximo)) +
            theme_bw() +
            scale_x_date(date_labels = "%Y-%m-%d")
        
        grafico
    })
    
    
    output$histograma <- renderPlot({
        df <- classe_selecionada()
        medida_selecionada <- input$medida
        
        frequencias <- pull(df, medida_selecionada)

        grafico <- df %>%
            ggplot(aes(frequencias)) +
            geom_histogram() +
            ylab("Histograma") +
            theme_bw()
        
        grafico
    })
    
    
    
    }