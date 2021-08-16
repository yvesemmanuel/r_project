server <- function(input, output){
    
    classe_selecionada <- eventReactive(input$submeter, {
        
        medida_selecionada <- input$medida
        intervalo_tempo <- input$intervalo_tempo
        
        dataframe_selecionado <- dados %>% select("date", medida_selecionada) %>% filter(date >= intervalo_tempo[1] & date <= intervalo_tempo[2])
        
        return(dataframe_selecionado)
    })
    
    classes_selecionadas_2 <- eventReactive(input$submeter_comparacao, {
        
        medidas_selecionadas <- input$medidas_comparadas
        intervalo_tempos_2 <- input$intervalo_tempos_2

        
        dataframe_selecionado_2 <- dados %>% select("date", medidas_selecionadas) %>% filter(date >= intervalo_tempos_2[1] & date <= intervalo_tempos_2[2])
        
        return(dataframe_selecionado_2)
        
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
    
    
    output$datas_comparacao <- renderUI({
        
        medidas_selecionadas <- input$medidas_comparadas
        
        df <- dados %>% select("date")
        
        tempo_minimo <- min(dados$date)
        tempo_maximo <- max(dados$date)
        
        dateRangeInput("intervalo_tempos_2", "Periodo de analise:",
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
    
    info_datatable_corr <- eventReactive(input$submeter_comparacao, {
        df_2 <- classes_selecionadas_2()
        Medida_2 <- input$medidas_comparadas
        
        lista_classe_2 <- pull(df_2, Medida_2[1])
        lista_classe_3 <- pull(df_2, Medida_2[2])
        
        correlation <- cor(lista_classe_3, lista_classe_2, method = "pearson", use = "complete.obs")
        return(correlation)
    })
    

    output$corr_table <- renderDT({
        info_datatable_corr() %>%
            as.data.frame() %>%
            DT::datatable(options = list(
                language = list(
                    url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json"
                )
            ))
        
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
            ggplot(aes(date, lista_classe, group = 1)) +
            geom_line() +
            ylab(Medida) +
            coord_cartesian(ylim = c(valor_minimo, valor_maximo)) +
            theme_bw() +
            scale_x_date(date_labels = "%Y-%m-%d")
        
        grafico
    })
    
    output$histograma <- renderPlot({
        df <- classe_selecionada()
        medida_selecionada <- input$medida
        
        frequências <- pull(df, medida_selecionada)
        
        grafico <- df %>%
            ggplot(aes(frequências)) +
            geom_histogram() +
            ylab(medida_selecionada) +
            theme_bw()
        
        grafico
    })
    
    output$boxplot <- renderPlot({
        df <- classe_selecionada()
        medida_selecionada <- input$medida
        
        valor <- pull(df, medida_selecionada)
        
        grafico <- df %>%
            ggplot(aes(valor)) +
            geom_boxplot() +
            theme_bw()
        
        grafico
    })
    
    output$scatter <- renderPlot({
        df <- classes_selecionadas_2()
        medidas_selecionadas <- input$medidas_comparadas
        
        valores_2 <- pull(df, medidas_selecionadas[1])
        valores_3 <- pull(df, medidas_selecionadas[2])

        
        grafico <- df %>%
            ggplot(aes(x = valores_2, y = valores_3)) +
            xlab(medidas_selecionadas[1]) +
            ylab(medidas_selecionadas[2]) +
            geom_point() +
            theme_bw()
        
        grafico
        
    })
    
    output$series_graph <- renderPlot({
        df <- classes_selecionadas_2()
        medidas_selecionadas <- input$medidas_comparadas
        
        classe_1 <- pull(df, medidas_selecionadas[1])
        classe_2 <- pull(df, medidas_selecionadas[2])
        
        df$date <- ymd(df$date)
        
        grafico <- df %>%
            ggplot(aes(date)) +
            geom_line(aes(y = classe_1), color = "blue") +
            geom_line(aes(y = classe_2), color = "black") 

            
        
        grafico
        

        
    })
    
    output$averages_graph <- renderPlot({
        df <- classes_selecionadas_2()
        medidas_selecionadas <- input$medidas_comparadas
        
        dat <- data.frame(
            Medias = c(mean(pull(df, medidas_selecionadas[1])),
                       mean(pull(df, medidas_selecionadas[2]))),
            Medidas = as.factor(c(medidas_selecionadas[1], medidas_selecionadas[2]))
        )
        
        dat_long <- dat %>%
            gather("Medida", "Valor", -Medidas)
        
        grafico <- dat_long %>%
            ggplot(aes(x=Medidas, y = Valor)) +
            geom_col(position = "dodge")
        
        
        grafico
    })
    
    
}
