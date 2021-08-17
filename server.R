server <- function(input, output){
    
    ################# TAB-1 FUNCTIONS
    
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
        
        dateRangeInput("intervalo_tempo", "Período de análise:",
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
            ylab(medida_selecionada) +
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
            ylab(medida_selecionada) +
            theme_bw()
        
        grafico
    })
    
    output$boxplot <- renderPlot({
        df <- classe_selecionada()
        medida_selecionada <- input$medida
        
        valores <- pull(df, medida_selecionada)
        
        grafico <- df %>%
            ggplot(aes(valores)) +
            geom_boxplot() +
            theme_bw()
        
        grafico
    })
    
    ################# TAB-1 FUNCTIONS - END
    
    
    ################# TAB-2 FUNCTIONS
    
    classes_selecionadas2 <- eventReactive(input$submeter_comparacao, {
        medidas_selecionadas <- input$medidas_comparadas
        intervalo_tempo2 <- input$intervalo_tempo
        
        dataframe_selecionado2 <- dados %>% select("date", medidas_selecionadas) %>% filter(date >= intervalo_tempo2[1] & date <= intervalo_tempo2[2])
        
        return (dataframe_selecionado2)
        
    })
    
    output$data_comparacao <- renderUI({
        medidas_selecionadas <- input$medidas_comparadas
        df <- dados %>% select("date")
        
        tempo_minimo <- min(dados$date)
        tempo_maximo <- max(dados$date)
        
        dateRangeInput("intervalo_tempo2", "Período de análise:",
                       end = tempo_maximo,
                       start = tempo_minimo,
                       min = tempo_minimo,
                       max = tempo_maximo,
                       format = "dd/mm/yy",
                       separator = " - ",
                       language = "pt-BR")
    })
    
    info_datatable_correlacao <- eventReactive(input$submeter_comparacao, {
        df <- classes_selecionadas2()
        medidas_selecionadas <- input$medidas_comparadas
        
        medida1 <- pull(df, medidas_selecionadas[1])
        medida2 <- pull(df, medidas_selecionadas[2])
        
        Pearson <- cor(medida1, medida2, method = "pearson", use = "complete.obs")
        Kendall <- cor(medida1, medida2, method = "kendall", use = "complete.obs")
        Spearman <- cor(medida1, medida2, method = "spearman", use = "complete.obs")
        
        df <- data.frame(Pearson, Kendall, Spearman)
        df <- as.data.frame(t(df))
        return (df)
    })

    output$tabela_correlacao <- renderDT({
        info_datatable_correlacao() %>%
            as.data.frame() %>%
            DT::datatable(options = list(
                language = list(
                    url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json"
                )
            ))
    })
    
    output$regressao <- renderPlot({
        df <- classes_selecionadas2()
        medidas_selecionadas <- input$medidas_comparadas
        
        medida1 <- pull(df, medidas_selecionadas[1])
        medida2 <- pull(df, medidas_selecionadas[2])
        
        regressao_linear <- df %>%
            ggplot(aes(x = medida1, y = medida2)) +
            xlab(medidas_selecionadas[1]) +
            ylab(medidas_selecionadas[2]) +
            geom_point() +
            geom_smooth(method = "lm") +
            theme_bw()
        
        regressao_linear
        
    })
    
    output$scatterplot <- renderPlot({
        df <- classes_selecionadas2()
        medidas_selecionadas <- input$medidas_comparadas
        
        medida1 <- pull(df, medidas_selecionadas[1])
        medida2 <- pull(df, medidas_selecionadas[2])

        grafico <- df %>%
            ggplot(aes(x = medida1, y = medida2)) +
            xlab(medidas_selecionadas[1]) +
            ylab(medidas_selecionadas[2]) +
            geom_point() +
            theme_bw()
        
        grafico
        
    })
    
    output$grafico_linha2 <- renderPlot({
        df <- classes_selecionadas2()
        medidas_selecionadas <- input$medidas_comparadas
        
        medida1 <- pull(df, medidas_selecionadas[1])
        medida2 <- pull(df, medidas_selecionadas[2])
        
        df$date <- ymd(df$date)
        
        grafico <- df %>%
            ggplot(aes(date)) +
            ylab(paste(paste(medidas_selecionadas[1], " (preto)"), paste(medidas_selecionadas[2], " (azul)"), sep = "   -   ")) +
            geom_line(aes(y = medida1), color = "blue") +
            geom_line(aes(y = medida2), color = "black") +
            theme_bw()
        
        grafico
    })
    
    output$grafico_medias <- renderPlot({
        df <- classes_selecionadas2()
        medidas_selecionadas <- input$medidas_comparadas
        
        df2 <- data.frame(
            Medias = c(mean(pull(df, medidas_selecionadas[1])),
                       mean(pull(df, medidas_selecionadas[2]))),
            Medidas = as.factor(c(medidas_selecionadas[1], medidas_selecionadas[2]))
        )
        
        df2_long <- df2 %>%
            gather("Medida", "Valor", -Medidas)
        
        grafico <- df2_long %>%
            ggplot(aes(Medidas, Valor)) +
            geom_col(position = "dodge")
        
        grafico
    })
    
    
}
