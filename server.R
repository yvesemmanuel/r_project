server <- function(input, output){
    
    selected_measurement <- eventReactive(input$submeter, {
        
        medida_selecionada <- input$medida
        intervalo_tempo <- input$intervalo_tempo
        
        dataframe_selecionado <- dados %>% select("date", measure_name)
        
        return(data_measure)
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
    
    }