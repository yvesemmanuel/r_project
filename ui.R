header <- dashboardHeader(title = "Projeto de Estatística",
                          dropdownMenu(type = "message",
                                       messageItem(
                                         from = "Yves Emmanuel",
                                         message = "Bem vindo ao nosso dashboard!",
                                         icon = icon("laugh-wink")
                                         ),
                                       messageItem(
                                         from = "David Londres",
                                         message = "Agradeçemos sua atenção!",
                                         icon = icon("handshake-o")
                                       ),
                                       messageItem(
                                         from = "Eduardo Geber",
                                         message = "Volte sempre!",
                                         icon = icon("mail-reply")
                                       )
                                       )
                          )


sidebar <- dashboardSidebar(
  sidebarMenu(
    
    menuItem("Métricas", tabName = "estatistica", icon = icon("calculator")),
    menuItem('Comparação', tabName = "comparar_medidas", icon = icon("chart-bar")),
    menuItem("Repositório Git", icon = icon("github"), href = "https://github.com/yvesemmanuel/r_project")
    
    )
  )


body <- dashboardBody(
  tabItems(
    
    tabItem(tabName = "estatistica",
            
            fluidRow(
              box(title = "Selecione suas opções", width = 12, solidHeader = TRUE, status = "primary",
                  selectInput("medida", "Classe a ser analisada:", medidas_climaticas, multiple = FALSE),
                  uiOutput("data"),
                  actionButton("submeter", "Submeter")
                  )
              ),
            
            fluidRow(
              box(title = "Informações sobre a classe selecionada", width = 12, solidHeader = TRUE, status = "primary",
                  DTOutput("info"))
            ),
            
            fluidRow(
              box(title = "Série da classe", width = 12, solidHeader = TRUE, status = "primary",
                  plotOutput("grafico_linha"))
            ),
            fluidRow(
              box(title = "Histograma", width = 12, solidHeader = TRUE, status = "primary",
                  plotOutput("histograma"))
            ),
            
            fluidRow(
              box(title = "Boxplot", width = 12, solidHeader = TRUE, status = "primary",
                  plotOutput("boxplot"))
            )
            
            ),
    
    tabItem(tabName = "comparar_medidas",
            
            fluidRow(
              box(title = "Selecione suas opções", width = 12, solidHeader = TRUE, status = "primary",
                  selectizeInput("medidas_comparadas", "Classes a serem relacionadas (escolha duas)", medidas_climaticas, multiple = TRUE, options = list(maxItems = 2)),
                  uiOutput("data_comparacao"),
                  actionButton("submeter_comparacao", "Submeter")
                  ),
              ),
              
              fluidRow(
                box(title = "Tabela de correlação", width = 12, solidHeader = TRUE, status = "primary",
                    DTOutput("tabela_correlacao"))
                 ),
            
              fluidRow(
                box(title = "Regressão Linear", width = 12, solidHeader = TRUE, status = "primary",
                    plotOutput("regressao"))
                ),
            
              fluidRow(
                box(title = "Gráfico das séries", width = 12, solidHeader = TRUE, status = "primary",
                    plotOutput("grafico_linha2"))
              ),
            
              fluidRow(
                box(title = "Gráfico das médias", width = 12, solidHeader = TRUE, status = "primary",
                    plotOutput("grafico_medias"))
              ),
            
              fluidRow(
                box(title = "Scatterplot das séries", width = 12, solidHeader = TRUE, status = "primary",
                    plotOutput("scatterplot"))
              ),
            
              
            )
    
      ),
  )
  

ui <- dashboardPage(
    skin = "blue",
    header, sidebar, body
    )