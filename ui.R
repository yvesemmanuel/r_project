header <- dashboardHeader(title = "Projeto de Estatística",
                          dropdownMenu(type = "message",
                                       messageItem(
                                         from = "Yves Emmanuel",
                                         message = "Bem vindo ao nosso dashboard!",
                                         icon = icon("laugh-wink")
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
              box(title = "Selecione suas opções:", width = 12, solidHeader = TRUE, status = "primary",
                  selectInput("medida", "Medidas", medidas_climaticas, multiple = FALSE),
                  uiOutput("data"),
                  actionButton("submeter", "Submit")
                  )
              ),
            
            fluidRow(
              box(title = "Informações sobre a medida selecionada:", width = 12, solidHeader = TRUE, status = "primary",
                  DTOutput("info"))
            ),
            
            fluidRow(
              box(title = "Gráfico em Linha", width = 12, solidHeader = TRUE, status = "primary",
                  plotOutput("grafico_linha"))
            ),
            
            fluidRow(
              box(title = "Histograma", width = 12, solidHeader = TRUE, status = "primary",
                  plotOutput("histograma"))
            ),
            
            fluidRow(
              box(title = "Box plot", width = 12, solidHeader = TRUE, status = "primary",
                  plotOutput("boxplot"))
            )
            
            ),
    tabItem(tabName = "comparar_medidas",
            fluidRow(
              box(title = "Selecione suas 2 opções:", width = 12, solidHeader = TRUE, status = "primary",
                  selectizeInput("medidas_comparadas", "Medidas", medidas_climaticas, multiple = TRUE, options = list(maxItems = 2)),
                  uiOutput("tempos_comparacao"),
                  actionButton("submeter_comparacao", "Submit")
                  )
            ))
    
    )
  )

ui <- dashboardPage(
    skin = "blue",
    header, sidebar, body
    )