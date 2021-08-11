header <- dashboardHeader(title = "Statistics Project",
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
    
    menuItem("Metricas", tabName = "estatistica", icon = icon("calculator")),
    menuItem('Comparacao', tabName = "comparar_medidas", icon = icon("chart-bar")),
    menuItem("Git Repository", icon = icon("github"), href = "https://github.com/yvesemmanuel/r_project")
    
    )
  )

body <- dashboardBody(
  tabItems(
    
    tabItem(tabName = "estatistica",
            
            fluidRow(
              box(title = "Selecione suas opcoes::", width = 12, solidHeader = TRUE, status = "primary",
                  selectInput("medida", "Medidas", medidas_climaticas, multiple = FALSE),
                  uiOutput("data"),
                  actionButton("submeter", "Submit")
                  )
              ),
            
            fluidRow(
              box(title = "Informacoes sobre a medida selecionada:", width = 12, solidHeader = TRUE, status = "primary",
                  DTOutput("info"))
            ),
            
            fluidRow(
              box(title = "Serie da medida:", width = 12, solidHeader = TRUE,
                  plotOutput("sh"))
            )
            
            ),
    tabItem(tabName = "comparar_medidas",
            fluidRow(
              box(title = "Selecione suas 2 opcoes:", width = 12, solidHeader = TRUE, status = "primary",
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