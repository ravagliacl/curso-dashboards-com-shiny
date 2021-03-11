###############################################################################
# Zetta Health Academy - Curso Dashboards com Shiny
# PROJETO 2 - Caixas interativas e Mapas
###############################################################################
# URL: https://github.com/zetta-academy/curso-dashboards-com-shiny
# Autor: Henrique Gomide, Ph.D.
#
# Esta e a interface grafica do aplicativo Shiny (o que vai aparecer na tela) 
# Para executar a aplicacao: clique em 'Run App' no canto superior direito deste
# quadrante 
#
# Recomendamos que busque mais informacoes sobre o shiny neste link:
#    http://shiny.rstudio.com/

# Você precisará instalar este pacote
# Documentação https://rstudio.github.io/shinydashboard/structure.html 
library(shinydashboard) # Você precisará instalar este pacote
library(readr) 
library(shiny)
library(leaflet)
library(shinyWidgets)
library(dashboardthemes)
library(plotly)


# Carrega a lista de cidades
city_options <- read_lines("data/city_selector.gz")

# fonte:  https://stackoverflow.com/questions/61645081/put-a-dropdown-button-on-shinydashboard-header-for-theme-selection 
# c('blue_gradient', 'boe_website', 'grey_light','grey_dark','onenote', 'poor_mans_flatly', 'purple_gradient'),
                                                 
    
#  Definir a interface grafica da aplicacao shiny 
shinyUI <- dashboardPage(
    
    dashboardHeader(title = "Final Claudia"
                    ),
    
    dashboardSidebar(
        
        img(src="home_marca_2019.png",width=200),
        
        sidebarMenu(
            menuItem("Mapa", 
                     tabName = "mapa", 
                     icon = icon("map") , 
                     selected = TRUE) ,
            menuItem("Graficos"     
                     , tabName = "grafico"   
                     , icon = icon("line-chart") 
                     ),
            menuItem("Tabela",  
                     tabName = "tabela", 
                     icon = icon("table") 
                    )
        )
    ),
    
    dashboardBody(
        shinyDashboardThemes(
            theme = 'poor_mans_flatly'
        ),
        
        tabItems(
                    tabItem(tabName = "mapa",   
                    fluidRow(
                        box(
                            status = "success", 
                            title = "Cidade: ",
                            selectInput("city",
                                        label = "Selecione uma cidade", 
                                        choices = city_options,
                                        selected = city_options[0]),
                        )
                        
                    ),
                    fluidRow(
                        valueBoxOutput("ultimadata"),
                        infoBoxOutput("numeroCasosUltimoDia"),
                        infoBoxOutput("numeroCasosAcumulados"),

                        
                    ),
                   fluidRow(
                    column(8,leafletOutput("mapa_exercicio_2", height = "550px" )),
                    column(4,plotlyOutput("efetivo", height = "550px"))
                    ),
                ),
        
        tabItem(tabName = "grafico",
                column(6,plotlyOutput("grafico1", height = "850px")),
                column(6,plotlyOutput("grafico2", height = "850px"))
        ),
        
        tabItem(tabName = "tabela",
                fluidRow(  h3("Dados COVID19"),
                           dataTableOutput("tabela")
                        )
        )
      ) 
    )
)