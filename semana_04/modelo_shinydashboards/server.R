###############################################################################
# Zetta Health Academy - Curso Dashboards com Shiny
# PROJETO 2 - Caixas interativas e Mapas
###############################################################################
# URL: https://github.com/zetta-academy/curso-dashboards-com-shiny
# Autor: Henrique Gomide, Ph.D.
#
# Esta e a logica do aplicativo Shiny (onde a magica realmente acontece) 
# Para executar a aplicacao: clique em 'Run App' no canto superior direito deste
# quadrante 
#
# Recomendamos que busque mais informacoes sobre o shiny neste link:
#    http://shiny.rstudio.com/

library(shiny)
library(shinydashboard) # Você precisará instalar

library(tidyverse)
library(slider)
library(distcrete)
library(epitrix)
library(incidence)
library(projections)
library(EpiEstim)
library(rgdal)
library(dplyr)
library(leaflet)

   
source("./utils/helpers.R") # Carregar funcoes uteis

# Abrir banco de dados
if (!"covid19" %in% ls()){
  covid19 <- fetch_data_brasil_io(use_cached_data = TRUE)   
}


# Define a logica da aplicacao 
shinyServer(function(input, output) {
  
    output$numeroCasosAcumulados <- renderInfoBox({
        codigo_cidade <- get_city_code(input$city)
        
        casos_acumulados <- 
            covid19 %>% 
            filter(is_last, city_ibge_code == codigo_cidade) %>% 
            select(last_available_confirmed)
        
        infoBox("Casos acumulados", 
                icon = icon("calculator"),
                color="teal",
                formatC(casos_acumulados$last_available_confirmed,
                        digits = 12,
                        big.mark = ".",
                        decimal.mark = ",")
                )
    })
    
    output$numeroCasosUltimoDia <- renderInfoBox({
        codigo_cidade <- get_city_code(input$city)
        
        casos_ultimo_dia <- 
            covid19 %>% 
            filter(is_last, city_ibge_code == codigo_cidade) %>% 
            select(new_confirmed)
        
        infoBox("Casos 24h ", 
                icon = icon("stethoscope"),
                formatC(casos_ultimo_dia$new_confirmed,
                        digits = 12,
                        big.mark = ".",
                        decimal.mark = ","),
                color="teal"
               )
    })
                
    output$ultimadata <- renderValueBox({
      #  Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
      codigo_cidade <- get_city_code(input$city)   
      
      ultima_data <- 
        covid19 %>% 
        filter(is_last, city_ibge_code == codigo_cidade) %>% 
        select(last_available_date )
      
      valueBox("Data última coleta",
               color="teal",
               h2(format(ultima_data$last_available_date,"%d.%m.%y")),
               icon = icon("calendar")
      )
      
    })
    
    output$numeroReproEfetivo <- renderValueBox({
      codigo_cidade <- get_city_code(input$city)
      
      data_cidade <- 
        covid19 %>% 
        filter(city_ibge_code == codigo_cidade) %>% 
        select(new_confirmed, date, city_ibge_code) 
      
      r_eff <-get_growth_estimates(data_cidade)
      mean_reff <- mean(r_eff[[2]])
      
      valueBox(
        value = formatC(mean_reff,
                        digits = 3,
                        big.mark = ".",
                        decimal.mark = ","),
        subtitle = "Número de Reproducao Efetivo",
        icon = icon("registered"),
        color = colorize_info(mean_reff)
      )
    })

      
    output$mapa_exercicio_2 <- renderLeaflet({
   
      codigo_cidade   <- get_city_code(input$city) 
      
      geolocalizacao  <- read_csv("data/base_geolocalizacao_br.csv")
      
      geolocalizacao %>%   filter(codigo_ibge == codigo_cidade)
      
      cidade_geo <- geolocalizacao %>%   filter(codigo_ibge == codigo_cidade)     
      
      # Adicionar divisas das cidades, marcadores e informações no mapa.
      # Ação - Identificar padrões na pasta 'started_template/data/geoson'
      
      covid19 %>% 
        filter(is_last & 
              grepl(sprintf("^%s",substr(codigo_cidade, start = 1, stop = 2)), city_ibge_code))
      
      #view(covid19)
      
      # Combinar bases de dados da COVID19 e geolocalizacao (latitude e longitude)
      dados_pesquisa <- 
        covid19 %>% 
        filter(is_last & 
                 grepl(sprintf("^%s",substr(codigo_cidade, start = 1, stop = 2)), city_ibge_code)) %>% 
        left_join(., 
                  geolocalizacao, 
                  by = c("city_ibge_code" = "codigo_ibge"))

      
      caminho_shapefile <- sprintf("data/geoson/geojs-%s-mun.json",substr(codigo_cidade, start = 1, stop = 2))
      
      shapefile <- readOGR(caminho_shapefile)
      glimpse(shapefile)
      
      
      # Join colunas do banco de dados da covid19 com shapefiles
      shapefile@data$id <- as.numeric(shapefile@data$id)
      
      shapefile@data <- 
        left_join(shapefile@data,
                  dados_pesquisa, 
                  by = c("id" = "city_ibge_code"))
    
      # Adicionar cidade ao centro do mapa
      # ajustado do exericios apresentados na aula
      
      #pal <- colorNumeric("viridis", NULL)
      # https://data.library.virginia.edu/setting-up-color-palettes-in-r/
      pal <- colorNumeric("Set3", NULL)
      
      leaflet(shapefile) %>% 
        addProviderTiles(providers$Stamen.TonerLite) %>% 
        setView(lng = cidade_geo$longitude,
                lat = cidade_geo$latitude, 
                zoom = 11) %>% 
        addPolygons(stroke = FALSE, 
                    smoothFactor =  .6 ,  # alterado
                    fillOpacity = .3  ,   # alterado
                    fillColor = ~pal(log(new_confirmed + 1e-5)), # alterado  ~pal(log(new_confirmed + 1e-3))
                    label = ~paste0(name, 
                                    ": ",
                                    formatC(new_confirmed,
                                            big.mark = ","))) %>% 
        addAwesomeMarkers(~longitude, 
                          ~latitude, 
                          icon = makeAwesomeIcon(icon = "",
                                                 text = "", 
                                                 markerColor = "cadetblue"),
                          popup = 
                            sprintf("<h3>%s</h3>
                                   <p>Última atualização: %s</p>
                                   <p><b>Últimas 24h:</b></h5>
                                   <p>Casos<b>: %0.f</b></p>
                                   <p>Mortes<b>: %0.f</b></p>
                                   ", 
                                    shapefile@data$nome,
                                    format.Date(shapefile@data$date, "%d/%m/%Y"),
                                    shapefile@data$new_confirmed,
                                    shapefile@data$new_deaths
                            ),
                          clusterOptions = markerClusterOptions()   
        )
      
      
      
      
    }) 

    
   #  https://stackoverflow.com/questions/41235113/how-to-use-scrollx-in-shiny-dt-renderdatatable
    
    output$tabela <- renderDataTable({
   
         codigo_cidade   <- get_city_code(input$city) 
      
      geolocalizacao  <- read_csv("data/base_geolocalizacao_br.csv")
      
      geolocalizacao %>%   filter(codigo_ibge == codigo_cidade)
      
      cidade_geo <- geolocalizacao %>%   filter(codigo_ibge == codigo_cidade)     
      
      # Adicionar divisas das cidades, marcadores e informações no mapa.
      # Ação - Identificar padrões na pasta 'started_template/data/geoson'
      
      covid19 %>% 
        filter(is_last & 
                 grepl(sprintf("^%s",substr(codigo_cidade, start = 1, stop = 2)), city_ibge_code))
      
      
      # Combinar bases de dados da COVID19 e geolocalizacao (latitude e longitude)
      dados_pesquisa <- 
        covid19 %>% 
        filter(is_last & 
                 grepl(sprintf("^%s",substr(codigo_cidade, start = 1, stop = 2)), city_ibge_code)) %>% 
        left_join(., 
                  geolocalizacao, 
                  by = c("city_ibge_code" = "codigo_ibge"))

    },
    options = list(scrollX=TRUE, scrolly=TRUE, pageLength = 10)
    ) 
    
    
    output$efetivo <- renderPlotly({
      
      
      # Parâmetros usados para estimar Número de Reprodução
      # [Média e DP do intervalo serial, tempo estimado em dias para o contágio de uma
      # pessoa doente]
      mu <- 7.5
      sigma <- 3.4
      pred_n_days <- 10
      
      # Carregar banco de dados -------------------------------------------------
      # Baixar dados da API Brasil IO
      # OBS: Pode demorar um pouco dependendo de sua conexao
      
      if (!"covid19" %in% ls()){
        covid19_r_efetivo <- fetch_data_brasil_io(use_cached_data = TRUE)   
      }
      
      
      codigo_da_cidade <- get_city_code(input$city) 
      
      # Transformar dados no formato incidência
      data_incidence_function_data <- 
        covid19_r_efetivo %>%
        filter(city_ibge_code == codigo_da_cidade) %>% 
        select(date, new_deaths) %>% 
        uncount(new_deaths)
      
      data_incidence_object <- incidence(data_incidence_function_data$date)
      
      
      # Estimar R Efetivo usando o Pacote EpiEstim
      t_start <- seq(2, nrow(data_incidence_object$counts) - 8)
      t_end <- t_start + 8
      effective_r <- estimate_R(data_incidence_object, method = "parametric_si", 
                                config = make_config(list(mean_si = mu, std_si = sigma,
                                                          t_start = t_start, 
                                                          t_end = t_end)))
      
      # Criar gráfico plotly do R Efetivo
      r_effective_chart <- 
        effective_r$R %>% 
        ggplot(aes(x = data_incidence_object$dates[10:length(data_incidence_object$dates)], 
                   y = `Mean(R)`)) + 
        geom_ribbon(aes(ymin = `Mean(R)` - 2*`Std(R)`,
                        ymax = `Mean(R)` + 2*`Std(R)`), 
                    fill = "grey60") + 
        geom_line(size = 1.2) + 
        scale_y_sqrt() + 
        geom_hline(yintercept = 1, linetype = "dashed", color = "navy") +
        xlab("") + ylab("") +
        labs(title = "Estimativa Reprodução Efetiva") 
      
      ggplotly(r_effective_chart)
      
    })
    
    
    
    
    output$grafico1 <- renderPlotly({
      
    
      if (!"covid19" %in% ls()){
        covid19 <- fetch_data_brasil_io(use_cached_data = TRUE)   
      }
      
      
      codigo_da_cidade <- get_city_code(input$city) 
      
      # Transformar dados no formato incidência
      data_incidence_function_data <- 
        covid19 %>%
        filter(city_ibge_code == codigo_da_cidade) %>% 
        select(date, new_deaths)
      
             
      ggplot(data_incidence_function_data, aes(x =date,
                          y = new_deaths)) +
        geom_point() +
        geom_smooth(method = "lm")        

      
    })
    
    
    output$grafico2 <- renderPlotly({
      # http://sillasgonzaga.com/material/cdr/ggplot2.html
      
      if (!"covid19" %in% ls()){
        covid19 <- fetch_data_brasil_io(use_cached_data = TRUE)   
      }
      
      
      codigo_da_cidade <- get_city_code(input$city) 
      
      # Transformar dados no formato incidência
      data_incidence_function_data <- 
        covid19 %>%
        filter(city_ibge_code == codigo_da_cidade) %>% 
        select(date, new_confirmed)
      
      
      ggplot(data_incidence_function_data, aes(x =date,
                                               y = new_confirmed),
                                               color = indicador) +
        geom_line() +
        geom_smooth(method = "loess", se = FALSE)        
      
      
    })  
    
})
