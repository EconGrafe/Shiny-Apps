# Call Libraries

pacman::p_load(shiny, shinydashboard, tidyverse, janitor, stringi, DescTools, zoo, plotly, sf, readxl, magrittr)

# Call Graph

setwd('Shiny-Apps/Merida Demography')

source('Graficos_Demo.R')

# Make UI

ui <- dashboardPage(title = 'Merida', skin = 'blue', 
                    dashboardHeader(title = 'Estado Merida: Datos Demograficos', 
                                    dropdownMenu(type = 'messages', 
                                                 messageItem(from = 'Zadquiel', 
                                                             'Hola'), 
                                                 messageItem(from = 'Zadquiel', 
                                                             'En el presente tablero encontraras datos demograficos y socioeconomicos del estado Merida a 2011')
                                                 ), 
                                    dropdownMenu(type = 'notifications', 
                                                 notificationItem(text = 'El tablero esta listo, navega por el')), 
                                    dropdownMenu(type = 'tasks', 
                                                 taskItem(value = 100, 
                                                          text = 'Advanced', 
                                                          color = 'green'))), 
                    dashboardSidebar(sidebarSearchForm('searchtext', 'buttonSearch', 'Busqueda', 
                                                       icon = shiny::icon('terminal')), 
                                     sidebarMenu(id = 'sidebarID', 
                                                 menuItem('Principal', 
                                                          tabName = 'Principal'), 
                                                 menuItem('Dinamica de Vida',
                                                          menuSubItem('Natalidad', tabName = 'Natalidad'),
                                                          menuSubItem('Mortalidad', tabName = 'Mortalidad')), 
                                                 menuItem('Socioeconomico', 
                                                          menuSubItem('Educacion', tabName = 'Educacion'), 
                                                          menuSubItem('Mercado Laboral', tabName = 'Mercado_Laboral'))), 
                                     selectInput(inputId = 'municipio', 
                                                 label = 'Municipio: ', 
                                                 choices = data_todos$municipio)), 
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = 'Principal',
                                fluidRow(box(valueBoxOutput('ext_ter'), 
                                             valueBoxOutput('q_mun'), 
                                             valueBoxOutput('q_par')), 
                                         box(valueBoxOutput('nro_hab'), 
                                             valueBoxOutput('pea'), 
                                             valueBoxOutput('unemp'))),
                                fluidRow(
                                  box(title = 'Mapa del Estado', plotlyOutput(outputId = 'Estado'), 
                                      solidHeader = T, status = 'primary'),
                                  box(title = 'Piramide Poblacional', 
                                      plotlyOutput(outputId = 'Piramide'), solidHeader = T, 
                                      status = 'primary')),
                                fluidRow(
                                  box(title = 'Estadisticas del Estado', DT::dataTableOutput('datos'), width = 12, 
                                      solidHeader = T, status = 'primary')
                                  )
                                ),
                        tabItem(tabName = 'Natalidad', 
                                fluidRow(box(valueBoxOutput('nat_n', width = 6), 
                                             valueBoxOutput('ea_n', width = 6)), 
                                         box(valueBoxOutput('tbn_n', width = 6), 
                                             valueBoxOutput('tgf_n', width = 6))),
                                fluidRow(
                                  box(title = 'Tasa Global de Fecundidad', plotlyOutput(outputId = 'TGF'), 
                                      solidHeader = T, status = 'primary'),
                                  box(title = 'Embarazo Adolescente', 
                                      plotlyOutput(outputId = 'EA'), solidHeader = T, 
                                      status = 'primary')),
                                fluidRow(
                                  box(title = 'Tasa Especifica de Natalidad', plotlyOutput(outputId = 'TEN'), width = 12, 
                                      solidHeader = T, status = 'primary')
                                )),
                        tabItem(tabName = 'Mortalidad', 
                                fluidRow(box(valueBoxOutput('def_n', width = 6), 
                                             valueBoxOutput('mi_n', width = 6)), 
                                         box(valueBoxOutput('tbm_n', width = 6), 
                                             valueBoxOutput('ap_n', width = 6))),
                                fluidRow(
                                  box(title = 'Mortalidad por Causa', plotlyOutput('mc'), solidHeader = T, status = 'primary'), 
                                  box(title = 'Mortalidad Infantil', plotlyOutput('mi'), solidHeader = T, status = 'primary')
                                ),
                                fluidRow(
                                  box(title = 'Tasa Específica de Mortalidad', 
                                      plotlyOutput(outputId = 'TEM'), width = 12, solidHeader = T, status = 'primary')
                                )
                                ), 
                        tabItem(tabName = 'Educacion', 
                                fluidRow(
                                  box(title = 'Matricula Estudiantil por Grado', 
                                      plotlyOutput(outputId = 'matricula_plot'), width = 12, solidHeader = T, status = 'primary')
                                ),
                                fluidRow(
                                  column(
                                    width = 6, 
                                    box(title = 'Planteles Educativos por Tipo', 
                                        plotlyOutput(outputId = 'plantel_plot'), width = 12, solidHeader = T, status = 'primary')
                                    ), 
                                  box(title = 'Datos sobre Educacion',
                                      valueBoxOutput('plantel_n', width = 6),
                                      valueBoxOutput('pob_est', width = 6),
                                      valueBoxOutput('rep_n', width = 6),
                                      valueBoxOutput('rep_tot', width = 6),
                                      valueBoxOutput('rep_prim', width = 6),
                                      valueBoxOutput('rep_sec', width = 6), solidHeader = T, status = 'primary')
                                  )
                                ),
                        tabItem(tabName = 'Mercado_Laboral', 
                                fluidRow(
                                  box(title = 'Empleo por Profesion', 
                                      plotlyOutput(outputId = 'profesion_plot'), solidHeader = T, status = 'primary'), 
                                  box(title = 'Mercado de Trabajo', 
                                      plotlyOutput(outputId = 'work_plot'), solidHeader = T, status = 'primary')), 
                                fluidRow(
                                  box(title = 'Ingresos vs Canasta Alimentaria Normativa', 
                                      plotlyOutput(outputId = 'can_plot'), width = 12, solidHeader = T, status = 'primary')
                                )
                                )
                        )
                      )
                    )




server <- function(input, output) {
  
  data0 <- reactive({
    data_comp <- read_excel('Merida Data.xlsx', sheet = 'municipio') %>% clean_names() %>%
      select(2:7, 9:11) %>%  mutate_at(c(1), ~stri_trans_general(., 'Latin-ASCII')) %>%
      mutate_at(c('municipio'), ~stri_trans_general(casefold(., upper = T), 'Latin-ASCII'))
  })
  
  data1 <- reactive({
    data_ten <- read_excel('Data 2.xlsx', sheet = 'TEN') %>% clean_names() %>%
      mutate_at(c(1, 3), ~stri_trans_general(casefold(., upper= T), 'Latin-ASCII'))
    data_ten[data_ten$municipio == input$municipio, ]

  })
  
  data2 <- reactive({
    data_pob <- read_excel('Data 2.xlsx', sheet = 'Data') %>% clean_names() %>%
      mutate_at(c('hombre', 'mujer', 'total', 'pond'), ~replace_na(as.double(.), 0)) %>% 
      mutate_at(c('rango_edad'), ~as.character(.)) %>%
      select(-total) %>%
      gather(key = 'sexo', value = 'pob', hombre, mujer) %>%
      mutate(pop = pob / sum(pob) * 100) %>% select(-pob) %>%
      mutate_at(c('municipio'), ~stri_trans_general(casefold(., upper = T), 'Latin-ASCII'))
    data_pob[data_pob$municipio == input$municipio, ]
  })
  
  data3 <- reactive({
    data_tem <- read_excel("Data 2.xlsx", sheet = "Mort") %>% clean_names() %>%
      mutate_at(c(1, 3), ~stri_trans_general(casefold(., upper= T), 'Latin-ASCII')) 
    data_tem[data_tem$municipio == input$municipio, ]
  })
  
  data_4educ <- read_excel("Merida Data.xlsx", sheet = "municipio") %>% clean_names() %>%
    select(2, 30:33) %>% mutate_at(c(4), ~round(., 2)) %>%
    mutate_at(c('municipio'), ~stri_trans_general(casefold(., upper = T), 'Latin-ASCII'))
  
  data_tgf <- read_excel("Data 2.xlsx", sheet = "TEN") %>% clean_names() %>%
      mutate_at(c(1, 3), ~stri_trans_general(casefold(., upper= T), 'Latin-ASCII')) %>%
      select(municipio, tgf, ea) %>%
      dplyr::group_by(municipio) %>% dplyr::summarise(tgf = mean(tgf), ea = mean(ea)) 
  
  data_dd <- read_excel("Merida Data.xlsx", sheet = "municipio") %>% clean_names() %>%
    mutate_at(c('municipio'), ~stri_trans_general(casefold(., upper = T), 'Latin-ASCII')) %>%
    select(2:4, 9, 12:16, 17, 19:29) %>% mutate_at(c(5:9), ~round(.*100, 2))
  
  data_dyn <- data_dd %>% select(1, 10, 11, 13:15, 19:21) %>% mutate(ea = ea * 100, ap = ap / mortalidad * 100)
  
  data_dd %<>% select(-natalidad)
    
  data_todos <- data_dd %>% select(1:11) %>%
    dplyr::rename(Municipio = 1, Edad_Mediana = 2, Población = 3, Rel_Masc = 4, 
                  Pob_Masc = 5, Pob_Fem = 6, Pob_Menor = 7, PEA = 8, Pob_Retiro = 9, 
                  TBN = 10, TBM = 11) %>% mutate_at(c(2, 10:11), ~round(., 2)) %>%
    filter(Municipio != 'ESTADO MERIDA')
  
  data_mm <- read_excel("Data 2.xlsx", sheet = "Mort Inf") %>% clean_names() %>%
    mutate_at(c(1, 3), ~stri_trans_general(casefold(., upper= T), 'Latin-ASCII')) %>% select(3, 9:12) %>%
    gather(key = 'tipo', value = 'cant', 2:5) %>% filter(municipio != 'ESTADO MERIDA') %>%
    mutate(tipo = case_when(
      tipo == 'altamente_prevenibles' ~ 'Alto',
      tipo == 'moderadamente_prevenibles' ~ 'Moderado',
      tipo == 'poco_prevenibles' ~ 'Poco', T ~ tipo
    ))
  
  data_mi <- read_excel("Data 2.xlsx", sheet = "Mort Inf") %>% clean_names() %>%
    mutate_at(c(1, 3), ~stri_trans_general(casefold(., upper= T), 'Latin-ASCII')) %>% select(municipio, mi)
  
  data_educ <- read_excel("Merida Data.xlsx", sheet = "matricula") %>% clean_names() %>%
    select(tipo, cantidad, proporcion) %>% na.omit()
  
  data_mat <- read_excel("Merida Data.xlsx", sheet = "matricula") %>% clean_names() %>%
    select(grado, matricula, pond) %>% na.omit()
  
  data_emp1 <- tibble(
    profesion = c('Profesionales', 'Servicios Turisticos', 'Vendedores', 
                  'Agricultores', 'Canteros', 'Otros', 'Gerentes y Directivos'), 
    per = c(21.58, 17.6, 17.33, 17.04, 15.35, 7.66, 3.45))
  
  data_emp2 <- tibble(
    sector = c('No Mercado', 'Turismo y Comercio', 'Otros', 
               'Agricultura', 'Manufactura'), 
    par = c(30.35, 24.09, 19.22, 17.62, 8.73))
  
  emp_rem <- tibble(
    sector = c('Sector Primario', 'Sector Secundario', 'Sector Terciario'), 
    rem = c(1271, 1719, 1612))
  
  data_rep_educ <- read_excel("Repitientes Educacion.xlsx") %>% clean_names() %>%
    mutate_at(c(2:4), ~round(.*100, 2)) %>% 
    mutate_at(c(1), ~stri_trans_general(casefold(., upper = T), 'Latin-ASCII'))
  
  et1 <- c('0 a 4',    '5 a 9',    '10 a 14',  '15 a 19',  '20 a 24',  '25 a 29',  '30 a 34',  '35 a 39', 
           '40 a 44',  '45 a 49',  '50 a 54',  '55 a 59',  '60 a 64',  '65 a 69',  '70 a 74',  '75 a 79', 
           '80 a 84',  '85 a 89',  '90 a 94',  '95 o mas')
  
  dext <- reactive({
    data_dd[data_dd$municipio == input$municipio, 17][[1]]
    })
  dmun <- reactive({
    data_dd[data_dd$municipio == input$municipio, 15][[1]]
  })
  dpar <- reactive({
    data_dd[data_dd$municipio == input$municipio, 16][[1]]
  })
  dpob <- reactive({
    data_dd[data_dd$municipio == input$municipio, 3][[1]]
  })
  dpea <- reactive({
    data_dd[data_dd$municipio == input$municipio, 8][[1]]
  })
  dnat_n <- reactive({
    data_dyn[data_dyn$municipio == input$municipio, 2][[1]]
  })
  dea_n <- reactive({
    data_dyn[data_dyn$municipio == input$municipio, 5][[1]]
  })
  dtbn_n <- reactive({
    data_dyn[data_dyn$municipio == input$municipio, 3][[1]]
  })
  dtgf_n <- reactive({
    data_dyn[data_dyn$municipio == input$municipio, 4][[1]]
  })
  ddef_n <- reactive({
    data_dyn[data_dyn$municipio == input$municipio, 7][[1]]
  })
  dmi_n <- reactive({
    data_dyn[data_dyn$municipio == input$municipio, 8][[1]]
  })
  dtbm_n <- reactive({
    data_dyn[data_dyn$municipio == input$municipio, 6][[1]]
  })
  dap_n <- reactive({
    data_dyn[data_dyn$municipio == input$municipio, 9][[1]]
  })
  
  data_plantel <- reactive({
    data_4educ[data_4educ$municipio == input$municipio, 2][[1]]
  })
  
  data_repitiente <- reactive({
    data_4educ[data_4educ$municipio == input$municipio, 3][[1]]
  })
  
  data_rep_x_plant <- reactive({
    data_4educ[data_4educ$municipio == input$municipio, 4][[1]]
  })
  
  data_pob_est <- reactive({
    data_4educ[data_4educ$municipio == input$municipio, 5][[1]]
  })
  
  data_rep_prim <- reactive({
    data_rep_educ[data_rep_educ$municipio == input$municipio, 2][[1]]
  })
  
  data_rep_sec <- reactive({
    data_rep_educ[data_rep_educ$municipio == input$municipio, 3][[1]]
  })
  
  data_rep_tot <- reactive({
    data_rep_educ[data_rep_educ$municipio == input$municipio, 4][[1]]
  })
  
  output$ext_ter <- renderValueBox({
    valueBox(value = round(as.numeric(dext()), 0), subtitle = 'Extension Territorial (Km2)', 
             color = 'blue')
  })
  
  output$q_mun <- renderValueBox({
    valueBox(value = round(as.numeric(dmun()), 0), subtitle = 'Municipios', 
             color = 'blue')
  })
  
  output$q_par <- renderValueBox({
    valueBox(value = round(as.numeric(dpar()), 0), subtitle = 'Parroquias', 
             color = 'blue')
  })
  
  output$nro_hab <- renderValueBox({
    valueBox(value = round(as.numeric(dpob()), 0), subtitle = 'Habitantes', 
             color = 'blue')
  })
  
  output$pea <- renderValueBox({
    valueBox(value = paste0(round(as.numeric(dpea()), 2), '%'), subtitle = 'PEA', 
             color = 'blue')
  })
  
  output$unemp <- renderValueBox({
    valueBox(value = '34.13%', subtitle = 'Desempleo (Estado)', 
             color = 'blue')
  })
  
  output$Estado <- renderPlotly(
    {
      print(estate)
    }
  )
  output$datos <- DT::renderDataTable(
    {
    DT::datatable(data_todos)
    }
  )
  output$Piramide <- renderPlotly(
    {
      piramid <- ggplot(data = data2(), aes(x = pond, fill = sexo,
                                             y = if_else(sexo == 'hombre', -pop, pop))) +
        geom_bar(stat = 'identity') + coord_flip() + 
        labs(title = 'Piramide Poblacional', caption = 'Fuente = Censo INE (2011)', 
             x = '', y = '') +
        scale_x_continuous(breaks = 1:20, label = et1) + theme_light() + 
        theme(plot.title = element_text(face = 'bold', hjust = 0.5)) + 
        scale_fill_manual(values = c('blue','violet'))
      
      piramid <- ggplotly(piramid)
      print(piramid)
    }
  )
  output$TEN <- renderPlotly(
    {
      tenk <- ggplot(data = data1(), aes(x = pond, y = ten)) +
        geom_col(col = 'blue', fill = NA) + 
        geom_point(size = 2, col = 'red') + 
        geom_line(col = 'red') +
        labs(title = 'Tasa Especifica de Natalidad', caption = 'Fuente = Censo INE (2011)', 
             x = '', y = '') +
        scale_x_continuous(breaks = 1:9, label = et) + theme_light() +
        theme(plot.title = element_text(face = 'bold', hjust = 0.5))
      
      tenk <- ggplotly(tenk)
      print(tenk)
    }
  )
  output$TEM <- renderPlotly(
    {
      tem <- ggplot(data = data3(), aes(x = pond, y = tem)) +
        geom_col(col = 'blue', fill = NA) + 
        geom_point(size = 2, col = 'red') + 
        geom_line(col = 'red') +
        labs(title = 'Tasa Especifica de Mortalidad', caption = 'Fuente = Censo INE (2011)', 
             x = '', y = '') +
        scale_x_continuous(breaks = 1:9, label = etm) + theme_light() +
        theme(plot.title = element_text(face = 'bold', hjust = 0.5))
      
      tem <- ggplotly(tem)
      print(tem)
    }
  )
  output$TGF <- renderPlotly(
    {
      tgf_plot <- ggplot(data = data_tgf, aes(x = reorder(municipio, tgf), y = tgf)) + 
        geom_col(col = 'blue', fill = if_else(data_tgf$municipio == 'ESTADO MERIDA', 'blue',NA)) + coord_flip() +
        labs(title = 'Tasa Global de Fecundidad', caption = 'Fuente = Censo INE (2011)', 
             x = '', y = '') + theme_light() +
        theme(plot.title = element_text(face = 'bold', hjust = 0.5))
      
      tgf_plot <- ggplotly(tgf_plot)
      print(tgf_plot)
    }
  )
  output$EA <- renderPlotly(
    {
      ea_plot <- ggplot(data = data_tgf, aes(x = reorder(municipio, ea), y = ea)) + 
        geom_col(col = 'blue', fill = if_else(data_tgf$municipio == 'ESTADO MERIDA', 'blue',NA)) + coord_flip() +
        labs(title = 'Embarazo Adolescente', caption = 'Fuente = Censo INE (2011)', 
             x = '', y = '') + theme_light() +
        theme(plot.title = element_text(face = 'bold', hjust = 0.5))
      
      ea_plot <- ggplotly(ea_plot)
      print(ea_plot)
    }
  )
  output$mc <- renderPlotly({
    mcplot <- ggplot(data = data_mm, aes(x = reorder(municipio, cant), y = cant, fill = tipo)) + 
      geom_bar(stat = 'identity') + coord_flip() +
      labs(title = 'Clasificacion de Defunciones', caption = 'Fuente = Censo INE (2011)', 
           x = '', y = '') + theme_light() +
      theme(plot.title = element_text(face = 'bold', hjust = 0.5), 
            legend.position = 'bottom') + scale_fill_brewer() + 
      guides(fill = guide_legend(title = 'Grado de Prevencion'))
    mcplot <- ggplotly(mcplot)
    print(mcplot)
  })
  output$mi <- renderPlotly({
    miplot <- ggplot(data = data_mi, aes(x = reorder(municipio, mi), y = mi)) + 
      geom_col(col = 'blue', fill = if_else(data_mi$municipio == 'ESTADO MERIDA', 'blue',NA)) + coord_flip() +
      labs(title = 'Mortalidad Infantil', caption = 'Fuente = Censo INE (2011)', 
           x = '', y = '') + theme_light() +
      theme(plot.title = element_text(face = 'bold', hjust = 0.5))
    miplot <- ggplotly(miplot)
    print(miplot)
  })
  
  output$nat_n <- renderValueBox({
    valueBox(value = round(as.numeric(dnat_n()), 0), subtitle = 'Nacimientos', 
             color = 'blue')
  })
  output$ea_n <- renderValueBox({
    valueBox(value = paste0(round(as.numeric(dea_n()), 2), '%'), subtitle = 'Embarazo Adolescente', 
             color = 'blue')
  })
  output$tbn_n <- renderValueBox({
    valueBox(value = round(as.numeric(dtbn_n()), 2), subtitle = 'Tasa Bruta de Natalidad', 
             color = 'blue')
  })
  output$tgf_n <- renderValueBox({
    valueBox(value = round(as.numeric(dtgf_n()), 2), subtitle = 'Tasa Global de Fecundidad', 
             color = 'blue')
  })
  output$def_n <- renderValueBox({
    valueBox(value = round(as.numeric(ddef_n()), 0), subtitle = 'Defunciones Totales', 
             color = 'blue')
  })
  output$mi_n <- renderValueBox({
    valueBox(value = round(as.numeric(dmi_n()), 2), subtitle = 'Mortalidad Infantil', 
             color = 'blue')
  })
  output$tbm_n <- renderValueBox({
    valueBox(value = round(as.numeric(dtbm_n()), 2), subtitle = 'Tasa Bruta de Mortalidad', 
             color = 'blue')
  })
  output$ap_n <- renderValueBox({
    valueBox(value = paste0(round(as.numeric(dap_n()), 2), '%'), subtitle = 'Muertes Altamente Prevenibles', 
             color = 'blue')
  })
  
  output$matricula_plot <- renderPlotly({
    matriculaplot <- ggplot(data = data_mat, aes(x = pond, y = matricula)) + 
      geom_line(col = 'blue', lwd = 1) + geom_point(aes(x = pond, y = matricula), col = 'blue') +
      geom_text(aes(label = matricula), vjust = -1) +
      labs(title = 'Matricula por Grado', caption = 'Fuente = Censo INE (2011)', 
           x = '', y = '') + theme_light() +
      scale_y_continuous(limits = c(1000, 18000)) +
      scale_x_continuous(breaks = 1:12, labels = data_mat$grado) +
      theme(plot.title = element_text(face = 'bold', hjust = 0.5), 
            axis.text.x = element_text(angle = 90))
    matriculaplot <- ggplotly(matriculaplot)
    print(matriculaplot)
  })
  
  output$plantel_plot <- renderPlotly({
    plantelplot <- ggplot(data = data_educ, aes(x = reorder(tipo, cantidad), y = cantidad)) + 
      geom_col(col = 'blue', fill = NA) + coord_flip() +
      geom_text(aes(label = paste0(round(proporcion * 100, 2), '%')), hjust = -0.2) +
      labs(title = 'Planteles Educativos', caption = 'Fuente = Censo INE (2011)', 
           x = '', y = '') + theme_light() +
      theme(plot.title = element_text(face = 'bold', hjust = 0.5)) + scale_y_continuous(limits = c(0, 700))
    
    plantelplot <- ggplotly(plantelplot)
    print(plantelplot)
  })
  
  output$plantel_n <- renderValueBox({
    valueBox(value = data_plantel(), subtitle = 'Nro de Planteles', 
             color = 'blue')
  })
  
  output$rep_n <- renderValueBox({
    valueBox(value = data_repitiente(), subtitle = 'Nro de Repitientes', 
             color = 'blue')
  })
  
  output$rep_pla <- renderValueBox({
    valueBox(value = data_rep_x_plant(), subtitle = 'Razon Repitientes por Plantel', 
             color = 'blue')
  })
  
  output$pob_est <- renderValueBox({
    valueBox(value = data_pob_est(), subtitle = 'Jovenes Edad Estudiantil', 
             color = 'blue')
  })
  
  output$profesion_plot <- renderPlotly({
    colou <- c('#ACD4EC', '#9FCAE6', '#7CADD2', '#5F90BB', '#4D7EAB', '#A5CFE9', '#2A5783')
    
    emp_plot <- plot_ly(data_emp1, labels = ~profesion, values = ~per, type = 'pie', 
                        textposition = 'inside',
                        textinfo = 'label+percent',
                        insidetextfont = list(color = '#FFFFFF'),
                        hoverinfo = 'text', 
                        text = ~paste0(per, '%'), 
                        marker = list(colors = colou, 
                                      line = list(color = 'black'))) %>%
      layout(title = 'Empleos por Profesion',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    emp_plot <- ggplotly(emp_plot)
    print(emp_plot)
  })
  
  output$work_plot <- renderPlotly({
    colou <- c('#ACD4EC', '#9FCAE6', '#7CADD2', '#5F90BB', '#4D7EAB', '#A5CFE9', '#2A5783')
    
    emp2_plot <- plot_ly(data_emp2, labels = ~sector, values = ~par, 
                         textposition = 'inside',
                         textinfo = 'label+percent',
                         insidetextfont = list(color = '#FFFFFF'),
                         hoverinfo = 'text', 
                         text = ~paste0(par, '%'), 
                         marker = list(colors = colou, 
                                       line = list(color = 'black'))) %>%
      add_pie(hole = 0.6) %>%
      layout(title = 'Empleos por Sector Economico',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    emp2_plot <- ggplotly(emp2_plot)
    print(emp2_plot)
  })
  
  output$can_plot <- renderPlotly({
    rem_plot <- ggplot(data = emp_rem, aes(x = sector, y = rem)) + 
      geom_col(fill = NA, col = 'blue') +
      geom_text(aes(label = rem), vjust = -1) +
      geom_hline(yintercept = 1370, linetype = 2, color = 'red') +
      labs(title = 'Remuneracion vs CAN', caption = 'Fuente = Censo INE (2011)', 
           x = '', y = '') + theme_light() +
      theme(plot.title = element_text(face = 'bold', hjust = 0.5)) +
      scale_y_continuous(limits = c(0, 2000))
    rem_plot <- ggplotly(rem_plot)
    print(rem_plot)
  })
  
  output$rep_prim <- renderValueBox({
    valueBox(value = data_rep_prim(), subtitle = '% Repitientes Primaria',
             color = 'blue')
  })
  
  output$rep_sec <- renderValueBox({
    valueBox(value = data_rep_sec(), subtitle = '% Repitientes Secundaria',
             color = 'blue')
  })
  
  output$rep_tot <- renderValueBox({
    valueBox(value = data_rep_tot(), subtitle = '% Repitientes Total',
             color = 'blue')
  })
  

}

shinyApp(ui, server)
