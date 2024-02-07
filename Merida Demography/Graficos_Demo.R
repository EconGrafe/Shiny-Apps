# Call Libraries

pacman::p_load(tidyverse, janitor, stringi, DescTools, zoo, plotly, sf, readxl, 
               shiny, magrittr, RColorBrewer)

# Call and Wrangle Data

data_todos <- read_excel("Merida Data.xlsx", sheet = "municipio") %>% clean_names() %>%
  select(2:4, 9, 12:16, 19, 23:26) %>% 
  mutate_at(c('municipio'), ~stri_trans_general(casefold(., upper = T), 'Latin-ASCII'))

data_pob <- read_excel("Data 2.xlsx", sheet = 'Data') %>% clean_names() %>%
  mutate_at(c('hombre', 'mujer', 'total', 'pond'), ~replace_na(as.double(.), 0)) %>% 
  mutate_at(c('rango_edad'), ~as.character(.)) %>%
  select(-total) %>%
  gather(key = 'sexo', value = 'pob', hombre, mujer) %>%
  mutate(pop = pob / sum(pob) * 100) %>% select(-pob) %>%
  mutate_at(c('municipio'), ~stri_trans_general(casefold(., upper = T), 'Latin-ASCII'))

map_mun <- st_read('Municipios/Municipios_Venezuela.shp') %>% clean_names() %>%
  mutate_at(c(2, 3), ~stri_trans_general(., 'Latin-ASCII')) %>%
  mutate_at(c('municipio'), ~stri_trans_general(casefold(., upper = T), 'Latin-ASCII')) %>%
  filter(estado == 'Merida' & id != 187 & id != 188) 

data_tot <- read_excel("Merida Data.xlsx", sheet = "municipio") %>% clean_names() %>%
  select(2, 4) %>%  mutate_at(c(1), ~stri_trans_general(., 'Latin-ASCII')) %>%
  mutate_at(c('municipio'), ~stri_trans_general(casefold(., upper = T), 'Latin-ASCII'))

map <- inner_join(data_tot, map_mun, by = 'municipio') 

data_ten <- read_excel("Data 2.xlsx", sheet = "TEN") %>% clean_names() %>%
  mutate_at(c(1, 3), ~stri_trans_general(casefold(., upper= T), 'Latin-ASCII')) %>%
  filter(municipio == 'ESTADO MERIDA') 

data_tem <- read_excel("Data 2.xlsx", sheet = "Mort") %>% clean_names() %>%
  mutate_at(c(1, 3), ~stri_trans_general(casefold(., upper= T), 'Latin-ASCII')) %>%
  filter(municipio == 'ESTADO MERIDA') 

# Make Graph MAP

estate <- ggplot() + geom_sf(data = map, aes(fill = poblacion, geometry = geometry), color = NA) + theme_light() +
  labs(title = "Estado Merida", subtitle = 'Poblacion por Municipio', caption = "Fuente: Censo INE 2011", 
       x = "", y = "") + 
  theme(legend.title = element_blank(), 
        axis.text.y = element_blank(), 
        axis.text.x = element_blank(),
        plot.title = element_text(face = 'bold', hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5)) + 
  scale_fill_continuous(type = "viridis")

estate <- ggplotly(estate)

# Make Graph PIRAMID

et1 <- c('0 a 4',    '5 a 9',    '10 a 14',  '15 a 19',  '20 a 24',  '25 a 29',  '30 a 34',  '35 a 39', 
         '40 a 44',  '45 a 49',  '50 a 54',  '55 a 59',  '60 a 64',  '65 a 69',  '70 a 74',  '75 a 79', 
         '80 a 84',  '85 a 89',  '90 a 94',  '95 o mas')
  
# piramid <- ggplot(data = filter(data_pob, municipio == 'Arzobispo Chacón'), aes(x = pond, fill = sexo,
#                                         y = if_else(sexo == 'hombre', -pop, pop))) +
#    geom_bar(stat = 'identity') + coord_flip() + 
#    labs(title = 'Piramide Poblacional', caption = 'Fuente = Censo INE (2011)', 
#         x = '', y = '') +
#    scale_x_continuous(breaks = 1:20, label = et1) + theme_light() + 
#    theme(plot.title = element_text(face = 'bold', hjust = 0.5)) + 
#    scale_fill_manual(values = c("blue","violet"))
# 
# piramid <- ggplotly(piramid)

# Make Histogram

et <- data_ten %>% select(rango_edad, municipio) %>% filter(municipio == 'ESTADO MERIDA') %>% select(1)

et <- et[[1]]

ten <- ggplot(data = data_ten, aes(x = pond, y = ten)) +
  geom_col(col = 'blue', fill = NA) + 
  geom_point(size = 2, col = 'red') + 
  geom_line(col = 'red') +
  labs(title = 'Tasa Especifica de Natalidad', caption = 'Fuente = Censo INE (2011)', 
       x = '', y = '') +
  scale_x_continuous(breaks = 1:9, label = et) + theme_light() +
  theme(plot.title = element_text(face = 'bold', hjust = 0.5))

ten <- ggplotly(ten)

etm <- data_tem %>% select(rango_edad, municipio) %>% filter(municipio == 'ESTADO MERIDA') %>% select(1)

etm <- etm[[1]]

tem <- ggplot(data = data_tem, aes(x = pond, y = tem)) +
  geom_col(col = 'blue', fill = NA) + 
  geom_point(size = 2, col = 'red') + 
  geom_line(col = 'red') +
  labs(title = 'Tasa Especifica de Mortalidad', caption = 'Fuente = Censo INE (2011)', 
       x = '', y = '') +
  scale_x_continuous(breaks = 1:9, label = etm) + theme_light() +
  theme(plot.title = element_text(face = 'bold', hjust = 0.5))

tem <- ggplotly(tem)
  
# grup <- plot_ly(sliderValue(), labels = ~grupo, values = ~n, type = 'pie') %>% 
#   layout(title = 'United States Personal Expenditures by Categories in 1960',
#                       xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#                       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

data_tgf <- read_excel("Data 2.xlsx", sheet = "TEN") %>% clean_names() %>%
  mutate_at(c(1, 3), ~stri_trans_general(casefold(., upper= T), 'Latin-ASCII')) %>%
  select(municipio, tgf, ea) %>%
  dplyr::group_by(municipio) %>% dplyr::summarise(tgf = mean(tgf), ea = mean(ea))

tgf_plot <- ggplot(data = data_tgf, aes(x = reorder(municipio, tgf), y = tgf)) + 
  geom_col(col = 'blue', fill = if_else(data_tgf$municipio == 'ESTADO MERIDA', 'blue',NA)) + coord_flip() +
  labs(title = 'Tasa Global de Fecundidad', caption = 'Fuente = Censo INE (2011)', 
       x = '', y = '') + theme_light() +
  theme(plot.title = element_text(face = 'bold', hjust = 0.5))
tgf_plot <- ggplotly(tgf_plot)

ea_plot <- ggplot(data = data_tgf, aes(x = reorder(municipio, ea), y = ea)) + 
  geom_col(col = 'blue', fill = if_else(data_tgf$municipio == 'ESTADO MERIDA', 'blue',NA)) + coord_flip() +
  labs(title = 'Embarazo Adolescente', caption = 'Fuente = Censo INE (2011)', 
       x = '', y = '') + theme_light() +
  theme(plot.title = element_text(face = 'bold', hjust = 0.5))
ea_plot <- ggplotly(ea_plot)

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

ggplot(data = data_mm, aes(x = reorder(municipio, cant), y = cant, fill = tipo)) + 
  geom_bar(stat = 'identity') + coord_flip() +
  labs(title = 'Clasificacion de Defunciones', caption = 'Fuente = Censo INE (2011)', 
       x = '', y = '') + theme_light() +
  theme(plot.title = element_text(face = 'bold', hjust = 0.5), 
        legend.position = 'bottom') + scale_fill_brewer() + 
  guides(fill = guide_legend(title = 'Grado de Prevencion'))

ggplot(data = data_mi, aes(x = reorder(municipio, mi), y = mi)) + 
  geom_col(col = 'blue', fill = if_else(data_mi$municipio == 'ESTADO MERIDA', 'blue',NA)) + coord_flip() +
  labs(title = 'Mortalidad Infantil', caption = 'Fuente = Censo INE (2011)', 
       x = '', y = '') + theme_light() +
  theme(plot.title = element_text(face = 'bold', hjust = 0.5))

data_dd <- read_excel("Merida Data.xlsx", sheet = "municipio") %>% clean_names() %>%
  mutate_at(c('municipio'), ~stri_trans_general(casefold(., upper = T), 'Latin-ASCII')) %>%
  select(2:4, 9, 12:16, 17, 19:29) %>% mutate_at(c(5:9), ~round(.*100, 2))

data_dyn <- data_dd %>% select(1, 10, 11, 13:15, 19:21) 

data_dd %<>% select(-natalidad)

data_educ <- read_excel("Merida Data.xlsx", sheet = "matricula") %>% clean_names() %>%
  select(tipo, cantidad, proporcion) %>% na.omit()

data_mat <- read_excel("Merida Data.xlsx", sheet = "matricula") %>% clean_names() %>%
  select(grado, matricula, pond) %>% na.omit()

ggplot(data = data_educ, aes(x = reorder(tipo, cantidad), y = cantidad)) + 
  geom_col(col = 'blue', fill = NA) + coord_flip() +
  geom_text(aes(label = paste0(round(proporcion * 100, 2), '%')), hjust = -0.2) +
  labs(title = 'Planteles Educativos', caption = 'Fuente = Censo INE (2011)', 
       x = '', y = '') + theme_light() +
  theme(plot.title = element_text(face = 'bold', hjust = 0.5)) + scale_y_continuous(limits = c(0, 700))

ggplot(data = data_mat, aes(x = pond, y = matricula)) + 
  geom_line(col = 'blue', lwd = 1) + geom_point(aes(x = pond, y = matricula), col = 'blue') +
  geom_text(aes(label = matricula), vjust = -1) +
  labs(title = 'Matricula por Grado', caption = 'Fuente = Censo INE (2011)', 
       x = '', y = '') + theme_light() +
  scale_y_continuous(limits = c(1000, 18000)) +
  scale_x_continuous(breaks = 1:12, labels = data_mat$grado) +
  theme(plot.title = element_text(face = 'bold', hjust = 0.5), 
        axis.text.x = element_text(angle = 90))
  
data_emp1 <- tibble(
  profesion = c('Profesionales', 'Servicios Turisticos', 'Vendedores', 
                'Agricultores', 'Canteros', 'Otros', 'Gerentes y Directivos'), 
  per = c(21.58, 17.6, 17.33, 17.04, 15.35, 7.66, 3.45))

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

data_emp2 <- tibble(
  sector = c('No Mercado', 'Turismo y Comercio', 'Otros', 
                'Agricultura', 'Manufactura'), 
  par = c(30.35, 24.09, 19.22, 17.62, 8.73))

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

emp_rem <- tibble(
  sector = c('Sector Primario', 'Sector Secundario', 'Sector Terciario'), 
  rem = c(1271, 1719, 1612))

ggplot(data = emp_rem, aes(x = sector, y = rem)) + 
  geom_col(fill = NA, col = 'blue') +
  geom_text(aes(label = rem), vjust = -1) +
  geom_hline(yintercept = 1370, linetype = 2, color = 'red') +
  labs(title = 'Remuneracion vs CAN', caption = 'Fuente = Censo INE (2011)', 
       x = '', y = '') + theme_light() +
  theme(plot.title = element_text(face = 'bold', hjust = 0.5)) +
  scale_y_continuous(limits = c(0, 2000))
