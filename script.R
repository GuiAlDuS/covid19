library(data.table)
library(countrycode)
library(lubridate)
library(stringr)

Sys.setlocale(locale="es_ES.UTF-8")
fecha_hoy <- as.character(Sys.Date())
#fecha_hoy <- "2020-04-03"
fecha_hoy_mod <- paste(month(fecha_hoy), 
                       day(fecha_hoy), 
                       str_extract(year(fecha_hoy), ".{2}$"), 
                       sep = "/")

covid19_contagios <- fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", header=T)[
  , tipo := "contagios"
]

covid19_recuperados <- fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv", header=T)[
  , tipo := "recuperados"
]

covid19_decesos <- fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", header=T)[
  , tipo := "decesos"
]

covid19 <- rbindlist(list(covid19_contagios, covid19_decesos, covid19_recuperados))[
  , `:=`(cod_pais = countrycode(`Country/Region`, 
                                origin = "country.name", destination = "iso3c"),
         pais = countrycode(`Country/Region`, 
                            origin = "country.name", destination = "un.name.es"),
         continente = countrycode(`Country/Region`,
                                  origin = "country.name", destination = "continent"),
         region = countrycode(`Country/Region`,
                              origin = "country.name", destination = "region"))
][, `:=`(`Province/State` = NULL,
         `Country/Region` = NULL,
         Lat = NULL,
         Long = NULL
         )]

setcolorder(covid19, c("pais", "cod_pais", "continente", "region", "tipo"))

covid19_tidy <- melt(covid19,
                     id.vars = c("pais", "cod_pais", "continente", "region", "tipo"),
                     variable.name = "fecha",
                     value.name = "casos")[
                       , fecha := mdy(fecha)
                     ]

cntry_list <- unique(covid19_tidy$`Country/Region`)

paises <- c("Costa Rica", "Panama", "Ecuador", "Colombia", "Peru", "Brazil", "Chile", "Uruguay")

t_cntrys <- covid19_tidy[continente == "Americas"
][
  , .(casos = sum(casos, na.rm = T)),
  by = c("fecha", "pais", "cod_pais", "continente", "region", "tipo") 
][
  , ID := seq_len(.N)
]

fecha_min <- t_cntrys[casos == 1, .SD[which.min(fecha)]]$fecha

library(ggplot2)
library(scales)
library(ggiraph)
library(ggrepel)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

world <- ne_countries(scale = "medium", returnclass = "sf")[,"iso_a3"]

breaks_mapa = c(1, 10, 100, 1000, 10000, 100000, 1000000)

americas_mapa <- covid19_tidy[continente == "Americas"
  ][
    , .(casos = sum(casos, na.rm = T)),
    by = c("fecha", "pais", "cod_pais", "continente", "region", "tipo") 
  ][
    , ID := seq_len(.N)
  ]

setorder(americas_mapa, pais, tipo, fecha)

americas_mapa <- americas_mapa[
    ,`:=`(nuevos_casos = casos - shift(casos, fill = 0),
         prct_dif_casos = round((casos - shift(casos, fill = 0))/(shift(casos, fill = 0))*100, 1)),
    , by = c("pais", "cod_pais", "continente", "region", "tipo")
  ][
    fecha == fecha_hoy
  ][
    is.nan(prct_dif_casos), prct_dif_casos := 0
  ]

americas_mapa <- merge(world, americas_mapa, by.x = "iso_a3", by.y = "cod_pais", all=FALSE)
library(ggthemes)

ggplot() +
  geom_sf(data = americas_mapa, aes(fill = casos), lwd = 0.1) +
  coord_sf(crs = "+proj=aeqd +lat_0=0 +lon_0=-60 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs ") +
  scale_fill_viridis_c(trans = "pseudo_log", 
                       breaks = breaks_mapa, 
                       label = label_number_si()) +
  facet_grid(cols = vars(tipo)) +
  theme_light() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_text(size = 0),
        legend.text = element_text(size = 8),
        legend.position="bottom", legend.box = "horizontal") +
  guides(fill = guide_colourbar(barwidth = 10, barheight = .6, label.vjust = -2))

ggplot() +
  geom_sf(data = americas_mapa, aes(fill = nuevos_casos), lwd = 0.1) +
  coord_sf(crs = "+proj=aeqd +lat_0=0 +lon_0=-60 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs ") +
  scale_fill_viridis_c(trans = "pseudo_log", 
                       breaks = breaks_mapa, 
                       label = label_number_si()) +
  facet_grid(cols = vars(tipo)) +
  theme_light() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_text(size = 0),
        legend.text = element_text(size = 8),
        legend.position="bottom", legend.box = "horizontal") +
  guides(fill = guide_colourbar(barwidth = 10, barheight = .6, label.vjust = -2))

g1 <- ggplot(t_cntrys[tipo == "contagios" & 
                        fecha >= fecha_min &
                        pais != "Costa Rica"], 
       aes(x = fecha, y = casos, group = pais, col = region)) + 
  geom_line(size = 0.3, alpha = 0.5) + 
  geom_line(
    data = t_cntrys[tipo == "contagios" &
               fecha >= fecha_min &
               pais == "Costa Rica"],
    aes(x = fecha, y = casos),
    size = 1, col = "green") +
  # geom_label_repel(aes(label = `Country/Region`),
  #                  nudge_x = 1,
  #                  na.rm = TRUE) + 
  scale_y_continuous(trans = "log10"
                      , limits = c(10, max(t_cntrys$casos))
                     ) +
  scale_x_date(date_breaks = "3 days", 
               minor_breaks = NULL,
               date_labels = '%d de %B',
               limits = c(dmy("03-03-2020", ymd(fecha_hoy))),
               ) +
  # scale_colour_brewer(palette = "Dark2") +
  # scale_colour_viridis_d() + 
  theme_light() +
  labs(x = "",
       y = "Número de casos reportados",
       col = "",
       title = "",
       subtitle = "Casos por fecha") +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        legend.position = "bottom")

g1_g <- g1 + 
  geom_point_interactive(
    aes(tooltip = paste0(
      pais, "\n", 
      paste0(day(fecha), " de ", month(fecha, label=TRUE, abbr=FALSE)), "\n", 
      casos," casos")), size = 0.75)
girafe(ggobj = g1_g, 
       options = list(opts_tooltip(use_fill = TRUE),
                      opts_hover(css = "fill:black;"),
                      opts_toolbar(saveaspng = FALSE)))

fecha_diauno <- t_cntrys[
  tipo == "contagios" & casos > 0 ][
    , .SD[which.min(fecha)], by = "pais" ][
      , .(pais,
          fecha_diauno = fecha)]
  
t_gt_diauno <- fecha_diauno[
  t_cntrys, on=c(pais = "pais") ][
    fecha >= fecha_diauno][
      order(pais, tipo, fecha) ][
        casos > 0, 
        `:=`(num_dia = as.integer(difftime(fecha, fecha_diauno, units = "days") + 1),
             nuevos_casos = casos - shift(casos, fill = 0),
             prct_dif_casos = round((casos - shift(casos, fill = 0))/(shift(casos, fill = NA))*100, 1)), 
        by = c("pais", "tipo", "cod_pais", "pais") ]

t_gt_diauno[is.na(t_gt_diauno)] <- 0

max_dia_CR <- max(t_gt_diauno[pais == "Costa Rica" & tipo == "contagios", num_dia])

g2 <- ggplot(t_gt_diauno[num_dia <= max_dia_CR & tipo == "contagios"], 
       aes(x = num_dia, y = casos, group = pais, col = region, )) + 
  geom_line(linetype = 1, size = 0.5, alpha = 0.4) +
  geom_line(
    data = t_gt_diauno[num_dia <= max_dia_CR & tipo == "contagios" & pais == "Costa Rica"],
    aes(x = num_dia, y = casos), 
    size = 1, col = "blue"
  )
  #geom_point(size = 0.1, alpha = 0.7) +
  scale_y_continuous(trans = "log10") +
  #scale_y_continuous() +
  scale_x_continuous(minor_breaks = NULL) + 
  #geom_smooth(method="lm", formula= (y ~ exp(2)), se=FALSE, color=1) +
  scale_colour_brewer(palette = "Dark2") +
  theme_light() +
  labs(x = "Días luego de primera detección",
       y = "Número de casos reportados",
       col = "",
       title = "",
       subtitle = "Casos a partir del día de la primera detección")

g2_g <- g2 + 
  geom_point_interactive(
    aes(tooltip = paste0(
    pais, "\n", 
    paste0(day(fecha), " de ", month(fecha, label=TRUE, abbr=FALSE)), "\n", 
    nuevos_casos," nuevos casos"), 
    data_id = pais), size = 0.75)
girafe(ggobj = g2_g, 
       options = list(opts_tooltip(use_fill = TRUE),
                      opts_hover(css = "fill:black;"),
                      opts_toolbar(saveaspng = FALSE)))

setorder(t_gt_diauno, -`Country/Region`, fecha)
t_gt_diauno$`Country/Region` <- factor(t_gt_diauno$`Country/Region`, 
                                       levels=unique(t_gt_diauno$`Country/Region`))

breaks = c(1, 10, 100, 1000)

g3 <- ggplot(t_gt_diauno[tipo == "contagios"], 
       aes(x = fecha, y = `Country/Region`, fill = nuevos_casos)) +
  geom_tile(width=.9, height=.8) +
  # scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
  scale_x_date(date_breaks = "3 days", 
               minor_breaks = NULL,
               date_labels =  '%d de %B') +
  theme_light() +
  # xlim(1, max_dia_CR) +
  scale_fill_viridis_c(trans = "pseudo_log", breaks = breaks, labels = breaks) +
  labs(x = "",
       y = "",
       fill = "Num. casos",
       title = "Evolución de pandemia por país",
       subtitle = "Nuevos casos diarios") +
  theme(axis.text.x=element_text(angle=60, hjust=1))

g3_g <- g3 + geom_tile_interactive(
  aes(tooltip = paste0(
    pais, "\n", 
    paste0(day(fecha), " de ", month(fecha, label=TRUE, abbr=FALSE)), "\n", 
    nuevos_casos," casos")), width=.9, height=.8)
girafe(ggobj = g3_g, 
       options = list(opts_toolbar(saveaspng = FALSE)))

####
poblacion <- fread("WPP2019_TotalPopulationBySex.csv")[
  Time == 2020 & Variant == "Medium",
  .(Location, 
    PopTotal = as.integer(PopTotal*1000))
]

tb_general <- merge(t_gt_diauno, poblacion, by.x = "Country/Region", by.y = "Location", all.x = TRUE)

tb_general[
  , casos_10k := casos*100000/PopTotal]

g4 <- ggplot(tb_general[tipo == "contagios" & 
                          fecha >= fecha_min &
                          fecha >= dmy("08/03/2020")], 
       aes(x = fecha, y = casos_10k, group = pais, col = pais)) + 
  geom_line(alpha = 0.7) +
  geom_point(size = 0.3) +
  #scale_y_continuous(trans = "log10") +
  scale_x_date(date_breaks = "3 days", 
               minor_breaks = NULL,
               date_labels =  '%d de %B') +
  scale_colour_brewer(palette = "Dark2") +
  #scale_colour_viridis_d() + 
  theme_light() +
  labs(x = "",
       y = "Casos por cada 100,000 habitantes",
       col = "",
       title = "",
       subtitle = "Casos por fecha") +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        legend.position = "none")

g4_g <- g4 + 
  geom_point_interactive(
    aes(tooltip = paste0(
      pais, "\n", 
      paste0(day(fecha), " de ", month(fecha, label=TRUE, abbr=FALSE)), "\n", 
      round(casos_10k,1)," casos/10,000 hab."), 
      data_id = pais), size = 0.75)
girafe(ggobj = g4_g, 
       options = list(opts_tooltip(use_fill = TRUE),
                      opts_hover(css = "fill:black;"),
                      opts_toolbar(saveaspng = FALSE)))

g5 <- ggplot(t_gt_diauno[tipo == "contagios" & 
                           fecha >= fecha_min &
                           fecha >= dmy("08/03/2020")],
  aes(x = fecha, y = pais, fill = prct_dif_casos)) + 
  geom_tile(width=.9, height=.8) +
  scale_fill_viridis_c(trans = "pseudo_log", breaks = breaks, labels = breaks) +
  # scale_y_continuous(labels=comma) +
  # scale_y_continuous(trans = "log10") +
  scale_x_date(date_breaks = "3 days", 
               minor_breaks = NULL,
               date_labels = '%d de %B') +
  scale_colour_brewer(palette = "Dark2") +
  #scale_colour_viridis_d() + 
  theme_light()

############
############

url_cant_uned <- paste0("http://geovision.uned.ac.cr/oges/archivos_covid/","")
cantones_uned <- fread("http://geovision.uned.ac.cr/oges/archivos_covid/04_09/04_09_CSV.csv")

cols <- c("provincia", "canton")

cantones_uned[ , (cols) := lapply(.SD, iconv, "macintosh", "UTF-8"), .SDcols = cols]
cantones_uned <- cantones_uned[,which(unlist(lapply(cantones_uned, function(x)!all(is.na(x))))),with=F]
cantones_uned[, `:=`(cod_provin = NULL, cod_canton = NULL)]
cantones_uned <- cantones_uned[!rowSums(cantones_uned[, 3:ncol(cantones_uned)])==0]

fwrite(cantones_uned, paste0(fecha_hoy, "_cantones.csv"))

cantones_tidy <- melt(cantones_uned,
                      id.vars = c("canton", "provincia"),
                      variable.name = "fecha",
                      value.name = "casos")[
                        , `:=`(fecha = dmy(fecha))
                      ]

# dt_cantones <- fread("2020-04-04_cantones.csv")

setorder(cantones_tidy, -canton, fecha)

cantones_tidy[
  , nuevos_casos := casos - shift(casos, fill = 0),
  by = "canton"
]

cantones_tidy$`Cantón` <- factor(cantones_tidy$`Cantón`, levels=unique(cantones_tidy$`Cantón`))

g_cant_1 <- ggplot(cantones_tidy, 
       aes(x = fecha, y = `Cantón`, fill = casos)) + 
  geom_tile(width=1, height=.8) +
  #geom_point(size = 0.4) +
  scale_x_date(limits = c(min = min(cantones_tidy$fecha), max = ymd(fecha_hoy)),
               date_breaks = "3 days", 
               minor_breaks = NULL,
               expand=c(0,0),
               labels = date_format('%d de %B')) +
               #date_labels = '%d de %B') +
  # scale_colour_brewer(palette = "Dark2") +
  scale_fill_viridis_c(trans = "pseudo_log") +
  theme_light() +
  labs(x = "",
       y = "",
       fill = "Num. casos",
       title = "",
       subtitle = "Acumulado de casos") +
  theme(axis.text.x = element_text(angle=60, hjust=1),
        #axis.text.y = element_text(size = 5),
        legend.position="bottom", legend.box = "horizontal",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 6)
        )

g_cant_1_g <- g_cant_1 + 
  geom_tile_interactive(
    aes(tooltip = paste0(
      "Provincia: ", Province, "\n",
      "Cantón: ", `Cantón`, "\n",
      paste0(day(fecha), " de ", month(fecha, label=TRUE, abbr=FALSE)), "\n", 
      casos," casos"),
      ), width=1, height=.8)
girafe(ggobj = g_cant_1_g, 
       options = list(opts_toolbar(saveaspng = FALSE),
                      use_fill = FALSE,
                      use_stroke = TRUE),
       width_svg = 5, height_svg = 7)

g_cant_2 <- ggplot(cantones_tidy, 
       aes(x = fecha, 
           y = `Cantón`, 
           fill = nuevos_casos)) + 
  geom_tile(width=1, height=.8) +
  #geom_point(size = 0.4) +
  scale_x_date(date_breaks = "3 days", 
               minor_breaks = NULL,
               date_labels = '%d de %B') +
  # scale_colour_brewer(palette = "Dark2") +
  scale_fill_viridis_c(option = "plasma", direction = 1) +
  theme_light() +
  labs(x = "",
       y = "",
       fill = "Num. casos",
       title = "",
       subtitle = "Nuevos casos por día") +
  theme(axis.text.x = element_text(angle=60, hjust=1),
        legend.position="bottom", 
        # legend.box = "horizontal",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 6))

g_cant_2_g <- g_cant_2 + 
  geom_tile_interactive(
    aes(tooltip = paste0(
      "Provincia: ", Province, "\n",
      "Cantón: ", `Cantón`, "\n",
      paste0(day(fecha), " de ", month(fecha, label=TRUE, abbr=FALSE)), "\n", 
      nuevos_casos," nuevos casos")), width=.9, height=.8)
girafe(ggobj = g_cant_2_g , 
       options = list(opts_toolbar(saveaspng = FALSE)),
       width_svg = 5, height_svg = 7)

library(sf)
library(tmap)
library(lwgeom)



cantones <- st_read("WFS:http://geos.snitcr.go.cr/be/IGN_5/wfs?", "IGN_5:limitecantonal_5k")

cantones_simp <- rmapshaper::ms_simplify(cantones, keep = 0.005, keep_shapes = TRUE)
cantones_simp <- rmapshaper::ms_filter_islands(cantones_simp, min_area = 5000*10000)
cantones_simp <- st_transform(cantones_simp, crs = 4326)
cantones_simp <-  st_cast(cantones_simp,"POLYGON")
#cantones_simp_spoly$area <- as.numeric(st_area(cantones_simp_spoly))/10000
# cantones_simp_1 <- cantones_simp_spoly[cantones_simp_spoly$area > 200,]

ggplot(cantones_simp) + geom_sf(aes(fill = objectid))

st_write(cantones_simp, "cantones.geojson")

cantones_simp <- st_read("cantones.geojson")

cantones_tidy_mapa <- cantones_tidy[
  fecha == dmy("28-03-2020")][
    , casos_chr := as.character(casos)]

cant_geo <- merge(cantones_simp, cantones_tidy_mapa, 
                  by.x = "canton", by.y = "Cantón", all.x = TRUE)

bbox <- st_bbox(c(xmin = 270000, xmax = 680000, ymax = 1260000, ymin = 880000))

cant_geo <- st_crop(cant_geo, bbox)

cant_geo <- st_make_valid(cant_geo)
cant_geo_cntrids <- st_centroid(cant_geo)

ggplot() +
  geom_sf(data = cant_geo, fill = "gray", col = "white", lwd = 0.1) +
  geom_sf(data = cant_geo_cntrids, aes(size = casos), col = "red", alpha = 0.3) +
  theme_light() 

tmap_mode("view")
tm_shape(cant_geo) +
  tm_borders(lwd = .1, alpha = 0.7) +
  tm_bubbles(size = "casos", 
             col = "red",
             style="fixed", breaks=c(-Inf, seq(0, 6, by=2), Inf),
             palette="-RdYlBu", contrast=1, 
             alpha = 0.6, 
             id = "", 
             popup.vars = c("Provincia:" = "Province", 
                            "Cantón:" = "canton",
                            "Casos:" = "casos_chr"))

##
wiki_tbl2 <- read_html(url) %>% 
  html_node(xpath = '//*[@id="mw-content-text"]/div/table[4]') %>% 
  html_table(fill = TRUE)

wiki_tbl_diaria_2 <- setDT(wiki_tbl2)[
  -1][
    , Fecha := lubridate::ymd(`Reported by`)
  ][, `Reported by` := NULL]

provincias <- wiki_tbl_diaria_2[, c(1:7,16)]
provincias <- melt(provincias,
                   id.vars = "Fecha",
                   variable.name = "Provincia",
                   value.name = "casos")[
                     , casos := as.integer(casos)
                   ]

g_prov <- ggplot(provincias, aes(x = Fecha, y = casos, fill = Provincia)) +
  geom_area_interactive(aes(y = casos), tooltip = "Provincia") +
  scale_fill_brewer(palette = "Set2") +
  theme_light() +
  labs(x = "",
       y = "Casos reportados",
       fill = "Provincia",
       title = "",
       subtitle = "Nuevos casos por día") +
  theme(axis.text.x = element_text(angle=60, hjust=1),
        # legend.position="bottom", 
        # legend.box = "horizontal",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 6))

girafe(ggobj = g_prov)  

CR_detalles <- wiki_tbl_diaria_2[, c(9,11:13, 16)]

for(col in c("Confirmed","Deaths", "ICU", "Rec"))
  set(CR_detalles, j = col, value = as.integer(CR_detalles[[col]]))

for (col in c("Confirmed", "Deaths", "ICU", "Rec")) CR_detalles[is.na(get(col)), (col) := 0]

CR_detalles <- CR_detalles[
  , .(Fecha,
      Casos = Confirmed,
      Fallecidos = Deaths - shift(Deaths, fill = 0),
      `Cuid. Intens.` = ICU,
      Recuperados = Rec - shift(Rec, fill = 0))
]

CR_detalles <- melt(CR_detalles,
                    id.vars = "Fecha",
                    variable.name = "grupo",
                    value.name = "numero")

ggplot(CR_detalles, aes(x = Fecha, y = numero)) +
  geom_col(position = "dodge") +
  facet_grid(rows = vars(grupo), scales = "free") +
  scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
  theme_light() +
  labs(x = "",
       y = "Casos reportados al día",
       title = "")

