library(data.table)
library(ggplot2)
library(readxl)
library(ggrepel)
library(httr)
library(scales)

dias_atras <- 7
fecha_hoy <- as.character(Sys.Date())
#fecha_hoy <- "2021-08-31"
mes <- substr(fecha_hoy, 6, 7)
dia <- substr(fecha_hoy, 9, 10)

url1<-paste0('http://geovision.uned.ac.cr/oges/archivos_covid/2022_', mes, '_',  dia, '/', mes, '_', dia, '_22_EXCEL_SERIES.xlsx')
GET(url1, write_disk(tf <- tempfile(fileext = ".xlsx")))

dt1 <- as.data.table(read_excel(tf, sheet = "3_4 DIST_ACTIV"))

dt1_long <- melt(dt1, 
                 id.vars = c(1:6), 
                 variable.name = "fecha", 
                 value.name = "activos")[
                   , fecha := as.Date(as.integer(as.character(fecha)), origin = "1899-12-30")
                 ]

setorder(dt1_long, fecha)

dt1_long[, cambio_activos := activos - shift(activos, n = 1L, fill = 0, type = "lag"), by = .(provincia, canton, distrito)]
#dt1_long <- dt1_long[fecha < fecha_hoy]
#dt1_long[, ttl_cmb_dia_canton := sum(cambio_activos), by = .(fecha, provincia, canton)]
#dt1_long[, ttl_cmb_dia_prov := sum(cambio_activos), by = .(fecha, provincia)]
dt1_long[, ttl_cmb_dia_pais := sum(abs(cambio_activos)), by = fecha]
dt1_long[, prct_cmb_dia_pais := cambio_activos / ttl_cmb_dia_pais]

dt1_long[
  , prmd_cmb_prev := shift(
    frollmean(cambio_activos, n = dias_atras, algo = "exact", align = "right")
    , n = 1, fill = 0, type = "lag")
  , by = .(provincia, canton, distrito)]

dt1_long[, prct_cmb_prev := cambio_activos / prmd_cmb_prev]
dt1_long[, ind_final := prct_cmb_prev * prct_cmb_dia_pais]

library(ggiraph)
g1 <- ggplot(dt1_long[, .SD[which.max(fecha)], by= .(provincia, canton, distrito)],
             aes(x = prct_cmb_dia_pais, y = prct_cmb_prev, col = provincia)) +
  geom_point_interactive(aes(tooltip = distrito))

girafe(ggobj = g1)

dt1_hoy <- dt1_long[fecha == fecha_hoy]

summary(dt1_hoy$cambio_activos)

cambios_postivos <- dt1_hoy[cambio_activos > 0, cambio_activos]


library(tidyxl)
library(zoo)
library(fuzzyjoin)
pob <- xlsx_cells(
  "repoblacev2011-2025-03.xlsx",
  sheets = "2020"
)

pob_dt <- as.data.table(pob)
pob_dt_filtered <- pob_dt[
  col %in% c(2,3)
][
  , .(provincia = fifelse(local_format_id == 6, character, ''),
      canton = fifelse(local_format_id == 9, character, ''),
      distrito = fifelse(local_format_id %in% c(10, 11), character, ''),
      pblcn = shift(numeric, -1))
][
  !is.na(pblcn)
]
pob_dt_filtered[pob_dt_filtered == ''] <- NA
pob_dt_filtered$provincia <- na.locf(pob_dt_filtered$provincia, na.rm = FALSE)
pob_dt_filtered$canton <- na.locf(pob_dt_filtered$canton, na.rm = FALSE)
pob_dt_filtered <- pob_dt_filtered[!is.na(distrito)
][ canton == "Valverde Vega", canton := "Sarchí"
][ canton == "Aguirre", canton := "Quepos"
]
pob_dt_filtered <- pob_dt_filtered[pob_dt_filtered[, .I[pblcn == max(pblcn)], by=.(provincia, canton, distrito)]$V1]

dt1_hoy_match <- merge(dt1_hoy, pob_dt_filtered, by = c("provincia", "canton", "distrito"))

dist_no_matching <- dt1_hoy[!pob_dt_filtered, on = c("provincia", "canton", "distrito")][
  distrito != "Sin información de distrito"
]

dt1_hoy_fuzzy_match <- as.data.table(stringdist_inner_join(dist_no_matching, pob_dt_filtered, 
                                                           by = c("provincia", "canton", "distrito"), 
                                                           max_dist = 3)
)[
  , .(provincia = provincia.x,
      canton = canton.x,
      distrito = distrito.x,
      cod_provin, cod_canton, codigo_dta, fecha, activos,
      cambio_activos, pblcn, ttl_cmb_dia_pais, prct_cmb_dia_pais,
      prmd_cmb_prev, prct_cmb_prev, ind_final)
]

dt1_hoy_m <- rbindlist(list(dt1_hoy_match, dt1_hoy_fuzzy_match), use.names=TRUE)

dt2 <- as.data.table(read_excel(tf, sheet = "3_1 DIST_ACUM"))
dt2_long <- melt(dt2, 
                 id.vars = c(1:6), 
                 variable.name = "fecha", 
                 value.name = "acumulados")[
                   , fecha := as.Date(as.integer(as.character(fecha)), origin = "1899-12-30")
                 ]
dt2_long[
  , nuevos_casos := acumulados - 
    shift(acumulados, n = 1L, fill = 0, type = "lag"), by = .(provincia, canton, distrito)]

dt2_hoy <- dt2_long[fecha == fecha_hoy]

dt3 <- as.data.table(read_excel(tf, sheet = "3_3 DIST_FALL"))
dt3_long <- melt(dt3,
                 id.vars = c(1:6), 
                 variable.name = "fecha", 
                 value.name = "fallecidos")[
                   , fecha := as.Date(as.integer(as.character(fecha)), origin = "1899-12-30")
                 ]
dt3_hoy <- dt3_long[fecha == fecha_hoy]

dt_hoy <- merge(dt1_hoy_m,
                dt2_hoy, by = c("provincia", "canton", "distrito", "cod_provin", "cod_canton", "codigo_dta", "fecha"))
dt_hoy <- merge(dt_hoy,
                dt3_hoy, by = c("provincia", "canton", "distrito", "cod_provin", "cod_canton", "codigo_dta", "fecha"))

dt_hoy[, `:=`(pobl_activa = pblcn - (acumulados - activos),
              prct_pob_acum = acumulados / pblcn,
              prct_pob_act = activos / (pblcn - (acumulados - activos)))]

nuevos_casos <- dt2_hoy[nuevos_casos > 0, nuevos_casos]

intervalos <- classInt::classIntervals(nuevos_casos, 3, style = "jenks")

dt_hoy[, cat_nuevos_casos := fcase(
                               nuevos_casos == 0, "sin cambio",
                               nuevos_casos > 0 & nuevos_casos <= intervalos$brks[1], 
                                paste0("aumento de ", intervalos$brks[1]),
                               nuevos_casos > intervalos$brks[1] & nuevos_casos < intervalos$brks[2], 
                                paste0("aumento de ", intervalos$brks[1] + 1, " a ", intervalos$brks[2] - 1),
                               nuevos_casos >= intervalos$brks[2] & nuevos_casos < intervalos$brks[3], 
                                paste0("aumento de ", intervalos$brks[2], " a ", intervalos$brks[3] - 1),
                               nuevos_casos >= intervalos$brks[3] & nuevos_casos <= intervalos$brks[4] - 1, 
                                paste0("aumento de ", intervalos$brks[3], " a ", intervalos$brks[4] - 1),
                               nuevos_casos >= intervalos$brks[4], 
                                paste0("aumento de ", intervalos$brks[4])
)
]

dt_hoy$cat_nuevos_casos <- ordered(dt_hoy$cat_nuevos_casos, 
                               levels = c(
                                          "sin cambio", 
                                          paste0("aumento de ", intervalos$brks[1]),
                                          paste0("aumento de ", intervalos$brks[1] + 1, " a ", intervalos$brks[2] - 1),
                                          paste0("aumento de ", intervalos$brks[2], " a ", intervalos$brks[3] - 1),
                                          paste0("aumento de ", intervalos$brks[3], " a ", intervalos$brks[4] - 1),
                                          paste0("aumento de ", intervalos$brks[4])))

int_cols <-  c("activos", "cambio_activos", "pblcn", "acumulados", "nuevos_casos", "fallecidos",
               "pobl_activa")

dt_hoy[, (int_cols) := lapply(.SD, as.integer), .SDcols = int_cols]



#regmatches(texto, regexpr("(?:(El|La) +)?([a-zA-Z0-9])", texto))
#stringr::str_extract(texto, "(?:(El|La) +)?([a-zA-Z0-9])")
#sub("(?:(El|La) +)", "", texto)

library(sf)
#library(gdalUtils)
#library(rgdal)
#library(rmapshaper)
# dsn <- "WFS:http://geos.snitcr.go.cr/be/IGN_5/wfs?"
# ogrListLayers(dsn)
# 
# distritos <- st_read(dsn, "IGN_5:limitedistrital_5k")
# distritos_simp <- ms_simplify(distritos, keep = 0.01)
# st_write(distritos_simp, "distritos.geojson")
distritos_simp <- st_read("distritos.geojson")

dist_mapa <- dplyr::left_join(distritos_simp[distritos_simp$distrito != "Isla del Coco", ], 
                              dt_hoy[, 
                                     .(codigo_dta, fecha, pblcn, acumulados, nuevos_casos, activos, 
                                       cambio_activos, ttl_cmb_dia_pais, 
                                       prct_cmb_dia_pais, prmd_cmb_prev, prct_cmb_prev, ind_final,
                                       cat_nuevos_casos, 
                                       prct_pob_act = percent(prct_pob_act, 0.01), 
                                       prct_pob_acum = percent(prct_pob_acum, 0.01), 
                                       fallecidos)]
                              , by = "codigo_dta")

dist_mapa$fecha <- lubridate::ymd(fecha_hoy)
dist_mapa[is.na(dist_mapa$cat_nuevos_casos),]$cat_nuevos_casos <- "sin cambio"
dist_mapa[is.na(dist_mapa)] <- 0

library(tmap)
tmap_mode("view")
mapa_dist <- tm_shape(dist_mapa) +
  tm_polygons(col = "cat_nuevos_casos",
              border.col = "black",
              border.alpha = 0.3,
              style = "cat",
              palette = "viridis",
              alpha = 0.5,
              #legend.hist = TRUE,
              title = paste0("Nuevos casos por distrito (", fecha_hoy, ")"),
              id = "distrito",
              popup.vars = c("Fecha" = "fecha", 
                             "Provincia" = "provincia",
                             "Cantón" = "canton",
                             "Distrito" = "distrito",
                             "Pob. estimada INEC" = "pblcn",
                             "Casos totales" = "acumulados",
                             "% de pob. contagiada" = "prct_pob_acum",
                             "Casos activos" = "activos",
                             "% de pob. contagio activo" = "prct_pob_act",
                             "Cambio de casos activos" = "cambio_activos",
                             "Nuevos casos" = "nuevos_casos",
                             "Fallecidos" = "fallecidos"))

tmap_save(mapa_dist, "mapa_distritos.html")

## Moravia

dt_moravia <- dt1_long[canton == "Moravia"]

ggplot(dt_moravia, aes(x = fecha, y = activos)) + 
  geom_col() +

###
tmap_mode("view")
mapa_dist_acum <- tm_shape(dist_mapa) +
  tm_polygons(col = "prct_pob_acum",
              alpha = 0.5,
              border.col = "black",
              border.alpha = 0.3,
              #style = "cat",
              palette = "viridis",
              #legend.hist = TRUE,
              title = paste0("% de población positivo al ", fecha_hoy),
              id = "distrito",
              popup.vars = c("Fecha" = "fecha", 
                             "Provincia" = "provincia",
                             "Cantón" = "canton",
                             "Distrito" = "distrito",
                             "Casos activos" = "activos",
                             "Cambio de casos activos" = "cambio_activos",
                             "% de pob. posit. activa" = "prct_pob_act",
                             "% de pob. posit. acumul." = "prct_pob_acum",
                             "Fallecidos" = "fallecidos"))

tmap_save(mapa_dist_acum, "mapa_distritos_acumulados.html")

######
t_cntrys_posit[, nuevos := casos - shift(casos, fill = 0)]
positivos <- tb_general[tipo == "positivos"]
setorder(positivos, pais, -nuevos_casos)
positivos[, orden := seq_len(.N), by = pais]

positivos[orden <= 2]
