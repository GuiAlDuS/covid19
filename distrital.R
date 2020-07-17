library(data.table)
library(ggplot2)
library(readxl)
library(ggrepel)
library(httr)

dias_atras <- 3
fecha_hoy <- as.character(Sys.Date())
#fecha_hoy <- "2020-07-15"
mes <- substr(fecha_hoy, 6, 7)
dia <- substr(fecha_hoy, 9, 10)


url1<-paste0('http://geovision.uned.ac.cr/oges/archivos_covid/', mes, '_',  dia, '/', mes, '_', dia, '_EXCEL_SERIES.xlsx')
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


ggplot(dt1_long[fecha == fecha_hoy], 
       aes(x = prct_cmb_dia_pais, y = prct_cmb_prev, col = provincia)) +
  geom_point()

dt1_hoy <- dt1_long[fecha == fecha_hoy]

summary(dt1_hoy$cambio_activos)

cambios_postivos <- dt1_hoy[cambio_activos > 0, cambio_activos]
intervalos <- classInt::classIntervals(cambios_postivos, 4, style = "jenks")

dt1_hoy[, cat_cmb_act := fcase( cambio_activos < 0, "reducción",
                                cambio_activos == 0, "sin cambio",
                                cambio_activos > 0 & cambio_activos <= intervalos$brks[1], 
                                paste0("aumento de ", intervalos$brks[1]),
                                cambio_activos > intervalos$brks[1] & cambio_activos < intervalos$brks[2], 
                                paste0("aumento de ", intervalos$brks[1] + 1, " a ", intervalos$brks[2] - 1),
                                cambio_activos >= intervalos$brks[2] & cambio_activos < intervalos$brks[3], 
                                paste0("aumento de ", intervalos$brks[2], " a ", intervalos$brks[3] - 1),
                                cambio_activos >= intervalos$brks[3] & cambio_activos <= intervalos$brks[4] - 1, 
                                paste0("aumento de ", intervalos$brks[3], " a ", intervalos$brks[4] - 1),
                                cambio_activos >= intervalos$brks[4], 
                                paste0("aumento de ", intervalos$brks[4], " o mayor")
                                )
        ]

dt1_hoy$cat_cmb_act <- ordered(dt1_hoy$cat_cmb_act, 
                               levels = c("reducción", 
                                          "sin cambio", 
                                          paste0("aumento de ", intervalos$brks[1]),
                                          paste0("aumento de ", intervalos$brks[1] + 1, " a ", intervalos$brks[2] - 1),
                                          paste0("aumento de ", intervalos$brks[2], " a ", intervalos$brks[3] - 1),
                                          paste0("aumento de ", intervalos$brks[3], " a ", intervalos$brks[4] - 1),
                                          paste0("aumento de ", intervalos$brks[4], " o mayor")))

library(sf)
library(gdalUtils)
library(rgdal)
library(rmapshaper)
dsn <- "WFS:http://geos.snitcr.go.cr/be/IGN_5/wfs?"
ogrListLayers(dsn)

distritos <- st_read(dsn, "IGN_5:limitedistrital_5k")
distritos_simp <- ms_simplify(distritos, keep = 0.01)
st_write(distritos_simp, "distritos.geojson")
distritos_simp <- st_read("distritos.geojson")

dist_mapa <- dplyr::inner_join(distritos_simp, dt1_hoy[distrito != "Isla del Coco", 
                                                      .(codigo_dta, fecha, activos, cambio_activos, ttl_cmb_dia_pais, 
                                                        prct_cmb_dia_pais, prmd_cmb_prev, prct_cmb_prev, ind_final,
                                                        cat_cmb_act)]
                              , by = "codigo_dta")

library(mapview)
mapview(dist_mapa, zcol = "activos")
mapview(dist_mapa, zcol = "cambio_activos")

library(tmap)

tmap_mode("view")
mapa_dist <- tm_shape(dist_mapa) +
  tm_polygons(col = "cat_cmb_act",
              alpha = 0.5,
              border.col = "black",
              border.alpha = 0.3,
              style = "cat",
              palette = "viridis",
              #legend.hist = TRUE,
              title = paste0("Cambio diario de casos activos (", fecha_hoy, ")"),
              id = "distrito",
              popup.vars = c("Fecha" = "fecha", 
                             "Provincia" = "provincia",
                             "Cantón" = "canton",
                             "Distrito" = "distrito",
                             "Casos activos" = "activos",
                             "Cambio de casos activos" = "cambio_activos"))

tmap_save(mapa_dist, "mapa_distritos.html")
