# PASO 1: Instalación de paquetes necesarios
install.packages("sf")       # Manejo de datos espaciales
install.packages("terra")    # Análisis raster y cálculos espaciales
install.packages("ggplot2")  # Visualización de datos

# PASO 2: Cargar las bibliotecas
library(sf)
library(terra)
library(ggplot2)

# PASO 3: Configuración de directorio y carga de datos
setwd("C:/Users/yanit/OneDrive/Escritorio/Contrato gobernacion yanith/script")  # Cambia esta ruta según corresponda

# Cargar shapefiles
datos <- st_read("datos_historicos_puntos_calor2.shp")  # Puntos históricos de calor
caqueta <- st_read("Caqueta.shp")  # Límites del Caquetá

# Verificar y transformar CRS si es necesario
if (st_crs(datos) != st_crs(caqueta)) {
  datos <- st_transform(datos, crs = st_crs(caqueta))  # Transformar CRS de puntos al mismo que el de Caquetá
}

# PASO 4: Crear un raster base para la densidad KDE
bbox <- st_bbox(caqueta)  # Definir extensión del área de interés
res <- 1000  # Resolución en metros (ajustar según el área de estudio)
raster_base <- rast(xmin = bbox["xmin"], xmax = bbox["xmax"], ymin = bbox["ymin"], ymax = bbox["ymax"],
                    resolution = res, crs = st_crs(caqueta)$proj4string)

# Rasterizar los puntos históricos
puntos_rast <- rasterize(vect(datos), raster_base, field = 1, fun = "sum", background = 0)

# Suavizar el raster de densidad con un filtro gaussiano
sigma <- 5  # Ajustar el ancho del kernel
filtro_gaussiano <- focalMat(puntos_rast, sigma, type = "Gauss")
densidad_kde <- focal(puntos_rast, filtro_gaussiano, fun = sum)

# Calcular la densidad KDE usando un filtro suavizado gaussiano
densidad_kde <- focal(puntos_rast, w = matrix(1, nrow = 5, ncol = 5), fun = mean)

# Visualizar la densidad KDE
plot(densidad_kde, main = "Densidad KDE de Puntos Históricos")

# Generar puntos proyectados para cada mes
set.seed(123)  # Asegurar reproducibilidad
meses <- 1:12
puntos_proyectados <- lapply(meses, function(mes) {
  # Definir el número de puntos a generar
  n_puntos <- 100  # Ajustar según el análisis
  
  # Generar puntos aleatorios basados en la densidad KDE
  puntos_mes <- spatSample(densidad_kde, size = n_puntos, method = "weights", as.points = TRUE)
  
  # Convertir a formato sf y agregar información del mes sin dplyr
  puntos_mes_sf <- st_as_sf(puntos_mes)
  puntos_mes_sf$mes <- mes
  puntos_mes_sf$anio <- 2025
  puntos_mes_sf
})

# Combinar todos los puntos proyectados
puntos_proyectados_sf <- do.call(rbind, puntos_proyectados)

# Recortar los puntos proyectados a los límites del Caquetá
puntos_proyectados_sf <- st_intersection(puntos_proyectados_sf, Caqueta)

# PASO 6: Exportar los puntos proyectados como shapefile
st_write(puntos_proyectados_sf, "puntos_calor_proyectados_kde_2025.shp", delete_layer = TRUE)

# PASO 7: Visualización de los puntos proyectados
ggplot() +
  geom_sf(data = caqueta, fill = "lightgray", color = "black") +
  geom_sf(data = puntos_proyectados_sf, aes(color = as.factor(mes)), size = 1, alpha = 0.7) +
  labs(title = "Proyección de Puntos de Calor por KDE - 2025",
       subtitle = "Distribución basada en densidad histórica",
       color = "Mes") +
  theme_minimal()

# PASO 1: Añadir nombres de los municipios al mapa
ggplot() +
  geom_sf(data = caqueta, fill = "lightgray", color = "black") +
  geom_sf(data = puntos_proyectados_sf, aes(color = as.factor(mes)), size = 1, alpha = 0.7) +
  geom_text(data = caqueta, aes(label = MpNombre, geometry = geometry), stat = "sf_coordinates", size = 3, hjust = 0.5) +
  labs(title = "Proyección de Puntos de Calor por KDE - 2025",
       subtitle = "Distribución basada en densidad histórica",
       color = "Mes") +
  scale_color_manual(values = scales::hue_pal()(12)) +  # Paleta de colores bien diferenciados
  theme_minimal()

