---
  title: "concurso_comovamos"
author: "Manuel Enriquez and Natalia Ortega"
date: "2024-07-02"
output: html_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Limpieza de Base de Datos

```{r}
library(readxl)
library(dplyr)
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(DescTools)
library(leaflet)
library(leaflet.extras)
library(tidyr)
library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(ggmap)
library(dplyr)
```

## Lectura de Archivo
```{r}
bd_comovamos <- read_csv("Real_como_vamos.csv")
```
## Limpieza Final

```{r}
# Paso 1
variables <- c("Orden_Gral_Muestra_MV","Factor_CVNL","NOM_MUN_MV","Latitud","Longitud","CP8_1","P2","P3","quehaceres_hogar", "cuidado_personas","total_min_trabajo_rem_y_norem", "P34","P36","P38_1","P38_2","P38_3", "P38_4", "P38_5", "P38_6", "P38_7", "P38_8","P39", "P41", "P46_1", "P46_2", "P46_3", "P46_4", "P46_5","P47","P49","P51","P52_1","P52_2","P52_3","P52_4","P52_5","P53", "P59", "P60", "P63", "P64", "P65","P71", "P72","P74","P95","P144")
df_cvnl <- bd_comovamos %>% select(all_of(variables))
df_cvnl <- df_cvnl[complete.cases(df_cvnl),]

# Paso 2
switch1 <- c("P38_5", "P38_6", "P71", "P72", "P74", "P95", "P34", "P41", "P47")
df_cvnl[switch1] <- lapply(df_cvnl[switch1], function(x) ifelse(x == 0, 1, ifelse(x == 1, 0, x)))

# Paso 3
df_cvnl$P49 <- ifelse(df_cvnl$P49 == 1, 3, ifelse(df_cvnl$P49 == 3, 1, df_cvnl$P49))

# Paso 4
df_cvnl <- df_cvnl %>%
  mutate(P51 = case_when(
    P51 == 2 ~ 7,
    P51 == 3 ~ 6,
    P51 == 4 ~ 5,
    P51 == 5 ~ 4,
    P51 == 6 ~ 3,
    P51 == 7 ~ 2,
    P51 == 8 ~ 1,
    TRUE ~ P51  # Mantener el valor original si no se cumple ninguna condición
  ))

# Paso 4
switch2 <- c("P63", "P64", "P65")
df_cvnl[switch2] <- lapply(df_cvnl[switch2], function(x) {
  ifelse(x == 1, 4,ifelse(x == 2, 3,ifelse(x == 3, 2,ifelse(x == 4, 1, x))))})

# Paso 5
df_cvnl[df_cvnl == 8888 | df_cvnl == 7777|df_cvnl == 9999] <- NA
df_cvnl <- na.omit(df_cvnl)

# Paso 6
P38 <- c("P38_1", "P38_2", "P38_3", "P38_4", "P38_5", "P38_6", "P38_7", "P38_8")
df_cvnl$P38 <- rowMeans(df_cvnl[P38], na.rm = TRUE)

# Paso 7
P52 <- c("P52_1", "P52_2", "P52_3", "P52_4", "P52_5")
df_cvnl$P52 <- rowMeans(df_cvnl[P52], na.rm = TRUE)

# Paso 8
remove <- c("P52_1", "P52_2", "P52_3", "P52_4", "P52_5","P38_1", "P38_2", "P38_3", "P38_4", "P38_5", "P38_6", "P38_7", "P38_8")
df_cvnl <- df_cvnl[, !(names(df_cvnl) %in% remove)]

# Paso 9
df_cvnl$P3 <- ifelse(df_cvnl$P3 %in% c(1, 2, 4, 6), 1, ifelse(df_cvnl$P3 %in% c(3, 5, 7, 8, 9), 0, df_cvnl$P3))

# Paso 9
df_cvnl <- df_cvnl %>%
  rename(
    nivel_estudios = CP8_1,
    internet = P2,
    econ_activo = P3,
    bicicletas = P34,
    num_vehiculos = P36,
    prom_banquetas = P38,
    estado_banquetes = P39,
    viviendas_abandonadas = P41,
    alumbrado_publico = P46_1,
    condicion_calles = P46_2,
    parques_plazas = P46_3,
    drenaje = P46_4,
    basura = P46_5,
    animales_abandonados = P47,
    tema_agua = P49,
    frecuencia_parques = P51,
    prom_parques = P52,
    calidad_aire = P53,
    aire_acondicionada = P59,
    calefaccion = P60,
    siteconomica_pasada = P63,
    siteconomica_futura = P64,
    siteconomica_presente = P65,
    diabetes = P71,
    hipertension = P72,
    obesidad = P74,
    victima_de_delito = P95,
    ingreso = P144
  )

df_cvnl_a <- df_cvnl

# Paso 10
df_cvnl <- df_cvnl %>%
  mutate(
    internet = as.factor(internet),
    econ_activo = as.factor(econ_activo),
    bicicletas = as.factor(bicicletas),
    viviendas_abandonadas = as.factor(viviendas_abandonadas),
    animales_abandonados = as.factor(animales_abandonados),
    aire_acondicionada = as.factor(aire_acondicionada),
    calefaccion = as.factor(calefaccion),
    diabetes = as.factor(diabetes),
    hipertension = as.factor(hipertension),
    obesidad = as.factor(obesidad),
    victima_de_delito = as.factor(victima_de_delito)
  )

df_ubicaciones <- df_cvnl[,c(1,2,3,4,5)]
df_cvnl <- df_cvnl[,-c(1,2,3,4,5)]
df_cvnl_a <- df_cvnl_a[,-c(1,2,3,4,5)]
```

# PCA


famd <- FAMD(df_cvnl, graph = FALSE, ncp = 5)
eigen_value <- get_eigenvalue(famd)
head(eigen_value)
fviz_screeplot(famd, addlabels = TRUE, ylim = c(0, 50))

if (!is.data.frame(eigen_value)) {
  eigen_value <- as.data.frame(eigen_value)
}

# Calculate cumulative variance
cumulative_variance <- cumsum(eigen_value[, "variance.percent"])

# Plot cumulative variance
plot(cumulative_variance, type = "b", xlab = "Dimensions", ylab = "Cumulative Variance (%)")
abline(h = 80, col = "red", lty = 2)  # Add a horizontal line at 80%

# Scree plot
fviz_screeplot(famd, addlabels = TRUE, ylim = c(0, 50))

fviz_contrib(famd, "var", axes = 1)
fviz_contrib(famd, "var", axes = 2)
fviz_contrib(famd, "var", axes = 3)
fviz_contrib(famd, "var", axes = 4)
fviz_contrib(famd, "var", axes = 5)



df_famd <- data.frame(famd$ind$coord)
fviz_nbclust(df_famd, kmeans, method = "wss")  # Elbow method
fviz_nbclust(df_famd, kmeans, method = "silhouette")  # Silhouette method

# Step 4: Perform K-means Clustering
# Assuming the optimal number of clusters is 3 (adjust based on the previous step)
set.seed(123)  # For reproducibility
kmeans_result <- kmeans(df_famd, centers = 3, nstart = 25)  # Using 4 clusters as an example

# Step 5: Add Cluster Labels to the Data Frame
df_famd$cluster <- as.numeric(kmeans_result$cluster)

# Step 6: Visualize the Clusters
fviz_cluster(kmeans_result, data = df_famd, geom = "point",
             ellipse.type = "convex", palette = "jco", ggtheme = theme_minimal())


# Incorporación de Clusters a DataFrames y Preparación para Mapeo

df_cvnl$cluster <- df_famd$cluster
df_cvnl_a$cluster <- df_famd$cluster
df_ubicaciones$cluster <- df_famd$cluster
df_ubicaciones$Latitud <- as.numeric(df_ubicaciones$Latitud)
df_ubicaciones$Longitud <- as.numeric(df_ubicaciones$Longitud)


# Resumen por Cluster


df_clusters_avg <- df_cvnl_a %>%
  group_by(cluster) %>%
  summarize(across(everything(), ~ round(mean(.), 2)))

df_clusters_avg <- t(df_clusters_avg)



df_ubicaciones <- df_ubicaciones %>%
  mutate(Longitud = ifelse(Orden_Gral_Muestra_MV == 4429, -99.53956, Longitud),
         Latitud = ifelse(Orden_Gral_Muestra_MV == 4429, 26.31641, Latitud))

df_ubicaciones <- df_ubicaciones %>%
  filter(Orden_Gral_Muestra_MV != 3347)

df_ubicaciones <- df_ubicaciones[complete.cases(df_ubicaciones),]
df_sf <- st_as_sf(df_ubicaciones, coords = c("Longitud", "Latitud"), crs = 4326)

ruta_shapefile_nuevo_leon <- "/Users/manuelenriquez/Downloads/19_nuevoleon/conjunto_de_datos/19a.shp"
manzanas <- st_read(ruta_shapefile_nuevo_leon)

# Suponiendo que tienes un shapefile para los municipios de Nuevo León
ruta_shapefile_municipios <- "/Users/manuelenriquez/Downloads/19_nuevoleon/conjunto_de_datos/19mun.shp"
municipios <- st_read(ruta_shapefile_municipios)


```{r}
municipios_amm <- c("Apodaca", "Cadereyta Jiménez", "El Carmen", "García", "San Pedro Garza García",
                    "General Escobedo", "Guadalupe", "Juárez", "Monterrey", "Salinas Victoria",
                    "San Nicolás de los Garza", "Santa Catarina", "Santiago")
claves_amm <- c("006", "009", "010", "018", "019", "021", "026", "031", "039", "045", "046", "048", "049")

# Filtrar df_ubicaciones para incluir solo los municipios de AMM
df_ubicaciones_amm <- df_ubicaciones %>%
  filter(NOM_MUN_MV %in% municipios_amm)

municipios_amm <- municipios %>%
  filter(CVE_MUN %in% claves_amm)

manzanas_amm <- manzanas %>%
  filter(CVE_MUN %in% claves_amm)

df_sf_amm <- st_as_sf(df_ubicaciones_amm, coords = c("Longitud", "Latitud"), crs = 4326)

mapa_nl <- ggplot() +
  # Delimitación de los municipios
  geom_sf(data = municipios, fill = NA, color = "black", size = 0.5) +
  # Puntos de las ubicaciones con los clusters
  geom_sf(data = df_sf, aes(color = factor(cluster)), size = 0.5) +
  # Escala de colores para los clusters
  scale_color_manual(values = c("darkgreen", "gold", "darkred")) +
  # Títulos y etiquetas
  labs(title = "Mapa de AGEBs en Nuevo León por Clusters",
       color = "Cluster") +
  # Estilo del mapa
  theme_minimal() +
  theme(legend.position = "bottom")

mapa_amm <- ggplot() +
  # Delimitación de los municipios
  geom_sf(data = municipios_amm, fill = NA, color = "black",size=0.5) +
  geom_sf(data = manzanas_amm, fill = NA, color = "grey", size = 0.5) +
  # Puntos de las ubicaciones con los clusters
  geom_sf(data = df_sf_amm, aes(color = factor(cluster)), size = 0.5) +
  # Escala de colores para los clusters
  scale_color_manual(values = c("darkgreen", "gold", "darkred")) +
  # Títulos y etiquetas
  labs(title = "Distribución de Clusters en las Manzanas del AMM",
       color = "Cluster") +
  # Estilo del mapa
  theme_minimal() +
  theme(legend.position = "bottom")

mapa_manzana_amm <- ggplot() +
  # Delimitación de los municipios
  geom_sf(data = manzanas_amm, fill = NA, color = "grey", size = 0.5) +
  # Puntos de las ubicaciones con los clusters
  geom_sf(data = df_sf_amm, aes(color = factor(cluster)), size = 1) +
  # Escala de colores para los clusters
  scale_color_manual(values = c("darkgreen", "gold", "darkred")) +
  # Títulos y etiquetas
  labs(title = "Distribución de Clusters en las Manzanas del AMM",
       color = "Cluster") +
  # Estilo del mapa
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("mapa_amm.png", plot = mapa_amm, width = 10, height = 10, units = "in", dpi = 600)
ggsave("mapa_manzana_amm.png", plot = mapa_manzana_amm, width = 10, height = 10, units = "in", dpi = 600)
ggsave("mapa_nl.png", plot = mapa_nl, width = 10, height = 10, units = "in", dpi = 600)
```


```{r}
df_percentages <- df_ubicaciones %>%
  group_by(NOM_MUN_MV, cluster) %>%
  summarise(total_factor = sum(Factor_CVNL, na.rm = TRUE)) %>%
  mutate(total = sum(total_factor)) %>%
  ungroup() %>%
  mutate(percentage = (total_factor / total) * 100) %>%
  select(-total_factor, -total) %>%
  spread(cluster, percentage, fill = 0) %>%
  rename_with(~paste0("Cluster_", .), -NOM_MUN_MV)
```
```{r}
df_ubicaciones2 <- df_ubicaciones[,c()]
df_analisis <- df_ubicaciones %>%
  inner_join(bd_comovamos, by = "Orden_Gral_Muestra_MV")
```

```{r}
preguntas_factores <- c("P69","P70_1","P70_2","P70_3","P70_4","P48","P54_1","P54_2","P54_3","P54_4","P54_5","P54_6","P61","P62","P66","P99","P111","P100_1","P100_2","P100_3","P100_4","P100_5","P100_6","P101_1","P101_2","P101_3","P101_4","P101_5","P143")
df_analisis[preguntas_factores] <- lapply(df_analisis[,preguntas_factores], as.factor)

# Calculate total weighted count for each cluster
total_cluster <- df_analisis %>%
  group_by(cluster) %>%
  summarize(total_factor = sum(Factor_CVNL.x)) %>%
  ungroup()
```

# Preguntas de Migración

```{r}
# Pregunta

levels(df_analisis$P69) <- c(
  "1" = "Ha incrementado y son de otro estado de México.",
  "2" = "Ha incrementado y son de otro país.",
  "3" = "Ha incrementado y son de otro estado de México y de otro país.",
  "4" = "Se ha mantenido igual.",
  "5" = "Ha disminuido.",
  "8888" = "No Sabe",
  "9999" = "No Contesta")

df_69 <- df_analisis %>%
  group_by(cluster, P69) %>%
  summarize(count = sum(Factor_CVNL.x)) %>%
  ungroup()
# Join and calculate percentage
df_69 <- df_69 %>%
  left_join(total_cluster, by = "cluster") %>%
  mutate(percentage = (count / total_factor) * 100) %>%
  arrange(cluster, P69)
# Plotting
ggplot(df_69, aes(x = factor(cluster), y = percentage, fill = P69)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "En los últimos 12 meses, ¿considera que la cantidad de 
       personas migrantes en el estado Nuevo León:",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()

# Pregunta

levels(df_analisis$P70_1) <- c(
  "0" = "En desacuerdo",
  "1" = "De acuerdo",
  "8888" = "No Sabe",
  "9999" = "No Contesta")

df_70_1 <- df_analisis %>%
  group_by(cluster, P70_1) %>%
  summarize(count = sum(Factor_CVNL.x)) %>%
  ungroup()
# Join and calculate percentage
df_70_1 <- df_70_1 %>%
  left_join(total_cluster, by = "cluster") %>%
  mutate(percentage = (count / total_factor) * 100) %>%
  arrange(cluster, P70_1)
# Plotting
ggplot(df_70_1, aes(x = factor(cluster), y = percentage, fill = P70_1)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Mencione si está de acuerdo con las siguientes afirmaciones: 
       México debería admitir a más personas migrantes al país.",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()

# Pregunta 

levels(df_analisis$P70_2) <- c(
  "0" = "En desacuerdo",
  "1" = "De acuerdo",
  "8888" = "No Sabe",
  "9999" = "No Contesta")

df_70_2 <- df_analisis %>%
  group_by(cluster, P70_2) %>%
  summarize(count = sum(Factor_CVNL.x)) %>%
  ungroup()
# Join and calculate percentage
df_70_2 <- df_70_2 %>%
  left_join(total_cluster, by = "cluster") %>%
  mutate(percentage = (count / total_factor) * 100) %>%
  arrange(cluster, P70_2)
# Plotting
ggplot(df_70_2, aes(x = factor(cluster), y = percentage, fill = P70_2)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Mencione si está de acuerdo con las siguientes afirmaciones: 
       A las personas migrantes se les debe dar la oportunidad de que 
       consigan un empleo en Nuevo León.",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()

# Pregunta

levels(df_analisis$P70_3) <- c(
  "0" = "En desacuerdo",
  "1" = "De acuerdo",
  "8888" = "No Sabe",
  "9999" = "No Contesta")

df_70_3 <- df_analisis %>%
  group_by(cluster, P70_3) %>%
  summarize(count = sum(Factor_CVNL.x)) %>%
  ungroup()
# Join and calculate percentage
df_70_3 <- df_70_3 %>%
  left_join(total_cluster, by = "cluster") %>%
  mutate(percentage = (count / total_factor) * 100) %>%
  arrange(cluster, P70_3)
# Plotting
ggplot(df_70_3, aes(x = factor(cluster), y = percentage, fill = P70_3)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Mencione si está de acuerdo con las siguientes afirmaciones: 
       Las personas migrantes deben tener las mismas oportunidades (empleo, 
       educación, programas sociales, etc.) que las personas mexicanas.",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()

# Pregunta

levels(df_analisis$P70_4) <- c(
  "0" = "En desacuerdo",
  "1" = "De acuerdo",
  "8888" = "No Sabe",
  "9999" = "No Contesta")

df_70_4 <- df_analisis %>%
  group_by(cluster, P70_4) %>%
  summarize(count = sum(Factor_CVNL.x)) %>%
  ungroup()
# Join and calculate percentage
df_70_4 <- df_70_4 %>%
  left_join(total_cluster, by = "cluster") %>%
  mutate(percentage = (count / total_factor) * 100) %>%
  arrange(cluster, P70_4)
# Plotting
ggplot(df_70_4, aes(x = factor(cluster), y = percentage, fill = P70_4)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Mencione si está de acuerdo con las siguientes afirmaciones: 
       El aumento de personas migrantes en el estado está relacionado con el 
       incremento de la inseguridad que se ha observado en los últimos años.",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()

```
# Desarrollo Urbano y Ambiental

```{r}
# Pregunta

levels(df_analisis$P48) <- c(
  "1" = "Grandes distancias entre viviendas y servicios",
  "2" = "Falta de áreas verdes o deportivas",
  "3" = "Invasión de áreas naturales/montañas por construcción",
  "4" = "Servicios públicos deficientes",
  "5" = "Desarrollo de construcciones en lugares sin infraestructura",
  "6" = "Viviendas abandonadas",
  "7" = "Obras públicas sin terminar o muy lentas",
  "8" = "Empresas contaminantes en el AMM",
  "9" = "Ninguno",
  "10" = "Empresas contaminantes en el AMM",
  "8888" = "No Sabe",
  "9999" = "No Contesta")
df_48 <- df_analisis %>%
  group_by(cluster, P48) %>%
  summarize(count = sum(Factor_CVNL.x)) %>%
  ungroup()
# Join and calculate percentage
df_48 <- df_48 %>%
  left_join(total_cluster, by = "cluster") %>%
  mutate(percentage = (count / total_factor) * 100) %>%
  arrange(cluster, P48)
# Plotting
ggplot(df_48, aes(x = factor(cluster), y = percentage, fill = P48)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "En su opinión, ¿cuál es el principal problema de 
       desarrollo urbano que se vive en su municipio?",
       x = "Cluster",
       y = "Percentage",
       fill = "Response") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()

# Pregunta

levels(df_analisis$P54_1) <- c(
  "0" = "No",
  "1" = "Sí",
  "8888" = "No Sabe",
  "9999" = "No Contesta")
df_54_1 <- df_analisis %>%
  group_by(cluster, P54_1) %>%
  summarize(count = sum(Factor_CVNL.x)) %>%
  ungroup()
# Join and calculate percentage
df_54_1 <- df_54_1 %>%
  left_join(total_cluster, by = "cluster") %>%
  mutate(percentage = (count / total_factor) * 100) %>%
  arrange(cluster, P54_1)
# Plotting
ggplot(df_54_1, aes(x = factor(cluster), y = percentage, fill = P54_1)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Para combatir el problema de la mala calidad del aire en el AMM, 
  usted estaría de acuerdo en implementar verificación vehicular.",
       x = "Cluster",
       y = "Percentage",
       fill = "Response") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()

# Pregunta

levels(df_analisis$P54_2) <- c(
  "0" = "No",
  "1" = "Sí",
  "8888" = "No Sabe",
  "9999" = "No Contesta")
df_54_2 <- df_analisis %>%
  group_by(cluster, P54_2) %>%
  summarize(count = sum(Factor_CVNL.x)) %>%
  ungroup()
# Join and calculate percentage
df_54_2 <- df_54_2 %>%
  left_join(total_cluster, by = "cluster") %>%
  mutate(percentage = (count / total_factor) * 100) %>%
  arrange(cluster, P54_2)
# Plotting
ggplot(df_54_2, aes(x = factor(cluster), y = percentage, fill = P54_2)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Para combatir el problema de la mala calidad del aire en el AMM, 
  usted estaría de acuerdo en implementar hoy no circula",
       x = "Cluster",
       y = "Percentage",
       fill = "Response") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()

# Pregunta

levels(df_analisis$P54_3) <- c(
  "0" = "No",
  "1" = "Sí",
  "8888" = "No Sabe",
  "9999" = "No Contesta")
df_54_3 <- df_analisis %>%
  group_by(cluster, P54_3) %>%
  summarize(count = sum(Factor_CVNL.x)) %>%
  ungroup()
# Join and calculate percentage
df_54_3 <- df_54_3 %>%
  left_join(total_cluster, by = "cluster") %>%
  mutate(percentage = (count / total_factor) * 100) %>%
  arrange(cluster, P54_3)
# Plotting
ggplot(df_54_3, aes(x = factor(cluster), y = percentage, fill = P54_3)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Para combatir el problema de la mala calidad del aire en el AMM, 
  usted estaría de acuerdo en retirar vehículos contaminantes",
       x = "Cluster",
       y = "Percentage",
       fill = "Response") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()

# Pregunta

levels(df_analisis$P54_4) <- c(
  "0" = "No",
  "1" = "Sí",
  "8888" = "No Sabe",
  "9999" = "No Contesta")
df_54_4 <- df_analisis %>%
  group_by(cluster, P54_4) %>%
  summarize(count = sum(Factor_CVNL.x)) %>%
  ungroup()
# Join and calculate percentage
df_54_4 <- df_54_4 %>%
  left_join(total_cluster, by = "cluster") %>%
  mutate(percentage = (count / total_factor) * 100) %>%
  arrange(cluster, P54_4)
# Plotting
ggplot(df_54_4, aes(x = factor(cluster), y = percentage, fill = P54_4)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Para combatir el problema de la mala calidad del aire en el AMM, 
  usted estaría de acuerdo en invertir más en transporte público y menos
  infraestructura para el automóvil.",
       x = "Cluster",
       y = "Percentage",
       fill = "Response") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()

# Pregunta

levels(df_analisis$P54_5) <- c(
  "0" = "No",
  "1" = "Sí",
  "8888" = "No Sabe",
  "9999" = "No Contesta")
df_54_5 <- df_analisis %>%
  group_by(cluster, P54_5) %>%
  summarize(count = sum(Factor_CVNL.x)) %>%
  ungroup()
# Join and calculate percentage
df_54_5 <- df_54_5 %>%
  left_join(total_cluster, by = "cluster") %>%
  mutate(percentage = (count / total_factor) * 100) %>%
  arrange(cluster, P54_5)
# Plotting
ggplot(df_54_5, aes(x = factor(cluster), y = percentage, fill = P54_5)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Para combatir el problema de la mala calidad del aire en el AMM, 
  usted estaría de acuerdo en sancionar industrias que contaminen, aunque
  implique su clausura.",
       x = "Cluster",
       y = "Percentage",
       fill = "Response") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()

# Pregunta

levels(df_analisis$P54_6) <- c(
  "0" = "No",
  "1" = "Sí",
  "8888" = "No Sabe",
  "9999" = "No Contesta")
df_54_6 <- df_analisis %>%
  group_by(cluster, P54_6) %>%
  summarize(count = sum(Factor_CVNL.x)) %>%
  ungroup()
# Join and calculate percentage
df_54_6 <- df_54_6 %>%
  left_join(total_cluster, by = "cluster") %>%
  mutate(percentage = (count / total_factor) * 100) %>%
  arrange(cluster, P54_6)
# Plotting
ggplot(df_54_6, aes(x = factor(cluster), y = percentage, fill = P54_6)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Para combatir el problema de la mala calidad del aire en el AMM, 
  usted estaría de acuerdo en que se reduzca el espacio para el auto y se destine
  un carril exclusivo para personas que van en bicileta.",
       x = "Cluster",
       y = "Percentage",
       fill = "Response") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()

# Pregunta

levels(df_analisis$P61) <- c(
  "1" = "En el Río Santa Catarina deberían realizar obras públicas",
  "2" = "El Río Santa Catarina debe preservar su ecosistema",
  "8888" = "No Sabe",
  "9999" = "No Contesta")
df_61 <- df_analisis %>%
  group_by(cluster, P61) %>%
  summarize(count = sum(Factor_CVNL.x)) %>%
  ungroup()
# Join and calculate percentage
df_61 <- df_61 %>%
  left_join(total_cluster, by = "cluster") %>%
  mutate(percentage = (count / total_factor) * 100) %>%
  arrange(cluster, P61)
# Plotting
ggplot(df_61, aes(x = factor(cluster), y = percentage, fill = P61)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Con respecto al Río Santa Catarina, ¿cuál de las 
       siguientes dos opciones refleja mejor su opinión?",
       x = "Cluster",
       y = "Percentage",
       fill = "Response") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()

# Pregunta 

levels(df_analisis$P62) <- c(
  "1" = "Contaminación del aire",
  "2" = "Contaminación de ríos y arroyos",
  "3" = "Falta de árboles, áreas verdes o mal estado",
  "4" = "Basura o Escombros",
  "5" = "Escasez o Desperdicio de Agua",
  "6" = "Falta de Reciclaje",
  "7" = "Cambio o Crisis Climática",
  "8" = "Contaminación Auditiva",
  "9" = "Ninguno",
  "10" = "Todas las Anteriores",
  "8888" = "No Sabe",
  "9999" = "No Contesta")
df_62 <- df_analisis %>%
  group_by(cluster, P62) %>%
  summarize(count = sum(Factor_CVNL.x)) %>%
  ungroup()
# Join and calculate percentage
df_62 <- df_62 %>%
  left_join(total_cluster, by = "cluster") %>%
  mutate(percentage = (count / total_factor) * 100) %>%
  arrange(cluster, P62)
# Plotting
ggplot(df_62, aes(x = factor(cluster), y = percentage, fill = P62)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "De acuerdo con su percepción, mencione el principal problema 
       ambiental que se vive en su municipio.",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()
```

# Desarrollo Económico

```{r}
# Pregunta

levels(df_analisis$P66) <- c(
  "1" = "Las empresas extranjeras contribuirán a la prosperidad económica de la ciudad.",
  "2" = "Las empresas extranjeras pueden traer problemas ambientales o de desarrollo urbano.",
  "8888" = "No Sabe",
  "9999" = "No Contesta")

df_66 <- df_analisis %>%
  group_by(cluster, P66) %>%
  summarize(count = sum(Factor_CVNL.x)) %>%
  ungroup()
# Join and calculate percentage
df_66 <- df_66 %>%
  left_join(total_cluster, by = "cluster") %>%
  mutate(percentage = (count / total_factor) * 100) %>%
  arrange(cluster, P66)
# Plotting
ggplot(df_66, aes(x = factor(cluster), y = percentage, fill = P66)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "¿Cuál de las siguientes afirmaciones refleja mejorsu perspectiva 
  ante la llegada de empresas extranjeras (como Tesla) al AMM?",
       x = "Cluster",
       y = "Percentage",
       fill = "Response") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()
```
# Seguridad

```{r}
levels(df_analisis$P99) <- c(
  "1" = "Muy Probable",
  "2" = "Probable",
  "3" = "Poco Probable",
  "4" = "Improbable",
  "8888" = "No Sabe",
  "9999" = "No Contesta")

df_99 <- df_analisis %>%
  group_by(cluster, P99) %>%
  summarize(count = sum(Factor_CVNL.x)) %>%
  ungroup()
# Join and calculate percentage
df_99 <- df_99 %>%
  left_join(total_cluster, by = "cluster") %>%
  mutate(percentage = (count / total_factor) * 100) %>%
  arrange(cluster, P99)
# Plotting
ggplot(df_99, aes(x = factor(cluster), y = percentage, fill = P99)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "¿Qué tan probable considera que los delitos denunciados sean 
       investigados y castigados por las autoridades?",
       x = "Cluster",
       y = "Percentage",
       fill = "Response") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()

# Pregunta

levels(df_analisis$P100_1) <- c(
  "0" = "No",
  "1" = "Sí",
  "8888" = "No Sabe",
  "9999" = "No Contesta")
df_100_1 <- df_analisis %>%
  group_by(cluster, P100_1) %>%
  summarize(count = sum(Factor_CVNL.x)) %>%
  ungroup()
# Join and calculate percentage
df_100_1 <- df_100_1 %>%
  left_join(total_cluster, by = "cluster") %>%
  mutate(percentage = (count / total_factor) * 100) %>%
  arrange(cluster, P100_1)
# Plotting
ggplot(df_100_1, aes(x = factor(cluster), y = percentage, fill = P100_1)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "En el tema de seguridad considera que:
       ¿La presencia de policías en su colonia es suficiente?",
       x = "Cluster",
       y = "Percentage",
       fill = "Response") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()

# Pregunta 

levels(df_analisis$P100_2) <- c(
  "0" = "No",
  "1" = "Sí",
  "8888" = "No Sabe",
  "9999" = "No Contesta")
df_100_2 <- df_analisis %>%
  group_by(cluster, P100_2) %>%
  summarize(count = sum(Factor_CVNL.x)) %>%
  ungroup()
# Join and calculate percentage
df_100_2 <- df_100_2 %>%
  left_join(total_cluster, by = "cluster") %>%
  mutate(percentage = (count / total_factor) * 100) %>%
  arrange(cluster, P100_2)
# Plotting
ggplot(df_100_2, aes(x = factor(cluster), y = percentage, fill = P100_2)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "En el tema de seguridad considera que:
       ¿La policía de su colonia lo hace sentir más seguro?",
       x = "Cluster",
       y = "Percentage",
       fill = "Response") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()

# Pregunta

levels(df_analisis$P100_3) <- c(
  "0" = "No",
  "1" = "Sí",
  "8888" = "No Sabe",
  "9999" = "No Contesta")
df_100_3 <- df_analisis %>%
  group_by(cluster, P100_3) %>%
  summarize(count = sum(Factor_CVNL.x)) %>%
  ungroup()
# Join and calculate percentage
df_100_3 <- df_100_3 %>%
  left_join(total_cluster, by = "cluster") %>%
  mutate(percentage = (count / total_factor) * 100) %>%
  arrange(cluster, P100_3)
# Plotting
ggplot(df_100_3, aes(x = factor(cluster), y = percentage, fill = P100_3)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "En el tema de seguridad considera que:
       ¿Confía en la policía de su colonia?",
       x = "Cluster",
       y = "Percentage",
       fill = "Response") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()

# Pregunta

levels(df_analisis$P100_4) <- c(
  "0" = "No",
  "1" = "Sí",
  "8888" = "No Sabe",
  "9999" = "No Contesta")
df_100_4 <- df_analisis %>%
  group_by(cluster, P100_4) %>%
  summarize(count = sum(Factor_CVNL.x)) %>%
  ungroup()
# Join and calculate percentage
df_100_4 <- df_100_4 %>%
  left_join(total_cluster, by = "cluster") %>%
  mutate(percentage = (count / total_factor) * 100) %>%
  arrange(cluster, P100_4)
# Plotting
ggplot(df_100_4, aes(x = factor(cluster), y = percentage, fill = P100_4)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "En el tema de seguridad considera que:
       ¿El trato del policía al ciudadano es de forma respetuosa?",
       x = "Cluster",
       y = "Percentage",
       fill = "Response") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()

# Pregunta

levels(df_analisis$P100_5) <- c(
  "0" = "No",
  "1" = "Sí",
  "8888" = "No Sabe",
  "9999" = "No Contesta")
df_100_5 <- df_analisis %>%
  group_by(cluster, P100_5) %>%
  summarize(count = sum(Factor_CVNL.x)) %>%
  ungroup()
# Join and calculate percentage
df_100_5 <- df_100_5 %>%
  left_join(total_cluster, by = "cluster") %>%
  mutate(percentage = (count / total_factor) * 100) %>%
  arrange(cluster, P100_5)
# Plotting
ggplot(df_100_5, aes(x = factor(cluster), y = percentage, fill = P100_5)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "En el tema de seguridad considera que:
       ¿Considera que las decisiones y actuar de la policía 
       benefician a las personas en su colonia?",
       x = "Cluster",
       y = "Percentage",
       fill = "Response") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()

# Pregunta 

levels(df_analisis$P100_6) <- c(
  "0" = "No",
  "1" = "Sí",
  "8888" = "No Sabe",
  "9999" = "No Contesta")
df_100_6 <- df_analisis %>%
  group_by(cluster, P100_6) %>%
  summarize(count = sum(Factor_CVNL.x)) %>%
  ungroup()
# Join and calculate percentage
df_100_6 <- df_100_6 %>%
  left_join(total_cluster, by = "cluster") %>%
  mutate(percentage = (count / total_factor) * 100) %>%
  arrange(cluster, P100_6)
# Plotting
ggplot(df_100_6, aes(x = factor(cluster), y = percentage, fill = P100_6)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "En el tema de seguridad considera que:
       ¿La policía es justa al tomar decisiones?",
       x = "Cluster",
       y = "Percentage",
       fill = "Response") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()

# Pregunta 

levels(df_analisis$P101_1) <- c(
  "1" = "1",
  "2" = "2",
  "3" = "3",
  "4" = "4",
  "5" = "5",
  "6" = "6",
  "7" = "7",
  "8" = "8",
  "9" = "9",
  "10" = "10",
  "8888" = "No Sabe",
  "9999" = "No Contesta")
df_101_1 <- df_analisis %>%
  group_by(cluster, P101_1) %>%
  summarize(count = sum(Factor_CVNL.x)) %>%
  ungroup()
# Join and calculate percentage
df_101_1 <- df_101_1 %>%
  left_join(total_cluster, by = "cluster") %>%
  mutate(percentage = (count / total_factor) * 100) %>%
  arrange(cluster, P101_1)
# Plotting
ggplot(df_101_1, aes(x = factor(cluster), y = percentage, fill = P101_1)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "¿Cómo evalúa el desempeño de los servicios de seguridad? En una 
  escala del 1 al 10, en donde 1 es muy malo y 10 muy bueno, Policía de su 
  Municipio y Barrio",
       x = "Cluster",
       y = "Percentage",
       fill = "Response") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()

# Pregunta 

levels(df_analisis$P101_2) <- c(
  "1" = "1",
  "2" = "2",
  "3" = "3",
  "4" = "4",
  "5" = "5",
  "6" = "6",
  "7" = "7",
  "8" = "8",
  "9" = "9",
  "10" = "10",
  "8888" = "No Sabe",
  "9999" = "No Contesta")
df_101_2 <- df_analisis %>%
  group_by(cluster, P101_2) %>%
  summarize(count = sum(Factor_CVNL.x)) %>%
  ungroup()
# Join and calculate percentage
df_101_2 <- df_101_2 %>%
  left_join(total_cluster, by = "cluster") %>%
  mutate(percentage = (count / total_factor) * 100) %>%
  arrange(cluster, P101_2)
# Plotting
ggplot(df_101_2, aes(x = factor(cluster), y = percentage, fill = P101_2)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "¿Cómo evalúa el desempeño de los servicios de seguridad? En una 
  escala del 1 al 10, en donde 1 es muy malo y 10 muy bueno, Fuerza Civil",
       x = "Cluster",
       y = "Percentage",
       fill = "Response") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()

# Pregunta

levels(df_analisis$P101_3) <- c(
  "1" = "1",
  "2" = "2",
  "3" = "3",
  "4" = "4",
  "5" = "5",
  "6" = "6",
  "7" = "7",
  "8" = "8",
  "9" = "9",
  "10" = "10",
  "8888" = "No Sabe",
  "9999" = "No Contesta")
df_101_3 <- df_analisis %>%
  group_by(cluster, P101_3) %>%
  summarize(count = sum(Factor_CVNL.x)) %>%
  ungroup()
# Join and calculate percentage
df_101_3 <- df_101_3 %>%
  left_join(total_cluster, by = "cluster") %>%
  mutate(percentage = (count / total_factor) * 100) %>%
  arrange(cluster, P101_3)
# Plotting
ggplot(df_101_3, aes(x = factor(cluster), y = percentage, fill = P101_3)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "¿Cómo evalúa el desempeño de los servicios de seguridad? En una 
  escala del 1 al 10, en donde 1 es muy malo y 10 muy bueno, Guardia Nacional",
       x = "Cluster",
       y = "Percentage",
       fill = "Response") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()

# Pregunta

levels(df_analisis$P101_4) <- c(
  "1" = "1",
  "2" = "2",
  "3" = "3",
  "4" = "4",
  "5" = "5",
  "6" = "6",
  "7" = "7",
  "8" = "8",
  "9" = "9",
  "10" = "10",
  "8888" = "No Sabe",
  "9999" = "No Contesta")
df_101_4 <- df_analisis %>%
  group_by(cluster, P101_4) %>%
  summarize(count = sum(Factor_CVNL.x)) %>%
  ungroup()
# Join and calculate percentage
df_101_4 <- df_101_4 %>%
  left_join(total_cluster, by = "cluster") %>%
  mutate(percentage = (count / total_factor) * 100) %>%
  arrange(cluster, P101_4)
# Plotting
ggplot(df_101_4, aes(x = factor(cluster), y = percentage, fill = P101_4)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "¿Cómo evalúa el desempeño de los servicios de seguridad? En una 
  escala del 1 al 10, en donde 1 es muy malo y 10 muy bueno, Ejército y Marina",
       x = "Cluster",
       y = "Percentage",
       fill = "Response") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()

# Pregunta

levels(df_analisis$P101_5) <- c(
  "1" = "1",
  "2" = "2",
  "3" = "3",
  "4" = "4",
  "5" = "5",
  "6" = "6",
  "7" = "7",
  "8" = "8",
  "9" = "9",
  "10" = "10",
  "8888" = "No Sabe",
  "9999" = "No Contesta")
df_101_5 <- df_analisis %>%
  group_by(cluster, P101_5) %>%
  summarize(count = sum(Factor_CVNL.x)) %>%
  ungroup()
# Join and calculate percentage
df_101_5 <- df_101_5 %>%
  left_join(total_cluster, by = "cluster") %>%
  mutate(percentage = (count / total_factor) * 100) %>%
  arrange(cluster, P101_5)
# Plotting
ggplot(df_101_5, aes(x = factor(cluster), y = percentage, fill = P101_5)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "¿Cómo evalúa el desempeño de los servicios de seguridad? En una 
  escala del 1 al 10, en donde 1 es muy malo y 10 muy bueno, Tránsito",
       x = "Cluster",
       y = "Percentage",
       fill = "Response") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()

# Pregunta

levels(df_analisis$P111) <- c(
  "1" = "Delitos de alto impacto.",
  "2" = "Robos patrimoniales.",
  "3" = "Delincuencia organizada.",
  "4" = "Violencia familia y/o contra la mujer.",
  "5" = "Violencia en la colonia",
  "7" = "Ninguno",
  "8" = "Todas las anteriores",
  "8888" = "No Sabe",
  "9999" = "No Contesta")

df_111 <- df_analisis %>%
  group_by(cluster, P111) %>%
  summarize(count = sum(Factor_CVNL.x)) %>%
  ungroup()
# Join and calculate percentage
df_111 <- df_111 %>%
  left_join(total_cluster, by = "cluster") %>%
  mutate(percentage = (count / total_factor) * 100) %>%
  arrange(cluster, P111)
# Plotting
ggplot(df_111, aes(x = factor(cluster), y = percentage, fill = P111)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "De acuerdo con su percepción, ¿Mencione cuál es el principal 
       problema de seguridad que se vive en su municipio?",
       x = "Cluster",
       y = "Percentage",
       fill = "Response") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()
```
# Principal Problema
```{r}
levels(df_analisis$P143) <- c(
  "1" = "Movilidad",
  "2" = "Desarrollo Urbano",
  "3" = "Medio Ambiente",
  "4" = "Desarrollo Social",
  "5" = "Seguridad",
  "6" = "Gobierno Eficiente",
  "9999" = "No Contesta")

df_143 <- df_analisis %>%
  group_by(cluster, P143) %>%
  summarize(count = sum(Factor_CVNL.x)) %>%
  ungroup()
# Join and calculate percentage
df_143 <- df_143 %>%
  left_join(total_cluster, by = "cluster") %>%
  mutate(percentage = (count / total_factor) * 100) %>%
  arrange(cluster, P143)
# Plotting
ggplot(df_143, aes(x = factor(cluster), y = percentage, fill = P143)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Para concluir: ¿En qué ámbito está el principal problema 
       que enfrenta actualmente el estado?",
       x = "Cluster",
       y = "Percentage",
       fill = "Response") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()
```

```{r}
#PREGUNTA P67

# Convertir P67 a factor con niveles adecuados
df_analisis$P67 <- as.factor(df_analisis$P67)

# Definir niveles con etiquetas significativas
levels(df_analisis$P67) <- c(
  "0" = "No",
  "1" = "Si",
  "8888" = "No Sabe",
  "9999" = "No Contestó"
)
# Crear la data que usaremos
df_P67 <- df_analisis %>%
  group_by(cluster, P67) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(cluster, P67)

# Crear el gráfico con ggplot2
ggplot(df_P67, aes(x = factor(cluster), y = percentage, fill = P67)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "¿Considera que las mujeres tienen las mismas oportunidades laborales, 
       políticas y sociales que los hombres?",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(labels = c(
    "0" = "No",
    "1" = "Si",
    "8888" = "No Sabe",
    "9999" = "No Contestó"
  )) +
  theme_minimal()


# Convertir P106 a factor con niveles adecuados
df_analisis$P106 <- as.factor(df_analisis$P106)

# Definir niveles con etiquetas significativas
levels(df_analisis$P106) <- c(
  "1" = "Nunca ocurre.",
  "2" = "Casi no ocurre.",
  "3" = "Ocurre con poca frecuencia.",
  "4" = "Ocurre frecuentemente.",
  "5" = "Siempre ocurre.",
  "8888" = "No sabe",
  "9999" = "No contesta"
)
# Crear la data que usaremos
df_106 <- df_analisis %>%
  group_by(cluster, P106) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(cluster, P106)

# Crear el gráfico con ggplot2
ggplot(df_106, aes(x = factor(cluster), y = percentage, fill = P106)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "¿Considera que la violencia hacia las mujeres en la vía pública:",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(labels = c(
    "1" = "Nunca ocurre",
    "2" = "Casi no ocurre",
    "3" = "Ocurre con poca frecuencia",
    "4" = "Ocurre frecuentemente",
    "5" = "Siempre ocurre",
    "8888" = "No sabe",
    "9999" = "No contesta"
  )) +
  theme_minimal()

#P 107_1

# Convertir P107 a factor con niveles adecuados
df_analisis$P107_1 <- as.factor(df_analisis$P107_1)

# Definir niveles con etiquetas significativas
levels(df_analisis$P107_1) <- c(
  "0" = "No",
  "1" = "Si",
  "8888" = "No Sabe",
  "9999" = "No Contestó"
)
# Crear la data que usaremos
df_107_1 <- df_analisis %>%
  group_by(cluster, P107_1) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(cluster, P107_1)

# Crear el gráfico con ggplot2
ggplot(df_107_1, aes(x = factor(cluster), y = percentage, fill = P107_1)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Los piropos en la calle / transporte público hacia las mujeres son ofensivos.",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(labels = c(
    "0" = "No",
    "1" = "Si",
    "8888" = "No Sabe",
    "9999" = "No Contestó"
  )) +
  theme_minimal()

#P 107_2

# Convertir P107_2 a factor con niveles adecuados
df_analisis$P107_2 <- as.factor(df_analisis$P107_2)

# Definir niveles con etiquetas significativas
levels(df_analisis$P107_2) <- c(
  "0" = "No",
  "1" = "Si",
  "8888" = "No Sabe",
  "9999" = "No Contestó"
)
# Crear la data que usaremos
df_107_2 <- df_analisis %>%
  group_by(cluster, P107_2) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(cluster, P107_2)

# Crear el gráfico con ggplot2
ggplot(df_107_2, aes(x = factor(cluster), y = percentage, fill = P107_2)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Las mujeres tienen un mayor riesgo de ser agredidas que los hombres",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(labels = c(
    "0" = "No",
    "1" = "Si",
    "8888" = "No Sabe",
    "9999" = "No Contestó"
  )) +
  theme_minimal()

#P 107_3

# Convertir P107_3 a factor con niveles adecuados
df_analisis$P107_3 <- as.factor(df_analisis$P107_3)

# Definir niveles con etiquetas significativas
levels(df_analisis$P107_3) <- c(
  "0" = "No",
  "1" = "Si",
  "8888" = "No Sabe",
  "9999" = "No Contestó"
)
# Crear la data que usaremos
df_107_3 <- df_analisis %>%
  group_by(cluster, P107_3) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(cluster, P107_3)

# Crear el gráfico con ggplot2
ggplot(df_107_3, aes(x = factor(cluster), y = percentage, fill = P107_3)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Las mujeres comparten la responsabilidad con sus victimarios de ser agredidas por su forma de vestir",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(labels = c(
    "0" = "No",
    "1" = "Si",
    "8888" = "No Sabe",
    "9999" = "No Contestó"
  )) +
  theme_minimal()


```

# Discriminación

```{r}
#P 68_1

# Convertir P68_1 a factor con niveles adecuados
df_analisis$P68_1 <- as.factor(df_analisis$P68_1)

# Definir niveles con etiquetas significativas
levels(df_analisis$P68_1) <- c(
  "0" = "No",
  "1" = "Si",
  "8888" = "No Sabe",
  "9999" = "No Contestó"
)
# Crear la data que usaremos
df_68_1 <- df_analisis %>%
  group_by(cluster, P68_1) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(cluster, P68_1)

# Crear el gráfico con ggplot2
ggplot(df_68_1, aes(x = factor(cluster), y = percentage, fill = P68_1)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "¿Cree que en Nuevo León se discrimina a la gente por ser mujer?",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(labels = c(
    "0" = "No",
    "1" = "Si",
    "8888" = "No Sabe",
    "9999" = "No Contestó"
  )) +
  theme_minimal()

#P 68_2

# Convertir P68_2 a factor con niveles adecuados
df_analisis$P68_2 <- as.factor(df_analisis$P68_2)

# Definir niveles con etiquetas significativas
levels(df_analisis$P68_2) <- c(
  "0" = "No",
  "1" = "Si",
  "8888" = "No Sabe",
  "9999" = "No Contestó"
)
# Crear la data que usaremos
df_68_2 <- df_analisis %>%
  group_by(cluster, P68_2) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(cluster, P68_2)

# Crear el gráfico con ggplot2
ggplot(df_68_2, aes(x = factor(cluster), y = percentage, fill = P68_2)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "¿Cree que en Nuevo León se discrimina a la 
       gente por estar en situación de calle?",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(labels = c(
    "0" = "No",
    "1" = "Si",
    "8888" = "No Sabe",
    "9999" = "No Contestó"
  )) +
  theme_minimal()

#P 68_3

# Convertir a factor
df_analisis$P68_3 <- as.factor(df_analisis$P68_3)

# Definir niveles con etiquetas significativas
levels(df_analisis$P68_3) <- c(
  "0" = "No",
  "1" = "Si",
  "8888" = "No Sabe",
  "9999" = "No Contestó"
)
# Crear la data que usaremos
df_68_3 <- df_analisis %>%
  group_by(cluster, P68_3) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(cluster, P68_3)

# Crear el gráfico con ggplot2
ggplot(df_68_3, aes(x = factor(cluster), y = percentage, fill = P68_3)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "¿Cree que en Nuevo León se discrimina a la gente 
       por su embarazo?",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(labels = c(
    "0" = "No",
    "1" = "Si",
    "8888" = "No Sabe",
    "9999" = "No Contestó"
  )) +
  theme(plot.title = element_text(size=10))

#P 68_4

# Convertir a factor
df_analisis$P68_4 <- as.factor(df_analisis$P68_4)

# Definir niveles con etiquetas significativas
levels(df_analisis$P68_4) <- c(
  "0" = "No",
  "1" = "Si",
  "8888" = "No Sabe",
  "9999" = "No Contestó"
)
# Crear la data que usaremos
df_68_4 <- df_analisis %>%
  group_by(cluster, P68_4) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(cluster, P68_4)

# Crear el gráfico con ggplot2
ggplot(df_68_4, aes(x = factor(cluster), y = percentage, fill = P68_4)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "¿Cree que en Nuevo León se discrimina a la gente por su edad?",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(labels = c(
    "0" = "No",
    "1" = "Si",
    "8888" = "No Sabe",
    "9999" = "No Contestó"
  )) +
  theme_minimal()

#P 68_5

# Convertir a factor
df_analisis$P68_5 <- as.factor(df_analisis$P68_5)

# Definir niveles con etiquetas significativas
levels(df_analisis$P68_5) <- c(
  "0" = "No",
  "1" = "Si",
  "8888" = "No Sabe",
  "9999" = "No Contestó"
)
# Crear la data que usaremos
df_68_5 <- df_analisis %>%
  group_by(cluster, P68_5) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(cluster, P68_5)

# Crear el gráfico con ggplot2
ggplot(df_68_5, aes(x = factor(cluster), y = percentage, fill = P68_5)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "¿Cree que en Nuevo León se discrimina a la gente por 
       su nivel económico?",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(labels = c(
    "0" = "No",
    "1" = "Si",
    "8888" = "No Sabe",
    "9999" = "No Contestó"
  )) +
  theme_minimal()

#P 68_6

# Convertir a factor
df_analisis$P68_6 <- as.factor(df_analisis$P68_6)

# Definir niveles con etiquetas significativas
levels(df_analisis$P68_6) <- c(
  "0" = "No",
  "1" = "Si",
  "8888" = "No Sabe",
  "9999" = "No Contestó"
)
# Crear la data que usaremos
df_68_6 <- df_analisis %>%
  group_by(cluster, P68_6) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(cluster, P68_6)

# Crear el gráfico con ggplot2
ggplot(df_68_6, aes(x = factor(cluster), y = percentage, fill = P68_6)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "¿Cree que en Nuevo León se discrimina a la gente por su 
       origen étnico (indígena, afrodescendiente, etc)?",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(labels = c(
    "0" = "No",
    "1" = "Si",
    "8888" = "No Sabe",
    "9999" = "No Contestó"
  )) +
  theme_minimal()

#P 68_7

# Convertir a factor
df_analisis$P68_7 <- as.factor(df_analisis$P68_7)

# Definir niveles con etiquetas significativas
levels(df_analisis$P68_7) <- c(
  "0" = "No",
  "1" = "Si",
  "8888" = "No Sabe",
  "9999" = "No Contestó"
)
# Crear la data que usaremos
df_68_7 <- df_analisis %>%
  group_by(cluster, P68_7) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(cluster, P68_7)

# Crear el gráfico con ggplot2
ggplot(df_68_7, aes(x = factor(cluster), y = percentage, fill = P68_7)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "¿Cree que en Nuevo León se discrimina a la gente por ser 
       persona con discapacidad?",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(labels = c(
    "0" = "No",
    "1" = "Si",
    "8888" = "No Sabe",
    "9999" = "No Contestó"
  )) +
  theme_minimal()

#P 68_8

# Convertir a factor
df_analisis$P68_8 <- as.factor(df_analisis$P68_8)

# Definir niveles con etiquetas significativas
levels(df_analisis$P68_8) <- c(
  "0" = "No",
  "1" = "Si",
  "8888" = "No Sabe",
  "9999" = "No Contestó"
)
# Crear la data que usaremos
df_68_8 <- df_analisis %>%
  group_by(cluster, P68_8) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(cluster, P68_8)

# Crear el gráfico con ggplot2
ggplot(df_68_8, aes(x = factor(cluster), y = percentage, fill = P68_8)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "¿Cree que en Nuevo León se discrimina a la gente 
       por su orientación sexual?",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(labels = c(
    "0" = "No",
    "1" = "Si",
    "8888" = "No Sabe",
    "9999" = "No Contestó"
  )) +
  theme_minimal()

#P 68_9

# Convertir a factor
df_analisis$P68_9 <- as.factor(df_analisis$P68_9)

# Definir niveles con etiquetas significativas
levels(df_analisis$P68_9) <- c(
  "0" = "No",
  "1" = "Si",
  "8888" = "No Sabe",
  "9999" = "No Contestó"
)
# Crear la data que usaremos
df_68_9 <- df_analisis %>%
  group_by(cluster, P68_9) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(cluster, P68_9)

# Crear el gráfico con ggplot2
ggplot(df_68_9, aes(x = factor(cluster), y = percentage, fill = P68_9)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "¿Cree que en Nuevo León se discrimina a la 
       gente por su color de piel?",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(labels = c(
    "0" = "No",
    "1" = "Si",
    "8888" = "No Sabe",
    "9999" = "No Contestó"
  )) +
  theme(plot.title = element_text(size=6.5))


#P 68_10

# Convertir a factor
df_analisis$P68_10 <- as.factor(df_analisis$P68_10)

# Definir niveles con etiquetas significativas
levels(df_analisis$P68_10) <- c(
  "0" = "No",
  "1" = "Si",
  "8888" = "No Sabe",
  "9999" = "No Contestó"
)
# Crear la data que usaremos
df_68_10 <- df_analisis %>%
  group_by(cluster, P68_10) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(cluster, P68_10)

# Crear el gráfico con ggplot2
ggplot(df_68_10, aes(x = factor(cluster), y = percentage, fill = P68_10)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "¿Cree que en Nuevo León se discrimina a la gente por su forma 
       de vestir o arreglo personal (tatuajes, ropa, 
       forma de peinarse, perforaciones)?",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(labels = c(
    "0" = "No",
    "1" = "Si",
    "8888" = "No Sabe",
    "9999" = "No Contestó"
  )) +
  theme(plot.title = element_text(size=10))

#P 68_11

# Convertir a factor
df_analisis$P68_11 <- as.factor(df_analisis$P68_11)

# Definir niveles con etiquetas significativas
levels(df_analisis$P68_11) <- c(
  "0" = "No",
  "1" = "Si",
  "8888" = "No Sabe",
  "9999" = "No Contestó"
)
# Crear la data que usaremos
df_68_11 <- df_analisis %>%
  group_by(cluster, P68_11) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(cluster, P68_11)

# Crear el gráfico con ggplot2
ggplot(df_68_11, aes(x = factor(cluster), y = percentage, fill = P68_11)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "¿Cree que en Nuevo León se discrimina a la gente por 
       tener sobrepeso u obesidad?",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(labels = c(
    "0" = "No",
    "1" = "Si",
    "8888" = "No Sabe",
    "9999" = "No Contestó"
  )) +
  theme_minimal()

#P 68_12

# Convertir a factor
df_analisis$P68_12 <- as.factor(df_analisis$P68_12)

# Definir niveles con etiquetas significativas
levels(df_analisis$P68_12) <- c(
  "0" = "No",
  "1" = "Si",
  "8888" = "No Sabe",
  "9999" = "No Contestó"
)
# Crear la data que usaremos
df_68_12 <- df_analisis %>%
  group_by(cluster, P68_12) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(cluster, P68_12)

# Crear el gráfico con ggplot2
ggplot(df_68_12, aes(x = factor(cluster), y = percentage, fill = P68_12)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "¿Cree que en Nuevo León se discrimina a la gente por 
       tener algún tipo de enfermedad?",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(labels = c(
    "0" = "No",
    "1" = "Si",
    "8888" = "No Sabe",
    "9999" = "No Contestó"
  )) +
  theme_minimal()

#P 68_13

# Convertir a factor
df_analisis$P68_13 <- as.factor(df_analisis$P68_13)

# Definir niveles con etiquetas significativas
levels(df_analisis$P68_13) <- c(
  "0" = "No",
  "1" = "Si",
  "8888" = "No Sabe",
  "9999" = "No Contestó"
)
# Crear la data que usaremos
df_68_13 <- df_analisis %>%
  group_by(cluster, P68_13) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(cluster, P68_13)

# Crear el gráfico con ggplot2
ggplot(df_68_13, aes(x = factor(cluster), y = percentage, fill = P68_13)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "¿Cree que en Nuevo León se discrimina a la 
       gente por ser migrante?",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(labels = c(
    "0" = "No",
    "1" = "Si",
    "8888" = "No Sabe",
    "9999" = "No Contestó"
  )) +
  theme_minimal()

#P 68_14

# Convertir a factor
df_analisis$P68_14 <- as.factor(df_analisis$P68_14)

# Definir niveles con etiquetas significativas
levels(df_analisis$P68_14) <- c(
  "0" = "No",
  "1" = "Si",
  "8888" = "No Sabe",
  "9999" = "No Contestó"
)
# Crear la data que usaremos
df_68_14 <- df_analisis %>%
  group_by(cluster, P68_14) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(cluster, P68_14)

# Crear el gráfico con ggplot2
ggplot(df_68_14, aes(x = factor(cluster), y = percentage, fill = P68_14)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "¿Cree que en Nuevo León se discrimina a la gente por su identidad 
       o expresión de género (personas trans y no binarias)?",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(labels = c(
    "0" = "No",
    "1" = "Si",
    "8888" = "No Sabe",
    "9999" = "No Contestó"
  )) +
  theme_minimal()

#P 68_15

# Convertir a factor
df_analisis$P68_15 <- as.factor(df_analisis$P68_15)

# Definir niveles con etiquetas significativas
levels(df_analisis$P68_15) <- c(
  "0" = "No",
  "1" = "Si",
  "8888" = "No Sabe",
  "9999" = "No Contestó"
)
# Crear la data que usaremos
df_68_15 <- df_analisis %>%
  group_by(cluster, P68_15) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(cluster, P68_15)

# Crear el gráfico con ggplot2
ggplot(df_68_15, aes(x = factor(cluster), y = percentage, fill = P68_15)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "¿Cree que en Nuevo León se discrimina a la gente por haber estado en la cárcel?",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(labels = c(
    "0" = "No",
    "1" = "Si",
    "8888" = "No Sabe",
    "9999" = "No Contestó"
  )) +
  theme_minimal()
```

# Gobierno y Ciudadanía

```{r}
#P 108

# Convertir a factor
df_analisis$P108 <- as.factor(df_analisis$P108)

# Definir niveles con etiquetas significativas
levels(df_analisis$P108) <- c(
  "1" = "La Fiscalía General de Justicia",
  "2" = "Policía Estatal",
  "3" = "Policía Municipal",
  "8888" = "No Sabe",
  "9999" = "No Contestó"
)
# Crear la data que usaremos
df_108<- df_analisis %>%
  group_by(cluster, P108) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(cluster, P108)

# Crear el gráfico con ggplot2
ggplot(df_108, aes(x = factor(cluster), y = percentage, fill = P108)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "¿Qué autoridad es la principal responsable de investigar y 
       perseguir los delitos en Nuevo León?",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(labels = c(
    "1" = "La Fiscalía General de Justicia",
    "2" = "Policía Estatal",
    "3" = "Policía Municipal",
    "8888" = "No Sabe",
    "9999" = "No Contestó"
  )) +
  theme_minimal()

# Convertir a factor
df_analisis$P109 <- as.factor(df_analisis$P109)

# Definir niveles con etiquetas significativas
levels(df_analisis$P109) <- c(
  "0" = "Desaprueba",
  "1" = "Aprueba",
  "8888" = "No Sabe",
  "9999" = "No Contestó"
)
# Crear la data que usaremos
df_109<- df_analisis %>%
  group_by(cluster, P109) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(cluster, P109)

# Crear el gráfico con ggplot2
ggplot(df_109, aes(x = factor(cluster), y = percentage, fill = P109)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "La Fiscalía General de Justicia es la responsable de investigar 
       y perseguir los delitos en Nuevo León, en términos generales: ¿Aprueba 
       o desaprueba el trabajo realizado por la fiscalía?",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(labels = c(
    "0" = "Desaprueba",
    "1" = "Aprueba",
    "8888" = "No Sabe",
    "9999" = "No Contestó"
  )) +
  theme(plot.title = element_text(size=10))

# Convertir a factor
df_analisis$P112 <- as.factor(df_analisis$P112)

# Definir niveles con etiquetas significativas
levels(df_analisis$P112) <- c(
  "1" = "Redes Sociales",
  "2" = "Periódico",
  "3" = "Televisión",
  "4" = "Radio",
  "5" = "Portal de noticias en internet",
  "6" = "Volanteo/perifoneo en la colonia",
  "7" = "Conversaciones con vecinos, familiares o amigos",
  "9" = "No se entera",
  "8888" = "No Sabe",
  "9999" = "No Contestó"
)
# Crear la data que usaremos
df_112<- df_analisis %>%
  group_by(cluster, P112) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(cluster, P112)

# Crear el gráfico con ggplot2
ggplot(df_112, aes(x = factor(cluster), y = percentage, fill = P112)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "¿Cuál es el medio por el que más se entera de los temas 
       de interés público y todo lo relacionado con gobierno?",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(labels = c(
    "1" = "Redes Sociales",
    "2" = "Periódico",
    "3" = "Televisión",
    "4" = "Radio",
    "5" = "Portal de noticias en internet",
    "6" = "Volanteo/perifoneo en la colonia",
    "7" = "Conversaciones con vecinos, familiares o amigos",
    "9" = "No se entera",
    "8888" = "No Sabe",
    "9999" = "No Contestó"
  )) +
  theme_minimal()

# Convertir a factor
df_analisis$P115 <- as.factor(df_analisis$P115)

# Definir niveles con etiquetas significativas
levels(df_analisis$P115) <- c(
  "0" = "No",
  "1" = "Sí",
  "8888" = "No Sabe",
  "9999" = "No Contestó"
)
# Crear la data que usaremos
df_115<- df_analisis %>%
  group_by(cluster, P115) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(cluster, P115)

# Crear el gráfico con ggplot2
ggplot(df_115, aes(x = factor(cluster), y = percentage, fill = P115)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "¿Desde tu perspectiva, ¿el gobierno es transparente con el 
       uso de los recursos públicos?",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(labels = c(
    "0" = "No",
    "1" = "Sí",
    "8888" = "No Sabe",
    "9999" = "No Contestó"
  )) +
  theme_minimal()

#P122

# Convertir a factor
df_analisis$P122 <- as.factor(df_analisis$P122)

# Definir niveles con etiquetas significativas
levels(df_analisis$P122) <- c(
  "0" = "No",
  "1" = "Sí",
  "8888" = "No Sabe",
  "9999" = "No Contestó"
)
# Crear la data que usaremos
df_122<- df_analisis %>%
  group_by(cluster, P122) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(cluster, P122)

# Crear el gráfico con ggplot2
ggplot(df_122, aes(x = factor(cluster), y = percentage, fill = P122)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "¿Consideras que las autoridades (municipales, estatales) 
       toman decisiones/ responden a los casos de acuerdo a los intereses 
       de los que tienen más dinero o influencia?",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(labels = c(
    "0" = "No",
    "1" = "Sí",
    "8888" = "No Sabe",
    "9999" = "No Contestó"
  )) +
  theme_minimal()

#P 142

# Convertir a factor
df_analisis$P142 <- as.factor(df_analisis$P142)

# Definir niveles con etiquetas significativas
levels(df_analisis$P142) <- c(
  "1" = "El gobierno no atiende las demandas ciudadanas.",
  "2" = "Los gobernantes no cumplen sus compromisos de campaña.",
  "3" = "Los gobernantes hacen uso inadecuado de los recursos públicos.",
  "4" = "No hay mecanismos efectivos de diálogo / participación ciudadana.",
  "5" = "Los gobernantes son poco efectivos para resolver los problemas de la ciudad / estado.",
  "6" = "Hay mucha corrupción en servidores públicos",
  "7" = "Ninguno",
  "8" = "Todas las anteriores",
  "8888" = "No Sabe",
  "9999" = "No Contestó"
)
# Crear la data que usaremos
df_142<- df_analisis %>%
  group_by(cluster, P142) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(cluster, P142)

# Crear el gráfico con ggplot2
ggplot(df_142, aes(x = factor(cluster), y = percentage, fill = P142)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "En su opinión: ¿cuál es el principal problema que enfrenta 
       la ciudadanía PARA CONFIAR EN SU GOBIERNO?",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(labels = c(
    "1" = "Redes Sociales",
    "2" = "Periódico",
    "3" = "Televisión",
    "4" = "Radio",
    "5" = "Portal de noticias en internet",
    "6" = "Volanteo/perifoneo en la colonia",
    "7" = "Conversaciones con vecinos, familiares o amigos",
    "9" = "No se entera",
    "8888" = "No Sabe",
    "9999" = "No Contestó"
  )) +
  theme_minimal()

#P139_1

# Convertir a factor
df_analisis$P139_1 <- as.factor(df_analisis$P139_1)

# Definir niveles con etiquetas significativas
levels(df_analisis$P139_1) <- c(
  "0" = "No",
  "1" = "Sí",
  "8888" = "No Sabe",
  "9999" = "No Contestó"
)
# Crear la data que usaremos
df_139_1<- df_analisis %>%
  group_by(cluster, P139_1) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(cluster, P139_1)

# Crear el gráfico con ggplot2
ggplot(df_139_1, aes(x = factor(cluster), y = percentage, fill = P139_1)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "¿Ha asistido a marchas, caminatas o manifestaciones en los últimos 
       12 meses para mejorar las condiciones de su estado??",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(labels = c(
    "0" = "No",
    "1" = "Sí",
    "8888" = "No Sabe",
    "9999" = "No Contestó"
  )) +
  theme_minimal()

#P139_2

# Convertir a factor
df_analisis$P139_2 <- as.factor(df_analisis$P139_2)

# Definir niveles con etiquetas significativas
levels(df_analisis$P139_2) <- c(
  "0" = "No",
  "1" = "Sí",
  "8888" = "No Sabe",
  "9999" = "No Contestó"
)
# Crear la data que usaremos
df_139_2<- df_analisis %>%
  group_by(cluster, P139_2) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(cluster, P139_2)

# Crear el gráfico con ggplot2
ggplot(df_139_2, aes(x = factor(cluster), y = percentage, fill = P139_2)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "¿Ha participado en diálogos con autoridades (Consejos o comités 
       ciudadanos, mesas de trabajo, audiencias públicas, etc) en los últimos 
       12 meses para mejorar las condiciones de su estado?",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(labels = c(
    "0" = "No",
    "1" = "Sí",
    "8888" = "No Sabe",
    "9999" = "No Contestó"
  )) +
  theme_minimal()

#P139_3

# Convertir a factor
df_analisis$P139_3 <- as.factor(df_analisis$P139_3)

# Definir niveles con etiquetas significativas
levels(df_analisis$P139_3) <- c(
  "0" = "No",
  "1" = "Sí",
  "8888" = "No Sabe",
  "9999" = "No Contestó"
)
# Crear la data que usaremos
df_139_3<- df_analisis %>%
  group_by(cluster, P139_3) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(cluster, P139_3)

# Crear el gráfico con ggplot2
ggplot(df_139_3, aes(x = factor(cluster), y = percentage, fill = P139_3)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "¿Ha voluntariado en una asociación civil o colectivo en los 
       últimos 12 meses para mejorar las condiciones de su estado?",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(labels = c(
    "0" = "No",
    "1" = "Sí",
    "8888" = "No Sabe",
    "9999" = "No Contestó"
  )) +
  theme_minimal()

#P139_4

# Convertir a factor
df_analisis$P139_4 <- as.factor(df_analisis$P139_4)

# Definir niveles con etiquetas significativas
levels(df_analisis$P139_4) <- c(
  "0" = "No",
  "1" = "Sí",
  "8888" = "No Sabe",
  "9999" = "No Contestó"
)
# Crear la data que usaremos
df_139_4<- df_analisis %>%
  group_by(cluster, P139_4) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(cluster, P139_4)

# Crear el gráfico con ggplot2
ggplot(df_139_4, aes(x = factor(cluster), y = percentage, fill = P139_4)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "¿Ha participado en juntas/comités de vecinos en los últimos 12 
       meses para mejorar las condiciones de su estado?",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(labels = c(
    "0" = "No",
    "1" = "Sí",
    "8888" = "No Sabe",
    "9999" = "No Contestó"
  )) +
  theme_minimal()

#P139_5

# Convertir a factor
df_analisis$P139_5 <- as.factor(df_analisis$P139_5)

# Definir niveles con etiquetas significativas
levels(df_analisis$P139_5) <- c(
  "0" = "No",
  "1" = "Sí",
  "8888" = "No Sabe",
  "9999" = "No Contestó"
)
# Crear la data que usaremos
df_139_5<- df_analisis %>%
  group_by(cluster, P139_5) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(cluster, P139_5)

# Crear el gráfico con ggplot2
ggplot(df_139_5, aes(x = factor(cluster), y = percentage, fill = P139_5)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "¿Ha participado en una asociación de padres de familia en los 
       últimos 12 meses para mejorar las condiciones de su estado?",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(labels = c(
    "0" = "No",
    "1" = "Sí",
    "8888" = "No Sabe",
    "9999" = "No Contestó"
  )) +
  theme_minimal()

#P139_6

# Convertir a factor
df_analisis$P139_6 <- as.factor(df_analisis$P139_6)

# Definir niveles con etiquetas significativas
levels(df_analisis$P139_6) <- c(
  "0" = "No",
  "1" = "Sí",
  "8888" = "No Sabe",
  "9999" = "No Contestó"
)
# Crear la data que usaremos
df_139_6<- df_analisis %>%
  group_by(cluster, P139_6) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(cluster, P139_6)

# Crear el gráfico con ggplot2
ggplot(df_139_6, aes(x = factor(cluster), y = percentage, fill = P139_6)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "¿Ha contactado a los medios de comunicación en los últimos 
       12 meses para mejorar las condiciones de su estado?",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(labels = c(
    "0" = "No",
    "1" = "Sí",
    "8888" = "No Sabe",
    "9999" = "No Contestó"
  )) +
  theme_minimal()

#P139_7

# Convertir a factor
df_analisis$P139_7 <- as.factor(df_analisis$P139_7)

# Definir niveles con etiquetas significativas
levels(df_analisis$P139_7) <- c(
  "0" = "No",
  "1" = "Sí",
  "8888" = "No Sabe",
  "9999" = "No Contestó"
)
# Crear la data que usaremos
df_139_7<- df_analisis %>%
  group_by(cluster, P139_7) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(cluster, P139_7)

# Crear el gráfico con ggplot2
ggplot(df_139_7, aes(x = factor(cluster), y = percentage, fill = P139_7)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "¿Ha firmado peticiones en línea en los últimos 12 meses 
       para mejorar las condiciones de su estado?",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(labels = c(
    "0" = "No",
    "1" = "Sí",
    "8888" = "No Sabe",
    "9999" = "No Contestó"
  )) +
  theme_minimal()

#P139_8

# Convertir a factor
df_analisis$P139_8 <- as.factor(df_analisis$P139_8)

# Definir niveles con etiquetas significativas
levels(df_analisis$P139_8) <- c(
  "0" = "No",
  "1" = "Sí",
  "8888" = "No Sabe",
  "9999" = "No Contestó"
)
# Crear la data que usaremos
df_139_8<- df_analisis %>%
  group_by(cluster, P139_8) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(cluster, P139_8)

# Crear el gráfico con ggplot2
ggplot(df_139_8, aes(x = factor(cluster), y = percentage, fill = P139_8)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "¿Ha votado o propuesto en qué se debería gastar el presupuesto 
       participativo en su municipio en los últimos 12 meses para mejorar las 
       condiciones de su estado?",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(labels = c(
    "0" = "No",
    "1" = "Sí",
    "8888" = "No Sabe",
    "9999" = "No Contestó"
  )) +
  theme_minimal()

#P139_9

# Convertir a factor
df_analisis$P139_9 <- as.factor(df_analisis$P139_9)

# Definir niveles con etiquetas significativas
levels(df_analisis$P139_9) <- c(
  "0" = "No",
  "1" = "Sí",
  "8888" = "No Sabe",
  "9999" = "No Contestó"
)
# Crear la data que usaremos
df_139_9<- df_analisis %>%
  group_by(cluster, P139_9) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(cluster, P139_9)

# Crear el gráfico con ggplot2
ggplot(df_139_9, aes(x = factor(cluster), y = percentage, fill = P139_9)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "¿Ha solicitado apoyo o presentado quejas a funcionarios o 
       autoridades (Reportar baches, luminarias, parques en mal estado u otros 
       aspectos que permitan la mejora de la ciudad) en los últimos 12 meses 
       para mejorar las condiciones de su estado?",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(labels = c(
    "0" = "No",
    "1" = "Sí",
    "8888" = "No Sabe",
    "9999" = "No Contestó"
  )) +
  theme_minimal()

#P118

# Convertir a factor
df_analisis$P118 <- as.factor(df_analisis$P118)

# Definir niveles con etiquetas significativas
levels(df_analisis$P118) <- c(
  "1" = "Muy frecuente",
  "2" = "Frecuente",
  "3" = "Poco frecuente",
  "4" = "No ocurre",
  "8888" = "No Sabe",
  "9999" = "No Contestó"
)
# Crear la data que usaremos
df_118<- df_analisis %>%
  group_by(cluster, P118) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(cluster, P118)

# Crear el gráfico con ggplot2
ggplot(df_118, aes(x = factor(cluster), y = percentage, fill = P118)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "La corrupción consiste en el abuso de poder para beneficio 
       propio ya sea en el sector público o en el privado. En su opinión, 
       ¿en el estado de Nuevo León la corrupción es una práctica:?",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(labels = c(
    "1" = "Muy frecuente",
    "2" = "Frecuente",
    "3" = "Poco frecuente",
    "4" = "No ocurre",
    "8888" = "No Sabe",
    "9999" = "No Contestó"
  )) +
  theme_minimal()
```

# Gobierno Estatal

```{r}
#P117

# Convertir a factor
df_analisis$P117 <- as.factor(df_analisis$P117)

# Definir niveles con etiquetas significativas
levels(df_analisis$P117) <- c(
  "0" = "No",
  "1" = "Sí",
  "8888" = "No Sabe",
  "9999" = "No Contestó"
)
# Crear la data que usaremos
df_117<- df_analisis %>%
  group_by(cluster, P117) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(cluster, P117)

# Crear el gráfico con ggplot2
ggplot(df_117, aes(x = factor(cluster), y = percentage, fill = P117)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "¿Considera que el gobierno de Samuel García 
       ha hecho un buen uso de los recursos públicos?",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(labels = c(
    "0" = "No",
    "1" = "Sí",
    "8888" = "No Sabe",
    "9999" = "No Contestó"
  )) +
  theme_minimal()

#P128_1

# Convertir a factor
df_analisis$P128_1<- as.factor(df_analisis$P128_1)

# Definir niveles con etiquetas significativas
levels(df_analisis$P128_1) <- c(
  "0" = "No",
  "1" = "Sí",
  "8888" = "No Sabe",
  "9999" = "No Contestó"
)
# Crear la data que usaremos
df_128_1<- df_analisis %>%
  group_by(cluster, P128_1) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(cluster, P128_1)

# Crear el gráfico con ggplot2
ggplot(df_128_1, aes(x = factor(cluster), y = percentage, fill = P128_1)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "¿Considera que Samuel García es un gobernante honesto?",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(labels = c(
    "0" = "No",
    "1" = "Sí",
    "8888" = "No Sabe",
    "9999" = "No Contestó"
  )) +
  theme_minimal()

#P128_2
# Convertir a factor
df_analisis$P128_2<- as.factor(df_analisis$P128_2)

# Definir niveles con etiquetas significativas
levels(df_analisis$P128_2) <- c(
  "0" = "No",
  "1" = "Sí",
  "8888" = "No Sabe",
  "9999" = "No Contestó"
)
# Crear la data que usaremos
df_128_2<- df_analisis %>%
  group_by(cluster, P128_2) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(cluster, P128_2)

# Crear el gráfico con ggplot2
ggplot(df_128_2, aes(x = factor(cluster), y = percentage, fill = P128_2)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "¿Considera que Samuel García es capaz 
       de resolver los problemas del Estado?",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(labels = c(
    "0" = "No",
    "1" = "Sí",
    "8888" = "No Sabe",
    "9999" = "No Contestó"
  )) +
  theme_minimal()

#P128_3
# Convertir a factor
df_analisis$P128_3<- as.factor(df_analisis$P128_3)

# Definir niveles con etiquetas significativas
levels(df_analisis$P128_3) <- c(
  "0" = "No",
  "1" = "Sí",
  "8888" = "No Sabe",
  "9999" = "No Contestó"
)
# Crear la data que usaremos
df_128_3<- df_analisis %>%
  group_by(cluster, P128_3) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(cluster, P128_3)

# Crear el gráfico con ggplot2
ggplot(df_128_3, aes(x = factor(cluster), y = percentage, fill = P128_3)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "¿Considera que Samuel García cuenta 
       con un equipo capacitado?",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(labels = c(
    "0" = "No",
    "1" = "Sí",
    "8888" = "No Sabe",
    "9999" = "No Contestó"
  )) +
  theme_minimal()

#P128_4
# Convertir a factor
df_analisis$P128_4<- as.factor(df_analisis$P128_4)

# Definir niveles con etiquetas significativas
levels(df_analisis$P128_4) <- c(
  "0" = "No",
  "1" = "Sí",
  "8888" = "No Sabe",
  "9999" = "No Contestó"
)
# Crear la data que usaremos
df_128_4<- df_analisis %>%
  group_by(cluster, P128_4) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(cluster, P128_4)

# Crear el gráfico con ggplot2
ggplot(df_128_4, aes(x = factor(cluster), y = percentage, fill = P128_4)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "¿Considera que Samuel García ha cumplido 
       con sus promesas de campaña?",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(labels = c(
    "0" = "No",
    "1" = "Sí",
    "8888" = "No Sabe",
    "9999" = "No Contestó"
  )) +
  theme_minimal()

#P129
# Convertir a factor
df_analisis$P129 <- as.factor(df_analisis$P129)

# Definir niveles con etiquetas significativas
levels(df_analisis$P129) <- c(
  "1" = "Pensando en el bienestar del estado",
  "2" = "Pensando en intereses particulares/personales",
  "8888" = "No Sabe",
  "9999" = "No Contestó"
)
# Crear la data que usaremos
df_129<- df_analisis %>%
  group_by(cluster, P129) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(cluster, P129)

# Crear el gráfico con ggplot2
ggplot(df_129, aes(x = factor(cluster), y = percentage, fill = P129)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "¿Usted considera que Samuel García toma la mayoría 
       de sus decisiones (elegir una):?",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(labels = c(
    "1" = "Pensando en el bienestar del estado",
    "2" = "Pensando en intereses particulares/personales",
    "8888" = "No Sabe",
    "9999" = "No Contestó"
  )) +
  theme_minimal()

#P130

# Convertir a factor
df_analisis$P130 <- as.factor(df_analisis$P130)

# Definir niveles con etiquetas significativas
levels(df_analisis$P130) <- c(
  "1" = "Mejor de lo que esperaba",
  "2" = "Igual de bien que lo que esperaba",
  "3" = "Igual de lo mal que lo que esperaba",
  "4" = "Peor de lo que esperaba",
  "8888" = "No Sabe",
  "9999" = "No Contestó"
)
# Crear la data que usaremos
df_130<- df_analisis %>%
  group_by(cluster, P130) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(cluster, P130)

# Crear el gráfico con ggplot2
ggplot(df_130, aes(x = factor(cluster), y = percentage, fill = P130)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "¿Para usted, el gobierno de Samuel García ha sido:?",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(labels = c(
    "1" = "Mejor de lo que esperaba",
    "2" = "Igual de bien que lo que esperaba",
    "3" = "Igual de lo mal que lo que esperaba",
    "4" = "Peor de lo que esperaba",
    "8888" = "No Sabe",
    "9999" = "No Contestó"
  )) +
  theme_minimal()

#P135

# Convertir a factor
df_analisis$P135 <- as.factor(df_analisis$P135)

# Definir niveles con etiquetas significativas
levels(df_analisis$P135) <- c(
  "1" = "Mucho",
  "2" = "Algo",
  "3" = "Poco",
  "4" = "Nada",
  "8888" = "No Sabe",
  "9999" = "No Contestó"
)
# Crear la data que usaremos
df_135<- df_analisis %>%
  group_by(cluster, P135) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(cluster, P135)

# Crear el gráfico con ggplot2
ggplot(df_135, aes(x = factor(cluster), y = percentage, fill = P135)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "¿Cuánto considera que los ciudadanos pueden influir 
       en las decisiones del gobierno del estado?",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(labels = c(
    "1" = "Mucho",
    "2" = "Algo",
    "3" = "Poco",
    "4" = "Nada",
    "8888" = "No Sabe",
    "9999" = "No Contestó"
  )) +
  theme_minimal()

#P137

# Convertir a factor
df_analisis$P137 <- as.factor(df_analisis$P137)

# Definir niveles con etiquetas significativas
levels(df_analisis$P137) <- c(
  "0" = "Desaprueba",
  "1" = "Aprueba",
  "8888" = "No Sabe",
  "9999" = "No Contestó"
)
# Crear la data que usaremos
df_137<- df_analisis %>%
  group_by(cluster, P137) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(cluster, P137)

# Crear el gráfico con ggplot2
ggplot(df_137, aes(x = factor(cluster), y = percentage, fill = P137)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "¿En términos generales, ¿aprueba o desaprueba el 
       trabajo que ha hecho Samuel García?",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(labels = c(
    "0" = "Desaprueba",
    "1" = "Aprueba",
    "8888" = "No Sabe",
    "9999" = "No Contestó"
  )) +
  theme_minimal()
```


# Municipio

```{r}
#P116

# Convertir a factor
df_analisis$P116 <- as.factor(df_analisis$P116)

# Definir niveles con etiquetas significativas
levels(df_analisis$P116) <- c(
  "0" = "No",
  "1" = "Sí",
  "8888" = "No Sabe",
  "9999" = "No Contestó"
)
# Crear la data que usaremos
df_116<- df_analisis %>%
  group_by(cluster, P116) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(cluster, P116)

# Crear el gráfico con ggplot2
ggplot(df_116, aes(x = factor(cluster), y = percentage, fill = P116)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "¿Considera que el gobierno de (Nombre de alcalde) ha hecho un 
       buen uso de los recursos públicos?",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(labels = c(
    "0" = "No",
    "1" = "Sí",
    "8888" = "No Sabe",
    "9999" = "No Contestó"
  )) +
  theme_minimal()
```

```{r}
#P119

# Convertir a factor
df_analisis$P119 <- as.factor(df_analisis$P119)

# Definir niveles con etiquetas significativas
levels(df_analisis$P119) <- c(
  "1" = "Muy frecuente",
  "2" = "Frecuente",
  "3" = "Poco frecuente",
  "4" = "No ocurre",
  "8888" = "No Sabe",
  "9999" = "No Contestó"
)
# Crear la data que usaremos
df_119<- df_analisis %>%
  group_by(cluster, P119) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(cluster, P119)

# Crear el gráfico con ggplot2
ggplot(df_119, aes(x = factor(cluster), y = percentage, fill = P119)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "¿En su opinión, en su municipio la corrupción es una práctica:?",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(labels = c(
    "1" = "Muy frecuente",
    "2" = "Frecuente",
    "3" = "Poco frecuente",
    "4" = "No ocurre",
    "8888" = "No Sabe",
    "9999" = "No Contestó"
  )) +
  theme_minimal()
```


```{r}
#P125

# Convertir a factor
df_analisis$P125 <- as.factor(df_analisis$P125)

# Definir niveles con etiquetas significativas
levels(df_analisis$P125) <- c(
  "1" = "Pensando en el bienestar del municipio",
  "2" = "Pensando en intereses particulares/personales",
  "8888" = "No Sabe",
  "9999" = "No Contestó"
)
# Crear la data que usaremos
df_125<- df_analisis %>%
  group_by(cluster, P125) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(cluster, P125)

# Crear el gráfico con ggplot2
ggplot(df_125, aes(x = factor(cluster), y = percentage, fill = P125)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "¿En su opinión, en su municipio la corrupción es una práctica:?",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(labels = c(
    "1" = "Pensando en el bienestar del municipio",
    "2" = "Pensando en intereses particulares/personales",
    "8888" = "No Sabe",
    "9999" = "No Contestó"
  )) +
  theme_minimal()
```
```{r}
#P126

# Convertir a factor
df_analisis$P126 <- as.factor(df_analisis$P126)

# Definir niveles con etiquetas significativas
levels(df_analisis$P126) <- c(
  "0" = "Desaprueba",
  "1" = "Aprueba",
  "8888" = "No Sabe",
  "9999" = "No Contestó"
)
# Crear la data que usaremos
df_126<- df_analisis %>%
  group_by(cluster, P126) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(cluster, P126)

# Crear el gráfico con ggplot2
ggplot(df_126, aes(x = factor(cluster), y = percentage, fill = P126)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "En términos generales, ¿aprueba o desaprueba el trabajo que ha hecho Nombre de ALCALDE/ALCALDESA)?",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(labels = c(
    "0" = "Desaprueba",
    "1" = "Aprueba",
    "8888" = "No Sabe",
    "9999" = "No Contestó"
  )) +
  theme_minimal()
```

```{r}
#P127

# Convertir a factor
df_analisis$P127 <- as.factor(df_analisis$P127)

# Definir niveles con etiquetas significativas
levels(df_analisis$P127) <- c(
  "1" = "Mejor de lo que esperaba",
  "2" = "Igual de bien que lo que esperaba",
  "3" = "Igual de lo mal que lo que esperaba",
  "4" = "Peor de lo que esperaba",
  "8888" = "No Sabe",
  "9999" = "No Contestó"
)
# Crear la data que usaremos
df_127<- df_analisis %>%
  group_by(cluster, P127) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(cluster, P127)

# Crear el gráfico con ggplot2
ggplot(df_127, aes(x = factor(cluster), y = percentage, fill = P127)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "¿Para usted, el gobierno de NOMBRE DE ALCALDE ha sido:?",
       x = "Cluster",
       y = "Porcentaje",
       fill = "Respuesta") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_discrete(labels = c(
    "1" = "Mejor de lo que esperaba",
    "2" = "Igual de bien que lo que esperaba",
    "3" = "Igual de lo mal que lo que esperaba",
    "4" = "Peor de lo que esperaba",
    "8888" = "No Sabe",
    "9999" = "No Contestó"
  )) +
  theme_minimal()
```

