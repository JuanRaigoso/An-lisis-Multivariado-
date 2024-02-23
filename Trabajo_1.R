
https://rpubs.com/juan_raigoso/1148525 # Enlace del proyecto.

############## Análisis Multivariado##################

# Cargar librerías
library(paqueteMODELOS)
library(mice)
library(tidyverse)
library(naniar)
library(pacman)
library(DataExplorer)
library(ggcorrplot)
library(Cairo)
library(factoextra)
library(ggrepel)
library(textshape)
library(cluster)
library(NbClust)
library(FactoMineR)
library(pander)
library(sf)
library(corrplot)

###### Análisis exploratorio ##########

## ver base de datos
print(vivienda)
## Estructura base de datos
str(vivienda)

## convertir piso a variable númerica
vivienda$piso<-as.numeric(vivienda$piso)
str(vivienda)

## verificación de valores nulos o NA por atributo
md.pattern(vivienda, rotate.names = TRUE)
title(main = "Matriz de Datos faltantes", sub = "Valores faltantes en el conjunto de datos")

# Otra forma de verificar los NA
gg_miss_var(vivienda) + labs(x="Variables", y = "Datos NA")
## suma de valores nulos por columna
colSums(is.na(vivienda))

## Eliminación de las variables piso y parqueaderos
vivienda_1 <- subset(vivienda, select = c('id', 'zona', 'estrato', 'preciom', 'areaconst', 'habitaciones', 'banios', 'tipo', 'barrio', 'longitud', 'latitud'))
vivienda_1 <- vivienda_1[complete.cases(vivienda_1$id), ]

## pasar acolumna "ID" a index --- Esto es una buena práctica
vivienda_1<-textshape::column_to_rownames(vivienda_1, loc = 1)

## verificación de que  no hayan valores nulos o NA en la nueva base de datos.
md.pattern(vivienda_1, rotate.names = TRUE)

## se excluyen las variables latitud y longitud de la base de datos
variables_a_incluir <- setdiff(names(vivienda_1), c("latitud", "longitud"))

## Gráficamos las variables, distribución por variable.
plot_histogram(vivienda_1[, variables_a_incluir])


## Correlación de las variables
correlacion <- select(vivienda_1, Estrato=estrato, Precio=preciom,'Área construida'=areaconst, ' baños'=banios, 'habitaciones'=habitaciones)
correl <- cor(correlacion)
ggcorrplot(correl, type = "upper", lab = TRUE)+
  labs(title = "Matriz de Correlación")+
  theme(plot.title = element_text(hjust = 0.5))

## Boxplot variables (Gráfico de caja)
# Definir una paleta de colores pasteles
colores_pasteles <- c("#FFB6C1", "#FFD700", "#98FB98", "#87CEEB", "#FFA07A", "#B0E0E6")

# Lista para almacenar gráficos
plots <- list()
for (i in seq_along(colnames(correlacion))) {
  p <- ggplot(correlacion, aes(x = 1, y = .data[[colnames(correlacion)[i]]])) +
    geom_boxplot(fill = colores_pasteles[i], color = "black") +
    labs(title = paste("Boxplot de", colnames(correlacion)[i]), x = "", y = colnames(correlacion)[i]) +
    theme_minimal() +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank())
  
  plots[[i]] <- p
}
# Organizar los gráficos en una cuadrícula de 2x2 con más espacio entre las filas
grid.arrange(grobs = plots, ncol = 2, heights = c(2, 2, 2, 2))

############## # Análisis de Componentes Principales (ACP)################

## Data Frame de columnas cde tipo caracter
columnas_chr <- sapply(vivienda_1, is.character)
## Se extrae de la base vivienda_1 todas las columnas diferente a caracter, es decir, las númericas (cuantitativas).
vivienda_cuantitativas <- vivienda_1[, !columnas_chr]
## Visualizació de la base
head(vivienda_cuantitativas)

## Se escalan las variables.
vivienda_1escala <- vivienda_cuantitativas %>%
  select(estrato, preciom, areaconst, habitaciones, banios) %>%
  scale()
## Visualización de la nueva base de datos pero con las variables escaladas.
head(vivienda_1escala)

## Desviación estandar.
res.pca <- prcomp(vivienda_1escala)
res.pca
summary(res.pca)


## Elección del número de componentes principales.
res.pca<-prcomp(vivienda_1escala)
fviz_eig(res.pca, addlabels = TRUE)

## Gráfica de las varaibles en los componentes
fviz_pca_var(res.pca,repel = T, colvar="cos2", col.var = "contrib", alpha.var = "contrib", gradient.cols=c("#FF7F00",  "#034D94"))

## Gráfica que muestra el peso de las varaibles según componente
corrplot(get_pca_var(res.pca)$cos2)

## Distribución de los individuos sobre el PCA
fviz_pca_ind(res.pca)

## Gráfico de individuos y PCA juntos.

color_variables <- "#1F618D"  # Puedes ajustar este color según tus preferencias
color_individuos <- "#95a5a6"

# Gráfico mejorado
fviz_pca_biplot(res.pca, repel = FALSE,
                col.var = color_variables,
                col.ind = color_individuos,
                geom.var = c("point", "text"), # Puntos y etiquetas para las variables
                geom.ind = c("point", "text"), # Puntos y etiquetas para los individuos
                legend.title = "Variables",   # Título de la leyenda
                ggtheme = theme_minimal(),     # Tema minimalista
) +
  theme(legend.position = "right",    # Posición de la leyenda
        legend.direction = "vertical", # Dirección de la leyenda
        legend.background = element_rect(fill = "white", color = "transparent"), # Fondo de la leyenda
        panel.grid.major = element_blank(),  # Eliminar líneas de la cuadrícula
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),        # Eliminar marcas de los ejes
        panel.border = element_blank(),      # Eliminar bordes del panel
        plot.title = element_text(hjust = 0.5), # Alineación del título
        text = element_text(size = 14, color = "black", face = "bold"), # Tamaño y estilo de texto
        axis.text = element_text(size = 12, color = "black", face = "bold") # Tamaño y estilo de texto de los ejes
  ) +
  theme(text = element_text(face = "bold"))  # Hacer que todo el texto sea en negrita


## proporción de varianza acumulada
prop_varianza <- res.pca$sdev^2 / sum(res.pca$sdev^2)
prop_varianza


##  Gráfico de la proporción de la varianza 
df <- data.frame(prop_varianza, pc = 1:length(prop_varianza))

# Graficar
ggplot(data = df,
       aes(x = pc, y = prop_varianza)) +
  geom_col(width = 0.3) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_bw() +
  labs(x = "Componente principal",
       y = "Prop. de varianza explicada")


## Varianza acumulada
prop_varianza_acum <- cumsum(prop_varianza)
prop_varianza_acum



########################## Análisis de conglomerados ##########################


# Distancia Euclidiana 
m.distancia <- get_dist(vivienda_1escala, method = "euclidean")

fviz_dist(m.distancia, gradient = list(low = "blue", mid = "white", high = "red"))


#estimar el número de clústers

fviz_nbclust(vivienda_1escala, kmeans, method = "wss")
fviz_nbclust(vivienda_1escala, kmeans, method = "silhouette")
fviz_nbclust(vivienda_1escala, kmeans, method = "gap_stat")
resnumclust<-NbClust(vivienda_1escala, distance = "euclidean", min.nc=2, max.nc=10, method = "kmeans", index = "alllong")
fviz_nbclust(vivienda_1escala, kmeans, method = "gap_stat")
resnumclust <- NbClust(vivienda_1escala, distance = "euclidean", min.nc = 2, max.nc=10, method = "complete", index = "all")

## Creación de los clusteres
set.seed(123)
k2 <- kmeans(vivienda_1escala, centers = 3, nstart = 25)
fviz_cluster(k2, data = vivienda_1escala)


set.seed(123)
asignacion_clusters <- k2$cluster
# Contar el número de observaciones en cada clúster
conteo_por_cluster <- table(asignacion_clusters)
# Mostrar el resultado
print(conteo_por_cluster)

## Visualización de cada clúster Relación precio - Área construida según clúster
vivienda_copia <- data.frame(vivienda_1)
vivienda_copia$clus<-as.factor(k2$cluster)
ggplot(data = vivienda_copia) +
  geom_point(mapping = aes(x =preciom, y =areaconst,  color= factor(clus)))+
  labs(title = "Relación Precio - Área Construida según Clúster", x = "Precio", y = "Area Construida")+
  scale_color_discrete(name = "Clústers")+
  theme(plot.title = element_text(hjust = 0.5))

## Visualización de cada clúster Relación Caja de bigotes por precio- Zona según Clúster
ggplot(vivienda_copia,mapping=aes(x=zona, y=preciom, fill=clus)) +
  geom_boxplot()+
  labs(title = "Relación Caja de bigotes por precio- Zona según Clúster",
       x = "Zona",
       y = "Precio",
       fill="clúster")+
  theme(plot.title = element_text(hjust = 0.5))


### Creación del mapa

write.csv(vivienda_copia, "vivienda_copia_mapa.csv", row.names = TRUE) ### Así se descarga o se exporta una base de datos de Rstudio

# Leer el shapefile con datos geoespaciales desde un archivo
datos_shapefile <- st_read("C:/Users/juanr/Downloads/mazanascali.shp")
vivienda_copia_mapa <- read.csv("~/vivienda_copia_mapa.csv")
# Convertir los datos de vivienda a un objeto sf con coordenadas especificadas y realizar buffer
vivienda_sf <- st_as_sf(vivienda_copia_mapa, coords = c("longitud", "latitud"), crs = st_crs(datos_shapefile))
datos_shapefile <- st_as_sf(datos_shapefile)
vivienda_sf <- st_buffer(vivienda_sf, dist = 0.01)
# Realizar una operación de unión espacial entre los datos del shapefile y los datos de vivienda
datos_combinados <- st_join(datos_shapefile, vivienda_sf)
# Reemplazar los valores NA en clus con un valor temporal (puedes ajustar esto según tus necesidades)
datos_combinados$clus_temp <- ifelse(is.na(datos_combinados$clus), "NA", as.factor(datos_combinados$clus))
# Crear el gráfico
ggplot() +
  geom_sf(data = datos_combinados, aes(fill = clus_temp), color = "#8f8f8f", size = 0.2) +
  scale_fill_manual(values = c("#0B7072", "#EFE081", "#F1713F", "#8f8f8f"), name = "clústeres") +
  ggtitle("Distribución de clústeres en Santiago de Cali") +
  theme_minimal() +
  labs(fill = "clústeres") +
  guides(fill = guide_legend(na.value = "transparent")) +
  theme(legend.key = element_rect(color = "grey"))


### Indicadores para en análisis 

#### Habitaciones 
### MEdia
aggregate(habitaciones ~ clus, data = vivienda_copia_mapa, FUN = mean)
#### Baños 
### MEdia
aggregate(banios ~ clus, data = vivienda_copia_mapa, FUN = median)
#### Precio.
### mdeiana
aggregate(preciom ~ clus, data = vivienda_copia_mapa, FUN = median)
### MEdia
aggregate(preciom ~ clus, data = vivienda_copia_mapa, FUN = mean)

#### Precio.
### mdeiana
aggregate(areaconst ~ clus, data = vivienda_copia_mapa, FUN = median)
### MEdia
aggregate(areaconst ~ clus, data = vivienda_copia_mapa, FUN = mean)

# Supongamos que 'precio_percentiles' es el nombre de la variable que contendrá los percentiles por cluster
aggregate(estrato ~ clus, data = vivienda_copia_mapa, FUN = function(x) quantile(x, c(0.25, 0.50, 0.75)))



vivienda_copia_mapa %>%
  group_by(clus, tipo) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)




#### Analisis de correspondencia MÚLTIPLE


## Seleccionamos variables categoricas

vivienda_categ <- vivienda_1[, c("tipo", "zona", "barrio")]
head(vivienda_categ)

# Se revisa si la base tiene datos faltantes (rectángulos de color rojo)
md.pattern(vivienda_categ)

# Graficos de zona y tipo - Conteo
F1<-ggplot(vivienda_1, aes(x=zona)) + geom_bar(fill= "#DDB4EB")
F2<-ggplot(vivienda_1, aes(x=tipo)) + geom_bar(fill= "#FFD4A5")
F3 <- grid.arrange(F1,F2)

# Creación tabla
tabla2 <- table(vivienda_categ$tipo,vivienda_categ$zona)
colnames(tabla2) <- c("Zona Centro", "Zona Norte", "Zona Oeste", "Zona Oriente", "ZonaSur")
rownames(tabla2) <- c("Apartamento", "Casa")
addmargins(tabla2)

## Pearson's Chi-squared test
chisq.test(tabla2) ### solo se pude con dos variables.

### MCA para solo dos varaibles
vivienda_categ_2 <- vivienda_1[, c("tipo", "zona")]
mca_result_1 <- MCA(vivienda_categ_2)

## Porpoción de la varianza según componente
fviz_screeplot(mca_result_1, addlabels = TRUE) 

#Resultado MCA
mca_result <- MCA(vivienda_categ)

## Eigenvalues / Varianzas.
eigenval <- get_eigenvalue(mca_result)
pander(head(eigenval))

## Coordenadas puntuales.
var <- get_mca_var(mca_result)
pander(head(round(var$coord, 2), 15))
fviz_screeplot(mca_result, addlabels = TRUE) 

# INdividual MCA 
fviz_mca_ind(mca_result,
             label = "none",
             habillage = "tipo",
             pallette = c("#CCCCFF", "#F08080"),
             addEllipses = TRUE,
             ggtheme = theme_grey())

fviz_ellipses(mca_result,1:2 ,
              geom = "point")

fviz_mca_ind(mca_result,
             label = "none",
             habillage = "zona",
             pallette = c("#CCCCFF", "#F08080"),
             addEllipses = TRUE,
             ggtheme = theme_grey())


