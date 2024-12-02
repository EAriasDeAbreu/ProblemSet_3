#############################################################################
###########  PROBLEM SET 3 - BIG DATA & MACHINE LEARNING      ###############
#############################################################################

## 0 ## Setting up
install.packages("stringdist")
install.packages("parallel")
install.packages("sf")
install.packages("osmdata")
if (!require("blockCV")) install.packages("blockCV")
if (!require("glmnet")) install.packages("glmnet")
if (!require("sf")) install.packages("sf")

library(blockCV)
library(glmnet)
library(sf)


library(stringdist)
library(parallel)
library(sf)
library(osmdata)

## 1 ## Let's put the data in shape:

#1.1# A nivel de persona
test_data <- read.csv("/Users/lucia_mr/Dropbox/2. PEG/2024-2/BigData/ProblemSet3/Data/test.csv")
train_data <- read.csv("/Users/lucia_mr/Dropbox/2. PEG/2024-2/BigData/ProblemSet3/Data/train.csv")

#--------------------------------------------------------------------------#
# JARO-WINKLER #

check_similarity <- function(description, keywords, threshold = 0.95) {
  words <- unlist(strsplit(description, "\\s+")) # Split description into words
  for (word in words) {
    # Calculate Jaro-Winkler similarity with each keyword
    similarities <- sapply(keywords, function(k) stringdist::stringdist(word, k, method = "jw"))
    similarities <- 1 - similarities # Convert distance to similarity
    similarities <- similarities[!is.na(similarities)] # Remove NA values
    if (any(similarities >= threshold)) {
      return(1) # Return 1 if any word matches the threshold
    }
  }
  return(0) # Return 0 otherwise
}

#1.2# Parqueaderos

keywords <- c("parqueadero", "garage", "estacionamiento")

# Number of cores 
num_cores <- detectCores() - 1

# Cluster
cl <- makeCluster(num_cores)

clusterExport(cl, varlist = c("check_similarity", "keywords", "stringdist"))

# Parallel processing for train_data
train_data$parquedero <- parSapply(cl, train_data$description, check_similarity, keywords = keywords, threshold = 0.99)

# Parallel processing for test_data
test_data$parquedero <- parSapply(cl, test_data$description, check_similarity, keywords = keywords, threshold = 0.99)

stopCluster(cl)

#--------------------------------------------------------------------------#
#1.3# Amoblado
keywords <- c("amoblado", "amueblado")

# Cluster
cl <- makeCluster(num_cores)

clusterExport(cl, varlist = c("check_similarity", "keywords", "stringdist"))

# Parallel processing for train_data
train_data$amoblado <- parSapply(cl, train_data$description, check_similarity, keywords = keywords, threshold = 0.99)

# Parallel processing for test_data
test_data$amoblado <- parSapply(cl, test_data$description, check_similarity, keywords = keywords, threshold = 0.99)

stopCluster(cl)

#--------------------------------------------------------------------------#
#1.3# BALCÓN
keywords <- c("balcon", "terraza")

# Cluster
cl <- makeCluster(num_cores)

clusterExport(cl, varlist = c("check_similarity", "keywords", "stringdist"))

# Parallel processing for train_data
train_data$balcon <- parSapply(cl, train_data$description, check_similarity, keywords = keywords, threshold = 0.99)

# Parallel processing for test_data
test_data$balcon <- parSapply(cl, test_data$description, check_similarity, keywords = keywords, threshold = 0.99)

stopCluster(cl)

#--------------------------------------------------------------------------#
#1.4# PATIO
keywords <- c("patio")

# Cluster
cl <- makeCluster(num_cores)

clusterExport(cl, varlist = c("check_similarity", "keywords", "stringdist"))

# Parallel processing for train_data
train_data$patio <- parSapply(cl, train_data$description, check_similarity, keywords = keywords, threshold = 0.99)

# Parallel processing for test_data
test_data$patio <- parSapply(cl, test_data$description, check_similarity, keywords = keywords, threshold = 0.99)

stopCluster(cl)

#--------------------------------------------------------------------------#
#1.5# Club house
keywords <- c("club")

# Cluster
cl <- makeCluster(num_cores)

clusterExport(cl, varlist = c("check_similarity", "keywords", "stringdist"))

# Parallel processing for train_data
train_data$club <- parSapply(cl, train_data$description, check_similarity, keywords = keywords, threshold = 0.99)

# Parallel processing for test_data
test_data$club <- parSapply(cl, test_data$description, check_similarity, keywords = keywords, threshold = 0.99)

stopCluster(cl)

#--------------------------------------------------------------------------#
#1.6# Lavanderia
keywords <- c("lavanderia", "laundry")

# Cluster
cl <- makeCluster(num_cores)

clusterExport(cl, varlist = c("check_similarity", "keywords", "stringdist"))

# Parallel processing for train_data
train_data$lavanderia <- parSapply(cl, train_data$description, check_similarity, keywords = keywords, threshold = 0.99)

# Parallel processing for test_data
test_data$lavanderia <- parSapply(cl, test_data$description, check_similarity, keywords = keywords, threshold = 0.99)

stopCluster(cl)

#--------------------------------------------------------------------------#
#1.6# REMODELADO
keywords <- c("remodelado")

# Cluster
cl <- makeCluster(num_cores)

clusterExport(cl, varlist = c("check_similarity", "keywords", "stringdist"))

# Parallel processing for train_data
train_data$remodelado <- parSapply(cl, train_data$description, check_similarity, keywords = keywords, threshold = 0.99)

# Parallel processing for test_data
test_data$remodelado <- parSapply(cl, test_data$description, check_similarity, keywords = keywords, threshold = 0.99)

stopCluster(cl)
table(train_data$remodelado)

#--------------------------------------------------------------------------#
#1.7# ASCENSOR
keywords <- c("ascensor")

# Cluster
cl <- makeCluster(num_cores)

clusterExport(cl, varlist = c("check_similarity", "keywords", "stringdist"))

# Parallel processing for train_data
train_data$ascensor <- parSapply(cl, train_data$description, check_similarity, keywords = keywords, threshold = 0.99)

# Parallel processing for test_data
test_data$ascensor <- parSapply(cl, test_data$description, check_similarity, keywords = keywords, threshold = 0.99)

stopCluster(cl)
table(train_data$ascensor)

#--------------------------------------------------------------------------#
# 2 # AREA

# Function to extract area from description
extract_area <- function(description) {
  # Check for NA or missing values
  if (is.na(description) || description == "") return(NA)
  
  # Define the regex pattern to capture numbers followed by specific area markers
  pattern <- "\\b(\\d+(\\.\\d+)?)(\\s?)(m2|mt2|metros|mt|m|mts)\\b"
  
  # Find matches in the description
  matches <- regmatches(description, gregexpr(pattern, description, perl = TRUE))
  
  # Extract the first valid match if it exists
  if (length(matches[[1]]) > 0) {
    # Extract the numeric part (group 1 of the regex)
    area_match <- matches[[1]][1]
    area_numeric <- sub("^(\\d+(\\.\\d+)?)(\\s?)(m2|mt2|metros|mt|m|mts)$", "\\1", area_match)
    area <- as.numeric(area_numeric)
    return(area)
  } else {
    return(NA) # Return NA if no match is found
  }
}

# Apply the function to the dataset
train_data$area <- sapply(train_data$description, extract_area)
train_data$surface_covered[is.na(train_data$surface_covered)] <- train_data$area[is.na(train_data$surface_covered)]

test_data$area <- sapply(test_data$description, extract_area)
test_data$surface_covered[is.na(test_data$surface_covered)] <- test_data$area[is.na(test_data$surface_covered)]

#--------------------------------------------------------------------------#
# 3 # ESTRATO

# Function to extract "estrato" number from description
extract_estrato <- function(description) {
  # Check for NA or missing values
  if (is.na(description) || description == "") return(NA)
  
  # Define the regex pattern to capture the estrato number (e.g., "estrato 3", "estrato 4")
  pattern <- "\\b(e\\s?strato\\s?)(\\d+)\\b"
  
  # Find matches in the description
  matches <- regmatches(description, gregexpr(pattern, description, perl = TRUE))
  
  # Extract the first valid match if it exists
  if (length(matches[[1]]) > 0) {
    # Extract the numeric part (group 2 of the regex)
    estrato_match <- matches[[1]][1]
    estrato_numeric <- sub(".*(\\d+)$", "\\1", estrato_match)
    estrato <- as.numeric(estrato_numeric)
    return(estrato)
  } else {
    return(NA) # Return NA if no match is found
  }
}

# Apply the function to the dataset
train_data$estrato <- sapply(train_data$description, extract_estrato)
test_data$estrato <- sapply(test_data$description, extract_estrato)



#--------------------------------------------------------------------------#
# 4 # ROOMS

# Function to extract the number of rooms from the description
extract_rooms <- function(description) {
  # Check for NA or missing values
  if (is.na(description) || description == "") return(NA)
  
  # Define the regex pattern to capture the number of rooms
  # Matches phrases like "habitación 2", "habitaciones 3", "cuartos 4", "pieza 1", etc.
  pattern <- "\\b(habitaci[oó]n(?:es)?|cuartos?|piezas?)\\s?(\\d+)\\b"
  
  # Find matches in the description
  matches <- regmatches(description, gregexpr(pattern, description, perl = TRUE))
  
  # Extract the first valid match if it exists
  if (length(matches[[1]]) > 0) {
    # Extract the numeric part (group 2 of the regex)
    room_match <- matches[[1]][1]
    room_numeric <- sub(".*(\\d+)$", "\\1", room_match)
    rooms <- as.numeric(room_numeric)
    return(rooms)
  } else {
    return(NA) # Return NA if no match is found
  }
}

# Apply the function to the dataset
train_data$roomssynth <- sapply(train_data$description, extract_rooms)
test_data$roomssynth <- sapply(test_data$description, extract_rooms)

train_data$rooms[is.na(train_data$rooms)] <- train_data$roomssynth[is.na(train_data$rooms)]
test_data$rooms[is.na(test_data$rooms)] <- test_data$roomssynth[is.na(test_data$rooms)]

#--------------------------------------------------------------------------#
# 4 # BATHROOMS

# Function to extract the number of bathrooms from the description
extract_bathrooms <- function(description) {
  # Check for NA or missing values
  if (is.na(description) || description == "") return(NA)
  
  # Define the regex pattern to capture the number of bathrooms
  # Matches phrases like "baño 2", "baños 3", etc.
  pattern <- "\\b(bañ[oó](?:s)?)\\s?(\\d+)\\b"
  
  # Find matches in the description
  matches <- regmatches(description, gregexpr(pattern, description, perl = TRUE))
  
  # Extract the first valid match if it exists
  if (length(matches[[1]]) > 0) {
    # Extract the numeric part (group 2 of the regex)
    bathroom_match <- matches[[1]][1]
    bathroom_numeric <- sub(".*(\\d+)$", "\\1", bathroom_match)
    bathrooms <- as.numeric(bathroom_numeric)
    return(bathrooms)
  } else {
    return(NA) # Return NA if no match is found
  }
}

# Apply the function to the dataset
train_data$bathroomssynth <- sapply(train_data$description, extract_bathrooms)
test_data$bathroomssynth <- sapply(test_data$description, extract_bathrooms)

train_data$bathrooms[is.na(train_data$bathrooms)] <- train_data$bathroomssynth[is.na(train_data$bathrooms)]
test_data$bathrooms[is.na(test_data$bathrooms)] <- test_data$bathroomssynth[is.na(test_data$bathrooms)]


#--------------------------------------------------------------------------#
# 2 # DISTANCIA A Transmilenio

# Crear una lista para estaciones de TransMilenio
locations_of_interest <- list(
  list(key = "public_transport", value = "station", label = "TransMilenio")
)

# Crear un sf object desde las coordenadas de 'lat' y 'lon'
train_data_sf <- st_as_sf(train_data, coords = c("lon", "lat"), crs = 4326)

# Crear una función para calcular la distancia a la estación más cercana
calculate_min_distance_transmilenio <- function(locations, sf_data) {
  for (location in locations) {
    # Realizar la consulta OSM para las estaciones de TransMilenio
    osm_query <- opq(bbox = c(-74.25, 4.45, -74, 4.9)) %>%  # Bogotá bbox
      add_osm_feature(key = location$key, value = location$value) %>%
      osmdata_sf()
    
    # Obtener puntos de las estaciones
    osm_points <- st_transform(osm_query$osm_points, crs = 4326) # Usamos WGS84 CRS
    
    # Calcular la distancia entre cada apartamento y las estaciones
    matrix_dist <- st_distance(x = sf_data, y = osm_points)
    min_dist <- apply(matrix_dist, 1, min)
    
    # Agregar la distancia mínima al dataset
    sf_data[[paste0("dist_", location$label)]] <- min_dist
  }
  
  return(sf_data)
}

# Calcular la distancia al TransMilenio más cercano
train_data_sf <- calculate_min_distance_transmilenio(locations_of_interest, train_data_sf)
train_data <- as.data.frame(train_data_sf)

test_data_sf <- st_as_sf(test_data, coords = c("lon", "lat"), crs = 4326)
test_data_sf <- calculate_min_distance_transmilenio(locations_of_interest, test_data_sf)
test_data <- as.data.frame(test_data_sf)


#--------------------------------------------------------------------------#
# 3 # Zona G

# Definir la ubicación central de la Zona G (Calle 72 con Carrera 7)
zona_g_point <- st_sfc(st_point(c(-74.057, 4.655)), crs = 4326) # Coordenadas en WGS84 CRS


##Train
# Crear un sf object desde las coordenadas de 'lat' y 'lon'
train_data_sf <- st_as_sf(train_data, crs = 4326)

# Función para calcular la distancia a un punto específico
calculate_distance_to_point <- function(sf_data, point, label) {
  # Calcular la distancia entre cada punto en sf_data y el punto central dado
  distances <- st_distance(sf_data, point)
  
  # Convertir las distancias a una columna en el sf object
  sf_data[[paste0("dist_", label)]] <- as.numeric(distances) # Convertir a numeric
  return(sf_data)
}

# Calcular la distancia a la Zona G
train_data_sf <- calculate_distance_to_point(train_data_sf, zona_g_point, "zona_g")
train_data <- as.data.frame(train_data_sf)

# Calcular la distancia a la Zona G
test_data_sf <- st_as_sf(test_data, crs = 4326)
test_data_sf <- calculate_distance_to_point(test_data_sf, zona_g_point, "zona_g")
test_data <- as.data.frame(test_data_sf)


#--------------------------------------------------------------------------#
# 3 # Centro Empresarial Santa Bárbara y Usaquén

# Definir la ubicación central de la Zona G (Calle 72 con Carrera 7)
santa_barbara_point <- st_sfc(st_point(c(-74.036, 4.694)), crs = 4326) # Coordenadas en WGS84 CRS


##Train
# Crear un sf object desde las coordenadas de 'lat' y 'lon'
train_data_sf <- st_as_sf(train_data, crs = 4326)

# Función para calcular la distancia a un punto específico
calculate_distance_to_point <- function(sf_data, point, label) {
  # Calcular la distancia entre cada punto en sf_data y el punto central dado
  distances <- st_distance(sf_data, point)
  
  # Convertir las distancias a una columna en el sf object
  sf_data[[paste0("dist_", label)]] <- as.numeric(distances) # Convertir a numeric
  return(sf_data)
}

# Calcular la distancia a la Zona G
train_data_sf <- calculate_distance_to_point(train_data_sf, santa_barbara_point, "santa_barbara")
train_data <- as.data.frame(train_data_sf)

# Calcular la distancia a la Zona G
test_data_sf <- st_as_sf(test_data, crs = 4326)
test_data_sf <- calculate_distance_to_point(test_data_sf, santa_barbara_point, "santa_barbara")
test_data <- as.data.frame(test_data_sf)


#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#
# 3 # PREDICTIONS

install.packages("caret")
install.packages("dplyr")
library(caret)
library(dplyr)

##Imputar
# Lista de variables a mantener
variables_to_keep <- c("price", "dist_TransMilenio", "dist_zona_g", "dist_santa_barbara", "estrato", "area", 
                       "ascensor", "remodelado", "lavanderia", "club", "patio", "balcon", 
                       "amoblado", "parquedero", "operation_type", "property_type", 
                       "bathrooms", "bedrooms", "rooms", "surface_covered", "property_id", "geometry")

# Filtrar train_data para mantener solo las variables especificadas
train_data <- train_data %>%
  select(all_of(variables_to_keep))

# Filtrar test_data para mantener solo las variables especificadas
test_data <- test_data %>%
  select(all_of(variables_to_keep))


test_data2 <- read.csv("/Users/lucia_mr/Dropbox/2. PEG/2024-2/BigData/ProblemSet3/Data/test.csv")
train_data2 <- read.csv("/Users/lucia_mr/Dropbox/2. PEG/2024-2/BigData/ProblemSet3/Data/train.csv")
train_data2 <- train_data2[, c("property_id", "lon", "lat")]
test_data2 <- test_data2[, c("property_id", "lon", "lat")]
merged_train_data <- merge(train_data2, train_data, by = "property_id")
merged_test_data <- merge(test_data2, test_data, by = "property_id")

merged_train_data <- merged_train_data %>%
  mutate(across(everything(), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

merged_test_data <- merged_test_data %>%
  mutate(across(everything(), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

#------------STANDAR EN - 5KM BUFFERS-----------#
# Load necessary libraries
# Librerías necesarias
# Librerías necesarias
library(dplyr)
library(sf)
library(glmnet)
library(caret)

# Crear un objeto sf a partir de train_data si no lo es
if (!"sf" %in% class(merged_train_data)) {
  # Asegúrate de que train_data tiene columnas de longitud y latitud
  if (!all(c("lon", "lat") %in% colnames(merged_train_data))) {
    stop("train_data debe tener columnas 'lon' y 'lat'.")
  }
  train_data <- st_as_sf(merged_train_data, coords = c("lon", "lat"), crs = 4326)
}

train_data <- st_make_valid(train_data)

# Reproyectar a un CRS adecuado para cálculos en metros (EPSG:3857 o similar)
train_data <- st_transform(train_data, crs = 3857)  # Web Mercator CRS

# Crear buffers de 5 km (5000 metros)
buffers <- st_make_grid(
  train_data,                     # Datos espaciales
  cellsize = 5000,                # Tamaño del buffer en metros
  square = FALSE                  # Crear celdas hexagonales
)

buffers_sf <- st_sf(geometry = buffers)

# Crear un plot de los buffers y los puntos
# Create the plot
plot <- ggplot() +
  geom_sf(data = buffers_sf, fill = "lightblue", alpha = 0.5, color = "darkgray") +
  geom_sf(data = train_data, color = "darkred", size = 1.5, shape = 21, fill = "white") +
  labs(
    title = "",
    subtitle = "",
    caption = "Source: Training data and calculated buffers",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.caption = element_text(size = 10, face = "italic", hjust = 1),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid = element_blank(),  # Removes the grid
    legend.position = "bottom"
  )

# Save the plot using ggsave
ggsave("/Users/lucia_mr/Downloads/MGN_2008academic_spatial_plot.png", plot = plot, width = 10, height = 10, dpi = 300)






# Identificar a qué buffer pertenece cada punto
train_data$buffer_id <- st_intersects(train_data, buffers, sparse = FALSE) %>% 
  apply(1, which.max)

# Revisar si buffer_id fue asignado correctamente
table(train_data$buffer_id)

# Convertir a un formato utilizable para modelos
train_data_df <- train_data %>%
  st_drop_geometry() %>%
  select(buffer_id, price, dist_TransMilenio, dist_zona_g, dist_santa_barbara, 
         estrato, area, ascensor, remodelado, lavanderia, club, patio, balcon,
         amoblado, parquedero, operation_type, property_type,
         bathrooms, bedrooms, rooms, surface_covered)

# Configuración de validación cruzada por buffers
set.seed(123)
train_control <- trainControl(
  method = "cv",
  index = createFolds(train_data_df$buffer_id, k = 5, returnTrain = TRUE)
)

# Crear matriz de predictores y respuesta
X <- as.matrix(train_data_df %>% select(-buffer_id, -price))
y <- train_data_df

# Entrenar un modelo Elastic Net con validación cruzada
model <- train(
  x = X, 
  y = y, 
  method = "glmnet", 
  trControl = train_control, 
  tuneLength = 10
)

# Resultados del modelo
print(model)


## PREDICTIONS

# Verificar si merged_test_data tiene columnas de longitud y latitud
if (!"sf" %in% class(merged_test_data)) {
  if (!all(c("lon", "lat") %in% colnames(merged_test_data))) {
    stop("merged_test_data debe tener columnas 'lon' y 'lat'.")
  }
  merged_test_data <- st_as_sf(merged_test_data, coords = c("lon", "lat"), crs = 4326)
}

# Reproyectar merged_test_data al CRS usado en el entrenamiento
merged_test_data <- st_transform(merged_test_data, crs = 3857)

# Convertir merged_test_data en un formato adecuado para predicción
test_data_df <- merged_test_data %>%
  st_drop_geometry() %>%
  select(dist_TransMilenio, dist_zona_g, dist_santa_barbara, 
         estrato, area, ascensor, remodelado, lavanderia, club, patio, balcon,
         amoblado, parquedero, operation_type, property_type,
         bathrooms, bedrooms, rooms, surface_covered)

# Asegurarse de que las columnas de prueba coincidan con las del entrenamiento
missing_cols <- setdiff(colnames(X), colnames(test_data_df))
if (length(missing_cols) > 0) {
  stop("Faltan las siguientes columnas en merged_test_data: ", paste(missing_cols, collapse = ", "))
}

# Crear la matriz de predictores para el conjunto de prueba
X_test <- as.matrix(test_data_df)

# Usar el modelo entrenado para hacer predicciones
predictions <- predict(model, newdata = X_test)

# Mostrar las predicciones
head(predictions)

# Si quieres agregar las predicciones al conjunto de prueba original
merged_test_data <- merged_test_data %>%
  mutate(predicted_price = predictions)

final <- merged_test_data %>%
  select(property_id, predicted_price) %>%
  rename(price = predicted_price)

final <- merged_test_data
if ("sf" %in% class(final)) {
  final <- st_drop_geometry(final)
}

write.csv(final, "/Users/lucia_mr/Dropbox/2. PEG/2024-2/BigData/ProblemSet3/Data/merged_test_data_predictions.csv", row.names = FALSE)


#------------FINETUNNED EN - 5KM BUFFERS-----------#
library(dplyr)
library(sf)
library(glmnet)
library(caret)

# Crear un objeto sf a partir de train_data si no lo es
if (!"sf" %in% class(merged_train_data)) {
  # Asegúrate de que train_data tiene columnas de longitud y latitud
  if (!all(c("lon", "lat") %in% colnames(merged_train_data))) {
    stop("train_data debe tener columnas 'lon' y 'lat'.")
  }
  train_data <- st_as_sf(merged_train_data, coords = c("lon", "lat"), crs = 4326)
}

train_data <- st_make_valid(train_data)

# Reproyectar a un CRS adecuado para cálculos en metros (EPSG:3857 o similar)
train_data <- st_transform(train_data, crs = 3857)  # Web Mercator CRS

# Crear buffers de 5 km (5000 metros)
buffers <- st_make_grid(
  train_data,                     # Datos espaciales
  cellsize = 1000,                # Tamaño del buffer en metros
  square = FALSE                  # Crear celdas hexagonales
)

buffers_sf <- st_sf(geometry = buffers)

# Crear un plot de los buffers y los puntos
ggplot() +
  geom_sf(data = buffers_sf, fill = "blue", alpha = 0.3, color = "black") +
  geom_sf(data = train_data, color = "red", size = 2) +
  labs(title = "Buffers de 5 km alrededor de los puntos",
       caption = "Hexágonos de 5 km creados alrededor de los datos de entrenamiento") +
  theme_minimal()

# Identificar a qué buffer pertenece cada punto
train_data$buffer_id <- st_intersects(train_data, buffers, sparse = FALSE) %>% 
  apply(1, which.max)

# Revisar si buffer_id fue asignado correctamente
table(train_data$buffer_id)

# Convertir a un formato utilizable para modelos
train_data_df <- train_data %>%
  st_drop_geometry() %>%
  select(buffer_id, price, dist_TransMilenio, dist_zona_g, dist_santa_barbara, 
         estrato, area, ascensor, remodelado, lavanderia, club, patio, balcon,
         amoblado, parquedero, operation_type, property_type,
         bathrooms, bedrooms, rooms, surface_covered)

# Configuración de validación cruzada por buffers
set.seed(123)
train_control <- trainControl(
  method = "cv",
  index = createFolds(train_data_df$buffer_id, k = 5, returnTrain = TRUE)
)

# Crear matriz de predictores y respuesta
X <- as.matrix(train_data_df %>% select(-buffer_id, -price))
y <- train_data_df$price

# Entrenar un modelo Elastic Net con validación cruzada
model <- train(
  x = X, 
  y = y, 
  method = "glmnet", 
  trControl = train_control, 
  tuneLength = 10
)

# Resultados del modelo
print(model)


## PREDICTIONS

# Verificar si merged_test_data tiene columnas de longitud y latitud
if (!"sf" %in% class(merged_test_data)) {
  if (!all(c("lon", "lat") %in% colnames(merged_test_data))) {
    stop("merged_test_data debe tener columnas 'lon' y 'lat'.")
  }
  merged_test_data <- st_as_sf(merged_test_data, coords = c("lon", "lat"), crs = 4326)
}

# Reproyectar merged_test_data al CRS usado en el entrenamiento
merged_test_data <- st_transform(merged_test_data, crs = 3857)

# Convertir merged_test_data en un formato adecuado para predicción
test_data_df <- merged_test_data %>%
  st_drop_geometry() %>%
  select(dist_TransMilenio, dist_zona_g, dist_santa_barbara, 
         estrato, area, ascensor, remodelado, lavanderia, club, patio, balcon,
         amoblado, parquedero, operation_type, property_type,
         bathrooms, bedrooms, rooms, surface_covered)

# Asegurarse de que las columnas de prueba coincidan con las del entrenamiento
missing_cols <- setdiff(colnames(X), colnames(test_data_df))
if (length(missing_cols) > 0) {
  stop("Faltan las siguientes columnas en merged_test_data: ", paste(missing_cols, collapse = ", "))
}

# Crear la matriz de predictores para el conjunto de prueba
X_test <- as.matrix(test_data_df)

# Usar el modelo entrenado para hacer predicciones
predictions <- predict(model, newdata = X_test)

# Mostrar las predicciones
head(predictions)

# Si quieres agregar las predicciones al conjunto de prueba original
merged_test_data <- merged_test_data %>%
  mutate(predicted_price = predictions)

final <- merged_test_data %>%
  select(property_id, predicted_price) %>%
  rename(price = predicted_price)

final <- merged_test_data
if ("sf" %in% class(final)) {
  final <- st_drop_geometry(final)
}


write.csv(final, "/Users/lucia_mr/Dropbox/2. PEG/2024-2/BigData/ProblemSet3/Data/merged_test_data_predictions2.csv", row.names = FALSE)
