# Cargar conjunto de datos Iris
data(iris)

# Establecer semilla para reproducibilidad
set.seed(123)

# Dividir los datos en entrenamiento (80%) y prueba (20%)
indice <- sample(2, nrow(iris), replace = TRUE, prob = c(0.8, 0.2))
train_data <- iris[indice == 1, ]
test_data <- iris[indice == 2, ]

# Función para calcular la entropía
entropy <- function(labels) {
  probs <- table(labels) / length(labels)
  -sum(probs * log2(probs + 1e-10))
}

# Función para calcular la ganancia de información
information_gain <- function(data, split_attribute_name, target_name) {
  total_entropy <- entropy(data[[target_name]])
  
  # Calculando la entropía después de la división
  vals <- unique(data[[split_attribute_name]])
  weighted_entropy <- 0
  for (val in vals) {
    subset_data <- data[data[[split_attribute_name]] == val, ]
    weighted_entropy <- weighted_entropy + 
      (nrow(subset_data) / nrow(data)) * entropy(subset_data[[target_name]])
  }
  
  information_gain <- total_entropy - weighted_entropy
  return(information_gain)
}

# Función para encontrar el mejor atributo para dividir
find_best_split <- function(data, target_name, attribute_names) {
  best_gain <- 0
  best_attribute <- NULL
  
  for (attribute in attribute_names) {
    gain <- information_gain(data, attribute, target_name)
    if (gain > best_gain) {
      best_gain <- gain
      best_attribute <- attribute
    }
  }
  
  return(best_attribute)
}
# Clase Nodo para construir el árbol
Node <- function(data, target_name, attribute_names) {
  node <- list()
  node$split_attribute <- NULL
  node$children <- list()
  
  # Verificar si los datos son homogéneos o no
  if (length(unique(data[[target_name]])) == 1) {
    node$label <- unique(data[[target_name]])
    return(node)
  }
  
  # Encontrar el mejor atributo para dividir
  node$split_attribute <- find_best_split(data, target_name, attribute_names)
  vals <- unique(data[[node$split_attribute]])

    # Crear nodos hijos y recursión
  for (val in vals) {
    subset_data <- data[data[[node$split_attribute]] == val, ]
    subset_data <- subset_data[, !names(subset_data) %in% node$split_attribute]
    node$children[[as.character(val)]] <- Node(subset_data, target_name, attribute_names)
  }
  
  return(node)
}








# Predecir con los datos de prueba
predictions <- apply(test_data[, attribute_names], 1, function(row) predict(tree, data.frame(t(row)), default_class))

# Mostrar las predicciones
print(predictions)

# Crear la matriz de confusión
conf_matrix <- table(ground_truth = test_data$Species, predicted = predictions)
print("Matriz de Confusión:")
print(conf_matrix)

# Calcular la precisión del modelo
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Precisión del modelo:", accuracy))
  
