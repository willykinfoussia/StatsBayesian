

ImportData <- function(path){
  # Importe le fichier CSV
  donnees <- read.csv(path)
  
  # Affiche la forme du dataset
  cat("Shape:\n")
  print(dim(donnees))
  
  # Affiche les variables (colonnes) du dataset
  cat("Variables:\n")
  print(colnames(donnees))
  
  # Affiche les premières lignes des données
  print(head(donnees))
  
  # Retourne le dataset
  return(donnees)
}

