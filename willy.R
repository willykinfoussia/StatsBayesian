
GetDatasetInformation <- function(data){
  # Affiche la forme du dataset
  cat("Shape:\n")
  print(dim(data))
  
  # Affiche les variables (colonnes) du dataset
  cat("Variables:\n")
  print(colnames(data))
  
  # Affiche les premières lignes des données
  print(head(data)) 
}

ImportData <- function(path){
  # Importe le fichier CSV
  donnees <- read.csv(path)
  
  GetDatasetInformation(donnees)
  
  # Retourne le dataset
  return(donnees)
}

RemoveNA <- function(data){
  donnees_nettoyees <- na.omit(data)
  return(donnees_nettoyees)
}

RemoveStringFeature <- function(data){
  # Teste le type de chaque colonne
  is_string <- sapply(data, function(x) is.character(x))
  
  # Sélectionne uniquement les colonnes numériques
  data <- data[, !is_string]
  
  return(data)
}

CleanData <- function(data){
  # data <- RemoveStringFeature(data)
  data <- RemoveNA(data)
  GetDatasetInformation(data)
  return(data)
}

BayesianLinearRegression <- function(data, targetFeature, prior = normal(location = 0, scale = 10), prior_intercept = normal(location = 0, scale = 10)){
  formule <- as.formula(paste(paste(targetFeature,"~"), paste(colnames(data)[!colnames(data) %in% c(targetFeature)], collapse = " + ")))
  model <- stan_glm(formule, data = data, prior = prior, prior_intercept = prior_intercept)
  print(model, digits = 3)
  return(model)
}

BayesianMSE <- function(model, data, targetFeature) {
  # Génère des prédictions à partir du modèle bayésien
  predictions <- posterior_predict(model, newdata = data)
  
  # Calcule l'erreur quadratique moyenne entre les prédictions et les valeurs réelles
  mse <- mean((predictions - data[[targetFeature]])^2)
  
  cat("MSE:\n")
  print(mse)
  
  return(mse)
}

MCMCTrace <- function(model){
  model %>% mcmc_trace()
}

MCMCRhat <- function(model){
  model %>%
    rhat() %>%
    mcmc_rhat() +
    yaxis_text()
}

# Define the plotting function
PlotMCMC <- function(model, data, print_trace = FALSE) {
  # Extract the MCMC samples as a matrix
  mcmc_samples <- as.matrix(model)
  
  # Calculate the number of parameters
  n_params <- ncol(mcmc_samples)
  
  # Get the parameter names from the dataset
  param_names <- colnames(data)[-1]
  
  # Generate trace plots for each parameter
  trace_plots <- lapply(1:n_params, function(i) {
    ggplot(data.frame(Iteration = 1:nrow(mcmc_samples), Value = mcmc_samples[, i]), aes(x = Iteration, y = Value)) +
      geom_line() +
      labs(x = "Iteration", y = paste("Value of parameter", param_names[i]), title = paste("Trace plot of parameter", param_names[i]))
  })
  
  # Generate density plots for each parameter
  density_plots <- lapply(1:n_params, function(i) {
    ggplot(data.frame(Value = mcmc_samples[, i]), aes(x = Value)) +
      geom_density() +
      labs(x = paste("Value of parameter", param_names[i]), y = "Density", title = paste("Density plot of parameter", param_names[i]))
  })
  if(print_trace == TRUE){
    print(trace_plots)
  }
  print(density_plots)
}

SummaryMCMC <- function(model){
  
  summary(model)
  
  # Convertit la chaîne en objet mcmc de coda
  #mcmc <- as.mcmc(model)
  
  # Trace l'évolution des paramètres au fil des itérations
  #traceplot(mcmc)
  
  # Affiche les statistiques sommaires des paramètres estimés
  #summary(mcmc)
  
  # Create density plots for each parameter
  #densplot(mcmc)
  
  # Create autocorrelation plots for each parameter
  #acf(mcmc)
  
  # Trace l'autocorrélation de la chaîne MCMC
  #autocorr.plot(mcmc)
  
  # Diagnostique la convergence de la chaîne en utilisant le critère de R après Gelman et Rubin
  #gelman.diag(mcmc)
  
  # Compute the effective sample size for each parameter
  #effectiveSize(mcmc)
}

TestMCMCRstanarm <- function(){
  # Load example data
  data(mtcars)
  
  # Fit a Bayesian linear regression model with mpg as the target feature
  fit_rstanarm <- stan_glm(mpg ~ ., data = mtcars, prior = normal(location = 0, scale = 10), prior_intercept = normal(location = 0, scale = 10))
  
  MCMCTrace(fit_rstanarm)
  
  MCMCRhat(fit_rstanarm)
}

TestMCMCRstan <- function(){
  # Load example data
  data(mtcars)
  
  predictors <- mtcars %>% select(-mpg)
  
  stan_data <- list(
    N = 32,
    K = 10,
    X = predictors,
    y = mtcars$mpg
  )
  
  fit_rstan <- stan(
    file = "mtcars.stan",
    data = stan_data
  )
  
  MCMCTrace(fit_rstan)
  
  MCMCRhat(fit_rstan)
}


