
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

ReplaceNaWithValue <- function(data_frame, columns, value) {
  # If columns is a single column name or index, convert it to a vector
  if (!is.vector(columns)) {
    columns <- as.character(columns)
  }
  
  # Loop through each specified column
  for (col in columns) {
    # Replace NA with the specified value
    data_frame[is.na(data_frame[[col]]) , col] <- value
  }
  
  return(data_frame)
}


ReplaceNaWithMean <- function(data_frame, columns) {
  # Calculate the mean of each specified column
  col_means <- colMeans(data_frame[columns], na.rm = TRUE)
  print(col_means)
  
  # Loop through each specified column
  for (col in columns) {
    # Replace NA with the mean of the respective column
    data_frame[is.na(data_frame[[col]]) , col] <- col_means[col]
  }
  
  return(data_frame)
}

RemoveStringFeature <- function(data){
  # Teste le type de chaque colonne
  is_string <- sapply(data, function(x) is.character(x))
  
  # Sélectionne uniquement les colonnes numériques
  data <- data[, !is_string]
  
  return(data)
}

CleanData <- function(data){
  data <- RemoveStringFeature(data)
  data <- RemoveNA(data)
  return(data)
}

BayesianLinearRegressionRstan <- function(data, targetFeature, stanFile = "StanModel.stan") {
  # Define the Stan data
  stan_data <- list(
    N = nrow(data),
    K = ncol(data) - 1,
    X = as.matrix(data[,-which(colnames(data) == targetFeature)]),
    y = data[[targetFeature]]
  )
  
  # Fit the Stan model
  fit <- stan(
    file = stanFile,
    data = stan_data,
    chains = 4,
    iter = 200,
    warmup = 100,
    cores = 4,
    control = list(max_treedepth = 15)
  )
  
  # Print the model summary
  print(fit, digits_summary = 3)
  
  # Return the model object
  return(fit)
}

BayesianLinearRegressionRstanarm <- function(data, targetFeature, prior = normal(location = 0, scale = 10), prior_intercept = normal(location = 0, scale = 10), family = gaussian()){
  formule <- as.formula(paste(paste(targetFeature,"~"), paste(colnames(data)[!colnames(data) %in% c(targetFeature)], collapse = " + ")))
  model <- stan_glm(formule, data = data, prior = prior, prior_intercept = prior_intercept, family = family)
  print(model, digits = 3)
  return(model)
}

BayesianNonLinearRegressionRstanarm <- function(data, targetFeature, prior  = normal(location = 0, scale = 10), prior_intercept = normal(location = 0, scale = 10), iter = 2000, chains = 4){
  formula <- paste(paste(targetFeature,"~"), paste(colnames(data)[!colnames(data) %in% c(targetFeature)], collapse = " + "))
  formule <- as.formula(paste(formula, "+ (educ | gender)"))
  print(formule)
  # Fit the non-linear Bayesian model
  model <- stan_lmer(formule, data = data,prior = prior,prior_intercept = prior_intercept,iter = iter, chains = chains)
  # Print the summary of the model
  print(model, digits = 3)
  # Return the model object
  return(model)
}

BayesianMSERstanarm <- function(model, data, targetFeature, predictions = NULL) {
  
  if (is.null(predictions)){
    # Génère des prédictions à partir du modèle bayésien
    predictions <- posterior_predict(model)
  }
  
  # Calcule l'erreur quadratique moyenne entre les prédictions et les valeurs réelles
  mse <- mean((predictions - data[[targetFeature]])^2)
  
  cat("MSE:\n")
  print(mse)
  
  return(mse)
}

EvaluateBayesianRstanModel <- function(model, data, targetFeature){
  X_data = as.matrix(data[,-which(colnames(data) == targetFeature)])
  y_data = as.matrix(data[[targetFeature]])
  
  # Extract the posterior samples of the parameters
  params <- extract(model)
  
  # Generate posterior predictive samples
  y_rep <- matrix(nrow = nrow(data), ncol = nrow(params$beta))
  for (i in 1:nrow(params$beta)) {
    y_rep[,i] <- rnorm(n = nrow(data), mean = X_data %*% params$beta[i,], sd = params$sigma[i])
  }
  y_mean_rep <- apply(y_rep, 1, mean)
  beta_mean <- as.matrix(apply(params$beta, 2, mean))
  
  # Compute the MSE for each set of posterior predictive samples
  mse <- apply(y_rep, 2, function(y_hat) mean((y_data - y_hat)^2))
  
  # Compute the posterior mean and credible intervals of the MSE
  mse_mean <- mean(mse)
  mse_ci <- quantile(mse, c(0.025, 0.975))
  
  # Print the results
  cat("MSE mean:", round(mse_mean, 3), "\n")
  cat("MSE 95% CI: [", round(mse_ci, 3), "] \n")
  
  # Plot the observed and predicted values
  plot(y_data, y_mean_rep, xlab = paste("Observed",targetFeature), ylab = paste("Predicted",targetFeature), col = "gray")
  abline(0, 1, lwd = 2)
  
  # Plot the residuals
  plot(y_data, y_data - y_mean_rep, xlab = paste("Observed",targetFeature), ylab = "Residuals")
  abline(h = 0, lwd = 2)
}

EvaluateBayesianRstanarmModel <- function(model, data, targetFeature){
  y_data = as.matrix(data[[targetFeature]])
  
  # Generate posterior predictive samples
  y_rep <- posterior_predict(model)
  y_mean_rep <- as.matrix(apply(y_rep, 2, mean))
  
  BayesianMSERstanarm(model, data, targetFeature, y_rep)
  
  # Plot the observed and predicted values
  plot(y_data, y_mean_rep, xlab = paste("Observed",targetFeature), ylab = paste("Predicted",targetFeature), col = "gray")
  abline(0, 1, lwd = 2)
  
  # Plot the residuals
  plot(y_data, residuals(model), xlab = paste("Predicted",targetFeature), ylab = "Residuals")
  abline(h = 0, lwd = 2)
  
  qqnorm(residuals(model))
  qqline(residuals(model))
  
  # Compute the LOO-CV estimates
  loo_fit <- loo(model)
  print(loo_fit)
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


