//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N; // number of observations
  int<lower=0> K; // number of predictors
  matrix[N, K] X; // predictor matrix
  vector[N] y; // response vector
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  vector[K] beta; // regression coefficients
  real<lower=0> sigma; // error standard deviation
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  y ~ normal(X * beta, sigma); // likelihood
  beta ~ normal(0, 10); // prior for coefficients
  sigma ~ cauchy(0, 2.5); // prior for error standard deviation
}

//generated quantities {
//  vector[N] y_hat; // fitted values
//  y_hat = X * beta;
//}

