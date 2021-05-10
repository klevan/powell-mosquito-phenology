// Stan code for the initial attempt at running an autoregressive model

data {
	int N; // sample size
  int<lower = 0> P; // Order of AR
  int y[N]; // observarion data
  vector[N] offset;
} 

transformed data {
  vector[N] y_obs;
  for( i in 1:N){
    y_obs[i] = y[i];
  }
}

parameters {
  vector[P] beta; // vector of betas for the different parameters
	real alpha; // intercept term
}

transformed parameters{
  vector[N] lambda; 
  
  lambda[1:P] = y_obs[1:P]; // setting intial values
  
  for( t in (P+1):N ) {
    lambda[t] = alpha;
    for(p in 1:P){
      lambda[t] += beta[p] * y[t-p];
    }
  }
}

model {
  y ~ poisson_log( lambda + log(offset));
}

generated quantities{
  vector[N] y_pred;
  
  y_pred[1:P] = y_obs[1:P];
  
  
  
}