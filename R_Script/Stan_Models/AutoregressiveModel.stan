data {
	int N; // sample size
	int p; // number of coefficients
	matrix[N, p] X; // matrix of observation level parameters 
	int y[N]; // vector of response variable data
	vector[N] offset; // vector of time for offset
}

parameters {
	vector[p] beta; // vector of betas for the different parameters
	vector[N] epsilon; // vector of random effects
	real<lower =0> sigma;
	real arc; // estimate of autoregressive covariate
}

model {
  // Priors
	beta ~ normal(0, 5);
	sigma ~ normal(0,3);
	epsilon ~ normal(0,sigma);
	
	//likelihood
	
	for ( n in 2:N){
	y[n] ~ poisson_log(X[n,]*beta + y[n-1]*arc+ 0 epsilon[n] + log(offset[n]));
	}
}
