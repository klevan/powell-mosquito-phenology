data {
	int n; // sample size
	int p; // number of coefficients
	matrix[n, p] X; // matrix of observation level parameters 
	int y[n]; // vector of response variable data
	vector[n] offset; // vector of time for offset
}

parameters {
	vector[p] beta; // vector of betas for the different parameters
	vector[n] epsilon; // vector of random effects
	real<lower =0> sigma;
}

model {
  // Priors
	beta ~ normal(0, 5);
	sigma ~ normal(0,3);
	epsilon ~ normal(0,sigma);
	
	//likelihood
	
	y ~ poisson_log(X*beta + epsilon + log(offset));
}