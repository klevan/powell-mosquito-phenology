data {
	int N; // sample size
	int p; // number of coefficients
	matrix[N, p] X; // matrix of observation level parameters 
	int y[N]; // vector of response variable data
	vector[N] offset; // vector of time for offset
	int G; // number of plots
	matrix[N, G] Plot; // matrix of plot assignment per observation

}

parameters {
	vector[p] beta; // vector of betas for the different parameters
	vector[N] epsilon; // vector of observation random effects
	vector[G] alpha; // vector of random intercept effect
	real<lower =0> sigma;
}

model {
  // Priors
// Priors
  // beta ~ normal(0,5); // Removing the prior for beta cause it is constricting
  // the abiltiy to explore the parameter space, not specifying a prior results
  // in stan using a non-informative prior for the beta term
	sigma ~ normal(0,3);
	epsilon ~ normal(0,sigma);
	alpha ~ normal(0,5);
	
	//likelihood
	for ( n in 1:N){
	y[n] ~ poisson_log(X[n,]*beta + epsilon[n] +
	 Plot[n,]*alpha + log(offset[n]));
	}
}
