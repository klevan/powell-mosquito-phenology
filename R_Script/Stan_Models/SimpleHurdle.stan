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
	vector[p] theta; // vector of betas for the different parameters for the bernoulli process
	vector[p] lambda; // vectors of betas for the differnt parameters for the poisson process
	vector[G] alpha_poisson; // vector of random intercept effect
	vector[G] alpha_bern; // vector of random intercept effect
//	vector[N] epsilon; // vector of random effects
	//real<lower =0> sigma;
}

model {
  // Priors
	theta ~ normal(0, 5);
	lambda ~ normal(0,5);
	alpha_poisson ~ normal(0,5);
	alpha_bern ~ normal(0,5);
	
	//likelihood
	for (n in 1:N){
	  	if( y[n] == 0 )
	1 ~ bernoulli_logit(X[n,]*theta+ Plot[n,]*alpha_bern);
	else {
	  0 ~ bernoulli_logit(X[n,]*theta+ Plot[n,]*alpha_bern);
	  y[n] ~ poisson_log(X[n,]*lambda +
	 Plot[n,]*alpha_poisson + log(offset[n]));
	 }
	}
}
