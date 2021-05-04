data {
	int N; // sample size
	int p; // number of coefficients should be 3 , intercept Doy and DOY2
	matrix[N, p] X; // matrix of observation level parameters 
	int y[N]; // vector of response variable data, Mosquito count data
	real offset[N]; // vector of time for offset, will be total trap hours
}

parameters {
	vector[p] beta; // vector of betas for the different parameters
}

model {
	beta ~ normal(0, 5);
	// Likelihood
	y ~ poisson_log(X*beta);
}