data {
	int n; // sample size
	int p; // number of coefficients
	matrix[n, p] X; // matrix of observation level parameters 
	int y[n]; // vector of response variable data
	vector[n] offset; // vector of time for offset
}

parameters {
	vector[p] beta; // vector of betas for the different parameters
}

model {
	beta ~ normal(0, 5);
	y ~ poisson_log(X*beta +log(offset));
}