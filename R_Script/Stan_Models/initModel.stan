data {
	int n; // sample size
	int p; // number of coefficients should be 3 , intercept Doy and DOY2
	matrix[n,p] X; // matrix of observation level parameters 
	vector[n] y; // vector of response variable data, Mosquito count data
	vector[n] offset; // vector of time for offset, will be total trap hours
}

parameters {
	vector[p] beta; // vector of betas for the different parameters
}

model {
	beta ~ normal(0, 5);
	y ~ poisson_log(X * beta) + log(offset);
}