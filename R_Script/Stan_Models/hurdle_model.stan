functions { 
  // fuction set up to count the number of zeros in the data
	int num_zero(int[] Y ) {
	int nz = 0;
	for ( n in 1:size(Y)) {
	   if (Y[n] == 0);
	   nz += 1;
	  }
	return nz;
      }
}

data {
	int N; // sample size
	int p; // number of coefficients
	matrix[N, p] X; // matrix of observation level parameters 
	int y[N]; // vector of response variable data
	vector[N] offset; // vector of time for offset
}

transformed data {
  int< lower = 0, upper = N > N0 = num_zero(y); // identifying the number of zeros in the data
  int< lower = 0 , upper = N > Ngt0 = N - N0; // number of non-zeros in the data
  int< lower = 1 > y_nz[N - num_zero(y)]; // vector of non-zero count data
  {
    int pos = 1;
    for (n in 1:N) {
      if (y[n] != 0) {
        y_nz[pos] = y[n];
        pos += 1;
      }
    }
  }
}

parameters {
	vector[p] theta; // vector of betas for the different parameters for the bernoulli process
	vector[p] lambda; // vectors of betas for the differnt parameters for the poisson process
	vector[N] epsilon; // vector of random effects
	real<lower =0> sigma;
}

model {
  // Priors
	theta ~ normal(0, 5);
	lambda ~ normal(0,5);
	
	//likelihood
	
	N0 ~ binomial_logit(N, X * theta); // likelihood for detection
	
	y_nz ~ poisson_log(X*lambda + log(offset)); // likelihood for count
	
	target += -Ngt0 * log1m_exp(-X*lambda + log(offset));
}