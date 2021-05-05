// Stan code for the initial attempt at running an autoregressive model
data {
	int N; // sample size
	//int p; // number of coefficients should be 3 , intercept Doy and DOY2
//	matrix[N, p] X; // matrix of observation level parameters 
	int y[N]; // vector of observations
	vector[N] offset; // vector of time for offset, will be total trap hours
	int G; // number of groups
	//matrix[N,G] Plot; // matrix of group level idenitiy
	int K; // number of lagged points ( lets just start with 1)
	int sizes[G]; // number of points per group
} 
transformed data {
  vector[N] Y_obs;
  for ( t in 1:N){
    Y_obs[t] = y[t];
  }
  
}
parameters {
 //vector[p] beta; // vector of betas for the different parameters
	vector[G] z ; // local AR coefficient for each group  
}

model {
  int pos; // poision in model
  pos = 1;
  
  for (i in 1:G) { // loop for groups
  int local_N; 
  vector[sizes[i]] local_y;
  local_N = sizes[i]; // defining the local number of observations 
  local_y = segment(Y_obs, pos, local_N); // splicing the data of interenst

  for( n in 1:(local_N-K)) { // loop over observation

  y[pos+n] ~ poisson_log( local_y[n]* z[i] + log(offset[pos+n]));
  }
    z[i]~ normal(0,5);
    pos = pos +local_N;
    
  }

  
}
