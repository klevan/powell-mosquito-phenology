// Stan code for the initial attempt at running an autoregressive model
data {
	int N; // sample size
	//int p; // number of coefficients should be 3 , intercept Doy and DOY2
//	matrix[N, p] X; // matrix of observation level parameters 
	int y[N]; // vector of response variable data, Mosquito count data
	real offset[N]; // vector of time for offset, will be total trap hours
	int G; // number of groups
	matrix[N,G] Plot; // matrix of group level idenitiy
	int K; // number of lagged points ( lets just start with 1)
	int sizes[G]; // number of points per group
} 

parameters {
 //vector[p] beta; // vector of betas for the different parameters
	vector[K] z[G]; // local AR coefficient for each group  
	vector[K] global_K; // global AR coefficient
}

transformed parameters{
  vector [K] group_betas[G]; // group level multiplies for mode
  for ( i in 1:G) {
    group_betas[i] = global_K .* z[i]; // allows us to just multiple each point once
  }
}

model {
  int pos; // poision in model
  pos = 1
  for (i in 1:G) { // loop for groups
  int local_N; 
  vector[sizes[i]] local_y; // defining the local y variable
  local_N = sizes[i]: // defining the local number of observations 
  local_y = segment(y, pos, local_N); // splicing the data of interenst
  loc_offset = segment(offset, pos, local_N);
  for( n in (K+1):local_N) { // loop over observation
  local_y[n] ~ poisson_log( group_betas[i][K] * local_y[n-K] + 
   log(loc_offset[n]))
    }
    z[i]~ normal(0,5);
    pos = pos +local_N;
  }
  // hyperparameters
  global_K ~ cauchy(0,5)
  
}
