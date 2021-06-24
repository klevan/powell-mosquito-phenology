// Stan code to develop a simple mechanistic model for mosquito abundacne 
// patterns

data {
	int N; // sample size
  vector[N] Temp; // temperature data
  int y[N]; // observarion data
  vector[N] offset;
  int T; // number of differnt types of paramets to estimate
  int A; // number of baselines to estimate
  int m ;
  int em ;
} 

parameters {
  real Toptim_m[A]; // vector for thermal performance parameters for survial
  real CTmax_M[A]; // vector for thermal performance parameters for emergence
  real<lower = 0> sigma_m; //  vector of estimated sigma values
  real Toptim_em[A]; // vector for thermal performance parameters for survial
  real CTmax_eM[A]; // vector for thermal performance parameters for emergence
  real<lower = 0> sigma_em; //  vector of estimated sigma values
  real<lower = 0, upper =1> alpham[A]; // scaler for survival
  real< lower = 0> alphae[A]; // scaler for pool of emergence 
}


transformed parameters{

  vector[N] lambda; // vector of predicted values

  vector[N] s; // vector of survial rates
  
  vector[N] emerge; // vector of number adultemerged 

  lambda[1] = y[1]; // setting intial values
  
  for( t in 2:N ){ 
    
    // setting the temperature sensitive parameters ( survival and emergence)
    
    if( Temp[t-1] <= m ){
      
      s[t-1] = alpham * pow( exp(1),(Temp[t-1]-Toptim_m[1])/ 
              pow( (2 * sigma_m), 2)));
      
    } else{
      
      s[t-1] =  alpham * ( 1- ( (Temp[t-1] - Toptim[1])/ pow((Toptim[1]-CtMax[1]),2 ) );
      
    }
      
      if( Temp[t-1] <= em ){
      
      emerge[t-1] = alphae *exp(1)^(Temp[t-1]-Toptim[2])/(2*Sigma[2])^2;
    
    } else{
      emerge[t-1] = alphae * ( 1- ( (Temp[t-1] - Toptim[2])/ (Toptim[2]-CtMax[2]))^2 );
    }
      
      
    // predicing  
    
      lambda[t] +=  y[t-1]*S[t-1] + emerge[t-1];
    }
  
}

model {
  y ~ poisson_log( lambda + log(offset) );
  
  // Priors
  
  alphae ~ normal(0,20);
  alpham ~ beta(1,1);
  
  Toptim ~ normal(25,4)
  CtMax ~ normal(30,4)
  Sigma ~  normal(0,10)

  
}
