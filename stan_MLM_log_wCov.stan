//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//


data {
  int<lower=0> N; //number of problem/attempts
  int<lower=0> N_prob; // number of problems
  int<lower=0, upper=N_prob> problem_id[N]; // num proups
  int<lower=0,upper=1> y[N]; 
  int<lower=0,upper=1> treatment[N]; // N after name makes this a list
  real feature1[N_prob];
  real feature2[N_prob];
  
}

parameters {
//  real<lower=0> sigma0;
//  real<lower=0> sigma1;
  real b0[N_prob];
  real g0;
  real g10;
  real g11;
    real g12;
 // real<lower=-1,upper=1> rho; // for one correlation
  corr_matrix[2] Omega;        // prior correlation
  vector<lower=0>[2] tau;      // prior scale 
  vector[2] b[N_prob]; // hint, err, time

}

transformed parameters {
    cov_matrix[2] SigmaProb;
    SigmaProb = quad_form_diag(Omega, tau);

}

model {
  vector[2] bMean[N_prob];  
  for(i in 1:N_prob){
    bMean[i][1]=0;
    bMean[i][2]=g10+g11*feature1[i]+g12*feature1[2];
    }
  
  // priors
  tau ~ cauchy(0, 2.5); // SD of intercepts and slope // cauchy allows for a weak prior b/c of its large outliers and it is unpredictable
  Omega ~ lkj_corr(2); //prior 
  
  b~multi_normal(bMean,SigmaProb);

  for (i in 1:N){
  y[i] ~ bernoulli_logit(
      b[problem_id[i]][1] + // random intcercpt
      b[problem_id[i]][2]*treatment[i] // random effect of treatement
    );
    }
}




