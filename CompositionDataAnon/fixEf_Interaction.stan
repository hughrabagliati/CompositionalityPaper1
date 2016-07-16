data {
  int<lower=1> N;
  real rt[N];
  real<lower=-1,upper=1> factor1[N];
  real<lower=-1,upper=1> factor2[N];
  real<lower=-1,upper=1> factor3[N];
  int<lower=1> J; // num of subject
  int<lower=1,upper=J> Subj[N]; //list of subject
}

parameters {
//  vector[J] u; //subject intercepts for mean
  real u_e1; //subject intercepts for mean
  vector[J] u_e2; //subject intercepts for mean
  real u_t_e1; //subject intercepts for tau
  vector[J] u_t_e2; //subject intercepts for tau
  real u_s_e1; //subject intercepts for sigma
  vector[J] u_s_e2; //subject intercepts for sigma

  vector[4] beta;
  vector[4] beta_t;
  vector[4] beta_s;
//  real mu;
//  real<lower=0> lambda;
//  real tau;
//  real<lower=0> sigma_e;

  real<lower=0> sigma_u_1;
  real<lower=0> sigma_u_t_1;
  real<lower=0> sigma_u_s_1;
//  real<lower=0> sigma_u_2;
//  real<lower=0> sigma_u_t_2;
//  real<lower=0> sigma_u_s_2;
  
}
transformed parameters {
// ----- was in transformed block
  real mu[N];
  real lambda[N];
  real tau[N];
  real sigma_e[N];
  vector[J] u; //subject intercepts for mean
  vector[J] u_t; //subject intercepts for tau
  vector[J] u_s; //subject intercepts for sigma
// ----- was in transformed block

// build by subject intercepts. I don't know why, but this technique of multiplying
// a vector by a scalar works to ensure that the model can be fit. If you just try to 
// estimate by subject intercepts for sigma and tau otherwise, you get too many exceptions
// due to impossible starting values, such as negative sigmas. 
// I learned this trick from http://www.maths.bath.ac.uk/~jjf23/stan/

  u <- u_e1 * u_e2;  
  u_t <- u_t_e1 * u_t_e2;  
  u_s <- u_s_e1 * u_s_e2;   
   
// build mu, lambda and sigma
  for (i in 1:N){
  mu[i] <-      beta[1] + beta[2] * factor1[i]+ beta[3] * factor2[i]+ beta[4] * factor3[i]  + u[Subj[i]];
 
  lambda[i] <-  beta_t[1] + beta_t[2] * factor1[i]+ beta_t[3] * factor2[i]+ beta_t[4] * factor3[i];//  + u_t[Subj[i]];
 
  sigma_e[i] <- beta_s[1] + beta_s[2] * factor1[i]+ beta_s[3] * factor2[i]+ beta_s[4] * factor3[i];//  + u_s[Subj[i]];  		
    tau[i] <- inv(lambda[i]);
  }
}

model {


 
  // priors on  variability in by subject intercepts
  sigma_u_1 ~ normal(0,1);
  sigma_u_t_1 ~ normal(0,1);
  sigma_u_s_1 ~ normal(0,1);
//  sigma_u_2 ~ normal(0,1);
//  sigma_u_t_2 ~ normal(0,1);
//  sigma_u_s_2 ~ normal(0,1);
  
  // parameters for generating by subject intercepts
  u_e2 ~ normal(0,sigma_u_1); // normal(0,1)
  u_e1 ~ cauchy(0,2.5); //u_e1 ~ cauchy(0,sigma_u or 2.5);
  u_t_e2 ~ normal(0,sigma_u_t_1);
  u_t_e1 ~ cauchy(0,2.5);
  u_s_e2 ~ normal(0,sigma_u_s_1);
  u_s_e1 ~ cauchy(0,2.5);  

  // priors on intercepts
  beta[1] ~ normal(800,10); //double_exponential
  beta_t[1] ~ normal(100,10);
  beta_s[1] ~ normal(150,10);//cauchy(150,10); 
 
  // priors on remaining betas - having these as uniform (ie not specifying) seems to lead to better fits
//  for (i in 2:3){ 
//    beta[i] ~ double_exponential(0,10);//cauchy(0,10);
//    beta_t[i] ~ double_exponential(0,10);//cauchy(0,10);
//    beta_s[i] ~ double_exponential(0,10);//cauchy(0,10); 
//  }
  


  
//  estimate rts
   rt ~ exp_mod_normal(mu,sigma_e,tau);
//}
}