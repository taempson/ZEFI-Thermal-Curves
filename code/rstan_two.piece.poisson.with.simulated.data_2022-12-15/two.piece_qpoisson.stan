// Goal: Fit qpoisson and models for two piecewise model 
// 2022-12-16:
// - Copied from two.piece_poisson.grouped.by.male.stan
// - Males can have own p value (M = L) which is fixed over all temp
//   or they can share them (pooled M = 1)


/* Math

\lambda ~ Gamma(\kappa = r, \theta = p/1-p) = Gamma(\alpha = r, \beta = (1-p)/p)
E(\lambda) = \kappa \theta = \alpha/\beta = r p/(1-p)
Var(\lambda) = E(\lambda) \theta = E(\lambda)/\beta = r (p/(1-p))^2

Y ~ Poisson(\lambda| \lambda ~ Gamma())
  ~ NB(r, p) = NB(\alpha = \kappa = r, \beta = (1-p)/p = = 1/\theta)

E(Y) = \alpha/beta = r p/(1-p) = E(\lambda)
Var(Y) = \alpha/\beta^2 (\beta +1) = Var(\lambda) (\beta + 1) = Var(\lambda)/p
       = r (p/(1-p))^2/p =  r p/(1-p)^2
If we want Var(Y) = \theta E(Y) with \theta > 1, we set p = 1/\theta and keep it constant over range of r values


An alternative formulation (NB2 in stan) is by mean and inverse, scaled dispersion factor.

Y ~ NB2(\mu, \phi)

E(Y) = \mu; Var(Y) = \mu + \mu^2/\phi

for Var(Y) = \theta E(Y) we set phi = \mu/(\theta - 1)
 

 */

functions {

  // Returns expected counts | model parameters and variable x
  real f_mu(real b0, real x0, real xmax, real y_xmax, real x) {
    real val1;
    real val2;

    val1 = xmax - x;
    val2 = xmax - x0;
    return y_xmax  - fmin(val1, val2) .* b0; // Can I use * instead of .*?
  }

  // Returns appropriate phi value for NB2 for Quasipoisson error given mu and theta
  real f_phi(real mu, real<lower = 1> theta) {

    return mu/(theta-1);
  }
}

data {
  int<lower=1> N; // # of data points
  int<lower=1> L; // Levels = # of males;
  int<lower=1, upper=L> M; // Levels for NB parameter p
  array[N] int<lower=0> y; //song_count
  vector[N] x; // predictor: temp values at each observation
  // This, along with male, is the predictor
  array[N] int<lower=1, upper=L> ll; // male id, one for each observation
  array[N] int<lower=1, upper=M> mm; // qpoisson parameter grouping
  // max x value and response (y) at max value
  real xmax; // max temp
  real<lower=0> y_xmax; // y value at xmax, generally will be 0
  // Ensure model behaves well.
  real x0min; //min threshold value
  real<lower=x0min+1> x0max; //max threshold value
  //  vector[100] tp; // points used to create predictions for plotting
  real<lower=1> sd_y0_prior;
  real<lower=0> alpha_theta_prior;
}

//transformed data {
//  array[N] row_vector[D] s  = xmax - x;
//  //vector[100] sp = xmax - tp;
//}

parameters { // One set of parameters for each level (male) 
  vector<upper=0>[L] b0; // slope of decrease
  vector<lower=x0min,upper=x0max>[L] x0; //first threshold
  vector<lower=0,upper=1>[M] theta;
}

transformed parameters {
  //  array[L] real<lower=x0min,upper=x0max> s0 = xmax - x0; // first threshold
  //array[L] real<lower=x0min,upper=x0max> a0 = - b0; // slope of decrease
  vector<lower=1>[L] y0;
  //  vector[N] lambda = (y_xmax  - fmin(xmax - x, xmax - x0[ll]) .* b0[ll]);
}

model {
  // I see examples where `target` is used as an (undefined) objective function.
  // This line of code runs, but fails to converge 
  // target += poisson_lupmf(y| y_xmax + fmin(tt, a0) * b0);
  vector[N] phi;
  vector[N] mu;
  
  //Priors
    for (l in 1:L) {
      //x0[l] ~ uniform(30, 44);
      y0[l] ~ normal(150, sd_y0_prior);
   }
  for (m in 1:M) {
    theta[M] ~ exponential(alpha_theta_prior)
   }
  //Data 
  for (n in 1:N) {
    mu[n] = f_mu(b0[ll[n]], x0[ll[n]], xmax, y_xmax, x[n]);
    phi[n] = f_phi(mu[n], theta[mm[n]])
    y[n] ~ neg_binomial_2_lupmf(mu[n], phi[n])
  }
}




// 2022-12-03:
//
// - initial and working model formulation using t1 and b0 rather than t1 and y0.
// - This runs well.
// - y0 is calculated as a transformed parameter
//
// 2022-12-09:
// Converted from poisson to nb and quasipoisson 
// For qpoisson, we assume alpha varies between individuals, but beta = shared
data {
  int<lower=0> N;  // # of data points
  int<lower=0> y[N]; //song_count
  vector[N] t; //temp
  //int<lower=0> M; // # of males;
  //int<lower=1> L; //# of line segments
  real tmax; // max temp
  real t1min; //min threshold value
  real t1max; //max threshold value
  real y_tmax; // y value at tmax, generally will be 0
  vector[100] tp; // points used to create predictions for plotting
  int<lower=0, upper=1> qpoisson_flag;
  
}

transformed data {
  vector[N] s  = tmax - t;
  vector[100] sp = tmax - tp;
}

parameters {
  //  vector[L] b_vals;
  real<upper=0> b1; // slope of decrease
  real<lower=t1min,upper=t1max> t1; //first threshold,
  // need upper bound to avoid chain getting caught at t1 = tmax
  real<lower=1> alpha; // target # of successes to observe
  // E(x| alpha, beta) = alpha/beta = alpha (1-omega)/omega
  // Var(x| alpha, beta) = alpha/beta*(beta+1)/beta = E(x) (beta+1)/beta = E(x) omega
  real<lower=1.001> omega; //overdispersion parameter; > 1
  // pr(success|trial) = p = 1/omega = beta/(1+beta);
  // beta = 1/(omega -1) = p/(1-p) = odds of 'success' 
}

transformed parameters {
  real s1 = tmax - t1; // first threshold
  real a1 = - b1; // slope of increase
  real y0 = s1*a1; // pre-threshold level
  real beta = 1/(omega-1)
  vector[N] lambda = y_tmax + fmin(s, s1) * a1;
}


model {
  // I see examples where `target` is used as an (undefined) objective function.
  // This line of code runs, but fails to converge 
  // target += poisson_lupmf(y| y_tmax + fmin(tt, a1) * bt1);

  // This runs and converges.
  // So long as 
  y ~ poisson(lambda);
}

generated quantities {

  // Get LLik values for each data point
  // These are used in the LOO analysis of model fit(s)
  vector[N] log_lik;
  vector[100] yp; //predicted values based on xp
  
  //yp = y_tmax + fmin(sp, s1) * a1;
  for(n in 1:100) {
      yp[n] = y_tmax + fmin(sp[n], s1) * a1;
  }
  for (n in 1:N) {
    log_lik[n] = poisson_lpmf(y[n] | lambda[n]);
  }
}
