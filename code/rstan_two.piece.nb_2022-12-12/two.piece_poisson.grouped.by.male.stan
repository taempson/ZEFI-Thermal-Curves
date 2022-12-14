// initial and working model formulation using t0 and b0 rather than t0 and y0.
// y0 is calculated as a transformed parameter
// Trying to follow Stan manual Ch 1.9: Hierarchical logistic regression


data {
  int<lower=1> N;  // # of data points
  int<lower=1> L; // Levels = # of males;
  array[N] int<lower=0> y; //song_count
  array[N] real x; // predictor: temp values at each observation
  // This, along with male, is the predictor
  array[N] int<lower=1, upper=L> ll; // male id, one for each observation
  // max value and response at max value
  real xmax; // max temp
  real<lower=0> y_xmax; // y value at xmax, generally will be 0
  // Ensure model behaves well.
  real x0min; //min threshold value
  real<lower=x0min+1> x0max; //max threshold value
  //  vector[100] tp; // points used to create predictions for plotting
}

//transformed data {
//  array[N] row_vector[D] s  = xmax - x;
//  //vector[100] sp = xmax - tp;
//}

parameters { // One set of parameters for each level (male) 
  array[L] real<upper=0>b0; // slope of decrease
  // Doesn't work: array[L] vector<lower=x0min,upper=x0max>[D] x0; //first threshold,
  array[L] real<lower=x0min,upper=x0max> x0; //first threshold,
}

transformed parameters {
  //  array[L] real<lower=x0min,upper=x0max> s0 = xmax - x0; // first threshold
  //array[L] real<lower=x0min,upper=x0max> a0 = - b0; // slope of decrease
  //vector[L] y0;
  //y0[ll] = (xmax - x0[ll]);// .* b0[ll]; // pre-threshold level
//  vector[N] lambda = (y_xmax  - fmin(xmax - x, xmax - x0[ll]) .* b0[ll]);
}

model {
  // I see examples where `target` is used as an (undefined) objective function.
  // This line of code runs, but fails to converge 
  // target += poisson_lupmf(y| y_xmax + fmin(tt, a0) * b0);
  real val1;
  real val2;
  vector[N] lambda;

  //Priors
  //  for (l in 1:L) {
  // }
  //Data 
  for (n in 1:N) {
    val1 = xmax - x[n];
    val2 = xmax - x0[ll[n]];
    lambda[n] = y_xmax  - fmin(val1, val2) .* b0[ll[n]];    
    y[n] ~ poisson(lambda[n]);
  }
}

