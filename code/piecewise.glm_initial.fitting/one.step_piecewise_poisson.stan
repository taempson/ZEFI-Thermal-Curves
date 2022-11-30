data {
  int<lower=0> N;  // # of data points
  int<lower=0> y[N]; //song_count
  vector[N] t; //temp
  //int<lower=0> M; // # of males;
  //int<lower=1> L; //# of line segments
  real tmax; // max temp
  real t1max; //max threshold value
  real y_tmax; // y value at tmax, generally will be 0  
}

transformed data {
  vector[N] s  = tmax - t; 
}

parameters {
  //  vector[L] b_vals;
  real<upper=0> b1; // slope of decrease
  real<lower=0,upper=t1max> t1; //first threshold,
  // need upper bound to avoid chain getting caught at t1 = tmax
}

transformed parameters {
  real s1 = tmax - t1; // first threshold
  real a1 = - b1; // slope of increase
}


model {
  // I see examples where `target` is used as an (undefined) objective function.
  // This line of code runs, but fails to converge 
  //target += poisson_lupmf(y| y_tmax + fmin(tt, a1) * bt1);

  //This runs and converges.
  y ~ poisson(y_tmax + fmin(s, s1) * a1);
}
