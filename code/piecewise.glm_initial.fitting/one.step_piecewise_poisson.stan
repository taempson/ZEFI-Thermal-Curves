data {
  int<lower=0> N;  // # of data points
  int<lower=0> y[N]; //song_count
  vector[N] x; //temp
  //int<lower=0> M; // # of males;
  //int<lower=1> L; //# of line segments
  real tmax; // max temp
  real y_tmax; // y value at tmax, generally will be 0  
}

transformed data {
  vector[N] xt  = tmax - x;
}
    
parameters {
  //  vector[L] b_vals;
  real<upper=0> b1;
  real<lower=0,upper=tmax> x1;
}

transformed parameters {
  real a1 = tmax - x1;
  real bt1 = - b1;
}

model {
  // I see examples where `target` is used as an (undefined) objective function.
  // This line of code runs, but fails to converge 
  //target += poisson_lupmf(y| y_tmax + fmin(xt, a1) * bt1);

  //This runs and converges.
  y ~ poisson(y_tmax + fmin(xt, a1) * bt1);
}
