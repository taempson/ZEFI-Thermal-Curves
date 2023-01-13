// generated with brms 2.18.0
functions {
}
data {
  int<lower=1> N;  // total number of observations
  int Y[N];  // response variable
  int<lower=1> K_x0;  // number of population-level effects
  matrix[N, K_x0] X_x0;  // population-level design matrix
  int<lower=1> K_y0;  // number of population-level effects
  matrix[N, K_y0] X_y0;  // population-level design matrix
  // covariate vectors for non-linear functions
  vector[N] C_1;
  // data for group-level effects of ID 1
  int<lower=1> N_1;  // number of grouping levels
  int<lower=1> M_1;  // number of coefficients per level
  int<lower=1> J_1[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_1_x0_1;
  // data for group-level effects of ID 2
  int<lower=1> N_2;  // number of grouping levels
  int<lower=1> M_2;  // number of coefficients per level
  int<lower=1> J_2[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_2_y0_1;
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
}
parameters {
  vector<lower=25,upper=45.5>[K_x0] b_x0;  // population-level effects
  vector<lower=0.1>[K_y0] b_y0;  // population-level effects
  real<lower=0> Intercept_shape;  // temporary intercept for centered predictors
  vector<lower=0.1,upper=4>[M_1] sd_1;  // group-level standard deviations
  vector[N_1] z_1[M_1];  // standardized group-level effects
  vector<lower=0.1,upper=200>[M_2] sd_2;  // group-level standard deviations
  vector[N_2] z_2[M_2];  // standardized group-level effects
}
transformed parameters {
  vector[N_1] r_1_x0_1;  // actual group-level effects
  vector[N_2] r_2_y0_1;  // actual group-level effects
  real lprior = 0;  // prior contributions to the log posterior
  r_1_x0_1 = (sd_1[1] * (z_1[1]));
  r_2_y0_1 = (sd_2[1] * (z_2[1]));
  lprior += uniform_lpdf(b_x0 | 25, 45.5)
    - 1 * log_diff_exp(uniform_lcdf(45.5 | 25, 45.5), uniform_lcdf(25 | 25, 45.5));
  lprior += normal_lpdf(b_y0 | 150, 200)
    - 1 * normal_lccdf(0.1 | 150, 200);
  lprior += gamma_lpdf(Intercept_shape | 0.01, 0.01);
  lprior += uniform_lpdf(sd_1 | 0.1, 10)
    - 1 * log_diff_exp(uniform_lcdf(4 | 0.1, 10), uniform_lcdf(0.1 | 0.1, 10));
  lprior += uniform_lpdf(sd_2 | 0.1, 200)
    - 1 * log_diff_exp(uniform_lcdf(200 | 0.1, 200), uniform_lcdf(0.1 | 0.1, 200));
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] nlp_x0 = rep_vector(0.0, N);
    // initialize linear predictor term
    vector[N] nlp_y0 = rep_vector(0.0, N);
    // initialize non-linear predictor term
    vector[N] mu;
    // initialize linear predictor term
    vector[N] shape = rep_vector(0.0, N);
    nlp_x0 += X_x0 * b_x0;
    nlp_y0 += X_y0 * b_y0;
    shape += Intercept_shape;
    for (n in 1:N) {
      // add more terms to the linear predictor
      nlp_x0[n] += r_1_x0_1[J_1[n]] * Z_1_x0_1[n];
    }
    for (n in 1:N) {
      // add more terms to the linear predictor
      nlp_y0[n] += r_2_y0_1[J_2[n]] * Z_2_y0_1[n];
    }
    for (n in 1:N) {
      // compute non-linear predictor values
      mu[n] = 0 - (46 - fmax(nlp_x0[n] , C_1[n])) * (0 - nlp_y0[n]) / (46 - nlp_x0[n]);
    }
    target += neg_binomial_2_lpmf(Y | mu, shape);
  }
  // priors including constants
  target += lprior;
  target += std_normal_lpdf(z_1[1]);
  target += std_normal_lpdf(z_2[1]);
}
generated quantities {
  // actual population-level intercept
  real b_shape_Intercept = Intercept_shape;
}
