data {
  int<lower=1> N;                   // individuals
  int<lower=1> N_jk;                // interactions
  int<lower=1> JK;                  // species x fertility
  int<lower=1> JKL;                 // interaction coefficients
  int<lower=1> B;                   // blocks

  vector<lower=0>[N] y;             // observations
  int<lower=1> n_l[N];              // competitors per pot
  int<lower=1, upper=JK> jk[N];     // species x fertility index
  int<lower=1, upper=JKL> jkl[N_jk];// species x fertility x competitor index

  int<lower=1, upper=B> b[N];       // block index
}
parameters{
  // constrain parameters
  vector<lower=0>[JK] a0;
  vector<lower=0>[JKL] a;
  vector<lower=0>[JK] log_sigma;

  // offset for blocks B-D
  vector<lower=0>[B-1] b_ref;
}
transformed parameters{
  vector[N] log_mu;
  vector<lower=0>[B] b_all = append_row(1, b_ref);

  {
    vector[N] w;
    int l_1 = 1;  // first competitor
    int l_n;      // last competitor
    for (i in 1:N) {
      l_n = l_1 + n_l[i] - 1;
      // expected weight
      w[i] = (a0[jk[i]] + sum(a[jkl[l_1:l_n]]))^-1 * b_all[b[i]];
      // transform to log-scale
      log_mu[i] = log(w[i]) - 0.5 * log_sigma[jk[i]]^2;
      l_1 = l_n + 1;
    }
  }
}
model {
  // observation model
  y ~ lognormal(log_mu, log_sigma[jk]);

  // priors
  a0 ~ gamma(10, 1);
  a ~ gamma(10, 1);
  b_ref ~ normal(1, 1);
  log_sigma ~ normal(0, 1);
}
generated quantities {
  vector<lower=0>[N] y_;

  for(i in 1:N){
    y_[i] = lognormal_rng(log_mu[i], log_sigma[jk[i]]);
  }
}
