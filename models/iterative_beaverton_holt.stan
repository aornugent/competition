data {
  int<lower=1> S;
  int<lower=1> K;
  int<lower=1> N_alpha;
  int<lower=1> N_block_effect;
  int<lower=1> N_individuals;
  int<lower=0> N_interactions;
  int ts;
  vector<lower=0>[N_individuals] biomass;
  int<lower=0> dens_m1[N_individuals];
  int<lower=1, upper=S> species[N_individuals];
  int<lower=1, upper=S*K> sp_by_fert[N_individuals];
  int<lower=1, upper=N_alpha> alpha_ij[N_interactions];
  int<lower=1, upper=N_individuals> individual_j[N_interactions];
  //int<lower=1, upper=N_block_effect> block_by_sp[N_individuals];
  //vector[S] log_mu_t0;
  //vector<lower=0>[S] log_sigma_t0;
  int<lower=1, upper=N_alpha> a_ii[N_alpha];
  int<lower=1, upper=N_alpha> a_jj[N_alpha];
  int<lower=1, upper=N_alpha> a_ij[N_alpha];
  int<lower=1, upper=N_alpha> a_ji[N_alpha];
  int<lower=1, upper=S * K> l_i[N_alpha];
  int<lower=1, upper=S * K> l_j[N_alpha];
}
transformed data{
  vector[N_individuals] log_biomass = log(biomass);
}
parameters {
  real<lower=0> log_sigma;
  vector[S * K] log_lambda;
  vector[N_alpha] log_alpha;
  //vector[N_block_effect] log_block_effect;
  vector[N_individuals] log_biomass_t0;
  real<lower=0> unknown_log_sigma_t0[3];
}
transformed parameters{
  vector[N_individuals] log_pred_biomass;

  vector[S] log_mu_t0 = [-5.5464, -5.1262, -9.0230, -6.2277, -8.3966, -7.7885]';

  vector[S] log_sigma_t0 = [0.7860, 0.4801, unknown_log_sigma_t0[1], 0.3850,
                        unknown_log_sigma_t0[2], unknown_log_sigma_t0[3]]';

  {
    vector[N_individuals] log_mu[2];
    vector[N_interactions] alpha;
    vector[N_individuals] log_sum_alpha;
    log_mu[1] = log_biomass_t0;

    for(t in 1:ts){
      int pos = 1;
      alpha = exp(log_alpha[alpha_ij] + log_mu[1, individual_j]);

      for(i in 1:N_individuals){
        log_sum_alpha[i] = log(1 + sum(segment(alpha, pos, dens_m1[i])));
        pos = pos + dens_m1[i];
      }

      log_mu[2] = log_lambda[sp_by_fert] - log_sum_alpha + log_mu[1];
      log_mu[1] = log_mu[2];
    }

    log_pred_biomass = log_mu[2];
  }
}
model{
  // Obs
  log_biomass ~ normal(log_pred_biomass, log_sigma);

  // Priorsrun
  unknown_log_sigma_t0 ~ normal(0.6942, 0.3731);
  log_biomass_t0 ~ normal(log_mu_t0[species], log_sigma_t0[species]);
  log_lambda ~ normal(0, 1);
  log_alpha ~ normal(0, 1);
  log_sigma ~ normal(0, 1);
}
generated quantities{
  vector[S * K] lambda;
  vector[N_alpha] alpha;
  vector[N_alpha] competitive_ability;
  vector[N_alpha] niche_overlap;
  vector[N_alpha] fitness_difference;
  vector[N_individuals] pred_biomass;

  lambda = exp(log_lambda);
  alpha = exp(log_alpha);

  competitive_ability = (lambda[l_i] - 1) ./ sqrt(alpha[a_ij] .* alpha[a_ii]);
  niche_overlap = sqrt((alpha[a_ij] ./ alpha[a_jj]) .* (alpha[a_ji] ./ alpha[a_ii]));
  {
    vector[N_alpha] eta = (lambda[l_j] - 1) ./ (lambda[l_i] - 1);
    vector[N_alpha] gamma = sqrt((alpha[a_ij] ./ alpha[a_jj]) .* (alpha[a_ii] ./ alpha[a_ji]));
    fitness_difference = eta .* gamma;
  }

  for(i in 1:N_individuals){
    pred_biomass[i] = exp(normal_rng(log_pred_biomass[i], log_sigma));
  }
}
