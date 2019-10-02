# Box 1 -------------------------------------------------------------------
# Test for non-identifiability in the joint-estimation of
# environmental repsonses.
library(tidyverse)
library(optimr)
library(furrr)
# library(ggpubr)

# Simulate abudance -------------------------------------------------------

# Shortcut to truncate above zero
L <- function(x) pmax(x, 0)

# Function to simulate abundance along a fertility gradient

# noise_sd : random independent variation in growth
# obs_sd   : random variation in final abundance, ie. measurement
# nsim     : number of points along gradient

sim <- function(pars, noise_sd, obs_sd, n_sim) {

  # Simulate gradient
  fert <- runif(n_sim, 0, 80)

  # Generate noise
  noise1 <- rnorm(n_sim, 0, noise_sd)
  noise2 <- rnorm(n_sim, 0, noise_sd)
  obs1 <- rnorm(n_sim, 0, obs_sd)
  obs2 <- rnorm(n_sim, 0, obs_sd)

  # Abundance of each species in isolation
  aloneN1 <- L((pars$r1 + pars$b1 * fert + noise1) / pars$a11 + obs1)
  aloneN2 <- L((pars$r2 + pars$b2 * fert + noise2) / pars$a22 + obs2)

  # Equilibrium abundances in the field
  # Unsure if it still makes sense to divide by 2sp carrying capacity if
  # either is N = 0.
  estN1 <- L((pars$a22 * L(pars$r1 + pars$b1 * fert + noise1) -
                pars$a12 * L(pars$r2 + pars$b2 * fert + noise2)) /
                  (pars$a22 * pars$a11 - pars$a12 * pars$a21) + obs1)

  estN2 <- L((pars$a11 * L(pars$r2 + pars$b2 * fert + noise2) -
                pars$a21 * L(pars$r1 + pars$b1 * fert + noise1)) /
               (pars$a22 * pars$a11 - pars$a12 * pars$a21) + obs2)

  return(list(
    mono = data.frame(
            fert = fert,
            N1 = aloneN1,
            N2 = aloneN2),
    mixt = data.frame(
            fert = fert,
            N1 = estN1,
            N2 = estN2),
    comp = data.frame(
            fert = fert,
            N1 = estN1 / aloneN1,
            N2 = estN2 / aloneN2)
    ))
}


# Intrinsic growth, response to fertility and interaction coefficients
par_list <- list(
  r1 = 0.8,
  r2 = 0.2,
  b1 = 0.01,
  b2 = 0.02,
  a11 = 0.05,
  a22 = 0.05,
  a12 = 0.04,
  a21 = 0.01
)


# Simulate with low noise
x <- sim(par_list, 0.01, 1, 1000)

# Plot monocultures
p1 <- gather(x$mono, sp, abun, N1:N2) %>%
  ggplot(., aes(x = fert, y = abun, color = sp)) +
  geom_point(size = 1) +
    scale_color_manual(values = c("darkgreen", "blue")) +
    coord_cartesian(ylim = c(0, 40), expand = F) +
    theme_bw() +
    guides(color = F) +
    labs(x = "Fertility",
         y = "Abundance",
         subtitle = "a) Abundance in isolation") +
    theme(aspect.ratio = 1)

# Plot mixtures
p2 <- gather(x$mixt, sp, abun, N1:N2) %>%
  ggplot(., aes(x = fert, y = abun, color = sp)) +
  geom_smooth(method = "lm", se = F, size = 2) +
  geom_point(size = 1) +
  scale_color_manual(values = c("darkgreen", "blue")) +
  coord_cartesian(ylim = c(0, 40), expand = F) +
  theme_bw() +
  guides(color = F) +
  labs(x = "Fertility",
       y = "Abundance",
       subtitle = "b) Abundance in competition") +
  theme(aspect.ratio = 1)

# Plot effect of competition
p3 <- gather(x$comp, sp, abun, N1:N2) %>%
  ggplot(., aes(x = fert, y = abun, color = sp)) +
  geom_point(size = 1) +
  scale_color_manual(values = c("darkgreen", "blue")) +
  coord_cartesian(ylim = c(0, 1.01), expand = F) +
  theme_bw() +
  guides(color = F) +
  labs(x = "Fertility",
       y = "mixture / monoculture",
       subtitle = "c) Intensity of competition") +
  theme(aspect.ratio = 1)


# Joint-species estimation ------------------------------------------------
# Likelihood function for bivariate normal
neg_ll <- function(b, y, x, tobit) {

  # check parameter constraints
  if(b[5] < -1 | b[5] > 1 | b[6] < 0 | b[7] < 0)
    return(-Inf)

  # mean vectors
  mu = matrix(c(b[1] + b[2] * x, b[3] + b[4] * x), ncol = 2)

  # covariance matrices
  sigma12 = b[5] * b[6] * b[7]
  Sigma = matrix(c(b[6]^2, sigma12, sigma12, b[7]^2), ncol = 2)

  if(tobit) {
    # latent parameters for tobit regression
    y_star <- mu + matrix(rnorm(length(mu)), ncol = 2) %*% chol(Sigma)

    # uncensored observations
    y[y < 0] <- y_star[y < 0]
  }

  # inverse and determinant
  invSigma = solve(Sigma)
  detSigma = det(Sigma)


  # log-likelihood
  ll = map2_dbl(.x = array_branch(mu, 1),
                .y = array_branch(y, 1),
                ~ -0.5 * (log(detSigma) + 2 * log(2 * pi) +
                            t(.y - .x) %*% invSigma %*% (.y - .x)))
  return(-sum(ll))
}

# Bivariate JSDM using MLE.
est <- function(mixt, tobit = F) {

    # set sensible starting values
  m1 <- lm(N1 ~ fert, data = mixt)
  m2 <- lm(N2 ~ fert, data = mixt)

  y = cbind(mixt$N1, mixt$N2)

  p = c(a1 = coef(m1)[1],
        b1 = coef(m1)[2],
        a2 = coef(m2)[1],
        b2 = coef(m2)[2],
        rho = cor(y)[1, 2],
        sigma11 = sd(y[, 1]),
        sigma22 = sd(y[, 2]))

  # MLE of bivariate normal
  m.ll <- optimr(par = abs(p),
                fn = neg_ll,
                y = y,
                x = mixt$fert,
                tobit = tobit,
                method = "BFGS")

  # return parameters
  return(enframe(m.ll$par) %>%
           spread(name, value) %>%
           mutate(log_lik = m.ll$value))
}

# ~5sec, with or without tobit
system.time(est(x$mixt, tobit = F))


# Simulate effect of increasing process noise
noise <- function(pars, min = 0.00, max = 0.5, obs_sd = 1, n_sim = 20) {
  # generate gradient of process noise
  noise_sd <- seq(min, max, len = n_sim)

  # simulate 1000 obs at each level
  x = map(noise_sd, ~ sim(pars, ., obs_sd, 1000))

  # run MLE at each level (parallelised)
  y = future_map_dfr(x, ~ est(.$mixt)) %>%
    rename_all(~ gsub("\\(Intercept\\)|fert", "", .))

  # calculate JSDM bias on abundance scale
  res <- data.frame(noise = noise_sd) %>%
    bind_cols(y) %>%
    mutate(e1 = (b1. - (pars$b1 / pars$a11)),
           e2 = (b2. - (pars$b2 / pars$a22)),
           cov = rho * sigma11 * sigma22)

  return(res)
}

# repeat sim along gradient of process noise
plan(multisession)
res <- noise(par_list, max = 1, n_sim = 2000)
save(res, file = "models/lv_jsdm.Rdata")


# Plot confounded env. response against process noise
p4 <- select(res, noise, e1, e2) %>%
  gather(sp, err, -noise) %>%
  ggplot(., aes(x = noise, y = err^2, color = sp)) +
  geom_point(size = 1) +
  scale_color_manual(values = c("darkgreen", "blue")) +
  coord_cartesian(ylim = c(0, 0.14), expand = F) +
  theme_bw() +
  guides(color = F) +
  labs(x = "Process noise",
       y = "Square error",
       subtitle = "d) JSDM error") +
  theme(aspect.ratio = 1)

ggpubr::ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2)

ggsave(last_plot(), file = "figures/f5_LV_JSDM_simulation.png",
       device = "png", dpi = 600, width = 7, height = 7)


# Supplementary figures
# Simulate with low noise
x <- sim(par_list, 0.01, 1, 2000)
p5 <- gather(x$mixt, sp, abun, N1:N2) %>%
  ggplot(., aes(x = fert, y = abun, color = sp)) +
  geom_smooth(method = "lm", se = F, size = 2) +
  geom_point(size = 1, alpha = 0.6) +
  scale_color_manual(values = c("darkgreen", "blue")) +
  coord_cartesian(ylim = c(0, 100), expand = F) +
  theme_bw() +
  guides(color = F) +
  labs(x = "Fertility",
       y = "Abundance",
       subtitle = "a) Process noise = 0.01") +
  theme(aspect.ratio = 1)

# Simulate with high noise
x <- sim(par_list, 1, 1, 2000)

p6 <- gather(x$mixt, sp, abun, N1:N2) %>%
  ggplot(., aes(x = fert, y = abun, color = sp)) +
  geom_smooth(method = "lm", se = F, size = 2) +
  geom_point(size = 1, alpha = 0.6) +
  scale_color_manual(values = c("darkgreen", "blue")) +
  coord_cartesian(ylim = c(0, 100), expand = F) +
  theme_bw() +
  guides(color = F) +
  labs(x = "Fertility",
       y = "Abundance",
       subtitle = "b) Process noise = 1") +
  theme(aspect.ratio = 1)


ggpubr::ggarrange(p5, p6)
ggsave(last_plot(), filename = "figures/s4_process_noise.png",
       device = "png", dpi = 600, width = 8, height = 4.5)

# Plot covariance against process noise
select(res, noise, correlation = rho, covariance = cov) %>%
  gather(meas, val, -noise) %>%
  ggplot(., aes(x = noise, y = val)) +
    geom_point(color = "red") +
    coord_cartesian(xlim = c(0, 1.01), expand = F) +
    facet_wrap(~ meas, scales = "free_y") +
    theme_bw() +
    guides(color = F) +
    labs(x = "Process noise",
         y = "Competitive association") +
    theme(aspect.ratio = 1)

ggsave(last_plot(), filename = "figures/s5_lv_jsdm_covariance.png",
       device = "png", dpi = 600, width = 6.5, height = 4)

# Plot confounded env. response against process noise
select(res, process_noise = noise, covariance = cov, e1, e2) %>%
  gather(sp, err, -process_noise, -covariance) %>%
  gather(meas, val, -sp, -err) %>%
    ggplot(., aes(x = val, y = err^2, color = sp)) +
    geom_point() +
    scale_color_manual(values = c("darkgreen", "blue")) +
    coord_cartesian(ylim = c(0, 0.14), expand = F) +
    facet_grid(~ meas, scales = "free_x") +
    theme_bw() +
    guides(color = F) +
    labs(x = "",
         y = "Square error (est - true)^2",
         subtitle = "d) JSDM error") +
    theme(aspect.ratio = 1)
