# Version for October 2023 "big pilot".
# 1) Generates figures & data (writes correct answers to .csv) for each problem.
# 2) Verifies coverage probability.
# Note: data parameters are set in "fn_params.R" module.

# Load functions
source("~/overprecision/fn_params.R")
source("~/overprecision/fn_data.R")
source("~/overprecision/fn_true_dist.R")
source("~/overprecision/fn_plot.R")
source("~/overprecision/fn_coverage.R")
source("~/overprecision/fn_answers.R")

# Training examples
# Helper grid to map different params to fn_data().
train_variants <- list(c('slope1', 'var1'), c('slope2', 'var2'),
                       c('slope3', 'var3'))
set.seed(2445633)  # Set training seed.
set_params <- fn_data_params()
params_n <- set_params$params_n_
params_train <- set_params$params_train_
# Generate training data for different slopes and variances.
train_data <-
  lapply(train_variants, function(x)
    fn_makedata(
      slope_ = params_train[x[1]],
      noisevec_ = rnorm(params_n['n'],
                        params_train['mean'], params_train[x[2]]),
      post_ = params_n['n_post']
    ))

# Unpack training data.
lowvarv <- train_data[[1]]
morevarv <- train_data[[2]]
nolinev <- train_data[[3]]

set.seed(24532)

# Helper grid to map different params to fn_makedata().
prac_variants <-
  list(
    c('slope1', 'scale1', 'var1'),
    c('slope2', 'scale2', 'var2'),
    c('slope3', 'scale3', 'var3'),
    c('slope4', 'scale4', 'var4')
  )

params_prac <- set_params$params_prac_
# Generate practice data for different slopes, scales, and variances.
prac_data <-
  lapply(prac_variants, function(x)
    fn_makedata(
      slope_ = params_prac[x[1]],
      noisevec_ = params_prac[x[2]] * rnorm(params_n['n'],
                                            params_prac['mean'], params_prac[x[3]]),
      post_ = params_n['n_post']
    ))
# Unpack practice data.
practice1 <- prac_data[[1]]
practice2 <- prac_data[[2]]
practice3 <- prac_data[[3]]
practice4 <- prac_data[[4]]

# Helper grid to map different params to fn_makedata().
comp_variants <- list(c('scale1'), c('scale2'))

params_comp <- set_params$params_comp_
# Generate comp data for different scales.
comp_data <-
  lapply(comp_variants, function(x)
    fn_makedata(
      slope_ = params_comp['slope'],
      noisevec_ = params_comp[x[1]] * rnorm(params_n['n'],
                                            params_comp['mean'], params_comp['var']),
      post_ = params_n['n_post']
    ))

# Unpack comp data.
comp1 <- comp_data[[1]]
comp2 <- comp_data[[2]]

x_lim = c(0, 19.5)

# Helper grid to map different params to fn_makeplot().
# NA for when the argument is not applicable (but lapply() needs a value)
fig_variants <- list(
  c('train1', list(lowvarv), params_train['slope1'], 1, 0, NA),
  c('train2', list(morevarv), params_train['slope2'], 1, 0, NA),
  c('train3', list(nolinev), NA, 0, 0, NA),
  c('practice1', list(practice1), params_prac['slope1'], 1, 0, NA),
  c('practice2', list(practice2), params_prac['slope2'], 1, 0, NA),
  c('practice3', list(practice3), NA, 0, 0, NA),
  c('practice4', list(practice4), NA, 0, 0, NA),
  c('comp1', list(comp1), NA, 0, 1, "A"),
  c('comp2', list(comp2), NA, 0, 1, "B")
)

# Generate plots for different data sets
train_data <- lapply(fig_variants, function(x)
  fn_makeplot(
    alldata_ = x[[2]],
    slope_ = x[[3]],
    tpred_ = params_n['n'],
    treat_ = x[[4]],
    xlim_ = x_lim,
    plot_name_ = x[[1]],
    text_ = x[[5]],
    text_letter_ = x[[6]]
  ))

fn_rangeplot(params_n, x_lim)

num_practice <- 4
practiceans <- data.frame(problem = c(1:num_practice))

# Helper grid to map different params to fn_makedata().
prac_answr_variants <- list(
  c(list(practice1), 'slope1', 1),
  c(list(practice2), 'slope2', 1),
  c(list(practice3), 'slope3', 0),
  c(list(practice4), 'slope4', 0)
)

# Generate practice answers for different psets.
prac_answr_data <-
  lapply(prac_answr_variants, function(x)
    fn_trudist(
      alldat_ = x[[1]],
      slope_ = params_prac[x[[2]]],
      line_ = x[[3]]
    ))

# Unpack practice answers
p1_dist <- prac_answr_data[[1]]
p2_dist <- prac_answr_data[[2]]
p3_dist <- prac_answr_data[[3]]
p4_dist <- prac_answr_data[[4]]

for (sum_stat in c("mean", "sd")) {
  practiceans[[sum_stat]] <-
    c(p1_dist[[sum_stat]], p2_dist[[sum_stat]], p3_dist[[sum_stat]],
      p4_dist[[sum_stat]])
}
write.csv(practiceans, "practiceans.csv")

# DATA PLOTS FOR THE PILOT
# For 1-6 the randomization is no line/line and near/far
n_sims <- 6 # Number of simulations
# Real SDs across simulations
sds <- seq(1, 8, length.out = n_sims)

# Pre data points
prepoints <- params_n['n_post']
# post for close problems
postc <- 3
# post for long problems
postf <- 13

set.seed(2342423)
bins <- seq(-2, 2, length.out = 7)
slopestrat <- rep(NA, n_sims)
# shuffling
for (i in 1:n_sims)
  slopestrat[i] <- runif(1, bins[i], bins[i + 1])

# Simulating data
# Randomizing the order of the slopes
slopes <- slopestrat[sample(1:n_sims, n_sims)]
# Empty list for data
dats <- list()
# Also making a separate "close" data that stops earlier;
# for easier input to the fn_trudist function
datsclose <- list()
for (i in 1:n_sims) {
  dats[[i]] <- fn_makedata(
    slope_ = slopes[i],
    pre_ = prepoints,
    post_ = postf,
    noisevec_ <- rnorm(prepoints + postf, 0, sds[i])
  )
  datsclose[[i]] <- dats[[i]][1:(prepoints + postc),]
}

# Making plots
c_lim = 0.20
f_lim = 0.30
for (i in 1:n_sims) {
  fn_makeplot(
    alldata_ = dats[[i]],
    slope_ = slopes[i],
    tpred_ = prepoints + postc,
    xlim_ = c(0, prepoints + postc - c_lim),
    plot_name_ = paste0(i, "c", "l"),
    text_ = 0,
    width_ = 300
  )
  
  fn_makeplot(
    alldata_ = dats[[i]],
    slope_ = slopes[i],
    tpred_ = prepoints + postf,
    xlim_ = c(0, prepoints + postf - f_lim),
    plot_name_ = paste0(i, "f", "l"),
    text_ = 0
  )
  
  fn_makeplot(
    alldata_ = dats[[i]],
    slope_ = slopes[i],
    treat_ = 0,
    tpred_ = prepoints + postc,
    xlim_ = c(0, prepoints + postc - c_lim),
    plot_name_ = paste0(i, "c", "n"),
    text_ = 0,
    width_ = 300
  )
  
  fn_makeplot(
    alldata_ = dats[[i]],
    slope_ = slopes[i],
    treat_ = 0,
    tpred_ = prepoints + postf,
    xlim_ = c(0, prepoints + postf - f_lim),
    plot_name_ = paste0(i, "f", "n"),
    text_ = 0
  )
}

# Right answers for each problem.
sols <-
  fn_solutions(datsclose, datsclose, dats, dats, slopes, n_sims, 0,
               num_practice)
# Combine to one data frame
ans <- sols[[1]]
for (i in 2:n_sims)
  ans <- rbind(ans, sols[[i]])
# Write answers
write.csv(ans, "ans.csv")

# For 7-12 the randomization is no line/line and near/far
# AND we scale the variance so the true answer is the same across these simulations

# Real SDs across simulations.
sds <- seq(8, 22, length.out = n_sims)

set.seed(242143)
bins <- seq(-2, 2, length.out = 7)
slopestrat <- rep(NA, n_sims)
# shuffling
for (i in 1:n_sims)
  slopestrat[i] <- runif(1, bins[i], bins[i + 1])

# Simulating data
# Randomizing the order of the slopes
slopes <- slopestrat[sample(1:n_sims, n_sims)]

# Empty lists for data storage
dats_lf <- list()
dats_lc <- list()
dats_nf <- list()
dats_nc <- list()
for (i in 1:n_sims) {
  # Baseline noise vector for the line problems.
  basenoise <-
    rnorm(prepoints + postf, params_train['mean'], sds[i])
  # Creating line data, first for far.
  dats_lf[[i]] <-
    fn_makedata(
      slope_ = slopes[i],
      pre_ = prepoints,
      post_ = postf,
      noisevec_ <- basenoise
    )
  # now chopping off the final points for the close version
  dats_lc[[i]] <- dats_lf[[i]][1:(prepoints + postc),]
  # Making the "guess" normalization factors
  dists <- list(c(dats_lf, 1), c(dats_lf, 0), c(dats_lc, 0))
  # Initialize an empty vector.
  line_combos <- c()
  for (dist in dists) {
    line_combos <-
      c(line_combos,
        fn_trudist(
          alldat_ = dist[[i]],
          slope_ = slopes[i],
          line_ = dist[[i + 1]]
        )$sd)
  }
  # Some guesses for the right factor
  n_tests <- params_n['n']
  testfactors_f <-
    seq(.8 * line_combos[1] / line_combos[3],
        1.2 * line_combos[1] / line_combos[3],
        length.out = n_tests)
  testfactors_c <-
    seq(.8 * line_combos[1] / line_combos[2],
        1.2 * line_combos[1] / line_combos[2],
        length.out = n_tests)
  testsds_f <- rep(NA, n_tests)
  testsds_c <- rep(NA, n_tests)
  for (test in 1:n_tests) {
    testsds_f[test] <-
      fn_trudist(
        alldat_ = fn_makedata(
          slope_ = slopes[i],
          pre_ = prepoints,
          post_ = postf,
          noisevec_ <-
            basenoise * testfactors_f[test]
        ),
        slope_ = slopes[i],
        line_ = 0
      )$sd
    testsds_c[test] <-
      fn_trudist(
        alldat_ = fn_makedata(
          slope_ = slopes[i],
          pre_ = prepoints,
          post_ = postc,
          noisevec_ <-
            basenoise[1:(prepoints + postc)] * testfactors_c[test]
        ),
        slope = slopes[i],
        line = 0
      )$sd
  }
  adjfactor_f <-
    testfactors_f[which.min(abs(testsds_f - line_combos[1]))]
  adjfactor_c <-
    testfactors_c[which.min(abs(testsds_c - line_combos[1]))]
  # Close data with normalized noise vector
  dats_nf[[i]] <-
    fn_makedata(
      slope_ = slopes[i],
      pre_ = prepoints,
      post_ = postf,
      noisevec_ <- basenoise * adjfactor_f
    )
  # For close version, need to truncate the noise vector
  dats_nc[[i]] <-
    fn_makedata(
      slope_ = slopes[i],
      pre_ = prepoints,
      post_ = postc,
      noisevec_ <-
        basenoise[1:(prepoints + postc)] * adjfactor_c
    )
}

# Making plots
for (i in 1:n_sims) {
  fn_makeplot(
    alldata_ = dats_lc[[i]],
    slope_ = slopes[i],
    tpred_ = prepoints + postc,
    xlim_ = c(0, prepoints + postc - .2),
    plot_name_ = paste0(n_sims + i, "c", "l"),
    text_ = 0,
    width_ = 300
  )
  
  fn_makeplot(
    alldata_ = dats_lf[[i]],
    slope_ = slopes[i],
    tpred_ = prepoints + postf,
    xlim_ = c(0, prepoints + postf - .3),
    plot_name_ = paste0(n_sims + i, "f", "l"),
    text_ = 0
  )
  
  fn_makeplot(
    alldata_ = dats_nc[[i]],
    slope_ = slopes[i],
    treat_ = 0,
    tpred_ = prepoints + postc,
    xlim_ = c(0, prepoints + postc - .2),
    plot_name_ = paste0(n_sims + i, "c", "n"),
    text_ = 0,
    width_ = 300
  )
  
  fn_makeplot(
    alldata_ = dats_nf[[i]],
    slope_ = slopes[i],
    treat_ = 0,
    tpred_ = prepoints + postf,
    xlim_ = c(0, prepoints + postf - .3),
    plot_name_ = paste0(n_sims + i, "f", "n"),
    text_ = 0,
  )
}

rightlist2 <- fn_solutions(dats_lc,
                           dats_nc,
                           dats_lf,
                           dats_nf,
                           slopes,
                           n_sims,
                           n_sims,
                           num_practice)

# Combinine to one data frame
ans2 <- rightlist2[[1]]
for (i in 2:n_sims)
  ans2 <- rbind(ans2, rightlist2[[i]])

allans <- rbind(ans, ans2)
allans$scaled <- ifelse(allans$problem > n_sims, 1, 0)
write.csv(allans, "ans.csv")

# Check coverage probability.
mean(replicate(10000, fn_simhit(var_ = "hit90")))  # 90 coverage w/ line
mean(replicate(10000, fn_simhit()))  # 95 coverage w/ line
mean(replicate(1000, fn_simhit(line_ = 0, var_ = "hit90")))  # 90 coverage w/ out line
mean(replicate(1000, fn_simhit(line_ = 0)))  # 95 coverage w/ out line