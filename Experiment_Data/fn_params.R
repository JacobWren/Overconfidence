# Configure parameters (e.g., line slope, variance, etc.).

fn_data_params <- function() {
  # Parameters for all figured
  params_n_ <- c(
    'n' = 20,
    'n_post' = 10,
    # Problems 1-36: the pretend is identical and the only thing we
    # change is the line/no line and the end date.
    'n_problems_set1' = 36,
    # Problems 37-48: the variance in the no line problems gets scaled down.
    'n_problems_set2' = 12
  )
  # Set training figures parameters
  params_train_ <- c(
    'mean' = 0,
    'var1' = 1.5,
    'var2' = 14,
    'var3' = 4,
    'slope1' = 1.6,
    'slope2' = -0.30,
    'slope3' = 0.40
  )
  # Set practice figures parameters
  params_prac_ <- c(
    'mean' = 0,
    'var1' = 1.5,
    'var2' = 1.5,
    'var3' = 4,
    'var4' = 15,
    'slope1' = -1.1,
    'slope2' = -1.1,
    'slope3' = 0.5,
    'slope4' = -0.5,
    'scale1' = 3,
    'scale2' = 11,
    'scale3' = 1,
    'scale4' = 1
  )
  # Set comp figures parameters
  params_comp_ <- c(
    'mean' = 0,
    'var' = 1,
    'slope' = 0.20,
    'scale1' = 2,
    'scale2' = 8
  )
  return(
    list(
      params_n_ = params_n_,
      params_train_ = params_train_,
      params_prac_ = params_prac_,
      params_comp_ = params_comp_
    )
  )
}