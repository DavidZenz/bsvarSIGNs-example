rm(list = ls())
invisible(gc())

library(bsvars)
library(bsvarSIGNs)

# Load ts_data directly
ts_data <- readRDS("minimal_reproducible_example.rds")

# Sign restriction matrix
n_vars <- ncol(ts_data)
sign_irf <- matrix(NA, nrow = n_vars, ncol = n_vars)
rownames(sign_irf) <- colnames(ts_data)
colnames(sign_irf) <- paste0("shock_", colnames(ts_data))

for (i in 2:n_vars) {
  short_code <- sub("^d_hr_", "", colnames(ts_data)[i])
  if (short_code %in% c("fxlcu", "wag")) {
    sign_irf[i, 1] <- NA
  } else {
    sign_irf[i, 1] <- 1
  }
}

for(lag in 1:8) {
  
  message(paste0("Lag: ", lag))
  
  # Estimate and plot
  spec <- specify_bsvarSIGN$new(ts_data, p = lag, sign_irf = sign_irf)
  out <- estimate(spec, 50000)
  
  fitted <- bsvars::compute_fitted_values(out)
  plot(fitted)
  
  forecasted <- bsvars::forecast(out, horizon = 8)
  plot(forecasted)
  
  fevd <- bsvars::compute_variance_decompositions(out, horizon = 8)
  plot(fevd)
}
