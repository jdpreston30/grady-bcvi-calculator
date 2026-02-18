#* 8: Equation construction
#+ 8.1: Load required objects
coefs_tbl <- readRDS("../Calculator/data/lasso_weighted_coefs.rds")
coefs_named <- setNames(coefs_tbl$Coefficient, coefs_tbl$Variable)
platt_model <- readRDS("../Calculator/data/platt_model.rds")
#+ 8.2: Rebuild logit equation string from coefs_tbl
logit_equation <- build_logit_equation(coefs_named)
#+ 8.3: Platt scaling equation
platt_coefs <- coef(platt_model)
platt_equation <- paste0(
  "scaled_logit = ",
  round(platt_coefs[1], 2), " + ",
  round(platt_coefs[2], 2), "·p\n",
  "scaled_prob = 1 / (1 + exp(-scaled_logit))"
)
#+ 8.3: Platt scaling equation
cat("Logistic Regression Model:\n")
cat(logit_equation, "\n\n")
cat("Platt Scaling Model:\n")
cat(platt_equation)
