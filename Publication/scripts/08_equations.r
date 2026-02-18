#* 8: Equation construction
#+ 8.1: Load required objects
coefs_tbl <- readRDS("Calculator/lasso_weighted_coefs.rds")
coefs_named <- setNames(coefs_tbl$Coefficient, coefs_tbl$Variable)
platt_model <- readRDS("Calculator/platt_model.rds")
#+ 8.2: Rebuild logit equation string from coefs_tbl
#- 8.2.1: Function to build eqn
build_logit_equation <- function(coefs) {
  # Drop NA coefficients
  valid_coefs <- coefs[!is.na(coefs)]

  # Pretty display names
  term_map <- c(
    "(Intercept)" = "(Intercept)",
    "ASA" = "ASA",
    "sexM" = "sexM",
    "max_carotid" = "max_carotid",
    "age_ASA" = "(Age × ASA)",
    "age_max_vert" = "(Age × max_vert)"
  )

  # Split intercept vs predictors
  intercept <- round(valid_coefs["(Intercept)"], 2)
  predictors <- setdiff(names(valid_coefs), "(Intercept)")

  # Round everything except age_max_vert
  pred_terms <- sapply(predictors, function(var) {
    coef_val <- valid_coefs[[var]]

    # Round to 3 decimals ONLY for age_max_vert
    rounded_val <- if (var == "age_max_vert") {
      round(coef_val, 3)
    } else {
      round(coef_val, 2)
    }

    paste0(rounded_val, "·", term_map[var])
  })

  # Combine into final equation
  paste(
    "logit(p) =", intercept,
    if (length(pred_terms) > 0) paste("+", paste(pred_terms, collapse = " + ")) else ""
  )
}
#- 8.2.2: Build the equation
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
