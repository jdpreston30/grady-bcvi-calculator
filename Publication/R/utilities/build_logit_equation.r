#' Build Human-Readable Logit Equation from Model Coefficients
#'
#' Constructs a formatted logit equation string from a named vector of model 
#' coefficients. Handles rounding, pretty variable names (including interaction 
#' terms with × notation), and drops NA coefficients. Special precision handling 
#' for age_max_vert interaction term (3 decimals vs 2 for others).
#'
#' @param coefs Named numeric vector of model coefficients. Must include 
#'   "(Intercept)" and predictor names matching term_map keys.
#' @return Character string of formatted logit equation, e.g.:
#'   "logit(p) = -4.32 + 0.85·ASA + 0.03·max_carotid + 0.001·(Age × max_vert)"
#' @export
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