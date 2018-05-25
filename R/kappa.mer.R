kappa.mer <- function (fit, scale = TRUE, center = FALSE, add.intercept = TRUE,
  exact = FALSE) {
  # adapted version of base R's kappa()
  X = getME(fit,"X")
  nam = names(fixef(fit))
  ## exclude intercepts
  nrp = sum(1 * (nam == "(Intercept)"))
  if (nrp > 0) {
    X = X[, -(1:nrp), drop = FALSE]
    nam = nam[-(1:nrp)]
  }
  if (add.intercept) {
    X = cbind(rep(1), scale(X, scale = scale, center = center))
    kappa(X, exact = exact)
  } else {
    kappa(scale(X, scale = scale, center = center), exact = exact)
  }
}
