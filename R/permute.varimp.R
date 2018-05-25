permute.varimp <- function(fit, data = NULL, verbose = FALSE, ranef = TRUE){
  require(lme4, quietly = T)
  require(Hmisc, quietly = T)
  require(MuMIn, quietly = T)

  if (class(fit)[1] %in% c("lmerMod", "glmerMod")){
    y <- fit@resp$y
    data <- as.data.frame(fit@frame)
    if (ranef){
      random <- names(ranef(fit))
      vars <- colnames(attr(attr(fit@frame, "terms"), "factors"))
    } else {
      # only fixed effs
      vars <- strsplit(toString(attr(attr(fit@frame, "terms"),
        "predvars.fixed")), ', ')[[1]][-c(1:2)]
    }
  } else if (class(fit)[1] == "lrm"){
    vars <- colnames(attr(f.lrm$terms, "factors"))
    if(is.null(fit$y)) {
      stop("Must specify lrm(..., y = T) when fitting lrm().")
    } else {
      y <- as.numeric(fit$y) - 1
    }
    if(is.null(data)) stop("Must specify data when using lrm().")
    ranef <- FALSE
  } else if (class(fit)[1] == "glm"){
    data <- fit$data
    vars <- colnames(attr(attr(fit$model, "terms"), "factors"))
    y <- as.numeric(fit$y)
    ranef <- FALSE
  } else {
    stop("fit is not a supported class")
  }
  # remove interaction terms
  vars <- vars[grep(":", vars, invert = T)]
  # add random effects terms to list
  if(ranef) vars <- c(vars, random)
  # get predictions from full model
  full.probs <- predict(fit, type = "response")
  full.C <- somers2(full.probs, y)[[1]]
  full.acc <- mean(round(full.probs) == y)
  full.AICc <- MuMIn::AICc(fit)
  varimp_mat <- matrix(nrow = length(vars), ncol = 3)
  if (verbose) {cat("variables run:\n")}
  # loop through (fixed effects) predictors
  for (i in seq(1, length(vars))){
    # find main effect and any interactions
    d <- data
    d[, vars[i]] <- sample(data[, vars[i]]) # reshuffle values
    if (class(fit)[1] == "glmerMod"){
      # Make sure the updated model inherits the control settings from original
      ctrl = glmerControl(optimizer = fit@optinfo$optimizer,
                          optCtrl = fit@optinfo$control)
      new_fit <- update(fit, data = d, control = ctrl)
    } else if (class(fit)[1] == "lmerMod") {
      # Make sure the updated model inherits the control settings from original
      ctrl = lmerControl(optimizer = fit@optinfo$optimizer,
                         optCtrl = fit@optinfo$control)
      new_fit <- update(fit, data = d, control = ctrl)
    } else {
      new_fit <- update(fit, data = d)
    }
    new.probs <- predict(new_fit, type = "response")

    if (class(fit)[1] %in% c("lmerMod", "glmerMod")){
      new_y <- new_fit@resp$y
    } else {
      new_y <- new_fit$y
    }
    new.C <- somers2(new.probs, new_y)[[1]]
    C.diff <- full.C - new.C
    new.acc <- mean(round(new.probs) == new_y)
    Acc.diff <- full.acc - new.acc
    new.AICc <- MuMIn::AICc(new_fit)
    # The difference in AICc here is the same as the likelihood ratio
    AICc.diff <- new.AICc - full.AICc
    varimp_mat[i, ] <- c(C.diff, Acc.diff, AICc.diff)
    if (verbose) {cat(vars[i], "... ", sep = "")}
  }
  rownames(varimp_mat) <- vars
  colnames(varimp_mat) <- c("C", "accuracy", "AICc")
  return(as.data.frame(varimp_mat))
}
