scores.mer <- function(fit, R2 = F, digits = 3){
  if (!class(fit)[1] %in% c("lmerMod", "glmerMod")) {
    stop("fit is not a merMod object")
  }
  require(dplyr)
  require(magrittr)
  require(lme4)
  require(Hmisc)
  require(MuMIn)
  d <- fit@frame
  probs <- predict(fit, type = "response")
  preds <- ifelse(probs > .5, 1, 0)
  y <- getME(fit, "y")
  d2 <- cbind(d, data.frame(probs = probs, preds = preds, y = y))
  # AUC and Dxy
  C <- Hmisc::somers2(probs, y)[1] %>% as.vector
  Dxy <- Hmisc::somers2(probs, y)[2] %>% as.vector
  AICc <- MuMIn::AICc(fit)
  logL <- logLik(fit)[1]
  N <- nrow(d)
  kappa <- collin.fnc.mer(fit)$cnumber
  # log-loss (- average log-likelihood)
  LL <- - mean(y*log(probs) + (1 - y)*log(1 - probs))
  # predictive accuracies
  acc <- length(which(preds == y))/nrow(d)
  base <- 1 - mean(y)
  resp <- levels(d[, 1])
  resp_col <- names(d)[1]
  d.1 <- dplyr::filter_(d2, paste0(resp_col," == '", resp[1],"'")) %>% droplevels
  d.2 <- dplyr::filter_(d2, paste0(resp_col," == '", resp[2],"'")) %>% droplevels
  class1.acc <- length(which(d.1$preds == d.1$y))/nrow(d.1)
  class2.acc <- length(which(d.2$preds == d.2$y))/nrow(d.2)
  out <- data.frame(N = N, C = C, Dxy = Dxy, AICc = AICc, kappa = kappa,
           predicted.corr = acc,
           baseline = base,
           Log.Loss = LL,
           Class.1.acc = class1.acc,
           Class.2.acc = class2.acc,
           Avg.Per.Class = mean(c(class1.acc, class2.acc)))
  if (R2){
    R2 <- MuMIn::r.squaredGLMM(fit)
    out <- dplyr::mutate(out, R2.m = R2[[1]], R2.c = R2[[2]])
  }
  if (is.null(digits)){
    return (out)
  } else {
    return (round(out, digits))}
}
