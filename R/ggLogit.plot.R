ggLogit.plot <- function (x, newdata = NULL, method = "cut",
    where = seq(0, 1, by = 0.1),
    scalesize = NA, r2 = FALSE,
    dot.shape = 19,
    dot.size = 2,
    dot.color = "black",
    add.se = FALSE){
  # Function for prettifying plot.logistic.fit.fnc. Uses ggplot2 and is
  # compatible with latest version of lme4 (1.1-7)
  # x is a model fit with glm, lrm, glmer, or averaged model

  require(lme4, quietly = TRUE)
  require(rms, quietly = TRUE)
  require(MuMIn, quietly = TRUE)
  require(ggplot2, quietly = TRUE)


  # e.g. library(languageR);
  # m <- glm(RealizationOfRecipient ~ PronomOfTheme + LengthOfRecipient, data = dative, family = binomial)
  # ggLogit.plot(m, data = dative)
  #
  if (!is.null(newdata)) {
    data <- as.data.frame(newdata)
    if (class(x)[1] == "glmerMod") {
      y <- attr(x@frame, "terms")
      depvar <- names(attr(terms(y), "dataClasses")[attr(terms(y),"response")])
      probs <- predict(m, newdata = data, type = "response", allow.new.levels = TRUE)
    } else if (class(x)[1] == "lrm") {
      depvar <- as.character(formula(x$call))[2]
      probs <- predict(x, newdata = data, type = "fitted")
    } else if (class(x)[1] == "glm") {
      depvar <- as.character(x$formula)[2]
      probs <- predict(m, newdata = data, type = "response")
    } else if (class(x)[1] == "averaging"){
      depvar <- as.character(x$formula)[2]
      probs <- predict(x, newdata = data, full = T, type = 'response')
    } else if (class(x) == "MCMCglmm"){
      depvar <- all.vars(object$Fixed$formula)[1]
      probs <- MCMCglmm.predict(mm2, data)$probs
    } else if (class(x) == "list"){
      depvar <- x[[2]]
      probs <- x[[1]]
    } else {
      stop("first argument is not a model object or list") }
  } else {
    data <- as.data.frame(data)
    if (class(x)[1] == "glmerMod") {
      y <- attr(x@frame, "terms")
      depvar <- names(attr(terms(y), "dataClasses")[attr(terms(y),"response")])
      probs <- fitted(x)
    } else if (class(x)[1] == "lrm") {
      depvar <- as.character(formula(x$call))[2]
      probs <- predict(x, type = "fitted")
    } else if (class(x)[1] == "glm") {
      depvar <- as.character(x$formula)[2]
      probs <- fitted(x)
    } else if (class(x)[1] == "averaging"){
      depvar <- as.character(x$formula)[2]
      probs <- predict(x, full = T, type = 'response')
    } else if (class(x) == "MCMCglmm"){
      depvar <- all.vars(object$Fixed$formula)[1]
      probs <- MCMCglmm.predict(mm2, data)$probs
    } else if (class(x) == "list"){
      depvar <- x[[2]]
      probs <- x[[1]]
    } else {
      stop("first argument is not a model object or list")
    }
  }

  if (method == "cut") {
    classes <- cut2(probs, where, levels.mean = TRUE)[drop = T]
    classCounts <- table(classes)
    means <- tapply(as.numeric(data[, depvar]) - 1, classes,
                   mean)
    means <- means[!is.na(means)]
    DF <- data.frame(pred.probs = as.numeric(names(means)),
                    obs.props = means,
                    errs = tapply(as.numeric(data[, depvar]), classes,se))
  } else {
    if (method == "shingle") {
      sh = equal.count(probs)
      means = rep(0, length(levels(sh)))
      midpoints = rep(0, length(means))
      for (i in 1:length(levels(sh))) {
        means[i] = mean(probs[probs > levels(sh)[[i]][1] &
                                probs < levels(sh)[[i]][2]])
        midpoints[i] = as.character(mean(levels(sh)[[i]]))
      }
      names(means) = as.character(midpoints)
    }}
  p <- ggplot(DF, aes(pred.probs, obs.props)) + ylim(0,1) +
    geom_abline(intercept = 0,
                slope = 1,
                color = "gray") +
    geom_point(size = dot.size,
               color = dot.color,
               shape = dot.shape) +
    labs(x="mean predicted probabilities", y = "observed proportions")
  #if ((method == "cut") & (!is.na(scalesize))) {
  #    symbols(as.numeric(names(means)), as.numeric(means),
  #        circles = as.numeric(classCounts), inches = scalesize,
  #        main = " ", add = T)
  # }
  #else {
  #    points(as.numeric(names(means)), means,
  # type=type, pch = dot.pch,cex = dot.size,lty=lty,lwd=lwd)
  if(add.se) {
    p <- p + geom_errorbarh(aes(xmin = pred.probs - errs,
                                xmax = pred.probs + errs))
  }
  #}
  if(r2) {
    p<- p + ggtitle(paste("R-squared: ",
      round(cor(DF$pred.probs, DF$obs.props)^2, 2), sep = ""))
  }
  return(p)
}
