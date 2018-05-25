collin.fnc.mer <- function(fit){
  # adaption of Baayen's collin.fnc() for compatibility with current version
  # of lme4 and R 3.4
  # require(languageR, quietly = T)
  if (class(fit) == "lmerMod" || class(fit) == "glmerMod"){
    data <- getME(fit, "X")[,-1]
    colvector <- 1:ncol(data)
    std.fnc <- function(vec) (vec - mean(vec))/sqrt(var(vec))
    # New from R 3.4: Add as.vector() to avoid warning "Recycling array of
    # length 1 in vector array arithmetic is deprecated"
    scale.fnc <- function(vec) (vec/sqrt(as.vector(t(vec) %*% vec)))
    x = data[, colvector]
    xlengte = length(x[, 1])
    colnames = dimnames(x)[[2]]
    onevec = rep(1, xlengte)
    Xdesign = cbind(onevec, as.matrix(x))
    X = Xdesign
    ncols = length(X[1, ])
    for (i in 1:ncols) {
      X[, i] = scale.fnc(as.numeric(X[, i]))
    }
    svd.X = svd(X, nu = 0)
    nu.X = max(svd.X$d)/svd.X$d
    kappa.X = max(nu.X)
    pi.X = svd.X$v
    for (i in (1:length(svd.X$d))) {
      pi.X[, i] = svd.X$v[, i]/svd.X$d[i]
      pi.X[, i] = pi.X[, i]^2
    }
    for (i in 1:length(svd.X$d)) {
      pi.X[i, ] = pi.X[i, ]/sum(pi.X[i, ])
    }
    pi.X = t(pi.X)
    pi.X = as.data.frame(pi.X)
    dimnames(pi.X)[[2]] = c("Constant", colnames)
    return(list(svd = svd.X, cindex = nu.X, cnumber = kappa.X,
                pi = pi.X))
  }
  else stop("model not a merMod object")
}
