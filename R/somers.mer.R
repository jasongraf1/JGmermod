somers.mer <- function(fit, ...){
  # C and somers Dxy for glmer using somers2()
  # from the Hmisc package
  require(Hmisc, quietly = TRUE)
  if (class(fit) == "glmerMod"){
    y = getME(fit, "y") # vector of responses
    somer <- Hmisc::somers2(fitted(fit), as.numeric(y), ...)
    return (somer)
  }
  else stop("object not of class glmerMod")
}
