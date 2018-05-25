ggResidMer.plot <- function(model, id = 5, type = "pearson"){
  require(ggplot2)
  require(lme4)
  if (!class(model) %in% c("merMod", "glmerMod")){
    stop(paste("Error: Object", model, "must be of class 'merMod' or 'glmerMod'!"))
  }
  d <- data.frame(fits = fitted(model),
                  resids = resid(model, type = type)
  )
  d_pt <- subset(d, abs(resids) < id)
  d_id <- subset(d, abs(resids) >= id)
  p <- ggplot(d, aes(fits, resids)) +
    geom_hline(yintercept = 0) +
    geom_point(data = d_pt, color = "blue", alpha = .5) +
    geom_text(data = d_id, label = rownames(d_id)) +
    labs(x = "Fits (predicted probs)", y = "Standardized residuals")
  return(p)
}
