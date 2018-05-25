ggPredictor.plot <- function(data, response, vars,
                             size = 4, text.col = "black",
                             hjust = 1.1){
  require(ggplot2)
  require(reshape2)
  require(magrittr)
  data <- as.data.frame(data)
  if (is.character(response)) resp <- response
  else resp <- deparse(substitute(response))

  if (is.numeric(vars)){
    dt <- reshape2::melt(data, resp, names(data)[vars])
  }
  else if (is.character(vars)){
    dt <- reshape2::melt(data, resp, vars)
  }
  # sort values
  vals <- c()
  for (i in 1:length(vars)){
    vals <- c(vals, levels(data[, vars[i]]))
  }
  dt$value <- factor(dt$value, levels = vals)
  names(dt)[1] <- "Response"
  levs <- levels(dt$Response)
  f <- Response ~ value
  propLabel <- aggregate(f, dt,
    FUN = function(x) sum(as.numeric(x) == 1) / length(x))
  propLabel <- paste0(round(propLabel[, "Response"], 2) * 100, "%")
  propY <- aggregate(f,
    dt[dt$Response == levs[1], ], length)$Response
  p <- ggplot() +
    labs(x = 'predictor level', y = "Number of tokens") +
    geom_bar(data = dt, aes(value, fill = variable), alpha = .5) +
    geom_bar(data = dt[dt$Response == levs[1],],
             aes(value, fill = variable)) +
    annotate("text", x = 1:length(levels(dt$value)),
             y = propY, label = propLabel, size = 4,
             hjust = 1.1, color = text.col) +
    coord_flip()
  return(p)
}
