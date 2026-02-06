#' Function to put output of linear model into a usable text format -
#' this works if model object is simple / for 1 model
#'
#' @param model Output of lm()
#' @return Character
#'
#' @importFrom stats coef
#'


lm_eqn <- function(model) {
  l <- list(a =  format(coef(model)[1], digits = 2),
            b =  format(abs(coef(model)[2]), digits = 2),
            r2 =  format(summary(model)$r.squared, digits = 3));
  if (coef(model)[2] ==  0) {
    eq <- substitute(italic(y) == a + b %.% italic(x)*","
                     ~~italic(r)^2~"="~r2, env=l)
  } else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*","
                     ~~italic(r)^2~"="~r2, env=l)
  }
  return(as.character(as.expression(eq)))
}
