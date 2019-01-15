#' filter nfunction --> GLEIS
#'
#' filtering data to drought treatment relevant
#' @param fit output of a lm regression
#' @param genotype genotype, for the title, default = NULL
#' @return ggplot
#' @keywords ggplot
#' @export
#' @examples
#' myFUN_ggplotRegression()
#'
myFUN_ggplotRegression <- function (fit, genotype= NULL) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Entry", ent, genotype, "Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}