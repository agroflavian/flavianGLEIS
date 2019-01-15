#' filter nfunction --> GLEIS
#'
#' filtering data to drought treatment relevant
#' @param geno genotype
#' @param smoothFactor number -> higher equals smoother
#' @param column which column should be smoothed --> e.g. "ler" 
#' @return returns a dataframe with the smoothed values
#' @keywords smoother
#' @export
#' @examples
#' myFUN_smoother()
myFUN_smoother <- function(geno, smoothFactor, column){
  rowName <- "rowName"
  smoothtest <- myGWC[[ent]]%>%
    filter(experiment == geno)%>%
    select_(column, rowName)%>%
    mutate(smoothed= rollmean(.[[1]], k= smoothFactor, fill = NA))
  return(data.frame(smoothtest))
}
