#' filter nfunction --> GLEIS
#'
#' filtering data to drought treatment relevant
#' @param df data.frame which contains the data
#' @param xaxis data on the x axis of the plot
#' @param yaxis data on the y axis of the plot
#' @param xalab title of the xlab, default = xaxis
#' @param yalab title of the ylab, default = yaxis
#' @param regression method of the regression, defaulkt = "reg.line"
#' @param xlimit x-axis definiton, default = c(0, 1.6)
#' @param ylimit y-axis definiton, default = NULL
#' @return returns a dataframe with the filtered values
#' @keywords filter
#' @export
#' @examples
#' myFUN_ggscatter()
#'
myFUN_ggscatter <- function(df,xaxis, yaxis, xlab= xaxis, ylab= yaxis, regression= "reg.line",xlimit= c(0,1.6), ylimit= NULL){
  
  p <-ggscatter(df, x = xaxis, y = yaxis,
                add = regression, add.params = list(color = "red", fill = "red"), color= "black", conf.int = TRUE, title = paste("Entry", ent, sep = " "),
                cor.coef = TRUE, cor.method = "pearson", xlim= xlimit, ylim= ylimit,
                xlab = xlab, ylab = ylab)
  return(p)
  
}
