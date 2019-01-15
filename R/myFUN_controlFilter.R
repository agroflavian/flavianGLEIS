#' filter nfunction --> GLEIS
#'
#' filtering data to drought treatment relevant
#' @param Entry nummer of Population (in myGWC )
#' @return returns a dataframe with the filtered values
#' @keywords filter
#' @export
#' @examples
#' myFUN_controlFilter()
#'
myFUN_controlFilter <- function(Entry){
  
  filterControl <- myGWC[[Entry]]%>%
    filter(treatment== "control")%>%
    filter(experiment %in% myMaxLcum[[Entry]])%>%
    filter(daytime== TRUE)%>%
    filter(gwc_dry > 0.5)
  return(data.frame(filterControl))
}
