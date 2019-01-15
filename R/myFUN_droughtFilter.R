#' filter nfunction --> GLEIS
#'
#' filtering data to drought treatment relevant
#' @param Entry nummer of Population (in myGWC )
#' @param cutWeight maximal weight which should be showed, default= 370
#' @param cutGWC maximal GWC which should be showed, default= 1.5
#' @return returns a dataframe with the filtered values
#' @keywords filter
#' @export
#' @examples
#' myFUN_droughtFilter()
#'
myFUN_droughtFilter <- function(Entry, cutWeight = 370, cutGWC=1.5){
  
  filterDrought <- myGWC[[Entry]]%>%
    filter(treatment== "drought")%>%
    filter(experiment %in% myMaxLcum[[Entry]])%>%
    filter(daytime== TRUE)%>%
    filter(weight < cutWeight)%>%
    filter(gwc_dry > 0.5)%>%
    filter(gwc_dry < cutGWC)
  return(data.frame(filterDrought))
}

