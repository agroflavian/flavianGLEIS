#' filter nfunction --> GLEIS
#'
#' filtering data to drought treatment relevant
#' @param entry nummer of Population (in myGWC )
#' @param smoothFactor number, the higher the smoother, default =20
#' @param column solumn in data.frame which should be smoothed, default = "ler"
#' @return myGWC[[entry]] with smoothed values
#' @keywords filter
#' @export
#' @examples
#' myFUN_LERsmoother()
#'
myFUN_LERsmoother <- function(entry, smoothFactor = 20, column = "ler"){
  
  # browser()
  myGenotypes <- unique(myGWC[[entry]][["experiment"]])
  # nur anwenden wenn es die ler gibt
  myGWC[[entry]] <- myGWC[[entry]]%>%
    filter(complete.cases(ler))
  
  for (genotype in myGenotypes) {
    #durchlauf printen
    print(paste(entry, genotype, sep= ": "))
    
    
    smoothtest <- myFUN_smoother(genotype, smoothFactor, column)
    # browser()
    smoothvect <- smoothtest$smoothed
    
    
    ## daten in my GWC eintragen
    indexes <- smoothtest$rowName
    fittedCounter <- 1
    for (index in indexes) {
      # browser()
      myGWC[[entry]][index,][["ler_smooth"]] <- smoothvect[fittedCounter]
      fittedCounter <- fittedCounter + 1
    }
  }
  return(data.frame(myGWC[[entry]]))
}
