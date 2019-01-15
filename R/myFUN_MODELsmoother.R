#' filter nfunction --> GLEIS
#'
#' filtering data to drought treatment relevant
#' @param entry nummer of Population (in myGWC )
#' @param smoothFactor number, the higher the smoother, default =20
#' @return myGWC[[entry]] with smoothed values
#' @keywords filter
#' @export
#' @examples
#' myFUN_MODELsmoother()
#'
myFUN_MODELsmoother <- function(entry, smoothFactor = 10){
  myGenotypes <- unique(myGWC[[entry]][["experiment"]])
  
  for (genotype in myGenotypes) {
    #durchlauf printen
    fittedCounter <- 1
    
    smoothtest1 <- myFUN_smoother(genotype, smoothFactor, "ler_pred")
    # smoothtest2 <- myFUN_smoother(genotype, smoothFactor, "ler_pred_date")
    # smoothtest3 <- myFUN_smoother(genotype, smoothFactor, "ler_pred_lcum")
    # smoothtest4 <- myFUN_smoother(genotype, smoothFactor, "ler_pred_humdate")
    
    smoothvect1 <- smoothtest1$smoothed
    # smoothvect2 <- smoothtest2$smoothed
    # smoothvect3 <- smoothtest3$smoothed
    # smoothvect4 <- smoothtest4$smoothed
    
    ## daten in my GWC eintragen
    indexes <- smoothtest1$rowName
    
    for (index in indexes) {
      # browser()
      myGWC[[entry]][index,][["ler_pred_smooth"]] <- smoothvect1[fittedCounter]
      # myGWC[[entry]][index,][["ler_pred_date_smooth"]] <- smoothvect2[fittedCounter]
      # myGWC[[entry]][index,][["ler_pred_lcum_smooth"]] <- smoothvect3[fittedCounter]
      # myGWC[[entry]][index,][["ler_pred_humdate_smooth"]] <- smoothvect4[fittedCounter]
      fittedCounter <- fittedCounter + 1
    }
    print(paste("smooth modell: Nr.", fittedCounter, "Entry" ,entry, genotype, sep= ": "))
  }
  return(data.frame(myGWC[[entry]]))
}

