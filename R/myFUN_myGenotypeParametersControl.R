#' filter nfunction --> GLEIS
#'
#' Parametrisieren der Drought-Treatments
#' @param Geno single experiment
#' @param input1 input 1 for the regression
#' @param input2 input 2 for the regression 
#' @return returns a vector with(Population, PlantID, Entry, Treatment, R2, P, Intercept, Slope)
#' @keywords parametrisieren
#' @export
#' @examples
#' myFUN_myGenotypeParametersControl()
myFUN_myGenotypeParametersControl <- function(Geno, input1 = "ler_smooth_diff", input2= "gwc_dry"){
  tablePlot <- myGWC[[ent]]%>%
    filter(treatment == "control")%>%
    filter(experiment %in% myMaxLcum[[ent]])%>%
    filter(daytime== TRUE)%>%
    # filter(weight < 370)%>%
    filter(gwc_dry > 0.5)%>%
    # filter(gwc_dry < 1.5)%>%
    # filter(complete.cases(gwc_dry,ler_smooth))%>%
    filter(complete.cases(input1,input2))%>%
    filter(ler_smooth >= 0)%>%
    filter(experiment == Geno)
  
  if(length(tablePlot[[input1]]) > 90){
    # if(length(tablePlot$ler_smooth_diff) > 1){
    # fit1 <- lm(formula = ler_smooth ~gwc_dry, data= tablePlot)
    model <- paste("formula =",input1, "~", input2)
    model <- as.formula(model)
    fit1 <- lm(model, data = tablePlot)
    
    p <- myFUN_ggplotRegression(fit= fit1, genotype = Geno)
    print(p)
    # fit1 <- lm(formula = ler_smooth_diff_date ~ input2, data= tablePlot)
    Pop <- Geno
    PlantID <- tablePlot$PlantID[1]
    Entry <- ent
    Treatment <- "control"
    R2 <- signif(summary(fit1)$adj.r.squared, 5)
    P <- signif(summary(fit1)$coef[2,4], 5)
    Intercept <- signif(fit1$coef[[1]],5 )
    Slope <- signif(fit1$coef[[2]], 5)
  } else { 
    Pop <- Geno
    PlantID <- NA
    Entry <- ent
    Treatment <- "control"
    R2 <- NA
    P <- NA
    Intercept <- NA
    Slope <- NA
  }
  return(c(Pop=Pop, PlantID= PlantID, Entry= Entry, Treatment=Treatment, R2=R2, P=P, Intercept=Intercept, Slope=Slope))
}
