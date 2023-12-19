

library(decisionSupport)

make_variables<-function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}


make_variables(estimate_read_csv("Seedling/Seedling-input.csv"))





decision_function <- function(x, varnames){
  #Setzling
  Setzling <-vv(Setzling, var_CV, n_years)
  
  #Ausgabe
  #Direkt Kosten 
  Heizmaterial <- vv(Heizmaterial, var_CV, n_years)
  Strom <- vv(Strom, var_CV, n_years)
  Samen <- vv(Samen, var_CV, n_years)
  Bewaesserung <- vv(Bewaesserung, var_CV, n_years)
  CO2 <- vv(CO2, var_CV, n_years)
  Cell <- vv(Cell, var_CV, n_years)
  Vermarktungsgebuehr <- vv(Vermarktungsgebuehr, var_CV, n_years)
  sonstige_Betriebsmittel <- vv(sonstige_Betriebsmittel, var_CV, n_years)
  Zinskosten <- vv(Zinskosten, var_CV, n_years)
  Salzpriming<- vv(Salzpriming, var_CV, n_years)
  Heatpriming<- vv(Heatpriming, var_CV, n_years)
  
  Direkt_Kosten_Salzpriming<- Heizmaterial + Strom + Samen + Bewaesserung + 
    CO2 +  Cell + Vermarktungsgebuehr + sonstige_Betriebsmittel 
  Direkt_Kosten_Heatpriming<- Heizmaterial + Strom + Samen + Bewaesserung + 
    CO2 +  Cell + Vermarktungsgebuehr + sonstige_Betriebsmittel 

  
  # Arbeitserledigungskosten
  Lohnkosten <- vv(Arbeit, var_CV, n_years)
 
  
  #Intervention of Priming 
  
  for (decision_priming in c(FALSE,TRUE)){
    
    
    ## Kosten für Setzlingproduction ##
    if (decision_priming){
      Kosten_mit_Salzpriming <-
        Direkt_Kosten_Salzpriming + Lohnkosten
    } else{
      Kosten_mit_Heatpriming <- Direkt_Kosten_Heatpriming + Lohnkosten
    }
    
    
    
    ## Total_ Gewinn_Kosten ##
    if (decision_priming){
      
      net_mit_Salzpriming <- Setzling - Kosten_mit_Salzpriming
      
      
      result_mit_Priming<- net_mit_Salzpriming
    }
    
    if (!decision_priming){
      net_mit_Heatpriming <- Setzling - Kosten_mit_Heatpriming
      result_mit_Heatpriming<- net_mit_Heatpriming
    }
    
  }#close implementation
  
  #NPV calculate
  
  NPV_mit_Salzpriming <-
    discount(result_mit_Priming, discount_rate, calculate_NPV = T)
  
  NPV_mit_Heatpriming <-
    discount(result_mit_Heatpriming, discount_rate, calculate_NPV = T)
  
  
  return(list(Salzpriming_NPV =  - NPV_mit_Salzpriming, 
              Heatpriming_NPV = - NPV_mit_Heatpriming, 
              NPV_bet_Priming =  NPV_mit_Salzpriming - NPV_mit_Heatpriming, 
              Cashflow_bet_Priming =  result_mit_Priming - result_mit_Heatpriming)) 
}


library(readr)
input_table <- read.csv("Seedling/Seedling-input.csv")
names(input_table)

###Model assessment###
mcSimulation_results1 <- mcSimulation(estimate = 
                                        estimate_read_csv("Seedling/Seedling-input.csv"),
                                      model_function = decision_function,
                                      numberOfModelRuns = 10000,
                                      functionSyntax = "plainNames"
)



plot_distributions(mcSimulation_object = mcSimulation_results1, 
                   vars = c("Priming_NPV",
                            "No_Priming_NPV"),
                   method = 'smooth_simple_overlay', 
                   base_size = 10)


decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results1, 
                                    vars = c("Priming_NPV",
                                             "No_Priming_NPV"),
                                    method = 'boxplot')


decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results1, 
                                    vars = "NPV_imle_Priming",
                                    method = "smooth_simple_overlay",
                                    old_names = "NPV_imle_Priming",
                                    new_names = "Outcome distribution for savings")



###Cashflow analysis###
plot_cashflow(mcSimulation_object = mcSimulation_results1, 
              cashflow_var_name = "Cashflow_mit_Priming")


###Value of Information (VoI) analysis###

#here we subset the outputs from the mcSimulation function (y) by selecting the correct variables
# choose this carefully and be sure to run the multi_EVPI only on the variables that the you want
#EVPI = (EOL : Expected Opportunity Loss)


mcSimulation_table <- data.frame(mcSimulation_results1$x, mcSimulation_results1$y[1:3])

evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "Priming_NPV")


plot_evpi(evpi, decision_vars = "NPV_decision_SSB_Tax")


#put here mcSimulation_result instead of pls_result
compound_figure(mcSimulation_object = mcSimulation_results1, 
                input_table = input_table, 
                plsrResults = mcSimulation_results1, 
                EVPIresults = evpi, decision_var_name = "NPV_decision_SSB_Tax", 
                cashflow_var_name = "Cashflow_decision_SSB_Tax", 
                base_size = 5)


###Projection to Latent Structures (PLS) analysis###

pls_result <- plsr.mcSimulation(object = mcSimulation_results1,
                                resultName = names(mcSimulation_results1$y)[3], ncomp = 1)
plot_pls(pls_result, input_table = input_table, threshold = 0)

