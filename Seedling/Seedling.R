
library(decisionSupport)

make_variables<-function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}


make_variables(estimate_read_csv("Seedling/Seedling-input (1).csv"))





decision_function <- function(x, varnames){
  #Setzling_yield
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
  
  Direkt_Kosten_ohne_Priming<- Heizmaterial + Strom + Samen + Bewaesserung + 
    CO2 +  Cell + Vermarktungsgebuehr + sonstige_Betriebsmittel
  Direkt_Kosten_Salzpriming_100<- Heizmaterial + Strom + Samen + Bewaesserung + 
    CO2 +  Cell + Vermarktungsgebuehr + sonstige_Betriebsmittel + Salzpriming 
  Direkt_Kosten_Salzpriming_200<- Heizmaterial + Strom + Samen + Bewaesserung + 
    CO2 +  Cell + Vermarktungsgebuehr + sonstige_Betriebsmittel + Salzpriming 
  Direkt_Kosten_Heatpriming<- Heizmaterial + Strom + Samen + Bewaesserung + 
    CO2 +  Cell + Vermarktungsgebuehr + sonstige_Betriebsmittel + Heatpriming

  
  # Arbeitserledigungskosten
  Lohnkosten <- vv(Arbeit, var_CV, n_years)
 
 
  #Priming risks: How many plants are primed, time risks (how long a seedling production takes)
  
   
  #Intervention of Heatpriming  
  
  for (decision_Heat in c(FALSE,TRUE)){
    
    ## Kosten für Setzlingproduction ##  
    if (decision_Heat){
      Kosten_mit_Heatpriming <- Direkt_Kosten_Heatpriming + Lohnkosten
    } else {
      Kosten_ohne_Priming <- Direkt_Kosten_ohne_Priming + Lohnkosten
    }
   
    
    ## Total_ Gewinn_Kosten ##
    if (decision_Heat){
      net_mit_Heatpriming <- Setzling - Kosten_mit_Heatpriming
      result_mit_Heatpriming<- net_mit_Heatpriming
    }
    
    if (!decision_Heat){
      net_ohne_Priming <- Setzling - Kosten_ohne_Priming
      result_ohne_Priming<- net_ohne_Priming
    }
    
  }#close implementation of Heat priming 
  
#Intervention of Salt priming  
  
  for (decision_Salt_100mM in c(FALSE,TRUE)){
    
    ## Kosten für Setzlingproduction ##  
  
    if (decision_Salt_100mM) {
      Kosten_mit_Salt_100mM <- Direkt_Kosten_Salzpriming_100 + Lohnkosten
    } else {
      Kosten_mit_Salt_200mM <- Direkt_Kosten_Salzpriming_200 + Lohnkosten
    }
    
    ## Total_ Gewinn_Kosten ##

    if (decision_Salt_100mM){
      
      net_mit_Salt_100mM <- Setzling - Kosten_mit_Salt_100mM
      result_mit_Salt_100mM<- net_mit_Salt_100mM
    }
    
    if (!decision_Salt_100mM){
      net_mit_Salt_200mM <- Setzling - Kosten_mit_Salt_200mM
      result_mit_Salt_200mM<- net_mit_Salt_200mM
    }
    
    
  }#close implementation Salt Priming  
  
  #NPV calculate
  
  NPV_ohne_Priming <-
    discount(result_ohne_Priming, discount_rate, calculate_NPV = T)
  NPV_mit_Heatpriming <-
    discount(result_mit_Heatpriming, discount_rate, calculate_NPV = T)
  NPV_mit_Salt_100mM <-
    discount(result_mit_Salt_100mM, discount_rate, calculate_NPV = T)
  NPV_mit_Salt_200mM <-
    discount(result_mit_Salt_200mM, discount_rate, calculate_NPV = T)
  
  
  return(list(Ohne_Priming_NPV = - NPV_ohne_Priming,
              Heatpriming_NPV = - NPV_mit_Heatpriming, 
              Salzpriming_100mM_NPV =  - NPV_mit_Salt_100mM,
              Salzpriming_200mM_NPV =  - NPV_mit_Salt_200mM, 
              NPV_bet_Heat_Priming =  NPV_ohne_Priming - NPV_mit_Heatpriming,
              NPV_bet_Salt_100mM_Priming =  NPV_ohne_Priming - NPV_mit_Salt_100mM, 
              NPV_bet_Salt_200mM_Priming =  NPV_ohne_Priming - NPV_mit_Salt_200mM, 
              NPV_bet_Salt_Priming =  NPV_mit_Salt_100mM - NPV_mit_Salt_200mM, 
              Cashflow_bet_Heat_Priming =  result_ohne_Priming - result_mit_Heatpriming,
              Cashflow_bet_Salt_100mM_Priming =  result_ohne_Priming - result_mit_Salt_100mM,
              Cashflow_bet_Salt_200mM_Priming =  result_ohne_Priming - result_mit_Salt_200mM,
              Cashflow_bet_Salt_Priming =  result_mit_Salt_100mM - result_mit_Salt_200mM)) 
}

print(decision_function(9))

library(readr)
input_table <- read.csv("Seedling/Seedling-input (1).csv")
names(input_table)

###Model assessment###
mcSimulation_results1 <- mcSimulation(estimate = 
                                        estimate_read_csv("Seedling/Seedling-input (1).csv"),
                                      model_function = decision_function,
                                      numberOfModelRuns = 10000,
                                      functionSyntax = "plainNames"
)



plot_distributions(mcSimulation_object = mcSimulation_results1, 
                   vars = c("Ohne_Priming_NPV",
                            "Heatpriming_NPV",
                            "Salzpriming_100mM_NPV",
                            "Salzpriming_200mM_NPV"
                            ),
                   method = 'smooth_simple_overlay', 
                   base_size = 10)


decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results1, 
                                    vars = c("Ohne_Priming_NPV",
                                             "Heatpriming_NPV",
                                             "Salzpriming_100mM_NPV",
                                             "Salzpriming_200mM_NPV"),
                                    method = 'boxplot')


decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results1, 
                                    vars = "NPV_bet_Heat_Priming",
                                    method = "smooth_simple_overlay",
                                    old_names = "NPV_bet_Heat_Priming",
                                    new_names = "Outcome distribution for saving")



###Cashflow analysis###
plot_cashflow(mcSimulation_object = mcSimulation_results1, 
              cashflow_var_name = "Cashflow_bet_Heat_Priming")

plot_cashflow(mcSimulation_object = mcSimulation_results1, 
              cashflow_var_name = "Cashflow_bet_Salt_100mM_Priming")

plot_cashflow(mcSimulation_object = mcSimulation_results1, 
              cashflow_var_name = "Cashflow_bet_Salt_200mM_Priming")

plot_cashflow(mcSimulation_object = mcSimulation_results1, 
              cashflow_var_name = "Cashflow_bet_Salt_Priming")


###Value of Information (VoI) analysis###

#here we subset the outputs from the mcSimulation function (y) by selecting the correct variables
# choose this carefully and be sure to run the multi_EVPI only on the variables that the you want
#EVPI = (EOL : Expected Opportunity Loss)


mcSimulation_table <- data.frame(mcSimulation_results1$x, mcSimulation_results1$y[1:8])

evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "Ohne_Priming_NPV")




plot_evpi(evpi, decision_vars = "Ohne_Priming_NPV")


#put here mcSimulation_result instead of pls_result
compound_figure(mcSimulation_object = mcSimulation_results1, 
                input_table = input_table, 
                plsrResults = mcSimulation_results1, 
                EVPIresults = evpi, decision_var_name = "NPV_decision_Priming", 
                cashflow_var_name = "Cashflow_decision_Priming", 
                base_size = 5)


###Projection to Latent Structures (PLS) analysis###

pls_result <- plsr.mcSimulation(object = mcSimulation_results1,
                                resultName = names(mcSimulation_results1$y)[3], ncomp = 1)
plot_pls(pls_result, input_table = input_table, threshold = 0)

