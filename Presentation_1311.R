

library(decisionSupport)

make_variables<-function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}


make_variables(estimate_read_csv("Sanghyo/Hortiprimed.csv"))





decision_function <- function(x, varnames){
 #Leistung
  Leistung <-vv(Leistung, var_CV, n_years)
  
 #Ausgabe
 #Direkt Kosten 
  Heizmaterial <- vv(Heizmaterial, var_CV, n_years)
  Strom <- vv(Strom, var_CV, n_years)
  Saat_Planzgut <- vv(Saat_Planzgut, var_CV, n_years)
  Matten_Folien <- vv(Matten_Folien, var_CV, n_years)
  Du_Bewaesserung <- vv(Du_Bewaesserung, var_CV, n_years)
  CO2 <- vv(CO2, var_CV, n_years)
  Pflanzenschutz <- vv(Pflanzenschutz, var_CV, n_years)
  Verpackung <- vv(Verpackung, var_CV, n_years)
  Transport <- vv(Transport, var_CV, n_years)
  Entsorgung <- vv(Entsorgung, var_CV, n_years)
  Vermarktungsgebuehr <- vv(Vermarktungsgebuehr, var_CV, n_years)
  sonstige_Betriebsmittel <- vv(sonstige_Betriebsmittel, var_CV, n_years)
  Zinskosten <- vv(Zinskosten, var_CV, n_years)
  
  Direkt_Kosten_Priming <- Heizmaterial + Strom + (1.2*Saat_Planzgut) + Matten_Folien + Du_Bewaesserung + 
    CO2 + (0.7*Pflanzenschutz) + Verpackung + Transport + Entsorgung + Vermarktungsgebuehr + 
    sonstige_Betriebsmittel 
  Direkt_Kosten_ohne_Priming <- Heizmaterial + Strom + Saat_Planzgut + Matten_Folien + Du_Bewaesserung + 
    CO2 + Pflanzenschutz + Verpackung + Transport + Entsorgung + Vermarktungsgebuehr + 
    sonstige_Betriebsmittel
  
  # Arbeitserledigungskosten
  variable_Kosten <- vv(variable_Kosten, var_CV, n_years)
  Dienstleistungen <- vv(Dienstleistungen, var_CV, n_years)
  fix_Lohnkosten <- vv(fix_Lohnkosten, var_CV, n_years)
  
  Arbeitserledigungskosten <- variable_Kosten + Dienstleistungen + fix_Lohnkosten
    
  #Intervention of Priming 
  
  for (decision_priming in c(FALSE,TRUE)){
    
    if (decision_priming){
      
      Kosten_mit_Priming <- TRUE
      Kosten_ohne_Priming <- TRUE
      Leistung <- TRUE
      
    } else
    {
      Kosten_mit_Priming <- FALSE
      Kosten_ohne_Priming <- FALSE
      Leistung <- TRUE
      
    }
    
    
    ## Kosten für Tomatenanbau ##
    if (decision_priming){
      Kosten_mit_Priming <-
        Direkt_Kosten_Priming + Arbeitserledigungskosten
    } else{
      Kosten_ohne_Priming <- Direkt_Kosten_ohne_Priming + Arbeitserledigungskosten
    }
    
   
   
    ## Total_ Gewinn_Kosten ##
    if (decision_priming){
      
      net_mit_Priming <- Leistung - Kosten_mit_Priming
        
      
      result_mit_Priming<- net_mit_Priming
    }
    
    if (!decision_priming){
      net_ohne_Priming <- Leistung - Kosten_ohne_Priming
      result_ohne_Priming <- net_ohne_Priming
    }
    
  }#close implementation
  
  #NPV calculate
  
  NPV_mit_Priming <-
    discount(result_mit_Priming, discount_rate, calculate_NPV = T)
  
  NPV_ohne_Priming <-
    discount(result_ohne_Priming, discount_rate, calculate_NPV = T)
  
  
  return(list(Imple_NPV =  - NPV_mit_Priming, 
              NO_Imple_NPV = - NPV_ohne_Priming, 
              NPV_imle_Priming =  NPV_mit_Priming - NPV_ohne_Priming, 
              Cashflow_mit_Priming =  result_ohne_Priming - result_mit_Priming)) 
}

library(readr)
input_table <- read.csv("Sanghyo/Hortiprimed.csv")
names(input_table)

###Model assessment###
mcSimulation_results1 <- mcSimulation(estimate = 
                                        estimate_read_csv("Sanghyo/Hortiprimed.csv"),
                                      model_function = decision_function,
                                      numberOfModelRuns = 10000,
                                      functionSyntax = "plainNames"
)



plot_distributions(mcSimulation_object = mcSimulation_results1, 
                   vars = c("Imple_NPV",
                            "NO_Imple_NPV"),
                   method = 'smooth_simple_overlay', 
                   base_size = 10)


decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results1, 
                                    vars = c("Imple_NPV",
                                             "NO_Imple_NPV"),
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

evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "Imple_NPV")


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

