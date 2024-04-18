

library(decisionSupport)

make_variables<-function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}


make_variables(estimate_read_csv("for_meetings/05042024/Hortiprimed_copy1.csv"))





decision_function <- function(x, varnames){
  #Leistung
  Ertragsmenge <-vv(Ertrag, var_CV, n_years)
  Tomatenpreis <- vv(Tomatenpreis, var_CV, n_years)
  Yield5 <- vv(Yield_5, var_CV, n_years)
  Yield10 <- vv(Yield_10, var_CV, n_years)
  Yield20 <- vv(Yield_20, var_CV, n_years)
  
  Leistung <- Ertragsmenge*Tomatenpreis*10000
  Leistung_5 <- Ertragsmenge*(1 + Yield5) * Tomatenpreis* 10000
  Leistung_10 <- Ertragsmenge*(1 + Yield10) * Tomatenpreis* 10000
  Leistung_20 <- Ertragsmenge*(1 + Yield20) * Tomatenpreis* 10000
  
   #Ausgabe
  #Direkt Kosten 
  Heizmaterial1 <- vv(Heizmaterial, var_CV, n_years)
  Strom1 <- vv(Strom, var_CV, n_years)
  Jungpflanzenmenge <- vv(Jungpflanzen_Menge, var_CV, n_years)
  Jungpflanzenpreis <- vv(Jungpflanzen_Preis, var_CV, n_years)
  Matten_Folien1 <- vv(Matten_Folien, var_CV, n_years)
  Du_Bewaesserung1 <- vv(Du_Bewaesserung, var_CV, n_years)
  CO21 <- vv(CO2, var_CV, n_years)
  Pflanzenschutz1 <- vv(Pflanzenschutz, var_CV, n_years)
  Verpackung1 <- vv(Verpackung, var_CV, n_years)
  Transport1 <- vv(Transport, var_CV, n_years)
  Entsorgungsmenge <- vv(Entsorgung_Menge, var_CV, n_years)
  Entsorgungspreis <- vv(Entsorgung_Preis, var_CV, n_years)
  Vermarktungsgebuehr1 <- vv(Vermarktungsgebuehr, var_CV, n_years)
  sonstige_Betriebsmittel1 <- vv(sonstige_Betriebsmittel, var_CV, n_years)
  Zinskosten1 <- vv(Zinskosten, var_CV, n_years)
  Seedlingcost20 <- vv(Seedling_cost_20, var_CV, n_years)
  ReBiowaste <- vv(Re_Biowaste, var_CV, n_years)
  RePSM <- vv(Re_PSM, var_CV, n_years)
  
  
  Direkt_Kosten_Priming <- Heizmaterial1 + Strom1 + (Jungpflanzenmenge*Jungpflanzenpreis/1000)*(1 + Seedlingcost20) + Matten_Folien1 + Du_Bewaesserung1 + 
    CO21 + Pflanzenschutz1*(RePSM) + Verpackung1 + Transport1 + (Entsorgungsmenge*Entsorgungspreis)*ReBiowaste + Vermarktungsgebuehr1 + 
    sonstige_Betriebsmittel1 
  Direkt_Kosten_ohne_Priming <- Heizmaterial1 + Strom1 + (Jungpflanzenmenge*Jungpflanzenpreis/1000) + Matten_Folien1 + Du_Bewaesserung1 + 
    CO21 + Pflanzenschutz1 + Verpackung1 + Transport1 + (Entsorgungsmenge*Entsorgungspreis) + Vermarktungsgebuehr1 + 
    sonstige_Betriebsmittel1
  
  # Arbeitserledigungskosten
  variable_Kosten1 <- vv(variable_Kosten, var_CV, n_years)
  Dienstleistungen1 <- vv(Dienstleistungen, var_CV, n_years)
  fix_Lohnkosten1 <- vv(fix_Lohnkosten, var_CV, n_years)
  
  Arbeitserledigungskosten <- variable_Kosten1 + Dienstleistungen1 + fix_Lohnkosten1
  
  
    
    ## Kosten für Tomatenanbau ##
  
      Kosten_mit_Priming <-
        Direkt_Kosten_Priming + Arbeitserledigungskosten
    
      Kosten_ohne_Priming <- Direkt_Kosten_ohne_Priming + Arbeitserledigungskosten
    
    
    ## Total_ Gewinn_Kosten ##
    
      
      net_mit_Priming_5 <- Leistung_5 - Kosten_mit_Priming
      result_mit_Priming_5<- net_mit_Priming_5
      net_mit_Priming_10 <- Leistung_10 - Kosten_mit_Priming
      result_mit_Priming_10<- net_mit_Priming_10
      net_mit_Priming_20 <- Leistung_20 - Kosten_mit_Priming
      result_mit_Priming_20<- net_mit_Priming_20
    
      net_ohne_Priming <- Leistung - Kosten_ohne_Priming
      result_ohne_Priming <- net_ohne_Priming
    
    
  #NPV calculate
  
  NPV_mit_Priming_5 <-
    discount(result_mit_Priming_5, discount_rate, calculate_NPV = T)
  NPV_mit_Priming_10 <-
    discount(result_mit_Priming_10, discount_rate, calculate_NPV = T)
  NPV_mit_Priming_20 <-
    discount(result_mit_Priming_20, discount_rate, calculate_NPV = T)
  
  NPV_ohne_Priming <-
    discount(result_ohne_Priming, discount_rate, calculate_NPV = T)
  
  
  return(list(Priming_5_NPV =  NPV_mit_Priming_5,
              Priming_10_NPV = NPV_mit_Priming_10,
              Priming_20_NPV =  NPV_mit_Priming_20,
              No_Priming_NPV = NPV_ohne_Priming, 
              NPV_imle_Priming_5 =  NPV_mit_Priming_5 - NPV_ohne_Priming, 
              NPV_imle_Priming_10 =  NPV_mit_Priming_10 - NPV_ohne_Priming, 
              NPV_imle_Priming_20 =  NPV_mit_Priming_20 - NPV_ohne_Priming, 
              Cashflow_mit_Priming_5 =  result_mit_Priming_5 - result_ohne_Priming,
              Cashflow_mit_Priming_10 =  result_mit_Priming_10 - result_ohne_Priming,
              Cashflow_mit_Priming_20 =  result_mit_Priming_20 - result_ohne_Priming)) 
}


library(readr)
input_table <- read_csv("for_meetings/05042024/Hortiprimed_copy1.csv")
names(input_table)

###Model assessment###
mcSimulation_results1 <- mcSimulation(estimate = 
                                        estimate_read_csv("for_meetings/05042024/Hortiprimed_copy1.csv"),
                                      model_function = decision_function,
                                      numberOfModelRuns = 10000,
                                      functionSyntax = "plainNames"
)


#smooth_simple_overlay
plot_distributions(mcSimulation_object = mcSimulation_results1, 
                   vars = c("Priming_5_NPV",
                            "No_Priming_NPV"),
                   method = 'smooth_simple_overlay', 
                   base_size = 10)

plot_distributions(mcSimulation_object = mcSimulation_results1, 
                   vars = c("Priming_10_NPV",
                            "No_Priming_NPV"),
                   method = 'smooth_simple_overlay', 
                   base_size = 10)

plot_distributions(mcSimulation_object = mcSimulation_results1, 
                   vars = c("Priming_20_NPV",
                            "No_Priming_NPV"),
                   method = 'smooth_simple_overlay', 
                   base_size = 10)

#boxplot
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results1, 
                                    vars = c("Priming_5_NPV",
                                             "Priming_10_NPV",
                                             "Priming_20_NPV",
                                             "No_Priming_NPV"),
                                    method = 'boxplot',
                                    base_size = 20)

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results1, 
                                    vars = c("Priming_10_NPV",
                                             "No_Priming_NPV"),
                                    method = 'boxplot')

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results1, 
                                    vars = c("Priming_20_NPV",
                                             "No_Priming_NPV"),
                                    method = 'boxplot')


decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results1, 
                                    vars = "NPV_imle_Priming_5",
                                    method = "smooth_simple_overlay",
                                    old_names = "NPV_imle_Priming_5",
                                    new_names = "Outcome distribution (1-5%)")

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results1, 
                                    vars = "NPV_imle_Priming_10",
                                    method = "smooth_simple_overlay",
                                    old_names = "NPV_imle_Priming_10",
                                    new_names = "Outcome distribution (5-10%)")

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results1, 
                                    vars = "NPV_imle_Priming_20",
                                    method = "smooth_simple_overlay",
                                    old_names = "NPV_imle_Priming_20",
                                    new_names = "Outcome distribution (10-20%)")




###Cashflow analysis###
plot_cashflow(mcSimulation_object = mcSimulation_results1, 
              cashflow_var_name = "Cashflow_mit_Priming_5")
plot_cashflow(mcSimulation_object = mcSimulation_results1, 
              cashflow_var_name = "Cashflow_mit_Priming_10")
plot_cashflow(mcSimulation_object = mcSimulation_results1, 
              cashflow_var_name = "Cashflow_mit_Priming_20")


###Value of Information (VoI) analysis###

#here we subset the outputs from the mcSimulation function (y) by selecting the correct variables
# choose this carefully and be sure to run the multi_EVPI only on the variables that the you want
#EVPI = (EOL : Expected Opportunity Loss)


#5% mehr Ertrag
mcSimulation_table <- data.frame(mcSimulation_results1$x, mcSimulation_results1$y[1:3])

evpi_5 <- multi_EVPI(mc = mcSimulation_table, first_out_var = "Priming_5_NPV")


plot_evpi(evpi_5, decision_vars = "Priming_5_NPV",base_size = 20)
plot_evpi(evpi_5, decision_vars = "Priming_10_NPV",base_size = 20)
plot_evpi(evpi_5, decision_vars = "Priming_20_NPV",base_size = 20)


#put here mcSimulation_result instead of pls_result
compound_figure(mcSimulation_object = mcSimulation_results1, 
                input_table = input_table, 
                plsrResults = mcSimulation_results1, 
                EVPIresults = evpi_5, decision_var_name = "Priming_5_NPV", 
                cashflow_var_name = "Cashflow_mit_Priming_5", 
                base_size = 5)

#10% mehr Ertrag

#put here mcSimulation_result instead of pls_result
compound_figure(mcSimulation_object = mcSimulation_results1, 
                input_table = input_table, 
                plsrResults = mcSimulation_results1, 
                EVPIresults = evpi_5, decision_var_name = "Priming_10_NPV", 
                cashflow_var_name = "Cashflow_mit_Priming_10", 
                base_size = 5)


#20% mehr Ertrag

#put here mcSimulation_result instead of pls_result
compound_figure(mcSimulation_object = mcSimulation_results1, 
                input_table = input_table, 
                plsrResults = mcSimulation_results1, 
                EVPIresults = evpi_5, decision_var_name = "Priming_20_NPV", 
                cashflow_var_name = "Cashflow_mit_Priming_20", 
                base_size = 5)

###Projection to Latent Structures (PLS) analysis###

pls_result5 <- plsr.mcSimulation(object = mcSimulation_results1,
                                resultName = names(mcSimulation_results1$y)[5], ncomp = 1)
plot_pls(pls_result5, input_table = input_table, threshold = 0)

pls_result10 <- plsr.mcSimulation(object = mcSimulation_results1,
                                resultName = names(mcSimulation_results1$y)[6], ncomp = 1)
plot_pls(pls_result10, input_table = input_table, threshold = 0)

pls_result20 <- plsr.mcSimulation(object = mcSimulation_results1,
                                resultName = names(mcSimulation_results1$y)[7], ncomp = 1)
plot_pls(pls_result20, input_table = input_table, threshold = 0)
