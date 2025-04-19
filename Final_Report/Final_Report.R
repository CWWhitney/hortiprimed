
library(ggplot2)
library(dplyr)
library(tidyverse)
library(decisionSupport)

set.seed(42)

# calculation of yield per square meters and per season with 90% convidence interval

Yield <- read_csv("Final_Report/Yield.csv")

# calculating CV and if there are two variables for a treatment then calculate average of the CV
Yield_clean <- Yield[ , !grepl("lose", names(Yield), ignore.case = TRUE) ]
Yield_clean_2 <- Yield_clean[ , -((ncol(Yield_clean)-2):ncol(Yield_clean)) ]
cv <- apply(Yield_clean_3, 2, function(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE))
cv_rounded <- round(cv, 3)
cv_percent <- cv * 100

# calculate sd 
sds <- apply(Yield_clean_2, 2, sd, na.rm = TRUE)

# Sum/13.8qm -> yield/qm/season 
  # 180qm Haus, 13 Rinnen, ... 
Summary_Yield <- colSums(Yield_clean_2, na.rm = TRUE)
column_sums_qm <- Summary_Yield / 13.8


# data frame
sum_qm_df <- data.frame(
  Column = names(column_sums_qm),
  sum_qm = column_sums_qm, 
  cv = cv,
  cv_percent = cv_percent,
  sd = sds)

# calculate lower and upper value with 90% convidence interval
# Set Z-score for 90% confidence level
z_score <- 1.645

# Calculate lower and upper bounds of the 90% confidence interval
sum_qm_df$lower_90 <- pmax(0, sum_qm_df$sum_qm - z_score * sum_qm_df$sd)  # lower bound, clipped at 0
sum_qm_df$upper_90 <- sum_qm_df$sum_qm + z_score * sum_qm_df$sd           # upper bound


# arrange the data frame
# Subset only the rows for KK_Tross and KK_Tross_1
kk_rows <- sum_qm_df[sum_qm_df$Column %in% c("KK_Tross", "KK_Tross_1"), ]
ks_rows <- sum_qm_df[sum_qm_df$Column %in% c("KS_Tross", "KS_Tross_3"), ]
kh_rows <- sum_qm_df[sum_qm_df$Column %in% c("KH_Tross", "KH_Tross_5"), ]

# Calculate mean for selected numeric columns
kk_means <- colMeans(kk_rows[, c("sum_qm", "cv", "sd", "lower_90", "upper_90")])
ks_means <- colMeans(ks_rows[, c("sum_qm", "cv", "sd", "lower_90", "upper_90")])
kh_means <- colMeans(kh_rows[, c("sum_qm", "cv", "sd", "lower_90", "upper_90")])

# Update the KK_Tross row with the new mean values
sum_qm_df[sum_qm_df$Column == "KK_Tross", c("sum_qm", "cv", "sd", "lower_90", "upper_90")] <- kk_means
sum_qm_df[sum_qm_df$Column == "KS_Tross", c("sum_qm", "cv", "sd", "lower_90", "upper_90")] <- ks_means
sum_qm_df[sum_qm_df$Column == "KH_Tross", c("sum_qm", "cv", "sd", "lower_90", "upper_90")] <- kh_means

# Remove the KK_Tross_1 row
sum_qm_df_2 <- sum_qm_df[sum_qm_df$Column != "KK_Tross_1",]
sum_qm_df_2 <- sum_qm_df_2[sum_qm_df_2$Column != "KS_Tross_3",]
sum_qm_df_3 <- sum_qm_df_2[sum_qm_df_2$Column != "KH_Tross_5",]
# use the value for a base line. 



# make_variables<-function(est,n=1)
# { x<-random(rho=est, n=n)
# for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}


make_variables(estimate_read_csv("Final_Report/baseline.csv"))





Priming_function <- function(x, varnames){
  #Leistung
  Yield_K_K <- vv(Yield_type_1, var_CV_Yield_type_1, n_years)
  Yield_K_H <- vv(Yield_type_2, var_CV_Yield_type_2, n_years)
  Yield_K_S <- vv(Yield_type_3, var_CV_Yield_type_3, n_years)
  Yield_H_K <- vv(Yield_type_4, var_CV_Yield_type_4, n_years)
  Yield_H_H <- vv(Yield_type_5, var_CV_Yield_type_5, n_years)
  Yield_H_S <- vv(Yield_type_6, var_CV_Yield_type_6, n_years)
  Yield_S_K <- vv(Yield_type_7, var_CV_Yield_type_7, n_years)
  Yield_S_H <- vv(Yield_type_8, var_CV_Yield_type_8, n_years)
  Yield_S_S <- vv(Yield_type_9, var_CV_Yield_type_9, n_years)
  
  TomatenPreis <-vv(Tomaten_Preis, var_CV_Tomaten_Preis, n_years)
  
  # Leistung
  Leistung_K_K <- Yield_K_K+TomatenPreis
  Leistung_K_H <- Yield_K_H + TomatenPreis
  Leistung_K_S <- Yield_K_S + TomatenPreis
  Leistung_H_K <- Yield_H_K + TomatenPreis
  Leistung_H_H <- Yield_H_H + TomatenPreis
  Leistung_H_S <- Yield_H_S + TomatenPreis
  Leistung_S_K <- Yield_S_K + TomatenPreis
  Leistung_S_H <- Yield_S_H + TomatenPreis
  Leistung_S_S <- Yield_S_S + TomatenPreis
  
  #Ausgabe
  #Direkt Kosten & Arbeitskosten
  Kosten_ohne_Priming <- Saatgut + Jungepflanzen + 
    Substrat + Energie_therm + Energie_elek + CO2_H2O_Due + Kordel +
    Hummel_Nutzlinge + PSM_chem + Heackseln_Entsorgung + Desinfektion +
    Versicherung + Arbeit
  
  Kosten_mit_Priming <- Saatgut + Jungepflanzen_Priming +
    Substrat + Energie_therm + Energie_elek + CO2_H2O_Due + Kordel +
    Hummel_Nutzlinge + PSM_chem + Heackseln_Entsorgung + Desinfektion +
    Versicherung + Arbeit
  
  
  ## Total_ Gewinn_Kosten ##
  
  
  Result_K_K <- Leistung_K_K - Kosten_ohne_Priming
  Result_K_H <- Leistung_K_H - Kosten_ohne_Priming
  Result_K_S <- Leistung_K_S - Kosten_ohne_Priming
  Result_H_K <- Leistung_H_K - Kosten_mit_Priming
  Result_H_H <- Leistung_H_H - Kosten_mit_Priming
  Result_H_S <- Leistung_H_S - Kosten_mit_Priming
  Result_S_K <- Leistung_S_K - Kosten_mit_Priming
  Result_S_H <- Leistung_S_H - Kosten_mit_Priming
  Result_S_S <- Leistung_S_S - Kosten_mit_Priming
  
  
  #NPV calculate
  
  NPV_K_K <- discount(Result_K_K, discount_rate, calculate_NPV = T)
  NPV_K_H <- discount(Result_K_H, discount_rate, calculate_NPV = T)
  NPV_K_S <- discount(Result_K_S, discount_rate, calculate_NPV = T)
  NPV_H_K <- discount(Result_H_K, discount_rate, calculate_NPV = T)
  NPV_H_H <- discount(Result_H_H, discount_rate, calculate_NPV = T)
  NPV_H_S <- discount(Result_H_S, discount_rate, calculate_NPV = T)
  NPV_S_K <- discount(Result_S_K, discount_rate, calculate_NPV = T)
  NPV_S_H <- discount(Result_S_H, discount_rate, calculate_NPV = T)
  NPV_S_S <- discount(Result_S_S, discount_rate, calculate_NPV = T)
  
  
  return(list(K_K_NPV = NPV_K_K,
              K_H_NPV = NPV_K_H, 
              K_S_NPV = NPV_K_S,
              H_K_NPV = NPV_H_K, 
              H_H_NPV = NPV_H_H,
              H_S_NPV = NPV_H_S,
              S_K_NPV = NPV_S_K,
              S_H_NPV = NPV_S_H,
              S_S_NPV = NPV_S_S,
              
              NPV_K_H_NPV_K_K = NPV_K_H - NPV_K_K,
              NPV_K_S_NPV_K_K = NPV_K_S - NPV_K_K,
              NPV_H_K_NPV_K_K = NPV_H_K - NPV_K_K,
              NPV_H_H_NPV_K_K = NPV_H_H - NPV_K_K,
              NPV_H_S_NPV_K_K = NPV_H_S - NPV_K_K,
              NPV_S_K_NPV_K_K = NPV_S_K - NPV_K_K,
              NPV_S_H_NPV_K_K = NPV_S_H - NPV_K_K,
              NPV_S_S_NPV_K_K = NPV_S_S - NPV_K_K,
              
              Cashflow_K_H = Result_K_H - Result_K_K,
              Cashflow_K_S = Result_K_S - Result_K_K,
              Cashflow_H_K = Result_H_K - Result_K_K,
              Cashflow_H_H = Result_H_S - Result_K_K,
              Cashflow_H_S = Result_H_S - Result_K_K,
              Cashflow_S_K = Result_S_K - Result_K_K,
              Cashflow_S_H = Result_S_H - Result_K_K,
              Cashflow_S_S = Result_S_S - Result_K_K
              
  )) 
}

#  ---- Simulation  ----
library(readr)

Priming_input_table <- read_csv("Final_Report/baseline.csv")

Priming_simulation<-mcSimulation(estimate=estimate_read_csv("Final_Report/baseline.csv"),
                           model_function=Priming_function,
                           numberOfModelRuns=10000,
                           functionSyntax="plainNames")

Priming_scenarios<-scenario_mc(base_estimate = estimate_read_csv("Final_Report/baseline.csv"),
                         scenarios = read.csv("Final_Report/scenarios.csv",
                                              fileEncoding = "UTF-8-BOM"),
                         model_function = Priming_function,
                         numberOfModelRuns = 3000,
                         functionSyntax = "plainNames")




#  ---- Smaller farm  ----
scenarios_0<-read.csv("Final_Report/scenarios.csv", fileEncoding = "UTF-8-BOM")
scenarios <- scenarios_0[, -ncol(scenarios_0)]
s_farm<-scenarios[,c(1,2,3)]

s_farm_Priming_scenarios<-scenario_mc(base_estimate = estimate_read_csv("Final_Report/baseline.csv"),
                                      scenarios = s_farm,
                                      model_function = Priming_function,
                                      numberOfModelRuns = 50000,
                                      functionSyntax = "plainNames")


plot_distributions(mcSimulation_object = s_farm_Priming_scenarios, 
                   vars = c("K_K_NPV","H_K_NPV","S_K_NPV"),
                   method = 'boxplot', 
                   old_names = c("K_K_NPV","H_K_NPV","S_K_NPV"),
                   new_names = c("no priming","heat priming","salt priming"),
                   base_size = 10,
                   colors = c("tomato","#009E73","#56B4E9"),
                   legend.position = "none") +
  labs(title = "NPV Distribution with Different Decision Options \nand no stress for Smaller Farms")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


plot_distributions(mcSimulation_object = s_farm_Priming_scenarios, 
                   vars = c("K_K_NPV","K_H_NPV","K_S_NPV"),
                   method = 'boxplot', 
                   old_names = c("K_K_NPV","K_H_NPV","K_S_NPV"),
                   new_names = c("no stress","heat stress","salt stress"),
                   base_size = 10,
                   colors = c("tomato","#009E73","#56B4E9"),
                   legend.position = "none") +
  labs(title = "NPV Distribution with No Prming \nand Different Stresses for Smaller Farms")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


plot_distributions(mcSimulation_object = s_farm_Priming_scenarios, 
                   vars = c("H_K_NPV","H_H_NPV","H_S_NPV"),
                   method = 'boxplot', 
                   old_names = c("H_K_NPV","H_H_NPV","H_S_NPV"),
                   new_names = c("no stress","heat stress","salt stress"),
                   base_size = 10,
                   colors = c("tomato","#009E73","#56B4E9"),
                   legend.position = "none") +
  labs(title = "NPV Distribution with Heat Prming \nand Different Stresses for Smaller Farms")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

plot_distributions(mcSimulation_object = s_farm_Priming_scenarios, 
                   vars = c("S_K_NPV","S_H_NPV","S_S_NPV"),
                   method = 'boxplot', 
                   old_names = c("S_K_NPV","S_H_NPV","S_S_NPV"),
                   new_names = c("no stress","heat stress","salt stress"),
                   base_size = 10,
                   colors = c("tomato","#009E73","#56B4E9"),
                   legend.position = "none") +
  labs(title = "NPV Distribution with Salt Prming \nand Different Stresses for Smaller Farms")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))



plot_cashflow(mcSimulation_object = s_farm_Priming_scenarios, 
              cashflow_var_name = "Cashflow_H_K",
              facet_labels = "Cash Flow Comparison Between No Priming \nand Heat Priming for Smaller Farms" )

plot_cashflow(mcSimulation_object = s_farm_Priming_scenarios, 
              cashflow_var_name = "Cashflow_S_K",
              facet_labels ="Cash Flow Comparison Between No Priming \nand Salt Priming for Smaller Farms" )

library(evpi)
#  ---- EVPI s farm ----
### Value of Information (VoI) analysis ###

#here we subset the outputs from the mcSimulation function (y) by selecting the correct variables
# choose this carefully and be sure to run the multi_EVPI only on the variables that the you want
#EVPI = (EOL : Expected Opportunity Loss)

# make a individual data.frame from the scenario data. 

# Monte carlo inputs
s_farm_list2 <- s_farm_Priming_scenarios[[2]]
#remove the row with character (Farm_1, Farm_2...)
s_farm_list2 <- s_farm_list2[, -36]

# Monte Carlo outputs
s_farm_list1 <- s_farm_Priming_scenarios[[1]]
# choose only NPV
s_farm_list1 <- s_farm_list1[1:7]

# ---- EVPI s farm KK - HK  ----

# vergleich kk zu HK
s_farm_list12 <- s_farm_list1[c(1,4)]

s_farm_EVPI <-  evpi::multi_evppi(s_farm_list2, s_farm_list12)

s_farm_EVPI <- data.frame(
  Variable = colnames(s_farm_list2),
  EVPI = s_farm_EVPI
)


# Filter and arrange data to include only rows where EVPI is greater than 0
s_farm_EVPI_filtered <- s_farm_EVPI %>%
  filter(EVPI > 0.02) %>% 
  arrange(desc(EVPI)) %>%
  head(n = 3) %>% # Exclude specific rows by Variable names
  mutate(Variable = factor(Variable, levels = Variable)) # Reorder factor based on EVPI


# Create the horizontal bar plot

ggplot(s_farm_EVPI_filtered, aes(x = reorder(Variable, EVPI), y = EVPI)) +
  geom_bar(stat = "identity",fill = "tomato") +
  coord_flip() +
  labs(x = "Variable", y = "EVPI [€/m²]", title = "EVPI (no Priming or Heat Priming)") +
  scale_x_discrete(labels = c("Yield_type_4" = "Yield of Heat primed tomato [kg/m2]", 
                              "Jungepflanzen_Priming" = "primed seedling cost",
                              "Yield_type_1" = "Yield of none primed tomato [kg/m2]")) +  # Change labels
  theme_minimal()



#  ---- EVPI s farm KK - SK  ----
s_farm_list17 <- s_farm_list1[c(1,7)]

s_farm_EVPI17 <-  evpi::multi_evppi(s_farm_list2, s_farm_list17)

s_farm_EVPI17 <- data.frame(
  Variable = colnames(s_farm_list2),
  EVPI = s_farm_EVPI17
)

# Filter and arrange data to include only rows where EVPI is greater than 0
s_farm_EVPI_filtered17 <- s_farm_EVPI17 %>%
  filter(EVPI > 0.05) %>% 
  arrange(desc(EVPI)) %>%
  head(n = 3) %>% # Exclude specific rows by Variable names
  mutate(Variable = factor(Variable, levels = Variable)) # Reorder factor based on EVPI


# Create the horizontal bar plot

ggplot(s_farm_EVPI_filtered17, aes(x = reorder(Variable, EVPI), y = EVPI)) +
  geom_bar(stat = "identity", fill = "#56B4E9") +
  coord_flip() +
  labs(x = "Variable", y = "EVPI [€/m²]", title = "EVPI (no Priming or Salt Priming)") +
  scale_x_discrete(labels = c( "Jungepflanzen_Priming" = "primed seedling cost",
                               "Yield_type_1" = "Yield of none primed tomato [kg/m2]")) +  # Change labels
  theme_minimal()






#  ---- larger farm   ----




scenarios_0<-read.csv("Final_Report/scenarios.csv", fileEncoding = "UTF-8-BOM")
scenarios <- scenarios_0[, -ncol(scenarios_0)]
b_farm<-scenarios[,c(1,2,4,5)]

b_farm_Priming_scenarios<-scenario_mc(base_estimate = estimate_read_csv("Final_Report/baseline.csv"),
                                      scenarios = b_farm,
                                      model_function = Priming_function,
                                      numberOfModelRuns = 25000,
                                      functionSyntax = "plainNames")


plot_distributions(mcSimulation_object = b_farm_Priming_scenarios, 
                   vars = c("K_K_NPV","H_K_NPV","S_K_NPV"),
                   method = 'boxplot', 
                   old_names = c("K_K_NPV","H_K_NPV","S_K_NPV"),
                   new_names = c("no priming","heat priming","salt priming"),
                   base_size = 10,
                   colors = c("tomato","#009E73","#56B4E9"),
                   legend.position = "none") +
  labs(title = "NPV Distribution with Different Decision Options \nand no stress for Larger Farms")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

plot_distributions(mcSimulation_object = b_farm_Priming_scenarios, 
                   vars = c("K_K_NPV","K_H_NPV","K_S_NPV"),
                   method = 'boxplot', 
                   old_names = c("K_K_NPV","K_H_NPV","K_S_NPV"),
                   new_names = c("no stress","heat stress","salt stress"),
                   base_size = 10,
                   colors = c("tomato","#009E73","#56B4E9"),
                   legend.position = "none") +
  labs(title = "NPV Distribution with No Prming \nand Different Stresses for Larger Farms")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


plot_distributions(mcSimulation_object = b_farm_Priming_scenarios, 
                   vars = c("H_K_NPV","H_H_NPV","H_S_NPV"),
                   method = 'boxplot', 
                   old_names = c("H_K_NPV","H_H_NPV","H_S_NPV"),
                   new_names = c("no stress","heat stress","salt stress"),
                   base_size = 10,
                   colors = c("tomato","#009E73","#56B4E9"),
                   legend.position = "none") +
  labs(title = "NPV Distribution with Heat Prming \nand Different Stresses for Larger Farms")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

plot_distributions(mcSimulation_object = b_farm_Priming_scenarios, 
                   vars = c("S_K_NPV","S_H_NPV","S_S_NPV"),
                   method = 'boxplot', 
                   old_names = c("S_K_NPV","S_H_NPV","S_S_NPV"),
                   new_names = c("no stress","heat stress","salt stress"),
                   base_size = 10,
                   colors = c("tomato","#009E73","#56B4E9"),
                   legend.position = "none") +
  labs(title = "NPV Distribution with Salt Prming \nand Different Stresses for Larger Farms")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))



plot_cashflow(mcSimulation_object = s_farm_Priming_scenarios, 
              cashflow_var_name = "Cashflow_H_K",
              facet_labels = "Cash Flow Comparison Between No Priming \nand Heat Priming for Larger Farms" )

plot_cashflow(mcSimulation_object = s_farm_Priming_scenarios, 
              cashflow_var_name = "Cashflow_S_K",
              facet_labels ="Cash Flow Comparison Between No Priming \nand Salt Priming for Larger Farms" )



#  ---- EVPI b farm  ----

# make a individual data.frame from the scenario data. 

# Monte carlo inputs
b_farm_list2 <- b_farm_Priming_scenarios[[2]]
#remove the row with character (Farm_1, Farm_2...)

b_farm_list2 <- b_farm_list2[, -36]
# Monte Carlo outputs
b_farm_list1 <- b_farm_Priming_scenarios[[1]]
# choose only NPV
b_farm_list1 <- b_farm_list1[1:7]

#  ---- EVPI b farm  KK - HK  ----

# vergleich KK zu HK
b_farm_list14 <- b_farm_list1[c(1,4)]

b_farm_EVPI14 <-  evpi::multi_evppi(b_farm_list2, b_farm_list14)

b_farm_EVPI14 <- data.frame(
  Variable = colnames(b_farm_list2),
  EVPI = b_farm_EVPI14
)


# Filter and arrange data to include only rows where EVPI is greater than 0
b_farm_EVPI_filtered14 <- b_farm_EVPI14 %>%
  filter(EVPI > 0.75) %>%
  arrange(desc(EVPI)) %>%        # Arrange by EVPI in descending order
  slice_head(n = 5) %>% 
  mutate(Variable = factor(Variable, levels = Variable)) # Reorder factor based on EVPI

# Create the horizontal bar plot
ggplot(b_farm_EVPI_filtered14, aes(x = reorder(Variable, EVPI), y = EVPI)) + 
  geom_bar(stat = "identity",fill = "tomato") + 
  coord_flip() + 
  labs(x = "Variable", y = "EVPI [€/m²]", title = "EVPI (no Priming or Heat Priming)") + 
  scale_x_discrete(labels = c("Yield_type_4" = "Yield of Heat primed tomato [kg/m2]", 
                              "Jungepflanzen_Priming" = "primed seedling cost",
                              "Yield_type_1" = "Yield of none primed tomato [kg/m2]")) +  # Change labels
  theme_minimal()




#  ---- EVPI b farm  KK - SK  ----

# vergleich KK zu HK
b_farm_list17 <- b_farm_list1[c(1,7)]

b_farm_EVPI17 <-  evpi::multi_evppi(b_farm_list2, b_farm_list17)

b_farm_EVPI17 <- data.frame(
  Variable = colnames(b_farm_list2),
  EVPI = b_farm_EVPI17
)


# Filter and arrange data to include only rows where EVPI is greater than 0
b_farm_EVPI_filtered17 <- b_farm_EVPI17 %>%
  filter(EVPI > 0.2) %>%
  arrange(desc(EVPI)) %>%        # Arrange by EVPI in descending order
  slice_head(n = 5) %>% 
  mutate(Variable = factor(Variable, levels = Variable)) # Reorder factor based on EVPI

# Create the horizontal bar plot
ggplot(b_farm_EVPI_filtered17, aes(x = reorder(Variable, EVPI), y = EVPI)) +
  geom_bar(stat = "identity",  fill = "#56B4E9") +
  coord_flip() +
  labs(x = "Variable", y = "EVPI [€/m²]", title = "EVPI (no Priming or Salt Priming)")+
  scale_x_discrete(labels = c("Yield_type_7" = "Yield of Salt primed tomato [kg/m2]", 
                              "Jungepflanzen_Priming" = "primed seedling cost",
                              "Yield_type_1" = "Yield of none primed tomato [kg/m2]",
                              "Hummel_Nutzlinge" = "bio. plant protection cost [€/m2]")) +  # Change labels
  theme_minimal()




# ---- Explain EVPI ----



# The Expected Value of Perfect Information (EVPI) represents
# the maximum amount you would be willing to pay for perfect
# information that reduces uncertainty in decision-making.

# This indicates that knowing more about the factors influencing
# the yield of heat primed tomatoes has higher economic value
# compared to none primed tomatoes.

#1. The EVPI values represent the maximum amount you'd be willing to
#pay to reduce uncertainty for each decision.

#2. A higher EVPI indicates that gaining more information for
#that specific decision could significantly improve outcomes.

#3. Comparing the EVPI values will help you prioritize which
#areas or decisions benefit the most from perfect information—investing
#more in acquiring information for those decisions would be most
#beneficial.



#  ---- check  ----


#  ---- PLS  ----
#Cory
#PLS
Priming_scenarios1 <- Priming_scenarios
Priming_scenarios1$x<-Priming_scenarios1$x[,which(!colnames(Priming_scenarios1$x)=="Scenario")]

legend_table<-read.csv("Final_Report/baseline.csv", fileEncoding = "UTF-8-BOM")

#PLS for Heat priming + no stress
pls_out<-plsr.mcSimulation(object=Priming_scenarios1, 
                           resultName = names(Priming_scenarios1$y)[4])


plot_pls(pls_out,input_table=legend_table, cut_off_line = 0.8)

# PLS for Salt priming + no stress
pls_out7<-plsr.mcSimulation(object=Priming_scenarios1, 
                            resultName = names(Priming_scenarios1$y)[7])


plot_pls(pls_out7,input_table=legend_table, cut_off_line = 0.8)
