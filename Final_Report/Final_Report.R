
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


no_stress_s_farm <- plot_distributions(mcSimulation_object = s_farm_Priming_scenarios, 
                   vars = c("K_K_NPV","H_K_NPV","S_K_NPV"),
                   method = 'boxplot', 
                   old_names = c("K_K_NPV","H_K_NPV","S_K_NPV"),
                   new_names = c("no priming","heat priming","salt priming"),
                   base_size = 10,
                   legend.position = "none") +
  labs(title = "NPV Distribution with Different Decision Options \nand no stress for Smaller Farms")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(
    values = c("no stress" = "#009E73", "salt stress" = "#56B4E9", "heat stress" = "tomato"),
    breaks = c("no stress", "salt stress", "heat stress")  # << THIS controls legend order
  )

ggsave( paste0("Final_Report/graphs/no_stress_s_farm",".tiff"), no_stress_s_farm, 
        width = 12, height = 8, units = 'cm', dpi = 750, compression = "lzw")


no_priming_s_farm <- plot_distributions(mcSimulation_object = s_farm_Priming_scenarios, 
                   vars = c("K_K_NPV","K_H_NPV","K_S_NPV"),
                   method = 'boxplot', 
                   old_names = c("K_K_NPV","K_H_NPV","K_S_NPV"),
                   new_names = c("no stress","heat stress","salt stress"),
                   base_size = 10,
                   legend.position = "none") +
  labs(title = "NPV Distribution with No Prming \nand Different Stresses for Smaller Farms")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(
    values = c("no stress" = "#009E73", "salt stress" = "#56B4E9", "heat stress" = "tomato"),
    breaks = c("no stress", "salt stress", "heat stress")  # << THIS controls legend order
  )
ggsave( paste0("Final_Report/graphs/no_priming_s_farm",".tiff"), no_priming_s_farm, 
        width = 12, height = 8, units = 'cm', dpi = 750, compression = "lzw")



heat_priming_s_farm <- plot_distributions(
  mcSimulation_object = s_farm_Priming_scenarios, 
  vars = c("H_K_NPV", "H_H_NPV", "H_S_NPV"),
  method = 'boxplot', 
  old_names = c("H_K_NPV", "H_H_NPV", "H_S_NPV"),
  new_names = c("no stress", "heat stress", "salt stress"),
  base_size = 10,
  legend.position = "none") +
  labs(title = "NPV Distribution with Heat Priming \nand Different Stresses for Smaller Farms") +
  theme_bw() +
  theme( plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(
    values = c("no stress" = "#009E73", "salt stress" = "#56B4E9", "heat stress" = "tomato"),
    breaks = c("no stress", "salt stress", "heat stress")) + # order y-axis and legend
  scale_y_discrete(limits = c(  "heat stress","salt stress","no stress"))  # << critical: y-axis order

ggsave( paste0("Final_Report/graphs/heat_priming_s_farm",".tiff"), heat_priming_s_farm, 
        width = 12, height = 8, units = 'cm', dpi = 750, compression = "lzw")

salt_priming_s_farm <- plot_distributions(mcSimulation_object = s_farm_Priming_scenarios, 
                   vars = c("S_K_NPV","S_H_NPV","S_S_NPV"),
                   method = 'boxplot', 
                   old_names = c("S_K_NPV","S_H_NPV","S_S_NPV"),
                   new_names = c("no stress","heat stress","salt stress"),
                   base_size = 10,
                   colors = c("tomato","#009E73","#56B4E9"),
                   legend.position = "none") +
  labs(title = "NPV Distribution with Salt Prming \nand Different Stresses for Smaller Farms")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(
    values = c("no stress" = "#009E73", "salt stress" = "#56B4E9", "heat stress" = "tomato"),
    breaks = c("no stress", "salt stress", "heat stress")) + # order y-axis and legend
  scale_y_discrete(limits = c(  "heat stress","salt stress","no stress"))  # << critical: y-axis order

ggsave( paste0("Final_Report/graphs/salt_priming_s_farm",".tiff"), salt_priming_s_farm, 
        width = 12, height = 8, units = 'cm', dpi = 750, compression = "lzw")




cashflow_KK_HK_s_farm <- plot_cashflow(mcSimulation_object = s_farm_Priming_scenarios, 
              cashflow_var_name = "Cashflow_H_K",
              facet_labels = "Cash Flow Comparison Between No Priming \nand Heat Priming for Smaller Farms" )
ggsave( paste0("Final_Report/graphs/cashflow_KK_HK_s_farm",".tiff"), cashflow_KK_HK_s_farm, 
        width = 12, height = 8, units = 'cm', dpi = 750, compression = "lzw")
cashflow_KK_SK_s_farm <- plot_cashflow(mcSimulation_object = s_farm_Priming_scenarios, 
              cashflow_var_name = "Cashflow_S_K",
              facet_labels ="Cash Flow Comparison Between No Priming \nand Salt Priming for Smaller Farms" )
ggsave( paste0("Final_Report/graphs/cashflow_KK_SK_s_farm",".tiff"), cashflow_KK_SK_s_farm, 
        width = 12, height = 8, units = 'cm', dpi = 750, compression = "lzw")

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

evpi_KK_HK_sfarm <- ggplot(s_farm_EVPI_filtered, aes(x = reorder(Variable, EVPI), y = EVPI)) +
  geom_bar(stat = "identity",fill = "tomato") +
  coord_flip() +
  labs(x = "Variable", y = "EVPI [€/m²]", title = "EVPI (no Priming or Heat Priming)") +
  scale_x_discrete(labels = c("Yield_type_4" = "Yield of Heat primed tomato [kg/m2]", 
                              "Jungepflanzen_Priming" = "primed seedling cost",
                              "Yield_type_1" = "Yield of none primed tomato [kg/m2]")) +  # Change labels
 
  theme_bw()

ggsave( paste0("Final_Report/graphs/evpi_KK_HK_sfarm",".tiff"), evpi_KK_HK_sfarm, 
        width = 14, height = 8, units = 'cm', dpi = 700, compression = "lzw")

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

evpi_KK_SK_sfarm <- ggplot(s_farm_EVPI_filtered17, aes(x = reorder(Variable, EVPI), y = EVPI)) +
  geom_bar(stat = "identity", fill = "#56B4E9") +
  coord_flip() +
  labs(x = "Variable", y = "EVPI [€/m²]", title = "EVPI (no Priming or Salt Priming)") +
  scale_x_discrete(labels = c( "Jungepflanzen_Priming" = "primed seedling cost",
                               "Yield_type_1" = "Yield of none primed tomato [kg/m2]",
                               "Yield_type_7" = "Yield of salt primed tomato [kg/m2]")) +  # Change labels
  theme_bw()


ggsave( paste0("Final_Report/graphs/evpi_KK_SK_sfarm",".tiff"), evpi_KK_SK_sfarm, 
        width = 14, height = 8, units = 'cm', dpi = 750, compression = "lzw")




#----PLS s farm ----

#PLS
Priming_scenarios_sfarm <- s_farm_Priming_scenarios
Priming_scenarios_sfarm$x<-Priming_scenarios_sfarm$x[,which(!colnames(Priming_scenarios_sfarm$x)=="Scenario")]

#legend_table<-read.csv("Final_Report/baseline.csv", fileEncoding = "UTF-8-BOM")

#PLS for Heat priming - no priming under control
pls_out_HK_KK_sfarm<-plsr.mcSimulation(object=Priming_scenarios_sfarm, 
                                 resultName = names(Priming_scenarios_sfarm$y)[12])


#plot_pls(pls_out_HK_KK,input_table=legend_table, cut_off_line = 0.8)
plot_pls(pls_out_HK_KK_sfarm,cut_off_line = 1, threshold = 0.8,
         x_axis_name = "Variable Importance in Projection")+
  labs( title = "the Variable Importance in the Projection (VIP) 
  of no Priming and Heat Priming without stress in small farm")+
  scale_y_discrete(labels = c("Yield_type_4" = "Yield of Heat primed tomato [kg/m2]", 
                              "Jungepflanzen_Priming" = "primed seedling cost",
                              "Yield_type_1" = "Yield of none primed tomato [kg/m2]"))

ggsave( paste0("Final_Report/graphs/pls_out_HK_KK_sfarm",".tiff"), pls_out_HK_KK_sfarm, 
        width = 14, height = 8, units = 'cm', dpi = 750, compression = "lzw")



# PLS for Salt priming - no priming under control
pls_out_SK_KK_sfarm<-plsr.mcSimulation(object=Priming_scenarios_sfarm, 
                                 resultName = names(Priming_scenarios_sfarm$y)[15])


plot_pls(pls_out_SK_KK_sfarm,cut_off_line = 1, threshold = 0.8,
         x_axis_name = "Variable Importance in Projection")+
  labs( title = "the Variable Importance in the Projection (VIP) of 
        \nno Priming and Salt Priming without stress")+
  scale_y_discrete(labels = c("Yield_type_7" = "Yield of Salt primed tomato [kg/m2]", 
                              "Jungepflanzen_Priming" = "primed seedling cost",
                              "Yield_type_1" = "Yield of none primed tomato [kg/m2]",
                              "Hummel_Nutzlinge" = "bio. plant protection cost [€/m2]"))


coeff1 <- pls_out_HK_KK_sfarm[["coefficients"]][,,1]
scale  <- pls_out_HK_KK_sfarm[["scale"]]

s_farm_EVPI_1 <- s_farm_EVPI[1:26, ]

VIP_table_HK_KK_sfarm_1 <- data.frame(
  name = names(scale),
  VIP = as.numeric(scale),
  coefficient = as.numeric(coeff1),
EVPI = as.numeric(s_farm_EVPI_1$EVPI))
VIP_table_HK_KK_sfarm <- VIP_table_HK_KK_sfarm_1[-26, ]




pls_subset <- subset(VIP_table_HK_KK_sfarm_1,VIP_table_HK_KK_sfarm_1$VIP>=0.8)
pls_subset <- subset(VIP_table_HK_KK_sfarm_1,VIP_table_HK_KK_sfarm_1$EVPI>=1)
pls_subset$Category[pls_subset$coefficient > 0] = "cadetblue"
pls_subset$Category[pls_subset$coefficient < 0] = "firebrick"
pls_subset$Category[pls_subset$VIP < 1] ="grey"
pls_dat <- as.data.frame(pls_subset)
name_mapping <- c(
  "Yield_type_4" = "Yield of Heat primed tomato [kg/m2]",
  "Yield_type_7" = "Yield of Salt primed tomato [kg/m2]", 
  "Jungepflanzen_Priming" = "primed seedling cost",
  "Yield_type_1" = "Yield of none primed tomato [kg/m2]",
  "Hummel_Nutzlinge" = "bio. plant protection cost [€/m2]"
)
pls_dat$X <- name_mapping[pls_dat$name]

p <- ggplot(pls_dat,aes(x=pls_dat$name,y=pls_dat$VIP))+
  geom_bar(aes(fill=pls_dat$Category),stat ="identity")+
  ggtitle("Variable Importance")+
  ylab("VIP scores")+
  xlab(NULL)+
  scale_fill_manual(values = c("cadetblue","firebrick","grey"))+
  theme(axis.title.y =element_text(color="black", size=8),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(1,-1,1,0), "mm")) +
  geom_hline(yintercept = 1, size=0.2)+
  theme(legend.position = "none")+
  scale_y_reverse() +
  coord_flip()
q <-  ggplot(data = pls_dat, aes(x = pls_dat$X, y = pls_dat$EVPI))+
  geom_bar(fill = "deepskyblue3",stat = "identity") +
  ggtitle("Value of Information") +
  ylab("EVPI")+
  xlab(NULL)+
  theme(axis.title.y = element_text(color="black", size=8),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(1,0,1,-1), "mm")) +
  coord_flip()
g.mid <- ggplot(pls_dat,aes(x=1,y=pls_dat$X))+geom_text(aes(label=pls_dat$X))+
  geom_segment(aes(x=0,xend=0,yend=pls_dat$X))+
  geom_segment(aes(x=0,xend=0,yend=pls_dat$X))+
  ggtitle("")+
  ylab(NULL)+
  scale_x_continuous(expand=c(0,0),limits=c(1.0,1.0))+
  theme(axis.title=element_blank(),
        panel.grid=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(1,0,1,0), "mm"))+
  scale_y_discrete(labels = c("Yield_type_7" = "Yield of Salt primed tomato [kg/m2]", 
                              "Jungepflanzen_Priming" = "primed seedling cost",
                              "Yield_type_1" = "Yield of none primed tomato [kg/m2]",
                              "Hummel_Nutzlinge" = "bio. plant protection cost [€/m2]"))
gg1 <- ggplot_gtable(ggplot_build(p))
gg2 <- ggplot_gtable(ggplot_build(q))
gg.mid <- ggplot_gtable(ggplot_build(g.mid))
cowplot::plot_grid(gg1,gg.mid,gg2, ncol = 3, align = "h")

#  ---- larger farm   ----




scenarios_0<-read.csv("Final_Report/scenarios.csv", fileEncoding = "UTF-8-BOM")
scenarios <- scenarios_0[, -ncol(scenarios_0)]
b_farm<-scenarios[,c(1,2,4,5)]

b_farm_Priming_scenarios<-scenario_mc(base_estimate = estimate_read_csv("Final_Report/baseline.csv"),
                                      scenarios = b_farm,
                                      model_function = Priming_function,
                                      numberOfModelRuns = 25000,
                                      functionSyntax = "plainNames")


no_stress_bfarm <- plot_distributions(mcSimulation_object = b_farm_Priming_scenarios, 
                   vars = c("K_K_NPV","H_K_NPV","S_K_NPV"),
                   method = 'boxplot', 
                   old_names = c("K_K_NPV","H_K_NPV","S_K_NPV"),
                   new_names = c("no priming","heat priming","salt priming"),
                   base_size = 10,
                   colors = c("tomato","#009E73","#56B4E9"),
                   legend.position = "none") +
  labs(title = "NPV Distribution with Different Decision Options \nand no stress for Larger Farms")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(
    values = c("no stress" = "#009E73", "salt stress" = "#56B4E9", "heat stress" = "tomato"),
    breaks = c("no stress", "salt stress", "heat stress")) + # order y-axis and legend
  scale_y_discrete(limits = c(  "heat stress","salt stress","no stress"))  # << critical: y-axis order


ggsave( paste0("Final_Report/graphs/no_stress_bfarm",".tiff"), no_stress_bfarm, 
        width = 12, height = 8, units = 'cm', dpi = 750, compression = "lzw")



no_priming_bfarm <- plot_distributions(mcSimulation_object = b_farm_Priming_scenarios, 
                   vars = c("K_K_NPV","K_H_NPV","K_S_NPV"),
                   method = 'boxplot', 
                   old_names = c("K_K_NPV","K_H_NPV","K_S_NPV"),
                   new_names = c("no stress","heat stress","salt stress"),
                   base_size = 10,
                   colors = c("tomato","#009E73","#56B4E9"),
                   legend.position = "none") +
  labs(title = "NPV Distribution with No Prming \nand Different Stresses for Larger Farms")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(
    values = c("no stress" = "#009E73", "salt stress" = "#56B4E9", "heat stress" = "tomato"),
    breaks = c("no stress", "salt stress", "heat stress")) + # order y-axis and legend
  scale_y_discrete(limits = c(  "heat stress","salt stress","no stress"))  # << critical: y-axis order


ggsave( paste0("Final_Report/graphs/no_priming_bfarm",".tiff"), no_priming_bfarm, 
        width = 12, height = 8, units = 'cm', dpi = 750, compression = "lzw")


heat_priming_bfarm <- plot_distributions(mcSimulation_object = b_farm_Priming_scenarios, 
                   vars = c("H_K_NPV","H_H_NPV","H_S_NPV"),
                   method = 'boxplot', 
                   old_names = c("H_K_NPV","H_H_NPV","H_S_NPV"),
                   new_names = c("no stress","heat stress","salt stress"),
                   base_size = 10,
                   colors = c("tomato","#009E73","#56B4E9"),
                   legend.position = "none") +
  labs(title = "NPV Distribution with Heat Prming \nand Different Stresses for Larger Farms")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(
    values = c("no stress" = "#009E73", "salt stress" = "#56B4E9", "heat stress" = "tomato"),
    breaks = c("no stress", "salt stress", "heat stress")) + # order y-axis and legend
  scale_y_discrete(limits = c(  "heat stress","salt stress","no stress"))  # << critical: y-axis order


ggsave( paste0("Final_Report/graphs/heat_priming_bfarm",".tiff"), heat_priming_bfarm, 
        width = 12, height = 8, units = 'cm', dpi = 750, compression = "lzw")


salt_priming_bfarm <- plot_distributions(mcSimulation_object = b_farm_Priming_scenarios, 
                   vars = c("S_K_NPV","S_H_NPV","S_S_NPV"),
                   method = 'boxplot', 
                   old_names = c("S_K_NPV","S_H_NPV","S_S_NPV"),
                   new_names = c("no stress","heat stress","salt stress"),
                   base_size = 10,
                   colors = c("tomato","#009E73","#56B4E9"),
                   legend.position = "none") +
  labs(title = "NPV Distribution with Salt Prming \nand Different Stresses for Larger Farms")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(
    values = c("no stress" = "#009E73", "salt stress" = "#56B4E9", "heat stress" = "tomato"),
    breaks = c("no stress", "salt stress", "heat stress")) + # order y-axis and legend
  scale_y_discrete(limits = c(  "heat stress","salt stress","no stress"))  # << critical: y-axis order


ggsave( paste0("Final_Report/graphs/salt_priming_bfarm",".tiff"), salt_priming_bfarm, 
        width = 12, height = 8, units = 'cm', dpi = 750, compression = "lzw")


cashflow_KK_HK_bfarm <- plot_cashflow(mcSimulation_object = s_farm_Priming_scenarios, 
              cashflow_var_name = "Cashflow_H_K",
              facet_labels = "Cash Flow Comparison Between No Priming \nand Heat Priming for Larger Farms" )

ggsave( paste0("Final_Report/graphs/cashflow_KK_HK_bfarm",".tiff"), cashflow_KK_HK_bfarm, 
        width = 12, height = 8, units = 'cm', dpi = 750, compression = "lzw")

cashflow_KK_SK_bfarm <- plot_cashflow(mcSimulation_object = s_farm_Priming_scenarios, 
              cashflow_var_name = "Cashflow_S_K",
              facet_labels ="Cash Flow Comparison Between No Priming \nand Salt Priming for Larger Farms" )
ggsave( paste0("Final_Report/graphs/cashflow_KK_SK_bfarm",".tiff"), cashflow_KK_SK_bfarm, 
        width = 12, height = 8, units = 'cm', dpi = 750, compression = "lzw")


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
evpi_KK_HK_bfarm <- ggplot(b_farm_EVPI_filtered14, aes(x = reorder(Variable, EVPI), y = EVPI)) + 
  geom_bar(stat = "identity",fill = "tomato") + 
  coord_flip() + 
  labs(x = "Variable", y = "EVPI [€/m²]", title = "EVPI (no Priming or Heat Priming)") + 
  scale_x_discrete(labels = c("Yield_type_4" = "Yield of Heat primed tomato [kg/m2]", 
                              "Jungepflanzen_Priming" = "primed seedling cost",
                              "Yield_type_1" = "Yield of none primed tomato [kg/m2]")) +  # Change labels
  theme_minimal()

ggsave( paste0("Final_Report/graphs/evpi_KK_HK_bfarm",".tiff"), evpi_KK_HK_bfarm, 
        width = 14, height = 8, units = 'cm', dpi = 750, compression = "lzw")


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
evpi_KK_SK_bfarm <- ggplot(b_farm_EVPI_filtered17, aes(x = reorder(Variable, EVPI), y = EVPI)) +
  geom_bar(stat = "identity",  fill = "#56B4E9") +
  coord_flip() +
  labs(x = "Variable", y = "EVPI [€/m²]", title = "EVPI (no Priming or Salt Priming)")+
  scale_x_discrete(labels = c("Yield_type_7" = "Yield of Salt primed tomato [kg/m2]", 
                              "Jungepflanzen_Priming" = "primed seedling cost",
                              "Yield_type_1" = "Yield of none primed tomato [kg/m2]",
                              "Hummel_Nutzlinge" = "bio. plant protection cost [€/m2]")) +  # Change labels
  theme_minimal()


ggsave( paste0("Final_Report/graphs/evpi_KK_SK_bfarm",".tiff"), evpi_KK_SK_bfarm, 
        width = 14, height = 8, units = 'cm', dpi = 750, compression = "lzw")

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

# check again.!!

#PLS
Priming_scenarios1 <- b_farm_Priming_scenarios
Priming_scenarios1$x<-Priming_scenarios1$x[,which(!colnames(Priming_scenarios1$x)=="Scenario")]

legend_table<-read.csv("Final_Report/baseline.csv", fileEncoding = "UTF-8-BOM")

#PLS for Heat priming - no priming under control
pls_out_HK_KK<-plsr.mcSimulation(object=Priming_scenarios1, 
                           resultName = names(Priming_scenarios1$y)[12])


#plot_pls(pls_out_HK_KK,input_table=legend_table, cut_off_line = 0.8)
plot_pls(pls_out_HK_KK,cut_off_line = 1, threshold = 0.8,
         x_axis_name = "Variable Importance in Projection")+
  labs( title = "the Variable Importance in the Projection (VIP) 
        of \nno Priming and Heat Priming without stress")+
  scale_y_discrete(labels = c("Yield_type_4" = "Yield of Heat primed tomato [kg/m2]", 
                              "Jungepflanzen_Priming" = "primed seedling cost",
                              "Yield_type_1" = "Yield of none primed tomato [kg/m2]"))
  



# PLS for Salt priming - no priming under control
pls_out_SK_KK<-plsr.mcSimulation(object=Priming_scenarios1, 
                            resultName = names(Priming_scenarios1$y)[15])


plot_pls(pls_out_SK_KK,cut_off_line = 1, threshold = 0.8,
         x_axis_name = "Variable Importance in Projection")+
  labs( title = "the Variable Importance in the Projection (VIP) of 
        \nno Priming and Salt Priming without stress")+
  scale_y_discrete(labels = c("Yield_type_7" = "Yield of Salt primed tomato [kg/m2]", 
                              "Jungepflanzen_Priming" = "primed seedling cost",
                              "Yield_type_1" = "Yield of none primed tomato [kg/m2]",
                              "Hummel_Nutzlinge" = "bio. plant protection cost [€/m2]"))
"#56B4E9"
"tomato"
#---- NPV from difference ---- 
NPV_H_K_NPV_K_K <- decisionSupport::plot_distributions(
  mcSimulation_object = Priming_simulation, 
  vars = c("NPV_H_K_NPV_K_K"),
  method = 'boxplot_density', 
  old_names = c("NPV_H_S_NPV_K_S"),
  new_names = c("NPV difference"),
  base_size = 10,
  colors = c("tomato")) +
  theme_bw() +
  theme( 
         legend.position = "none",
         axis.title.x = element_blank(),
         axis.title.y = element_blank(),
         legend.title = element_blank(),
         strip.text = element_blank()) 

# ggsave( paste0("Final_Report/graphs/all_farms/NPV_H_K_NPV_K_K",".png"), NPV_H_K_NPV_K_K, 
#         width = 14, height = 8, units = 'cm', dpi = 750)




plot_cashflow(mcSimulation_object = Priming_simulation, 
                               cashflow_var_name = "Cashflow_HK_KK",
                               y_axis_name = "Cashflow [€/m²]",
                               facet_labels = "Cash Flow Comparison Between No Priming \nand Heat Priming",
                               base_size = 14,
              color_25_75 = "coral")



plot_cashflow(mcSimulation_object = Priming_simulation, 
                                cashflow_var_name = "Cashflow_SK_KK",
                                y_axis_name = "Cashflow [€/m²]",
                                facet_labels ="Cash Flow Comparison Between No Priming \nand Salt Priming",
                                base_size = 14,
              color_25_75 = "lightskyblue")



plot_distributions(mcSimulation_object = Priming_simulation, 
                   vars = c("NPV_H_K_NPV_K_K","NPV_S_K_NPV_K_K"),
                   method = 'boxplot', 
                   old_names = c("NPV_H_K_NPV_K_K","NPV_S_K_NPV_K_K"),
                   new_names = c("No Priming vs. Heat Priming",
                                 "No Priming vs. Salt Priming"),
                   base_size = 10,
                   legend.position = "none",
                   x_axis_name = "Outcome distribution [€/m²]",
                   y_axis_name = "Comaparison options") +
  labs(title = "Distribution of NPV Comparison between \nNo Priming and Heat or Salt Priming")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values = c( "No Priming vs. Heat Priming" = "#ff876e", 
                                "No Priming vs. Salt Priming" = "#7cc3ee"))


# Cash flow table
  # Use `sapply` to compute the mean for each column
  means <- sapply(Priming_simulation$y[, grepl("^Cashflow.*_KK", names(Priming_simulation$y))], mean)
  medians <- sapply(Priming_simulation$y[, grepl("^Cashflow.*_KK", names(Priming_simulation$y))], median)
  cf_05 <- sapply(Priming_simulation$y[, grepl("^Cashflow.*_KK", names(Priming_simulation$y))], function(x) quantile(x, 0.05))
  cf_25 <- sapply(Priming_simulation$y[, grepl("^Cashflow.*_KK", names(Priming_simulation$y))], function(x) quantile(x, 0.25))
  cf_75 <- sapply(Priming_simulation$y[, grepl("^Cashflow.*_KK", names(Priming_simulation$y))], function(x) quantile(x, 0.75))
  cf_95 <- sapply(Priming_simulation$y[, grepl("^Cashflow.*_KK", names(Priming_simulation$y))], function(x) quantile(x, 0.95))
  
  # Convert to a data frame for display
 Cashflow <-  data.frame("Cashflow" = names(means), cf_5 = round(cf_05,2), Q1 =round(cf_25, 2), 
                         Median = round(medians,2), Mean = round(means, 2), Q3 =round(cf_75, 2), cf_95 = round(cf_95,2))
 names(Cashflow)[1]<-paste("Annual Cash flow")
 names(Cashflow)[2]<-paste("5% Quantile [€/m²]")
 names(Cashflow)[3]<-paste("25% Quantle [€/m²]")
 names(Cashflow)[4]<-paste("Median [€/m²]")
 names(Cashflow)[5]<-paste("Mean (50%) [€/m²]")
 names(Cashflow)[6]<-paste("75% Quantile [€/m²]")
 names(Cashflow)[7]<-paste("95% Quantile [€/m²]")
 library(dplyr)
 Cashflow_1 <- tail(Cashflow,n =10)             
print(Cashflow_1$`Median [€/m²]`)
library(ggplot2)



# NPV tables


summary_simulaton <- summary(Priming_simulation$y)
summary_simulaton_table <- as.data.frame(summary_simulaton)

summary_simulaton_table <- summary_simulaton_table %>%
  as.data.frame() %>%
  filter(grepl("K_K_NPV", Var2) | grepl("H_K_NPV", Var2)| grepl("S_K_NPV", Var2) | grepl("^Cashflow.*_KK", Var2))

# NPV distribution plots
Priming_simulation_y <- Priming_simulation$y %>%
  dplyr::select (c("K_K_NPV", "H_K_NPV", "S_K_NPV", "NPV_H_K_NPV_K_K", "NPV_S_K_NPV_K_K"))%>%
  tidyr::pivot_longer(cols = c("K_K_NPV", "H_K_NPV", "S_K_NPV", "NPV_H_K_NPV_K_K", "NPV_S_K_NPV_K_K"),
                      names_to = "name", values_to = "value") %>%
  dplyr::mutate(name = dplyr::recode(name, 
                                     "K_K_NPV" = "NPV without priming",
                                     "H_K_NPV" = "NPV with heat priming",
                                     "S_K_NPV" = "NPV with salt priming",
                                     "NPV_H_K_NPV_K_K" = "NPV comparison between heat primed and non-primed", 
                                     "NPV_S_K_NPV_K_K"= "NPV comparison between salt primed and non-primed"))

data <- data.frame(percentage = c(0.1, 0.15, 0.5, 4), 
                   options_difference = c(100, 11229249, 58838895, 507997898))
regression <- stats::lm(percentage ~ options_difference, 
                        data = data)
options_difference <- max(Priming_simulation_y$value) - 
  min(Priming_simulation_y$value)
boxploth_width_correction <- stats::coefficients(regression)[[1]] + 
  (stats::coefficients(regression)[[2]] * options_difference)


# Cash flow table
# Use `sapply` to compute the mean for each column
NPV_means <- sapply(Priming_simulation$y[, grepl("*K_NPV_K_K", names(Priming_simulation$y))], mean)
NPV_medians <- sapply(Priming_simulation$y[, grepl("*K_NPV_K_K", names(Priming_simulation$y))], median)
NPV_cf_05 <- sapply(Priming_simulation$y[, grepl("*K_NPV_K_K", names(Priming_simulation$y))], function(x) quantile(x, 0.05))
NPV_cf_95 <- sapply(Priming_simulation$y[, grepl("*K_NPV_K_K", names(Priming_simulation$y))], function(x) quantile(x, 0.95))

# Convert to a data frame for display
NPV_Table_2 <-  data.frame("NPV" = names(NPV_means), cf_5 = round(NPV_cf_05,2), 
                        Median = round(NPV_medians,2), Mean = round(NPV_means, 2), cf_95 = round(NPV_cf_95,2))
names(NPV_means)[1]<-paste("NPV")
names(NPV_means)[2]<-paste("5% Quantile [€/m²]")
names(NPV_means)[3]<-paste("Median [€/m²]")
names(NPV_means)[4]<-paste("Mean (50%) [€/m²]")
names(NPV_means)[5]<-paste("95% Quantile [€/m²]")








  NPV_no_priming <- Priming_simulation_y %>% filter(name %in% c("NPV without priming")) %>%
    ggplot(aes(x = value, fill = name)) +
    geom_density(mapping = aes(y = after_stat(count)),alpha = 0.4, size = 0.7, fill = "#009E73") + # Create density plot with transparency and line size
    xlim( (min(Priming_simulation_y$value) - max(Priming_simulation_y$value)/5),
          (max(Priming_simulation_y$value) + max(Priming_simulation_y$value)/5))+
    geom_boxplot(aes(y = 0),alpha = 0.4, outlier.size = 0.2, outlier.alpha = 0.2, fill = "#009E73",
                 varwidth = TRUE, size = 0.3, width = 10) + # Reduce outlier size and make them half transparent
    theme_minimal() +
    annotate("text",x = -450,y = 45,
             label = 'atop(bold("No Priming"))',size = 4,parse = TRUE) +
    theme(
      axis.title = element_text(size = 15, colour = "black"),
      axis.text = element_text(size = 12,colour = "black"),
      axis.ticks.y = element_line(), 
      axis.ticks.x = element_line(), 
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      legend.position = "none") +
    labs( x = "Outcome distribution [€/m²]", y = "Frequency") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 0.5)


  NPV_heat_priming <- Priming_simulation_y %>% filter(name %in% c("NPV with heat priming")) %>%
    ggplot(aes(x = value, fill = name)) +
    geom_density(mapping = aes(y = after_stat(count)),alpha = 0.4, size = 0.7, fill = "tomato") + # Create density plot with transparency and line size
    xlim( (min(Priming_simulation_y$value) - max(Priming_simulation_y$value)/5),
          (max(Priming_simulation_y$value) + max(Priming_simulation_y$value)/5))+
    geom_boxplot(aes(y = 0),alpha = 0.4, outlier.size = 0.2, outlier.alpha = 0.2, fill = "tomato",
                 varwidth = TRUE, size = 0.3, width = 10) + # Reduce outlier size and make them half transparent
    theme_minimal() +
    annotate("text",x = -450,y = 40,
             label = 'atop(bold("Heat Priming"))',size = 4,parse = TRUE) +
    theme(
      axis.title = element_text(size = 15, colour = "black"),
      axis.text = element_text(size = 12,colour = "black"),
      axis.ticks.y = element_line(),
      axis.ticks.x = element_line(), 
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "none") +
    labs( x = "Outcome distribution [€/m²]", y = "Frequency") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 0.5)
  
  
  NPV_salt_priming <- Priming_simulation_y %>% filter(name %in% c("NPV with heat priming")) %>%
    ggplot(aes(x = value, fill = name)) +
    geom_density(mapping = aes(y = after_stat(count)),alpha = 0.4, size = 0.7, fill = "#56B4E9") + # Create density plot with transparency and line size
    xlim( (min(Priming_simulation_y$value) - max(Priming_simulation_y$value)/5),
          (max(Priming_simulation_y$value) + max(Priming_simulation_y$value)/5))+
    geom_boxplot(aes(y = 0),alpha = 0.4, outlier.size = 0.2, outlier.alpha = 0.2, fill = "#56B4E9",
                 varwidth = TRUE, size = 0.3, width = 10) + # Reduce outlier size and make them half transparent
    theme_minimal() +
    annotate("text",x = -450,y = 38,
             label = 'atop(bold("Salt Priming"))',size = 4,parse = TRUE) +
    theme(
      axis.title = element_text(size = 15, colour = "black"),
      axis.text = element_text(size = 12,colour = "black"),
      axis.ticks.y = element_line(), 
      axis.title.y = element_blank(),
      legend.position = "none") +
    labs( x = "Outcome distribution [€/m²]", y = "Frequency") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 0.5)
  
  
library(ggpubr)  
  merged_distribution_boxplots <-
    ggarrange( NPV_heat_priming,NPV_no_priming,NPV_salt_priming,
      ncol = 1,nrow = 3,align = "v")  
  

  
  NPV_comparison_heat_priming <- Priming_simulation_y %>% filter(name %in% c("NPV comparison between heat primed and non-primed")) %>%
    ggplot(aes(x = value, fill = name)) +
    geom_density(mapping = aes(y = after_stat(count)),alpha = 0.4, size = 0.7, fill = "tomato") + # Create density plot with transparency and line size
    xlim( (min(Priming_simulation_y$value) - max(Priming_simulation_y$value)/5),
          (max(Priming_simulation_y$value) + max(Priming_simulation_y$value)/5))+
    geom_boxplot(aes(y = 0),alpha = 0.4, outlier.size = 0.2, outlier.alpha = 0.2, fill = "tomato",
                 varwidth = TRUE, size = 0.3, width = 10) + # Reduce outlier size and make them half transparent
    theme_minimal() +
    theme(
      axis.title = element_text(size = 15, colour = "black"),
      axis.text = element_text(size = 12,colour = "black"),
      axis.ticks.y = element_line(),
      axis.ticks.x = element_line(), 
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "none") +
    labs( x = "Outcome distribution [€/m²]", y = "Frequency") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 0.5)
  
  
  NPV_comparison_salt_priming <- Priming_simulation_y %>% filter(name %in% c("NPV comparison between salt primed and non-primed")) %>%
    ggplot(aes(x = value, fill = name)) +
    geom_density(mapping = aes(y = after_stat(count)),alpha = 0.4, size = 0.7, fill = "#56B4E9") + # Create density plot with transparency and line size
    xlim( (min(Priming_simulation_y$value) - max(Priming_simulation_y$value)/5),
          (max(Priming_simulation_y$value) + max(Priming_simulation_y$value)/5))+
    geom_boxplot(aes(y = 0),alpha = 0.4, outlier.size = 0.2, outlier.alpha = 0.2, fill = "#56B4E9",
                 varwidth = TRUE, size = 0.3, width = 10) + # Reduce outlier size and make them half transparent
    theme_minimal()  +
    theme(
      axis.title = element_text(size = 15, colour = "black"),
      axis.text = element_text(size = 12,colour = "black"),
      axis.ticks.y = element_line(), 
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      legend.position = "none") +
    labs( x = "Outcome distribution [€/m²]", y = "Frequency") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 0.5)
  
  
  library(ggpubr)  
  merged_distribution_boxplots_comparison <-
    ggarrange( NPV_comparison_heat_priming,NPV_comparison_salt_priming,
               ncol = 1,nrow = 2,align = "v")  
  