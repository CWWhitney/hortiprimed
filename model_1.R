library(decisionSupport)
g
yield_reduction<-function(x, varnames)
{
  risk1<-chance_event(risk_1,1,0,n=n_years)
  risk2<-chance_event(risk_2,1,0,n=n_years)
  risk3<-chance_event(risk_3,1,0,n=n_years)
  
  risk1_dmg<-vv(risk_1_damage,var_CV,n_years)
  risk2_dmg<-vv(risk_2_damage,var_CV,n_years)
  risk3_dmg<-vv(risk_3_damage,var_CV,n_years)
  
  priming_yield_red<-vv(priming_yield_reduction,var_CV,n_years)
  
  priming_risk_1_red<-vv(priming_risk_1_reduction,var_CV,n_years)
  priming_risk_2_red<-vv(priming_risk_2_reduction,var_CV,n_years)
  priming_risk_3_red<-vv(priming_risk_3_reduction,var_CV,n_years)
  
  ppm_risk_1_red<-vv(ppm_risk_1_reduction,var_CV,n_years)
  ppm_risk_2_red<-vv(ppm_risk_2_reduction,var_CV,n_years)
  ppm_risk_3_red<-vv(ppm_risk_3_reduction,var_CV,n_years)
  
  yield_100<-vv(yield, var_CV, n_years)
  yield_priming<-yield_100*priming_yield_red/100
  
  if (risk1){
    yield<-yield_100*risk1_dmg/100
    yield_p<-yield_100*(risk1_dmg-priming_risk_1_red)/100
    yield_ppm<-yield_100*(risk1_dmg-ppm_risk_1_red)/100
  }
  if (risk2){
    yield<-yield_100*risk2_dmg/100
    yield_p<-yield_100*(risk2_dmg-priming_risk_1_red)/100
    yield_ppm<-yield_100*(risk2_dmg-ppm_risk_1_red)/100
  }
  

}
production<-function(x, varnames){
  # Initialize variables
  total_yield = vv(yield, var_CV, n_years) # Total yield in pounds
  disease_loss = 0.05 # Disease causes a 5% loss
  pest_loss = 0.03 # Pest infestation causes a 3% loss
  nutrient_loss = 0.02 # Nutrient deficiencies cause a 2% loss
  sunscald_loss = 0.01 # Sunscald causes a 1% loss
  blossom_end_loss = 0.02 # Blossom end rot causes a 2% loss
  watering_loss = 0.01 # Overwatering or under watering causes a 1% loss
  heatwave_loss = 0.05 # Heatwave causes a 5% loss in some cases
  priming_yield_loss = 0.1 # Priming decreases the general yield by 10% 
  priming_loss_reduction = 0.05 #Priming reduces all losses by 50%
  
  # Determine if a heatwave occurs
  heatwave = sample(c(TRUE, FALSE), 1)
  
  # Calculate total yield loss
  total_loss = disease_loss + pest_loss + nutrient_loss + sunscald_loss + blossom_end_loss + watering_loss
  
  # Add heatwave loss if it occurs
  if (heatwave) {
    total_loss = total_loss + heatwave_loss
  }
  
  for (priming in c(TRUE, FALSE)){
  # Reduce all losses by 10% if priming is activated
    if (priming) {
      total_loss_priming = total_loss - priming_loss_reduction
      total_yield_priming = total_yield - (total_yield * priming_yield_loss)
      # Calculate final yield
      final_yield_priming = total_yield_priming - (total_yield_priming * total_loss_priming)
    } else {
      # Calculate final yield
      final_yield = total_yield - (total_yield * total_loss)}
  }

  

return(list(final_yield, final_yield_priming))
}

decisionSupport("input.csv",
                outputPath='results',
                welfareFunction=production,
                numberOfModelRuns=10000,
                functionSyntax="plainNames")

make_variables<-function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}

make_variables(estimate_read_csv("input.csv"))



