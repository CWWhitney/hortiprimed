

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
  
  for (priming1 in c(FALSE,TRUE)){
    for (priming2 in c(FALSE,TRUE)){
      for (priming3 in c(FALSE,TRUE))
      {
        
      }}}
}