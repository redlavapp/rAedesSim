#' pvapsat
#'
#' Saturation pressure of water in air as function of temperature (°C)
#'
#' The saturation pressure of water is the pressure at or above which water
#' would condense from from the air. It is a function of temperature only.
#'
#'
#' @param t numeric: air temperature in Celsius
#'
#' @return numeric: saturation pressure of water (hPa)
#' @export
#' 

pvapsat<-function(tair=24)
{
   es=6.1078*10^((7.5*tair)/(237.7+tair));
  
  return(es);
}

#' dewpoint
#'
#' @description Saturation pressure of water in air as function of temperature (°C)
#'
#' The saturation pressure of water is the pressure at or above which water
#' would condense from from the air. It is a function of temperature only.
#'
#'
#'
#' @param t numeric: air temperature in Celsius
#'
#' @return numeric: Return dewpoint in Celsius
#' @export
#' 

dewpoint<-function(tair=24,urel=50) {
	dpt=(17.271*(tair+273))/(237.7+(tair+273))-log(urel/100)
	return (dpt);
}

#' degsat
#'
#' @description Air degree of saturation. Is a meausere of air dryness
#'
#'
#'
#'
#' @param tair numeric: air temperature in Celsius
#' @param urel numeric: Air Relative umidity (\%)
#'
#' @return numeric: saturation deficit pressure in millibar 10-30.
#' @export
#' 


degsat<-function(tair=24,rh=50) {


 
   mu=6.1078*10^((7.5*tair)/(237.7+tair))*(1-urel/100);
   mu=ifelse(mu>30,30,mu=mu)
   return (mu);
}



#' delta_vapor_density
#'
#' @description Saturation pressure of water in air as function of temperature (°C)
#'
#'
#'
#'
#' @param tair numeric: air temperature in Celsius
#' @param twater numeric: water temperature in Celsius
#' @param urel numeric: realtive humidity
#' @return delta_vap_interface numeric: gradient of vapour pressure at surface air-water
#' @export
#' 

delta_vapor_density<-function(tair=24,twater=21,urel=60)
{  
   R=461.48; #J K-l Kg-l.
   g_standard=9.80665;
   deltad=((100*pvapsat(twater))/(R*(twater+273)))-(100*(urel/100)*pvapsat(tair))/(R*(tair+273));
   return(deltad)
}

#' delta_vapor_pressure
#'
#' @description  Difference water vapour pressure between cointaner water surface an free air. 
#'
#'
#' @param tair numeric: Air temperature in Celsius
#' @param twater numeric: Water temperature in Celsius
#' @param urel numeric: Air Relative umidity (\%)
#'
#' @return vap_pressure numeric: Vapour pressure gradient
#' @export
#' 

delta_vap_pressure<-function(tair=24,twater=21,urel=60)
{   
   delta_p=((100*pvapsat(twater))-(100*(urel/100)*pvapsat(tair)))

   return(delta_p)
}


#' delta_air_density
#'
#'  @description  Difference air density between cointaner water surface an free air. 
#'
#'
#' @param tair numeric: Air temperature in Celsius
#' @param twater numeric: Water temperature in Celsius
#' @param urel numeric: Air Relative umidity (\%)
#'
#'
#' @return air_density numeric: Air density gradient
#' @export
#' 

delta_air_density<-function(tair=24,twater=21,rh=60)
{   
    R=461.48; #J K-l Kg-l.
	g_standard=9.80665
    delta=((100*pvapsat(twater))/(R*(twater+273)))+(100*(rh/100)*pvapsat(tair))/(R*(tair+273))/2;
    return(delta)
}



#' mass_diffusion_vap
#'
#'  @description  Diffusivity or diffusion coefficient of vapor in function to  temperature for Air-Water Vapor Mixtures  Bolz and Tuve (1976). 
#'
#'
#' @param tair numeric: Air temperature in Celsius
#'
#'
#' @return m_vap numeric: Diffusion coefficient of vapor (mq/s).
#' @export
#' 

mass_diffusion<-function(tair=24)
{  
  diff_t=(2.07*(10**-6))+4.479*(10**-8)*(tair+273)+1.656*(10**-10)*(tair+273)^2;
  return(diff_t);
}


#' mass_transport_coef
#'
#' @description  Mass transfer coefficient in function to air temperature in forced convection  (v_air > 0.1).
#'
#'
#' @param tair numeric: Air temperature in Celsius
#' @param twater numeric: Water temperature in Celsius
#' @param urel numeric: Air Relative umidity (\%)
#' @param v_air numeric: Velocity of air movement (m/sec)
#'
#' @return numeric: Mass transfer coefficient for water. Only vapour pressure gradient is considered.
#' @export
#' 

mass_transport_coef<-function(tair=23,twater=21,urel=50,v_air=0.5)

{  ktrasp_vap=3.4*(10**-8);
   k_trasp=ktrasp_vap*delta_vap_pressure(tair,twater,urel);
   return(k_trasp);
}


#' evaporation_params
#'
#' @description Give a set of parameters  to assess the evaporative losses for a generic cointainer under forced convetion (v_air > 0.1).
#'
#'
#' @param tair numeric: Air temperature in Celsius
#' @param twater numeric: Water temperature in Celsius
#' @param urel numeric: Air Relative umidity (\%)
#' @param v_air numeric: Velocity of air movement (m/sec)
#' @param L numeric: Dimension caracterize the cointainer. Generally the diameter (m)

#' @return evap_pars numeric list:  Element mevday give a the mass of  evaporate (mg) for day for a cointanertaking into account dimension.
#' @export
#' 

evaporation_params<-function(tair=24,twater=21,urel=60,v_air=0.5,L=0.1,A=NULL)
{ 

             ####################################################################
             # Build  a list

	     A = ifelse(is.null(A),3.1415*(L/2)**2,A) # Area of water container
             mv = 18.016; #kg/kmole
             d_aria_20 = 0.8216; #kg/m3
             cp_aria = 1.005; #KJ/kg K° # Specific heat of air
             visc_aria = 1.583*(10**-5);# air viscosity
             g_standard = 9.80665;
             dilataz_aria = 1/(tair+273.16);
             Rzero=8314/mv; #J K^-l kg^-l.
             entalp_vap = 40.8; #kJ/mol
			 
             ####################################################################
             # Build  a list
			 
             res = list()
             res$reynolds = ( v_air * L )/ visc_aria;
             res$raylegh = g_standard * ((delta_air_density(tair,twater,urel) * (L**3))/(delta_vap_pressure(tair,twater,urel) * visc_aria * mass_diffusion(tair)));
             res$schmidt = 0.6;
             res$sherwood = 0.230*((res$schmidt)**(1/3)) * res$raylegh**(0.321);
             res$hm = (res$sherwood * mass_diffusion(tair)) / L;
             res$mev = res$hm*A*(delta_vap_pressure(tair,twater,urel)/(461 * (twater + 273 ))); # kg/sec
             res$mevday = res$mev * 3600 * 24 * 1000; # mg day integrate from a seconds
	     ####################################################################
			 
             return(res)
}

 


