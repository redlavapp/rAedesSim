#' develop_rate
#'
#' @description Calculate developmental rates for Aedes in function to the air temperature. 
#' 
#' @param temp   Numeric  Temperature in Celsius degree.
#' @param DR25   Numeric Developmental rate standard at 25 Celsius.     
#' @param EA  Numeric Enthalpy of activation for determinate stage. 
#' @param EI   Numeric Enthalpy of inactivation for determinate stage.     
#' @param TI  Numeric Temperature of inactivation for determinate stage.     
#' @param t25  Numeric Temperature of inactivation for determinate stage.     
#' @return Numeric Develompental rate for 
#' @references  Otero, M., Solari, H., Schweigmann, N., 2006. A stochastic population dynamic model for aedes aegypti: formulation and application to a city with temperate climate. Bull. Math. Biol. 68, 1945�974.
#' @author  Istituto di Biometeorologia Firenze Italy  Alfonso crisci \email{a.crisci@@ibimet.cnr.it} ASL 2 LUCCA Marco Selmi \email{m.selmi@@usl2.toscana.it} 
#' @keywords  develpmentantal rate
#'
#' 
#' 
#' @export
 


develop_rate<-function(temp,DR25,EA,EI,TI,t25=298)
			           {
                        temp = temp + 273.5;
	                    DR = ((DR25 * (temp/t25) * exp((EA/R) * (1/t25 - 1/temp))) / (1 + exp((EI/R) * (1/TI - 1/temp))));
           	            return(DR);
                        } 
						