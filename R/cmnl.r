#' cmnl
#'
#' @description Calculate a proxy of carryng capacity as ratio between alfa0 larval factor and breeding site (BS) number
#' 
#' @param  alfa0  Number Is the  competition parameter at larval stage.
#' @param nrecipients  Number Number of container defined in simulation.      
#' @return Numeric proxy of carrying capacity saying how many container are engaged in mosquito spreading.
#' @references Otero, M., Solari, H., Schweigmann, N., 2006. A stochastic population dynamic model for aedes aegypti: formulation and application to a city with temperate climate. Bull. Math. Biol. 68, 1945–974.
#'  
#' @author  Istituto di Biometeorologia Firenze Italy  Alfonso crisci \email{a.crisci@@ibimet.cnr.it} ASL 2 LUCCA Marco Selmi \email{m.selmi@@usl2.toscana.it} 
#' @keywords  Container
#'
#' 
#' 
#' @export
 
  
cmnl<-function(alfa0,nrecipients){
	
	cmnl=(alfa0/nrecipients)
	return(cmnl)
	}	
	