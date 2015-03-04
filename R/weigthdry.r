#' weigthdry
#'
#' @description Calculate a exponential weigth in function to the amunt of dry spell.
#' 
#' @param drylength numeric vector: length of dry spell as number of consecutive dry days.    
#' @param tresh nuemric: threshold in days 


#' @return Numerical vector of cumulate sequence


#' @seealso \code{\link{},\link{drydaycons}}
 
#' @author  Istituto di Biometeorologia Firenze Italy  Alfonso crisci \email{a.crisci@@ibimet.cnr.it} ASL 2 LUCCA Marco Selmi \email{m.selmi@@usl2.toscana.it} 
#' @keywords  Drought severity
#'
#' 
#' 
#' @export
						   
weigthdry<-function(drylength,tresh=5) {
                                   res=NULL;
                                   res=round(1-exp(drylength/S-drylength),digits=2) 
								   return(res)
						            }