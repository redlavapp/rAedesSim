#' container_capacity
#'
#' @description Calculate the volume of cointaner taking into account the underplate precence.
#' 
#' @param base_area  Basal area of container ( square meters).
#' @param heigth  Heigth of container ( meters) .      
#' @param underplate Presence of underplate Default underplate=FALSE 
#' @param fraction_underplate Fraction of undeplate engaged 
#' @return Volume capacity of container in liters
#'
#' @author  Istituto di Biometeorologia Firenze Italy  Alfonso crisci \email{a.crisci@@ibimet.cnr.it} ASL 2 LUCCA Marco Selmi \email{m.selmi@@usl2.toscana.it} 
#' @keywords  Container
#'
#' 
#' 
#' @export
 
  
container_capacity<-function(base_area,heigth,underplate=FALSE,fraction_underplate)
{
 capacity = base_area * heigth * 0.001;  
 if (underplate == TRUE) { capacity= capacity * ( 1 - fraction_underplate) }
 return(capacity);
}