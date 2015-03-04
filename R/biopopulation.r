#' biopopulation
#'
#' @description S3 class to describe biomodelday object to be used in rAedesSim model. 
#' 
#' @param eggs Numeric Eggs Count.  
#' @param larvae Numeric Larvae Count.  
#' @param pupae Numeric Pupae Count. 
#' @param adults Numeric Mosquito Female Actual Count. 
#' @param eggs_diap Numeric Diapausant Eggs Count.
#' @param ID String Population. 
#' @return S3 object biopopulation object.
#' Istituto di Biometeorologia Firenze Italy 
#' ASL 2 LUCCA 
#' @author  Alfonso crisci \email{a.crisci@@ibimet.cnr.it} Marco Selmi \email{m.selmi@@usl2.toscana.it} 
#' @keywords  container
#'
#' 
#' 
#' @export

biopopulation<- function( eggs=10000,
			              larvae=0,	
                          pupae=0,
			              adults=0,
			              eggs_diap=10,
			              ID="NA"
			   ) {
                 object <- list(
			     eggs=eggs,
			     larvae=larvae,	
                 pupae=pupae,
			     adults=adults,
			     eggs_diap=eggs_diap,
	             ID=ID);
				 
  class(object) <- "biopopulation"
  attr(object,"eggs") <- "Eggs Count"
  attr(object,"larvae") <- "Larvae Count"
  attr(object,"pupae") <- "Pupae Count"
  attr(object,"adults") <- "Female Actual Count"
  attr(object,"eggs_diap") <- "Diapausant Eggs Count"
  attr(object,"ID")<-"ID population"
  class(object) <- "biopopulation"
  return(object)
}