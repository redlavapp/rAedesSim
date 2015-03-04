#' diapause_rate
#'
#' @description Calculate the percentage of emergency of diapausant eggs in function to legth of day.
#' 
#' @param dates   Date in  YYYY-MM-DD format    
#' @param lon Numerical Geographical Longitude of site. Default is 11.10.
#' @param lat Numeric Geographical Latitude of site. Default is 43.77.
#' @return Rate of egg exclosion and inversaly the diapausant one.
#' @seealso \code{\link{},\link{trans_rate}}
#' @author  Istituto di Biometeorologia Firenze Italy  Alfonso crisci \email{a.crisci@@ibimet.cnr.it} ASL 2 LUCCA Marco Selmi \email{m.selmi@@usl2.toscana.it} 
#' @keywords  diapause
#' 
#' @export

diapause_rate<-function(daylength) {
                         diap=pnorm(27.61*log10(daylength)-29.7);
						 diap=ifelse(diap>0.98,1,diap);
						 return(diap)
                       }