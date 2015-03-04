#' viewwhere
#'
#' @description Visualize the location of object in a leaflet framework
#' 
#' @param object  rAedesSim object with location coordinate.    
#' @return Return leaflet map as view.
#' @references  The function is a wrapper for leaflet R package RStudio \url{"http://rstudio.github.io/leaflet"}
#'
#' @author  Istituto di Biometeorologia Firenze Italy  Alfonso crisci \email{a.crisci@@ibimet.cnr.it} ASL 2 LUCCA Marco Selmi \email{m.selmi@@usl2.toscana.it} 
#' @keywords  View location
#' 
#' 
#' @export

viewwhere<-function(object,zoom = 17){
          require(leaflet)
		  
          if (is.null(object$lon) && is.null(object$lat))  
		     { stop("Object  must be have coordinates lat and lon slot." )};
            m = leaflet() %>% addTiles()
            m = m %>% setView(object$lon, object$lat, zoom = zoom)
            m %>% addPopups(object$lon,object$lat, paste0('The location of the oject is here!'))
         return(m)

}
