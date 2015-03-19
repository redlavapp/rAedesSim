#' biocontainer
#'
#' @description \code{biocontainer} S3 class for container object to be used in rAedesSim model. 
#' 
#' @param type char string:   Container type.  Default Trap used in REDLAV project: volume 0.75 750 cm^3 lt, heigth 13 cm, Area 50 cm^2.Describe the tipology of cointainer considered. For example (i) cointaner (ii) manhole (iii) others. 
#' @param nrecipients numeric:  Numerosity of reply simulating the number of breeding sites BS. Default ( 1 lt 1000 cm^3).
#' @param container_shape numeric: If a surface container a numerical label indicate the shape . Parallelepiped plate (1)  Cilynder plate(2) Unspecified (3). Default is 3. 
#' @param capacity numeric:  Maximum capacity of water in trap/container. Default ( 1 lt 1000 cm^3). 
#' @param init_Vol numeric:  Unitarian initial volume of water in trap/container. Default ( 0.75 lt 750 ml).
#' @param frac_initVol numeric: Fraction of volume present in trap/location. Default is 0.
#' @param surface_area  numeric: water-air surface of container in squared centimeter (cmq).
#' @param underplate  logical: Presence of underplate. Default is FALSE.
#' @param frac_underplate numeric: Fraction of underplate engaged ( 0-1) Default is 0.
#' @param daily_evap_loss numeric: Volume of water losses daily due to evaporation. Default is 0.
#' @param frac_covering numeric: Fraction of container covering  ( 0-1) Default is 0.
#' @param lat numeric: Mean geographical latitude of container location. Default is 43.5. 
#' @param lon numeric: Mean geographical longitude of container location. Default is 11.27.
#' @param CRS   char string: Projection of coordinate in proj4 format.
#' @param elevation numeric:  Mean elevation  of container location. Default is 100.
#' @param sourcedata data.frame or ascii file:  Water temperature measurements. Fields required are dates ( YYYY-MM-DD) format, Daily mean of water tmedwater.
#' @param meteodata Rbiosim object:  with the data useful for model water calibration.
#' @param watermodel logical: Statistical model oject ( gam - lm - glm).Default is NULL.
#' @param watermodel_fit logical: Fit a statistical model oject ( gam - lm - glm).Default is FALSE.
#' @param model_type char string:  Statistical model class.
#' @param date_format char string: Format of the dates in raw data. Default is YMD. 
#' @param field_delimiter char string: field delimiter of file. Default is comma ",". 
#' @param timeseries xts R object: Timeseries of data 
#' @param ID  char string: ID of container's set. 
#' @return rAedesSim object: Biocontainer object.
#' @author  Istituto di Biometeorologia Firenze Italy  Alfonso crisci \email{a.crisci@@ibimet.cnr.it} ASL 2 LUCCA Marco Selmi \email{m.selmi@@usl2.toscana.it} 
#' @keywords  container
#'
#' 
#' 
#' @export


biocontainer<- function
                        (  type="Trap  REDLAV ITA",
			   nrecipients=50,
			   container_shape=3,
			   capacity=1000,
			   initVol=750,
			   frac_initVol=0.75,
			   surface_area=50,
			   underplate=FALSE,
			   frac_underplate=0,
			   frac_covering=0,
			   lat=43.5,
			   lon=11.27,
			   elevation=100,
	  		   CRS="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
			   sourcedata=NULL,
                           meteodata=NULL,
			   watermodel=NULL,
                           watermodel_fit=FALSE,
                           model_type="gam",
			   date_format="YMD",
                           field_delimiter=",", 
                           ID=c("NA") ,
			   site_name=c("NA")			   
			   ) {
                
				require(mgcv);
                                require(lubridate);
				require(zoo);
				require(xts);
				require(sp);
				require(rgdal);
				
				#################################################################################################################
				# location conversionre if coordinates are not geographic.
				
				epgs4386="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs";
				splocation=data.frame(lon=lon,lat=lat)
				coordinates(splocation) <- c('lon','lat')
				proj4string(splocation) <- CRS
				   
				#################################################################################################################
				newsp <-SpatialPointsDataFrame(splocation,data.frame(Elevation=elevation))
                    
				if (CRS != "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
				      {
					   newsp <- spTransform(splocation,CRS(epgs4386))
					   newsp <-SpatialPointsDataFrame(newsp,data.frame(Elevation=elevation))
                    
					  }
				     
				lon_geo=as.numeric(coordinates(newsp))[1]
				lat_geo=as.numeric(coordinates(newsp))[2]
				
				#################################################################################################################
				
				if ( (is.null(meteodata) || is.null(sourcedata) ) &&  watermodel_fit ==TRUE)
                                          { warning( "To fit a watermodel a meteodata object and a datasource of water temperatures are needed.")
					  }	
				
				#################################################################################################################
				  
                               				  
  
				#################################################################################################################
				# Instance an objects
				
				full_ts=NULL;
				
				#################################################################################################################
				# Check row index
				if  ( watermodel_fit == TRUE && is.null(watermodel)) 
				    {
				     if (is.data.frame(sourcedata) || is.matrix(sourcedata))
				      { filedata = as.data.frame(sourcedata)
				      }
                                       else 
				      {
                                       filedata = read.table(sourcedata, header=TRUE,sep=field_delimiter,na.strings="NA", dec=".", strip.white=TRUE)
				      }
                   
				     if ( !length(grep("tmedwater",names(filedata))) > 0 || !length( grep("dates",names(filedata))) > 0 )
                                       { stop( "Field dates and tmedwater in datasource are needed.Check header file and change names.")
				       }
				     if ( date_format == "DMY") {filedata$dates=dmy(filedata$dates)};
                                     if ( date_format == "MDY") {filedata$dates=mdy(filedata$dates)};
                   
			         	rownames(filedata)<-1:nrow(filedata)
				
				     filedata$daylength =day_length(as.Date(as.character(filedata$dates)),lon_geo,lat_geo)$daylength;
				   
				     full_ts = try(as.xts(zoo(data.frame(daylength=filedata$daylength,tmedwater=filedata$tmedwater),as.Date(as.character(filedata$dates)))))
				
				     if   (!exists("full_ts"))
				         {
						 stop( "Timeseries creation invalid! Check data and dates in data sources")
			                 }
				
				
				###################################################################################################################
				# Prepare 
			   
				    full_ts=merge.xts(full_ts,meteodata$timeseries)
				    full_ts_df=na.omit(as.data.frame(full_ts))
				   	rownames(full_ts_df) <- NULL;
				
				    if (model_type == "lin") 
				       {
				        watermodel=try(lm(tmedwater~daylength+tmed+tmin,data=full_ts_df))
					};
				
				    if (model_type == "gam") 
				       {
					    if ( nrow(full_ts_df) <45)
						    {
				                    stop( "\n\n Gam modeling require more data to avoid overfitting. \nTry with linear models (lin) in argument." )
						     }
					    
                                        watermodel = try(gam(tmedwater~s(daylength)+s(tmed)+s(tmin),data=full_ts_df))
				        }	  
										  
				}						  
                #################################################################################################################
				
				object <- list(type=type,
				               nrecipients = nrecipients,
			                       container_shape = container_shape,
					       surface_area = surface_area,
			                       underplate = underplate,
			                       frac_underplate = frac_underplate,
				               frac_covering = frac_covering,
				               initVol = initVol,
			                       frac_initVol = frac_initVol,
				               capacity = capacity,
				               pooled_volume_init = initVol*frac_initVol*nrecipients,
				               pooled_volume_current = initVol*frac_initVol*nrecipients,
				               lat = lat_geo,
			                       lon = lon_geo,
				               CRS = epgs4386,
			                       elevation = elevation,
				               meteodata = meteodata,
                                               watermodel = watermodel,
                                               model_type = model_type,
					       date_model = Sys.Date(),
	                                       timeseries = full_ts,
					       sp_obj = newsp,
				               ID = ID,
					       site_name = site_name
			);
				 
    attr(object,"type") <- "Describe the tipology of cointainer"
    attr(object,"nrecipients") <- "Breeding sites numerosity"
    attr(object,"container_shape") <- "Container's shape. Parallelepiped plate (1) Cilynder plate(2) Unspecified (3)."
    attr(object,"surface_area") <- "Surface at water-air interface of container (cm^2) "
    attr(object,"underplate") <- " Underplate presence"
    attr(object,"frac_underplate") <- "Underplate fraction occupated by plate"
    attr(object,"frac_covering") <- "Fraction of initial pooled volume(0 if open, 0.9 full covered)"
    attr(object,"initVol") <- "Fraction of initial unitarian volume"
    attr(object,"frac_initVol") <- "Fraction of initial pooled volume"
    attr(object,"capacity")<-"Maximum capacity of water in trap/container (cm^3)"
    attr(object,"pooled_volume_current")<-"Current Pooled volume of container's set (cm^3)"
    attr(object,"pooled_volume_init")<-"Pooled volume of container's set (cm^3)"
    attr(object,"lat")<-"latitude coordinates of  biocointainer"
    attr(object,"lon")<-"longitude coordinates of  biocointainer"
    attr(object,"CRS")<-"Projection used for the coordinate in proj4 string format"	
    attr(object,"elevation")<-"Mean elevation"
    attr(object,"metameteo")<-"Rbiomsim object to fit model"
    attr(object,"watermodel")<-"Model fitted from raw data of water temperature measurements"
    attr(object,"watermodel_fit")<-"If water model was fitted"
    attr(object,"model_type")<-"Statistical model class used for data fitting"
    attr(object,"date_model")<-"Model creation date" 
    attr(object,"timeseries")<-"Timeseries object obtained from data"
    attr(object,"sp_obj")<-"SpatialPointDataFrame" 
    attr(object,"ID")<-"ID label of container set"
    attr(object,"site_name")<-"Name of sites"
    class(object) <- "biocontainer"  
  return(object)
}
