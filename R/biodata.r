#' biodata
#'
#' @description 
#' \code{biodata} Biodata is a rAedesSim S3 class to describe data collection from the field concerning.
#' @param parameter_definition char string: Name of site of observations.
#' @param units char string: Measure unity of observations.  
#' @param location char string: Name of site of observations. 
#' @param instrument  char string: Name of engine to provide observation
#' @param network  char string: If data is collected inside an observative network.
#' @param common_name char string: Data tipology - Observation - Sensor monitoring - Simulation
#' @param scientific_name char string: Data tipology - Observation - Sensor monitoring - Simulation
#' @param phenology char string: Data tipology - Observation - Sensor monitoring - Simulation
#' @param obs_standard char string: if data belong to a phenology Data standard class es SYNOP 
#' @param sourcedata Matrix or data.frame or ascii file of raw data.
#' @param data_provider char string: Institution / Private data manager.
#' @param data_maintaner char string: maintainer's  name or contact or its contact . 
#' @param data_licence char string: Licence of data.
#' @param sourcedata Matrix or data.frame or ascii file of raw data. Field required are dates and parameter.
#' @param field_delimiter char string: field delimiter of file. Default is comma ",". 				                   
#' @param lat numeric: latitude coordinates of  where data are collected.
#' @param lon numeric: longitude coordinates of  where data are collected.
#' @param CRS char string: Projection of coordinate in proj4 format.
#' @param elevation  numeric: Elevation of the Default is 40.
#' @param CRS char string: Projection of coordinate in proj4 format.
#' @param geonotes char string: Annotations in regard to the contest of observation.
#' @param ID char string: ID of monitoring. 
#' @param urban logical: Flag indicating if data belong to urban area. Automatic detection is done in according to UMZ Urban Morfological Zone EAA http://database.espon.eu
#' @param nasa_radiance numeric: Night radiance value. Is a proxy for urbanity.
#' @param feature_population: population density estimation.
#' @return Rbiosim object Biodata object 
#' @author   Istituto di Biometeorologia Firenze Alfonso crisci \email{a.crisci@@ibimet.cnr.it} ASL 2 LUCCA  Marco Selmi \email{m.selmi@@usl2.toscana.it} 
#' @keywords  biomodel 
#'
#' 
#' 
#' @export

biodata <- function(parameter_definition = "Egg counts in trap", 
                    unity_measure = "Integer count", 
					location, 
                    instrument = "Trappola REDLAV Ita",
                    network="",
                    common_name="Zanzara Tigre",
                    scientific_name="Aedes Albopictus",
                    phenology="Eggs Hatching",
                    obs_standard="",
					sourcedata,
                    data_provider="ASL Lucca",
                    data_maintaner="Marco Selmi - m.selmi@usl2.toscana.it",
                    data_licence="",
                    field_delimiter=",",
                    lat,
                    lon,
					elevation,
                    CRS="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
                    geonotes="",
                    ID,
                    urban,
                    nasa_radiance,
                    feature_population)
                  {	
                   require(lubridate);
				   require(zoo);
				   require(xts);
				   require(sp);
				   require(rgdal);
				   
				  #################################################################################################################
				   # location conversion
				   
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
				   	
					
					if ( is.null(sourcedata)) { stop( "To build meteodata valid object almost a data source is required.\n 
                                   					   Date of day in YYYY-MM-DD format (dates), 
													   parameter field.Checking Header is suggested to avoid variable error." 
													) 
											} 
				   
				   #################################################################################################################
				  
                   if (is.data.frame(sourcedata) || is.matrix(sourcedata))
				      { filedata=as.data.frame(sourcedata)
					  }
                   else 
				      {
                       filedata=read.table(sourcedata, header=TRUE, sep=field_delimiter,na.strings="NA", dec=".", strip.white=TRUE)
				       }
					
				    ts_zoo=NULL;
				    ts_zoo=try(as.xts(zoo(filedata$parameter,as.Date(as.character(filedata$dates)))));
				   
				   
				   
				    if ( is.null(ts_zoo))
				       { warning( "Timeseries creation was invalid! Check data and dates in data sources")
				       }
				  
   				  #################################################################################################################
				  						 
				   object <- list(parameter_definition=parameter_definition,
                                  unity_measure = unity_measure,
                                  location=location, 
                                  instrument=instrument,
                                  network=network,
                                  common_name=common_name,
                                  scientific_name=scientific_name,
                                  phenology=phenology,
                                  obs_standard=obs_standard,
                                  data_provider=data_provider,
                                  data_maintaner=data_maintaner,
                                  data_licence=data_licence,      
                                  lat=lat_geo,
                                  lon=lon_geo,
								  elevation=elevation,
                                  CRS=epgs4386,
                                  geonotes=geonotes,
                                  ID=ID,
                                  urban=urban,
                                  nasa_radiance=nasa_radiance,
                                  feature_population=feature_population,
                                  ts_data=ts_zoo,
						          sp_obj=newsp
				 );
 
                 attr(object,"parameter_definition")<-"Name of site of observations" 
                 attr(object,"unity_measure")<-"Measure/Observation units" 
				 attr(object,"location")<-"Name of site of observations" 
                 attr(object,"instrument")<-"Name of engine to provide observation"
                 attr(object,"network")<-"If data is collected inside an observative network"
                 attr(object,"common_name")<-"Data tipology - Observation - Sensor monitoring - Simulation"
                 attr(object,"scientific_name")<-"Data tipology - Observation - Sensor monitoring - Simulation"
                 attr(object,"phenology")<-"Data tipology - Observation - Sensor monitoring - Simulation"
                 attr(object,"obs_standard")<-"If data belong to a phenology data standard class" 
                 attr(object,"data_provider")<-"Institution / Private data manager"
                 attr(object,"data_maintaner")<-"maintainer's  name or contact or its contact"
                 attr(object,"data_licence")<-"Licence of data"                   
                 attr(object,"lat")<-"latitude coordinates of  where data are collected"
                 attr(object,"lon")<-"longitude coordinates of  where data are collected"
                 attr(object,"CRS")<-"Projection of coordinate in proj4 format"
                 attr(object,"elevation")<-"Elevation (m)"	
                 attr(object,"geonotes ")<-"Annotations in regard to the contest of observation"
                 attr(object,"ID")<-"ID of monitoring/data collection"
                 attr(object,"urban")<-"Flag indicating if data belong to urban area. Automatic detection is done in according to UMZ Urban Morfological Zone EAA http://database.espon.eu"
                 attr(object,"nasa_radiance ")<-"NASA Night radiance value. Is a proxy for urbanity"
                 attr(object,"feature_population")<-"population density estimation"
                 attr(object,"ts_data")<-"Time series object where data area"
	             attr(object,"sp_obj")<-"SpazialPointDataFrame"   
				 class(object) <-"biodata"
                 return(object)
}  
					  
