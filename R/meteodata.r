#' meteodata
#'
#' @description 
#' \code{meteodata} Create meteodata object for rAedesSim.
#'
#' @param station_name char string: Name of station 
#' @param network  char string: Observative or set of data
#' @param data_type char string: Data tipology - Observation - Sensor monitoring - Simulation
#' @param standard char string: Data standard class es SYNOP 
#' @param data_provider char string: Institution / Private data manager.
#' @param data_maintaner char string: maintainer's  name or contact or its contact . 
#' @param data_licence char string: Licence of data.    
#' @param lat numeric: latitude coordinates of  where data are collected.
#' @param lon numeric: longitude coordinates of  where data are collected.
#' @param CRS char string: Projection of coordinate in proj4 format.
#' @param elevation  numeric: Elevation of the Default is 40.
#' @param timeformat  numeric: Native period of aggregation. Default is "daily".
#' @param sourcedata Matrix or data.frame or ascii file of raw data.
#' @param field_delimiter char string: field delimiter of file. Default is comma ",".
#' @param date_format char string: Format of the dates in raw data. Default is YMD. 
#' @param timeseries xts R object Timeseries of data 
#' @seealso \code{\link{},\link{biometeo}}
#' @author  Istituto di Biometeorologia Firenze Italy  Alfonso crisci \email{a.crisci@@ibimet.cnr.it} ASL 2 LUCCA Marco Selmi \email{m.selmi@@usl2.toscana.it} 
#' @keywords  metorological data 
#' 
#' 
#' @export


meteodata<-function(station_name="Pisa San Giusto",
		    network="Aeronautica Militare",
		    data_type="Simulation",
		    standard="SYREP",
		    data_provider="IBIMET CNR",	
                    data_maintainer="",
 		    data_licence="",
		    date_format="YMD",
		    lat=43.0,	
                    lon=11.0,
		    elevation=40,
		    timeformat="daily",
		    sourcedata=NULL,
		    field_delimiter=",",
		    timeseries=NULL,
		    CRS="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
		    )
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
				    
					
				    if ( is.null(sourcedata)) { stop( "To build meteodata valid object almost a data source is required as : data.frame / ascii  formatted file.\n
                                                     Format must be daily with fields : Date of day better in YYYY-MM-DD format (dates /data), Mean temperature (tmed),\n
													 Maximum temperature (tmax), Minimum temperature (tmin), Mean relative humidity (urel/rhum), Cumulated rainfall ( prec).\n
													 Header is suggested to avoid variable error. Mean temperature and minimum temperature fileds are required."
													 ) 
											} 
				   
				   #################################################################################################################
				  
                                    if (is.data.frame(sourcedata) || is.matrix(sourcedata))
				      { filemeteo=as.data.frame(sourcedata)
				      }
                                      else 
				      {
                                       filemeteo=read.table(sourcedata, header=TRUE, sep=field_delimiter,na.strings="NA", dec=".", strip.white=TRUE)
				      }
                   
				   #################################################################################################################
				   
				   if ( length(grep("tmed",names(filemeteo)))==0 || length( grep("dates",names(filemeteo)))== 0 || length( grep("rhum",names(filemeteo)))== 0)
                                          { stop( "Field dates/data, tmed and rhum/urel in datasource are needed.")
										  }				  

				   
				   if ( length(grep("anno",names(filemeteo)))>0 && length(grep("mese",names(filemeteo)))>0 && length(grep("mese",names(filemeteo))>0 ))
                                          { names(filemeteo)<-gsub("anno","year",names(filemeteo))
										    names(filemeteo)<-gsub("mese","month",names(filemeteo))
											names(filemeteo)<-gsub("giorno","day",names(filemeteo))
											names(filemeteo)<-gsub("urel","rhum",names(filemeteo))
										    names(filemeteo)<-gsub("data","dates",names(filemeteo))
										  
										  }	
				   
				   if ( !length(grep("year",names(filemeteo)))>0 && !length(grep("month",names(filemeteo)))>0 && length(grep("day",names(filemeteo))>0 ))
                                          { filemeteo$year<-year(filemeteo$dates)
										    filemeteo$month<-month(filemeteo$dates)
										    filemeteo$day<-day(filemeteo$dates)
										  }  
				   #################################################################################################################
				   
				   filemeteo$dates=ymd(filemeteo$dates,tz =Sys.timezone());
                                   if ( date_format == "DMY") {filemeteo$dates=dmy(filemeteo$dates,tz =Sys.timezone())};
                                   if ( date_format == "MDY") {filemeteo$dates=mdy(filemeteo$dates,tz =Sys.timezone())};
                   
				   #################################################################################################################
				   rownames(filemeteo)<-1:nrow(filemeteo)
				
				        
				   period=as.numeric(max(as.Date(filemeteo$dates))-min(as.Date(filemeteo$dates)))
				   
				   
				   length_data_ini=nrow(filemeteo)
				   row_na=nrow(filemeteo[!complete.cases(filemeteo),])
				   perc_missing=(row_na/nrow(filemeteo))*100
				   
				   if ( period > length_data_ini-1) { 
				                          fill_dates=data.frame(dates=seq(min(as.Date(filemeteo$dates)),max(as.Date(filemeteo$dates)),1));
				                          filemeteo=merge(fill_dates,filemeteo,by=c("dates"),all = T)
				                          }
				   
				   filemeteo$daylength = day_length(as.Date(filemeteo$dates),lon_geo,lat_geo)$daylength;
				   
				   #################################################################################################################
				  
				   if ( period < length_data_ini-1)
				                             { stop( "Data source is not correcly time indexed! Suspect date duplicates.")
										     }				  

				   
				   continuity=ifelse(period==length_data_ini-1,TRUE,FALSE)
				   
				   #################################################################################################################
				  
				   variables=names(filemeteo)[grep("tmed|tmax|tmin|rhum|prec",names(filemeteo))]
				   if ( is.null(timeseries) )
				   {
				    
				   ts_zoo=try(as.xts(zoo(filemeteo[variables],as.Date(as.character(filemeteo$dates)))))
				   
				   }
				   if ( !exists("ts_zoo"))
				   {
				    warning( "Timeseries creation invalid! Check data and dates in data sources")
			           }
				  
   				  #################################################################################################################
				  						 
				   object <- list(station_name=station_name,
			                      network=network,
			                      data_type=data_type, 
			                      standard=standard,
			                      data_provider=data_provider,
                               	              data_maintainer=data_maintainer,
					      data_licence=data_licence,
			                      lat=lat_geo,	
                                              lon=lon_geo,
					      CRS=epgs4386,
			                      elevation=elevation,
			                      timeformat=timeformat,
					      tmed=filemeteo$tmed,
					      tmax=filemeteo$tmax,
					      tmin=filemeteo$tmin,
					      urel=filemeteo$rhum,
					      prec=filemeteo$prec,
					      dates=filemeteo$dates,
					      length_data_ini=length_data_ini,
					      ndays=length(filemeteo$dates),
					      daylenght=filemeteo$daylength,
					      continuity=continuity,
					      perc_missing_data=perc_missing,
					      timeseries=ts_zoo,
					     sp_obj=newsp
				 );
 
                 attr(object,"station_name") <- "Name of station"
                 attr(object,"network") <- "Network of station"
                 attr(object,"data_type")<-"Station Type"
                 attr(object,"standard")<-"Data class or relative reference standard"
                 attr(object,"data_provider")<-"Istitution or Private data manager"
                 attr(object,"data_maintainer")<-"Name or contact of data maintainer"
		 attr(object,"data_licence")<-"Licence of data"
                 attr(object,"lat")<-"Latitude in decimal degrees. Datum WGS 84"
                 attr(object,"lon")<-"Longitude in decimal degrees . Datum WGS 84"	
                 attr(object,"CRS")<-"Projection used for the coordinate in proj4 string format"	
                 attr(object,"elevation")<-"Elevation (m)"	
                 attr(object,"timeformat")<-"Period of aggregation"
                 attr(object,"tmed")<-"Mean daily temperature"
                 attr(object,"tmax")<-"Maximum daily temperature"
                 attr(object,"tmin")<-"Minimum daily temperature"
                 attr(object,"urel")<-"Relative humidity daily average"
                 attr(object,"prec")<-"Rainfall cumulated in a day"
                 attr(object,"dates")<-"Dates of meteorological data matrix"
                 attr(object,"length_data_ini")<-"Initial data length of raw data"
                 attr(object,"ndays")<-"Number of days"
                 attr(object,"daylenght")<-"Day length for each date"
                 attr(object,"continuity")<-"If the temporal continuity of raw data is detected"
                 attr(object,"perc_missing_data")<-"Percentage of missing values"
                 attr(object,"timeseries")<-"Data timeseries as R xts object"
                 attr(object,"sp_obj")<-"SpatialPointDataFrame"  
                 class(object) <-"meteodata"
               return(object)
}
