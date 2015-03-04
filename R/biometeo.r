#' biometeo
#'
#' @description S3 
#' \code{biometeo} class to describe  object to be used in Rbiosim model. 
#' 
#' @param meteodata   Rbiosim object.
#' @param biocontainer  Rbiosim object .
#' @param deltatmax numeric: Mean Error Bias  considered for maximum air temperature. Default is 0.
#' @param deltatmin numeric: Mean Error Bias  considered for minimum air temperature. Default is 0.
#' @param deltatmed numeric: Mean Error Bias  considered for mean air temperature. Default is 0.
#' @param tresh_rain_dry numeric: Rain threshold for effective precipitation. Default is 4.
#' @param weigth_k Weighting Paraemter to take into account dry spell impact See also \code{\link{weigthdry}}. Default is 7.
#' @return  Return a biometeo object.
#' @author  Istituto di Biometeorologia Firenze Italy  Alfonso crisci \email{a.crisci@@ibimet.cnr.it} ASL 2 LUCCA Marco Selmi \email{m.selmi@@usl2.toscana.it} 
#' @keywords  container
#'
#' 
#' 
#' @export


biometeo <- function(meteo,
                    i_biocontainer,
					deltatmax=0,
					deltatmin=0,
					deltatmed=0,
					tresh_rain=4,
					weigth_k=5
					)
                    {
						    
					require(xts);
					require(lubridate);
					
                    if (class(meteo)!="meteodata") { stop(" meteo argument must be an object meteodata" ) } 
					if (class(i_biocontainer)!="biocontainer") { stop("Object biocontainer argument must be an object biocontainer" ) } 
							
					####################################################################################################################
                              
					tmax=meteo$tmax+deltatmax
					tmin=meteo$tmin+deltatmin
					tmed=meteo$tmed+deltatmed
					
					dates=as.Date(as.character(meteo$dates));
					day_len=day_length(dates,as.numeric(i_biocontainer$lon),as.numeric(i_biocontainer$lat))$daylength;		
					#####################################################################################################################
                    # Water temperature estimation
							
					new_data=data.frame(daylength=day_len,tmed=tmed,tmin=tmin)
							
					w_tmed=predict(i_biocontainer$watermodel,newdata=new_data)
					id.na<-which(is.na(w_tmed)) # check missing values
					w_tmed[id.na]<-tmed[id.na] # replace missing values with mean temperatures 
							
					#######################################################################################################################
                    # Water evaporation temperature estimation
							
					tot_evap=NA;
					weight_dry=NA;
					prevdrydays=NA;
							
					if (!is.null(meteo$urel) || length(which(is.na(as.numeric(mete$urel)))) == length(as.numeric(meteo$urel)))
					    {
						evap<-evaporation_params(tmed, w_tmed, meteo$urel , A=i_biocontainer$base_area);
						tot_evap=evap$mevday * i_biocontainer$nrecipients;
						}
										 
					#######################################################################################################################
                    # Daylength and related parameter estimation ( diapause
							
			
					diapause_emergency=sapply(day_len,diapause_rate);
							
					if (length(which(is.na(meteo$prec))) == length(meteo$prec) ||  meteo$perc_missing_data > 20) 
					   {
						weight_dry=weigthrain(drydaycons(meteo$prec),S=weigth_k);
						prevdrydays=drydaycons(meteo$prec,S=tresh_rain)
						}
					
                    ts_zoo=as.xts(zoo(data.frame(tmedwater=w_tmed,tmed=tmed,diapause=diapause_emergency,lengthday=day_len,cdrydays=prevdrydays),dates))				
					
					#######################################################################################################################
							
					object <- list(         tmed_est=tmed,
							                twater_est=w_tmed,
							              	prec=meteo$prec,
											rhum=meteo$urel,
											daylength=day_len,
											ndays=meteo$ndays,
											diapause_emergency=diapause_emergency,
											tot_evap=tot_evap,
											tresh_rain_dry=tresh_rain,
											prevdrydays=prevdrydays,						   
                                            weight_k=weigth_k,
											weight_dry=weight_dry,
											timeformat=meteo$timeformat,
											dates=dates,
											timeseries=ts_zoo
			                                )
                    attr(object,"tmed_est") <- "Daily mean air temperature estimated"
					attr(object,"twater_est") <- "Daily mean water temperature estimated"
					attr(object,"prec") <- "Daily precipitation"
                    attr(object,"rhum") <- "Daily precipitation"
                    attr(object,"tot_evap") <- "Mass of evaporative losses for day in mg"
					attr(object,"nrecipients") <- "Number of recipient as proxy of mosquito breeding sites BS"
					attr(object,"tresh_rain_dry") <- "Rain threshold for effective precipitation"
					attr(object,"prevdrydays") <- "Consecutive dry days"
					attr(object,"weight_k") <- "Weighting Paraemter to take into account dryness impact"
					attr(object,"daylength") <- "Length of days"
					attr(object,"diapause_emergency") <- " Percentage of exclosion of diapausant eggs"
					attr(object,"dates")<-"Dates of data matrix"
  					attr(object,"timeformat")<- "Period of data aggregation"
					attr(object,"timeseries")<- " R xts object of data"
					class(object) <- "biometeo"
					return(object)						   					   
				    }
				   
						   
						   