#' biofitmodel
#'
#' @description 
#' \code{biofitmodel} A class to perform the model tuning to find the more effective parameters which lead minimal RMSE error having field data.
#' 
#' @param i_biometeo object: rAedesSim object concerning a implemented biomodel.  
#' @param i_biopopulation object: rAedesSim object concerning starting population object.  
#' @param i_biocointaner object: rAedesSim object concerning biocointaner object used in simulation.  
#' @param i_monitoring object: rAedesSim biodata object concerning mosquito eggs field observations.
#' @param range_alpha_a numeric: rAedesSim vector of sorted  guess' values of female adult competition. Default is obtained by c(0,seq(0,0.002,0.001)).
#' @param range_alpha_l numeric: rAedesSim vector of sorted guess' values of larval competition.  Default is obtained by seq(0.8,1.6,0.2).
#' @param range_density_l numeric: rAedesSim object guess' values of maximaum larval competion in function of waer volume in cointaner. Default is obtained by seq(40,70,10).
#' @param stocastic logical: if stocastic simulation are carried out in simulation.Defalut is True.
#' @param n_sampling numeric: number  of resampling if stochastic is implemented.Defalut is 10.
#' @param inibition logical: if larval density is considered.Defalut is FALSE.
#' @param plotresults logical: if is true a plot is done. 
#' @return rAedesSim object biofitmodel object.
#' @author  Istituto di Biometeorologia Firenze Italy  Alfonso crisci \email{a.crisci@@ibimet.cnr.it} ASL 2 LUCCA Marco Selmi \email{m.selmi@@usl2.toscana.it} 
#' @keywords  fit a rAedesModel modeling
#'
#' 
#' 
#' @export

biofitmodel  <- function(i_biometeo,
                         i_biopopulation,
                         i_biocontainer,
                         i_monitoring,
                         range_alpha_a=c(0,seq(0,0.002,0.001)),
                         range_alpha_l=seq(0.8,1.6,0.2),
                         range_density_l=seq(40,70,10),
                         stocastic=TRUE,
                         n_sampling=10,
                         inibition=FALSE,
                         plotresults=FALSE
		         )
	             { 
			   
			    #########################################################################################à
			    # Check availability libraries.
			 
			    require(deSolve);
			    require(lubridate);
			    require(xts);
			    require(verification);
			   
			    #########################################################################################à
			    # Check arguments of object.
			 
			     if (class(i_biometeo) != "biometeo") { stop(" Object argument must be a  rAedesSim biometeo class." )};
			     if (class(i_biopopulation) != "biopopulation") { stop(" Object argument must be a rAedesSim bioparameters class." )};
			     if (class(i_biocontainer) != "biocontainer") { stop(" Object argument must be a rAedesSim biocontainer class." )};
  			     if (class(i_monitoring) != "biodata") { stop(" Object argument must be a rAedesSim biodata class." )};
  			   
			      
			   					
			    #########################################################################################
			    # Create matrix and list for testing parameters
				
			    
			    replies=cbind(merge(range_alpha_a,range_alpha_l),z=rep(range_density_l,nrow(merge(range_alpha_a,range_alpha_l))))
			    replies=replies[,1:3]
			    biopar_list=list()
				
				for ( i in 1:nrow(replies)) { biopar_list[[i]]=bioparameters(alfa_l=replies$y[i], alfa_a=replies$x[i],l_density=replies$z[i])}
				
			    #########################################################################################
			    # Create list for outcomes
				
			     simul_ts=list();
			     simul_RMSE=numeric(nrow(replies));
			     simul_RMSE_no_diap=numeric(nrow(replies));
			     success_vector=logical(nrow(replies));
			     ########################################################################################################
				
                             for ( i in seq_along(biopar_list)){ message(paste("Working on:", i));
 
				                                simulation = tryCatch({biomodel(i_biometeo=i_biometeo,
                                                                                                i_biocontainer=i_biocontainer,
										                i_bioparameters=biopar_list[[i]],
                                                                                                stocastic = stocastic,
                                                                                                n_sampling = n_sampling,
                                                                                                inibition = inibition)
												},
                                                                                error=function(cond) {
                                                                                                success_vector[i] = FALSE
                                                                                                simul_ts[[i]] = NA
										                simul_RMSE[i]=NA
                                                                                                simul_RMSE_no_diap[i]=NA
										                message(paste("Processed case:", i,"Simulation aborted!"))
                                                                                             },
                                                                                finally=      {
                                                                                                 success_vector[i] = TRUE
                                                                                             }
                                                                                       )
						                if (success_vector[i] = TRUE) {
													                                    Eggs=simulation$ts_population$eggs+simulation$ts_population$diapausant_eggs
                                                                                                Eggs_no_diap=simulation$ts_population$eggs
                                                                                                Eggs_obs=i_monitoring$ts_data
                                                                                                merged=merge.xts(Eggs,Eggs_obs,join = "inner");
                                                                                                merged_no_diap=merge.xts(Eggs_no_diap,Eggs_obs,join = "inner");
                                                                                                simul_ts[[i]]=merged; 
                                                                                                simul_RMSE[i]=sqrt(verify(as.vector( merged$eggs), as.vector(merged$Eggs_obs), frcst.type = "cont", obs.type = "cont")$MSE)
                                                                                                simul_RMSE_no_diap[i]=sqrt(verify(as.vector( merged_no_diap$eggs), as.vector(merged_no_diap$Eggs_obs), frcst.type = "cont", obs.type = "cont")$MSE)
                                                                                                message(paste("Processed case:", i,"Simulation ok!"))
                                                                                                }													  
								}	
		   
				  
                
				#########################################################################################################################################
				# Fill spatial objects
				
				best=which.min(simul_RMSE)
				best_no_diap=which.min(simul_RMSE_no_diap);
				names(replies)<-c("alpha_a","alpha_l","density_max_l");
				replies_best=replies[best,]
				replies_best_no_diap=replies[best_no_diap,]
				
				#########################################################################################################################################
				# Fill spatial objects
				
				i_biocontainer$sp_obj$alpha_a=replies_best[1]
				i_biocontainer$sp_obj$alpha_l=replies_best[2]
				i_biocontainer$sp_obj$density_max_l=replies_best[3]
				i_biocontainer$sp_obj$alpha_a_no_diap=replies_best_no_diap[1]
				i_biocontainer$sp_obj$alpha_l_no_diap=replies_best_no_diap[2]
				i_biocontainer$sp_obj$density_max_l_no_diap=replies_best_no_diap[3]
				
				plot_ts=NULL
				if ( plotresults == TRUE)   { plot_ts=plot(simul_ts[[best]],
                                                                           main = paste("Observed (red) & Assessed (Black) - ",as.character(i_biocontainer$type),"-","Stage's competivity index: Larvae=",replies_best[1],"   Adults=",replies_best[1],"   Larval MaxDensity=",replies_best[3]),
				                                           cex.axis = 1.2,
				                                           cex.main = 2.5,
				                                           legend.loc = "bottomright", 
				                                           legend.pars = list(bty = "n",cex=2,horiz=TRUE),
				                                           legend.names = c("Observed","Assessed")) 
                                                              }
				#########################################################################################################################################
			   
                object  <-  list(name_model="rAedesSim",
							guess_parameter=replies,
							best_simul = simul_ts[[best]],
							best_biopar_no_diap = simul_ts[[best_no_diap]],
							best_simul_RMSE = simul_RMSE[[best]],
							best_nodiap_RMSE = simul_RMSE_no_diap[[best_no_diap]],
							par_fitted_best=replies_best,
							par_fitted_best_no_diap=replies_best_no_diap,
							simul_RMSE=simul_RMSE,
							simul_RMSE_no_diap=simul_RMSE_no_diap,
							n_replies=length(na.omit(simul_RMSE)),
							stocastic=stocastic,
							n_sampling=n_sampling,
							inibition=inibition,
							ID=i_biocontainer$ID,
							site_name=i_biocontainer$site_name,
							sp_obj=i_biocontainer$sp_obj,
							lat=i_biocontainer$lat,
							lon=i_biocontainer$lon,
							plot_ts=plot_ts
 
					 );
			            									  
				               

                #########################################################################################################################################
				             	    
                attr(object,"name_model") <- "Model's name"
                attr(object,"guess_parameter") <- "Matrix of guess values."
                attr(object,"best_simul") <- "Timeseries object: Best simulation taking into account diapause."
                attr(object,"best_biopar_no_diap") <-  "Timeseries object: Best simulation NOT taking  into account diapause."
                attr(object,"best_simul_RMSE") <- "Root mean square error of simulation"
                attr(object,"best_nodiap_RMSE") <- "Root mean square error of simulation NOT taking  into account diapause."
                attr(object,"par_fitted_best") <- "Parameter fitted."
                attr(object,"par_fitted_best_no_diap") <- "Parameter fitted NOT taking  into account diapause."
                attr(object,"simul_RMSE") <- "Root mean square error for all simulation."
                attr(object,"simul_RMSE_no_diap") <- "Root mean square error for all simulation NOT taking  into account diapause."
                attr(object,"n_replies") <- "Number of simulation."
                attr(object,"stocastic") <- "If stocasticity in simulation are considered."
                attr(object,"n_sampling") <- "Number of resamplig."
                attr(object,"inibition") <- "Logical if larval inibition are taken into account in simulation."
                attr(object,"ID")<-"ID label of container set."
                attr(object,"site_name")<-"Name of site."
                attr(object,"sp_obj")<-"SpatialPointDataFrame of location."
                attr(object,"lat")<-"latitude coordinates of simulations."
                attr(object,"lon")<-"longitude coordinates of simulations."
                attr(object,"plot_ts")<-"Plot fitted vs observed."
		class(object) <- "biofitmodel"
                return(object)
}
