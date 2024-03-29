# rAedesSim
R package for population mosquito modeling Progetto REDLAV IBIMET CNR ASL2 Lucca 


To install package 

```R
if (!require(devtools)) { install.packages("devtools")}
devtools::install_github("alfcrisci/rAedesSim")
library(rAedesSim)
```

An example of operative work-chain 

```R
library(rAedesSim)

#################################################################################################
# Load my meteorological data obtained by Weather Local Model simulation for 2012 year


data(Castiglione_della_Pescaia_P4_meteo)


#################################################################################################
# Load different weather-water models respectively for a different trap mosquito traps or manhole.
# Data manhole come from Cesena Venturelli Claudio Emilia Romagna.
 
data(trappola_wmodel)
data(tombino_wmodel)

#################################################################################################
# Create a habitat niche and define breeding site number ( -> rAedesSim biocontainer ).
# Load a opportune model for to assess water temperature.

i_biocontainer_tomb=biocontainer(nrecipients=50,
                                 watermodel=tombino_wmodel, 
                                 lat=42.76090556,
                                 lon=10.88788889,
                                 elevation=5)
                                 
i_biocontainer_trap=biocontainer(nrecipients=50,
                                 watermodel=trappola_wmodel,
                                 model_type="lin",
                                 lat=42.76090556,
                                 lon=10.88788889,
                                 elevation=5)

#################################################################################################
# Retrieve biodaa object from location.  Mosquito eggs monitoring 

data(Castiglione_della_Pescaia_P4_monitoring)

#################################################################################################
# Create a biometeo objects

C_Pescaia_P4_bio_tombino=biometeo(Castiglione_della_Pescaia_P4_meteo,i_biocontainer_tomb)
C_Pescaia_P4_bio_trap=biometeo(Castiglione_della_Pescaia_P4_meteo,i_biocontainer_trap)

##################################################################################################
# Create a simulation. The location is done by container object. 
# Meteorological data and Biometeorological derivatives  are considered associated with the location.



simulation=biomodel(biopopulation(eggs=100,larvae=0,pupae=0,adults=0,eggs_diap=10),
                    bioparameters(alfa_l=1,alfa_a=0,l_density=40),
                    C_Pescaia_P4_bio_tombino,
                    i_biocontainer_tomb)
					  


##################################################################################################
# Create a simulation. The location is done by container object. 
# Meteorological data and Biometeorological derivatives  are considered associated with the location.
# View where is the place of simulation in  its urban context.

viewwhere(simulation)

##################################################################################################
# Fitting parametrs for  one locationCastiglione_della_Pescaia ad view plot Observed vs Simulated Eggs.


i_biocontainer=biocontainer(nrecipients=100,
                                 watermodel=trappola_wmodel,
                                 model_type="lin",
                                 lat=Castiglione_della_Pescaia_P4_monitoring$lat,
                                 lon=Castiglione_della_Pescaia_P4_monitoring$lon,
                                 elevation=Castiglione_della_Pescaia_P4_monitoring$elevation
								 )

i_biometeo=biometeo(Castiglione_della_Pescaia_P4_meteo,i_biocontainer)


i_biopopulation=biopopulation(eggs=100,larvae=0,pupae=0,adults=0,eggs_diap=10)


simulation_fit=biofitmodel(i_biometeo=i_biometeo,
                           i_biopopulation=i_biopopulation,
                           i_biocontainer=i_biocontainer,
                           i_monitoring=Castiglione_della_Pescaia_P4_monitoring,
		                   range_alpha_a=c(0,seq(0,0.002,0.001)),
		                   range_alpha_l=seq(0.6,1.6,0.2),
                           range_density_l=70,
						   plotresults=TRUE
			               )	


simulation_fit

##################################################################################################
```
