
# Clean environment, set working directory and load packages
cat("\f")
rm(list = ls())  
packageList<-c('dplyr','data.table','ggmap')
sapply(packageList,require,character.only=TRUE)
setwd("~/Dropbox/web_page/geocode-addresses")
options("scipen"=100, "digits"=4) # Force R not to use e +

# Load database
load("data/originals/providers.Rdata")
providers <- providers[1:10,]

# You can get the Google key here: https://cloud.google.com/docs/authentication/api-keys
register_google(key = "Write the appy key here") 

# Geocode Fucntion 
f_geocode <- function(i,data,address_var,city_var) {
                
             # Get address and city
             address <- paste0(data[i,grep(city_var,colnames(data))], " , ",
                               data[i,grep(address_var,colnames(data))]) %>% as.character()
             print(address)
            
             # Add column lon and lat 
             df <- mutate(data[i,],lon_google=NA,lat_google=NA) %>% as.data.frame(stringsAsFactors=F)
              
             # Geocode for the first time
             google <- ggmap::geocode(location = address , output="latlon",source="google")
             
             # If not geocode for the first time
             if (is.na(google[1])==T | is.na(google[2])==T){
             warning(paste0("Warning! ", address," not found for the first time")) 
                     
                                            # Geocode for the second time
                                            Sys.sleep(5)
                                            google <- ggmap::geocode(address,output="latlon",source="google")
                                            # If not geocode for the second time
                                            if (is.na(google[1])==T | is.na(google[2])==T){
                                            warning(paste0("Advertencia! ", address," not found for the second time"))
                                                                        
                                                                        # Geocode for the third time
                                                                        Sys.sleep(5)   
                                                                        google <- ggmap::geocode(address,output="latlon",source="google")
                                                                        df[1,5] <- google[1]
                                                                        df[1,6] <- google[2]
                                                                        return(df)
                                                                        
                                            }
                                            
                                            # If it was geocoded on the second try
                                            else {
                                                  df[1,5] <- google[1]
                                                  df[1,6] <- google[2]
                                                  return(df)
                                                  }
                                            
                                                        
             }
             
             # If it was geocoded on the first try
             else {
                   df[1,5] <- google[1]
                   df[1,6] <- google[2]
                   return(df)
             }
}

# To apply function
providers <- lapply(1:nrow(providers), function(x) f_geocode(i = x ,data = providers , address_var = "direccion1" , city_var = "depa_nombre" )) %>% 
             data.table::rbindlist(use.names = T,fill = T) %>% data.frame() 
  
# Save database
save(providers,file= "data/processed/providers.Rdata")




