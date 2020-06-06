
# Clean environment, set working directory and load packages
cat("\f")
rm(list = ls())  
packageList<-c('dplyr','readxl','stringr')
sapply(packageList,require,character.only=TRUE)
setwd("~/Dropbox/web_page/geocode-addresses")
options("scipen"=100, "digits"=4) # Force R not to use e +

# Load database
providers <- readxl::read_excel(path = "data/originals/prestadores.xls", sheet = "prestadores", col_names = TRUE)

# Clean district hospitals 
providers <- dplyr::filter(providers,caracter != "DISTRITAL" | is.na(caracter) == T) %>% 
             plyr::rbind.fill(read_excel(path = "data/originals/subcentrooriente.xls", sheet = "subcentrooriente", col_names = TRUE),
                              read_excel(path = "data/originals/subnorte.xls", sheet = "subnorte", col_names = TRUE),
                              read_excel(path = "data/originals/suboccidente.xls", sheet = "suboccidente", col_names = TRUE),
                              read_excel(path = "data/originals/subsur.xls", sheet = "subsur", col_names = TRUE))

# Clean address variable
providers$direccion1 <- providers$direccion %>% toupper() %>% gsub("#","No.",.) %>% gsub(" NO. "," No. ",.) %>% 
                        gsub(" NO "," No. ",.) %>% gsub(" N° "," No. ",.) %>% gsub("CR ","KR ",.) %>% 
                        gsub("CALLE ","CL ",.) %>% gsub("CARRERA ","KR ",.) %>% gsub(" NUMERO "," No. ",.)
providers$direccion1 <- ifelse(is.na(str_locate(string = providers$direccion1 , pattern = "PISO ")[,1]) == F, 
                                 substr(providers$direccion1 , 1 , str_locate(string = providers$direccion1 , pattern = "PISO ")[,1]-1),
                                 providers$direccion1)
providers$direccion1 <- ifelse(is.na(str_locate(string = providers$direccion1 , pattern = "CONSULTORIO")[,1]) == F, 
                                 substr(providers$direccion1 , 1 , str_locate(string = providers$direccion1 , pattern = "CONSULTORIO")[,1]-1),
                                 providers$direccion1)
providers$direccion1 <- ifelse(is.na(str_locate(string = providers$direccion1 , pattern = "CONS")[,1]) == F, 
                                 substr(providers$direccion1 , 1 , str_locate(string = providers$direccion1 , pattern = "CONS")[,1]-1),
                                 providers$direccion1)
providers$direccion1 <- ifelse(is.na(str_locate(string = providers$direccion1 , pattern = "CON ")[,1]) == F, 
                                 substr(providers$direccion1 , 1 , str_locate(string = providers$direccion1 , pattern = "CON ")[,1]-1),
                                 providers$direccion1)
providers$direccion1 <- ifelse(is.na(str_locate(string = providers$direccion1 , pattern = " CS")[,1]) == F, 
                                 substr(providers$direccion1 , 1 , str_locate(string = providers$direccion1 , pattern = " CS")[,1]-1),
                                 providers$direccion1)
providers$direccion1 <- ifelse(is.na(str_locate(string = providers$direccion1 , pattern = "/")[,1]) == F, 
                                 substr(providers$direccion1 , 1 , str_locate(string = providers$direccion1 , pattern = "/")[,1]-1),
                                 providers$direccion1)
providers$direccion1 <- ifelse(is.na(str_locate(string = providers$direccion1 , pattern = "OFICINA")[,1]) == F, 
                                 substr(providers$direccion1 , 1 , str_locate(string = providers$direccion1 , pattern = "OFICINA")[,1]-1),
                                 providers$direccion1)
providers$direccion1 <- ifelse(is.na(str_locate(string = providers$direccion1 , pattern = " OF ")[,1]) == F, 
                                 substr(providers$direccion1 , 1 , str_locate(string = providers$direccion1 , pattern = " OF ")[,1]-1),
                                 providers$direccion1)
providers$direccion1 <- ifelse(is.na(str_locate(string = providers$direccion1 , pattern = " OF. ")[,1]) == F, 
                                 substr(providers$direccion1 , 1 , str_locate(string = providers$direccion1 , pattern = " OF. ")[,1]-1),
                                 providers$direccion1)
providers$direccion1 <- ifelse(is.na(str_locate(string = providers$direccion1 , pattern = " OFI")[,1]) == F, 
                                 substr(providers$direccion1 , 1 , str_locate(string = providers$direccion1 , pattern = " OFI")[,1]-1),
                                 providers$direccion1)
providers$direccion1 <- ifelse(is.na(str_locate(string = providers$direccion1 , pattern = " PISO")[,1]) == F, 
                                 substr(providers$direccion1 , 1 , str_locate(string = providers$direccion1 , pattern = " PISO")[,1]-1),
                                 providers$direccion1)
providers$direccion1 <- ifelse(is.na(str_locate(string = providers$direccion1 , pattern = " PISOS ")[,1]) == F, 
                                 substr(providers$direccion1 , 1 , str_locate(string = providers$direccion1 , pattern = " PISOS ")[,1]-1),
                                 providers$direccion1)
providers$direccion1 <- ifelse(is.na(str_locate(string = providers$direccion1 , pattern = "PRIMER PISO")[,1]) == F, 
                                 substr(providers$direccion1 , 1 , str_locate(string = providers$direccion1 , pattern = "PRIMER PISO")[,1]-1),
                                 providers$direccion1)
providers$direccion1 <- ifelse(is.na(str_locate(string = providers$direccion1 , pattern = " LOCAL ")[,1]) == F, 
                                 substr(providers$direccion1 , 1 , str_locate(string = providers$direccion1 , pattern = " LOCAL ")[,1]-1),
                                 providers$direccion1)
providers$direccion1 <- ifelse(is.na(str_locate(string = providers$direccion1 , pattern = " LC ")[,1]) == F, 
                                 substr(providers$direccion1 , 1 , str_locate(string = providers$direccion1 , pattern = " LC ")[,1]-1),
                                 providers$direccion1)
providers$direccion1 <- ifelse(is.na(str_locate(string = providers$direccion1 , pattern = " BARRIO")[,1]) == F, 
                                 substr(providers$direccion1 , 1 , str_locate(string = providers$direccion1 , pattern = " BARRIO")[,1]-1),
                                 providers$direccion1)
providers$direccion1 <- ifelse(is.na(str_locate(string = providers$direccion1 , pattern = " TORRE ")[,1]) == F, 
                                 substr(providers$direccion1 , 1 , str_locate(string = providers$direccion1 , pattern = " TORRE ")[,1]-1),
                                 providers$direccion1)
providers$direccion1 <- ifelse(is.na(str_locate(string = providers$direccion1 , pattern = " TO ")[,1]) == F, 
                                 substr(providers$direccion1 , 1 , str_locate(string = providers$direccion1 , pattern = " TO ")[,1]-1),
                                 providers$direccion1)
providers$direccion1 <- ifelse(is.na(str_locate(string = providers$direccion1 , pattern = " INTERIOR ")[,1]) == F, 
                                 substr(providers$direccion1 , 1 , str_locate(string = providers$direccion1 , pattern = " INTERIOR ")[,1]-1),
                                 providers$direccion1)
providers$direccion1 <- ifelse(is.na(str_locate(string = providers$direccion1 , pattern = "SOTANO")[,1]) == F, 
                                 substr(providers$direccion1 , 1 , str_locate(string = providers$direccion1 , pattern = "SOTANO")[,1]-1),
                                 providers$direccion1)

# Save database
providers <- providers[,c("codigo_habilitacion","depa_nombre","nombre_prestador","direccion1")]
save(providers,file = "data/originals/providers.Rdata")
