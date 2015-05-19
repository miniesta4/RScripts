castData <- function(){
    ## Load libraries
    library(dplyr)
    library(tidyr)
    library(xlsx)
    
    
    ## Read file
    ds <- read.xlsx2("../files/Libro2.xlsx", sheetIndex = 1, colClasses = "character", 
                     stringsAsFactors = FALSE)
    
    ## Cast
    ds1 <- spread(ds, var, value)
    
    ## Set names
    cnames <- make.names(names(ds1))
    ds1 <- setNames(ds1, cnames)
    
    ## Select cols
    ds2 <- ds1 %>% select(Tipo.de.cambio, Nombre, Apellidos, Entidad.Aseguradora...Delegación, 
                          Departamento...Área, Teléfono, Fax, Correo.Electónico, Dirección..,
                          Localidad)
    
    ## Write sheet
    write.xlsx2(ds2, file = "../files/Carga.xlsx", sheetName = "Carga", col.names = TRUE, row.names = FALSE)
}