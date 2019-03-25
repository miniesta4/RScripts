
## V06 25/3/2019

convert_medicos_cp <- function(x = "../files/CARGA CP GESMED 15.01.2019.-ULTIMA.csv") {
  
  ## Load libraries.
  check_pkg_deps()

  ## Check if file exists in working directory.
  if (!file.exists(x)) stop (paste (x, "file not found."))
  
  ## Read file. Must be a csv file.
  medicos_cp <- read_csv2(x, col_types = paste0(rep("c", 15), collapse = ""))
  
  ## Check columns.
  cols <- c("CDPOSTAL", "MSEGUIMIENTO", "MVALORACION", "MDICTAMEN PERICIAL",
            "MCONSIGNACION", "MOFERTA MOTIVADA", "MVALSECUIELAS", "M2ªOPINION",
            "PRAXIS SANIDAD", "VDC SANIDAD", "MTELEVALORACION", "PERICIAL+VISITA",
            "ASESORIA MEDICA", "VAL. EXPEDIENTE SANIDAD", "NEXO NB")
  
  if (!identical(cols,  names(medicos_cp))) stop ("Check columns first!.")
  
  ## Set column names to report types.
  colvars <- c("CP_ASIG", "SEGUIMIENTONB", "VALORACIONNB", "PERICIALNB",
               "CONSIGNACIONNB", "OFERTANB", "SECUELASNB", "SEGUNDANB",
               "SANIDADNB", "COMISIONNB", "TELEVALORACIONNB", "PERICIALVNB",
               "ASESORIAMD", "VALSANIDADNB", "NEXONB")
  
  names(medicos_cp) <- colvars
  
  ## Remove duplicates and NA rows.
  medicos_cp_u <- unique(medicos_cp)
  medicos_cp_uc <- medicos_cp_u[complete.cases(medicos_cp_u), ]
  
  ## Melt table.
  medicos_melted <- medicos_cp_uc %>%
    gather(key = "TIPO", value = "NIF", -CP_ASIG)
  
  ## Add column.
  medicos_melted$POBL_ASIG <- "TODAS"
  
  ## Reorder columns.
  medicos_melted <- medicos_melted %>% select(NIF, POBL_ASIG, CP_ASIG, TIPO)
  
  ## Write file.
  salida <- file.path(dirname(x), "salida")
  if (!dir.exists(salida)) dir.create(salida)
  filename <- paste0(format(Sys.time(), "%Y%m%d%H%M%S"), ".csv")
  
  write_csv2(medicos_melted, file.path(salida, filename))
  
}


check_pkg_deps <- function() {
  if(!require(readr)) {
    stop("The 'readr' package needs to be installed first.")
  }
  
  if(!require(dplyr))
    stop("the 'dplyr' package needs to be installed first.")
  
  if (!require(tidyr))
    stop("the 'tidyr' package needs to be installed first.")
}