## v01 03/03/2015
convert_medicos_cp <- function(x) {
	## Load libraries.
	library(data.table)
	library(reshape2)
	
	## Check if file exists in working directory.
	if (!file.exists(x)) stop (paste (x, "file not found."))
	
	## Read file.
	medicos_cp <- fread(x, colClasses = rep("character",11), na.strings = "")
	
	## Check columns.
	cols <- c("CDPOSTAL", "MSEGUIMIENTO", "MVALORACION", "MDICTAMEN PERICIAL", "MCONSIGNACION",
	"MOFERTA MOTIVADA", "MVALSECUIELAS", "M2ªOPINION", "MDICTAMEN SANIDAD", "MENCARGO COMISION", "MTELEVALORACION")
	if (!identical(cols,  names(medicos_cp))) stop ("Check columns first!.")
	
	## Set column names to report types.
	colvars <- c("CP_ASIG", "SEGUIMIENTO", "VALORACION", "PERICIAL", "CONSIGNACION", "OFERTA", "SECUELAS", 
	"SEGUNDA", "SANIDAD", "COMISION", "TELEVALORACION")
	setnames(medicos_cp, colvars)
	
	## Remove duplicates and NA rows.
	medicos_cp <- unique(medicos_cp)
	medicos_cp <- medicos_cp[complete.cases(medicos_cp)]
	
	## Melt table.
	medicos_melted <- melt(medicos_cp, id.vars = "CP_ASIG", measure.vars = 2:11, variable.name = "TIPO", value.name = "NIF")
	
	## Add column.
	medicos_melted[, POBL_ASIG := "TODAS"]
	
	## Reorder columns.
	setcolorder(medicos_melted, c("NIF", "POBL_ASIG", "CP_ASIG", "TIPO"))
	
	## Write file to working directory.
	filename <- paste0(gsub(":", "", Sys.time()), "-toExp.csv")
	write.csv2(medicos_melted,  file = filename,  row.names = FALSE,  quote = FALSE)
}

