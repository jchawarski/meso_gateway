# ARGO floats
library(ncdf4)
library(oce)
#unpack the tarball (tar.gz) from https://usgodae.org/cgi-bin/argo_select.pl

argo1 <- nc_open(nc.files[1])

nc.files <-  list.files(pattern= "\\_prof.nc$", recursive = T)  # load all argo profiles in _prof.nc format
argo.all <- lapply(nc.files, function(i){nc_open(i)})                          # concatenates and trims upcast from all CTD files into large list
meta.tbl <- setNames(data.frame(matrix(ncol = 3, nrow = length(files)), c("Site", "Date", "ID")) # create empty df for summary data
                     # in this example nrow is the number of cast files contained in the folder - change it as needed
                     meta.tbl[,1] <-  paste(substr(sapply(CTD.cnv, '[[', "filename"), 26, 50))             # selects and trims file name, put in in blank matrix
                     meta.tbl[,2] <- sapply(CTD.cnv, function(i){paste(unique(i[["date"]]))})                # pulls date from each @data in data.in
                     uniqueID <- c(1:118)                                                                   # create unique ID by cast
                     #once again change the length of uniqueID to match number of CTD casts
                     meta.tbl[,3] <-  uniqueID                                                             # assigns a unique ID
                     
