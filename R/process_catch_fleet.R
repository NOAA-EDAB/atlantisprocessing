#' Creates post-processing Catch output for Atlantis
#'
#' Reads in catch output from catch.nc file by species, fleet, box, time
#'
#' @param fishery.prm string. Path to location of atlantis fishery csv file
#' @param catch string. path to location of atlantis catch output nc file
#' @param groups.file string. path to location of atlantis functional groups csv file
#'
#' @importFrom magrittr "%>%"
#'
#' @return returns a dataframe
#'
#' \item{species}{long species name}
#' \item{fleet}{fleet name}
#' \item{polygon}{atlantis box/polygon number}
#' \item{time}{time step}
#' \item{atoutput}{metric tons}
#'
#' Author: Joseph Caracappa
#'
#' @export

process_catch_fleet = function(fishery.prm, catch, groups.file){

  fisheries = read.csv(fishery.prm)
  fgs = read.csv(groups.file)

  catch.nc =ncdf4::nc_open(catch)

  catch.varname = names(catch.nc$var)

  times = catch.nc$dim$t$vals/86400

  get_fleet_num = function(x){
    a = strsplit(x,'_')[[1]][3]
    b = strsplit(a,'FC')[[1]][2]
    return(as.numeric(b)-1)
  }

  spp.catch.out.ls = list()
  s=1
  for(s in 1:nrow(fgs)){

    spp.vars = grep(paste0('^',fgs$Code[s],'_'),catch.varname,value =T)

    if(length(spp.vars) == 0){next()}

    spp.catch.vars = grep('Catch',spp.vars,value =T)

    if(length(spp.catch.vars) == 0){next()}

    #get the fleet ID
    fleet.nums = sapply(spp.catch.vars,get_fleet_num,USE.NAMES = F)
    fleet.names = fisheries$Code[match(fleet.nums,fisheries$Index)]

    fleet.out.ls = list()
    f=1
    for(f in 1:length(spp.catch.vars)){

      data.fleet = ncdf4::ncvar_get(catch.nc,spp.catch.vars[f])%>%
        as.data.frame()

      colnames(data.fleet) = times
      fleet.out.ls[[f]] = dplyr::bind_cols(data.frame(polygon = (1:nrow(data.fleet))-1), data.fleet)%>%
        tidyr::gather('time','atoutput',-polygon)%>%
        dplyr::mutate(fleet = fleet.names[f],
                      species = fgs$LongName[s])%>%
        dplyr::mutate(time = round(as.numeric(time)/365,1))%>%
        dplyr::select(species,fleet,polygon,time,atoutput)

    }

    spp.catch.out.ls[[s]] = dplyr::bind_rows(fleet.out.ls)

  }

  catch.fleet = dplyr::bind_rows(spp.catch.out.ls)

  ncdf4::nc_close(catch.nc)

  return(catch.fleet)

}
