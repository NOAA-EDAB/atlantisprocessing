#'
#'
#'
#'
#' @export


get_rawdata <- function(group,group.type){
  if(group.type == 'age'){
    main.vars = age.vars
  }else{
    main.vars = bp.vars
  }

  if(load_fgs(param.ls$groups.file)$IsTurnedOn[which(load_fgs(param.ls$groups.file)$Name == group)] == 0){
    rawdata.spp.f = list(data.frame(species = group.types$species[i], polygon =NA, agecl = NA,layer = NA, time = 0, atoutput = NA))
    next()
  }else{
    rawdata.spp.f =Map(load_nc_temp,
                       select_variable = main.vars,
                       select_groups = group,
                       MoreArgs = list(nc = param.ls$main.nc, bps = bio.pools,
                                       fgs = param.ls$groups.file,prm_run = param.ls$run.prm,
                                       bboxes = bboxes ))
  }

  if(group.types$group[i] == 'bp'){
    rawdata.spp.f[[1]]$agecl =1
  }
  return(rawdata.spp.f)
}
