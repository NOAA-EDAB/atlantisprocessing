#' Creates post-processing output for Atlantis
#'
#' @param param.dir string. Path to location of atlantis parameter files
#' @param atl.dir string. path to location of atlantis output files
#' @param out.dir string. path to desired location of post-processed output
#' @param run.prefix string. Prefix for atlantis run output (specified in runcommand.bat)
#' @param param.ls list generated from get_atl_paramfiles()
#' @param agg.scale Scale to aggregate dietcheck biomass from (either 'raw','month', or 'year' )
#' @param large.file Boolean.
#' @param system String. "Windows" or "Linux"
#' @param process.all Boolean. Global option to process all components
#' @param plot.spatial.overlap Boolean
#' @param plot.catch.fleet Boolean
#'
#' @inheritParams make_atlantis_diagnostic_figures
#'
#' @importFrom magrittr "%>%"
#'
#' @return Either saves an R object or returns a list called "result"
#'
#' Author: Ryan Morse, modified by Joseph Caracappa
#'
#' @export

process_atl_output = function(param.dir,
                              atl.dir,
                              out.dir = file.path(atl.dir,'Post_Processed/Data/'),
                              run.prefix,
                              param.ls,
                              agg.scale = 'day',
                              large.file = F,
                              system,
                              process.all = F,
                              plot.all = F,
                              plot.benthic =F,
                              plot.overall.biomass =F,
                              plot.biomass.timeseries = F,
                              plot.length.age = F,
                              plot.biomass.box=F,
                              plot.c.mum=F,
                              plot.sn.rn=F,
                              plot.recruits=F,
                              plot.numbers.timeseries=F,
                              plot.physics=F,
                              plot.growth.cons=F,
                              plot.cohort=F,
                              plot.diet=F,
                              plot.consumption= F,
                              plot.spatial.biomass=F,
                              plot.spatial.biomass.seasonal = F,
                              plot.catch =F,
                              plot.catch.fleet = F,
                              plot.spatial.catch =F,
                              plot.mortality=F,
                              plot.weight = F,
                              plot.spatial.overlap =F

){

  # memory.limit(size = 56000)
  #source(here::here('R','Post_Processing','load_nc_temp.R'))

  if (!dir.exists(out.dir)) {
    dir.create(out.dir,recursive = T)
  }
  if(large.file){
    dir.create(paste0(atl.dir,'/Aggregated/'))
  }


  #Read in groups file
  fgs = atlantistools::load_fgs(param.ls$groups.file)%>%
    dplyr::select(Code,Name,LongName)

  #Get boundary box
  bboxes =  atlantistools::get_boundary(boxinfo = atlantistools::load_box(param.ls$bgm.file))

  #Get epibenthic biopool groups
  bio.pools = atlantistools::load_bps(param.ls$groups.file,param.ls$init.file)

  #Get biomass conversion scalar
  bio.conv = atlantistools::get_conv_mgnbiot(param.ls$biol.prm)

  #All groups extracted (names, age-structured, biopools, and codes)
  group.names = atlantistools::get_groups(param.ls$groups.file)
  groups.age = atlantistools::get_age_groups(param.ls$groups.file)
  groups.bp = group.names[!group.names %in% groups.age]
  codes.age = atlantistools::get_age_acronyms(param.ls$groups.file)
  groups.data = atlantistools::load_fgs(param.ls$groups.file)

  # Read Physics ------------------------------------------------------------

  #Always make volume objects
  vol.dz = atlantistools::load_nc_physics(nc = param.ls$main.nc, select_physics = c('volume','dz'),
                                          prm_run = param.ls$run.prm, bboxes = bboxes)
  dz = dplyr::filter(vol.dz, variable == 'dz')
  vol = dplyr::filter(vol.dz, variable == 'volume')

  #Aggregate volume vertically
  vol.ts = atlantistools::agg_data(vol, groups = c('time','polygon'), fun = sum, out = 'volume')

  nominal.dz = as.data.frame(atlantistools::load_init(init = param.ls$init.file, vars = 'nominal_dz') )
  nominal.dz = dplyr::filter(nominal.dz,!is.na(layer))


  saveRDS(vol.ts,file = file.path(out.dir,'volume.rds'))
  saveRDS(dz,file = file.path(out.dir,'dz.rds'))
  saveRDS(nominal.dz, file = file.path(out.dir,'nominal_dz.rds'))
  rm(vol.ts)

  if(plot.physics|plot.all|process.all){
    flux = atlantistools::load_nc_physics(nc = param.ls$main.nc, select_physics = c('eflux','vflux'),
                                          prm_run = param.ls$run.prm, bboxes = bboxes)
    source.sink = atlantistools::load_nc_physics(nc = param.ls$main.nc, select_physics = c('hdsource','hdsink'),
                                                 prm_run = param.ls$run.prm, bboxes = bboxes)
    phys.statevars = atlantistools::load_nc_physics(nc = param.ls$main.nc,
                                                    select_physics = c('salt','NO3','NH3','Temp','Chl_a','Oxygen','Light'),
                                                    prm_run = param.ls$run.prm, bboxes = bboxes)

    #Exclude sediment from salinity
    phys.statevars = dplyr::filter(phys.statevars, !(variable == 'salt' & layer == max(layer) & time == min(time) ))

    #write and remove physics objects with no further use
    saveRDS(flux,file = file.path(out.dir,'flux.rds'))
    saveRDS(source.sink,file = file.path(out.dir,'source_sink.rds'))
    saveRDS(phys.statevars,file = file.path(out.dir,'physics_statevars.rds'))
    rm(flux,source.sink,phys.statevars)
    gc()
  }

  # Other Parameter Objects -------------------------------------------------
  #Read in age matrix
  data.age.mat = atlantistools::prm_to_df(prm_biol = param.ls$biol.prm, fgs = param.ls$groups.file,
                                          group = codes.age, parameter = 'age_mat')
  saveRDS(data.age.mat,file = file.path(out.dir,'data_age_mat.rds'))

  #Read in diet matrix
  data.diet.mat = atlantistools::load_dietmatrix(prm_biol = param.ls$biol.prm,fgs = param.ls$groups.file, convert_names = T)
  # saveRDS(data.diet.mat,file = file.path(out.dir,'diet_matrix.rds'))

  #length.age tempmat
  biol.prm.lines = read.table(param.ls$biol.prm,col.name = 1:100, comment.char = '', fill = T, header = F)
  lia.match =biol.prm.lines[grep('li_a_',biol.prm.lines[,1]),1:20]
  tempmat = matrix(NA,nrow = nrow(lia.match), ncol = 3)
  for(igroup in 1:nrow(tempmat)){
    tempmat[igroup,1] = strsplit(as.character(lia.match[igroup,1]),'li_a_')[[1]][2]
  }
  tempmat[,2] = as.numeric(as.character(lia.match[,2]))
  lib.match = grep('li_b_',biol.prm.lines[,1])
  tempmat[,3] = as.numeric(as.character(biol.prm.lines[lib.match,2]))

  groups.data2 = groups.data[,c('Code','LongName')]
  tempmat2 = as.data.frame(tempmat[2:dim(tempmat)[1],])
  colnames(tempmat2) = c('Code','li_a','li_b')
  tempmat3 = dplyr::left_join(tempmat2, groups.data2,by = 'Code')



  ##Growth relative to initial conditions
  recruit.weight = atlantistools::prm_to_df(prm_biol = param.ls$biol.prm, fgs = param.ls$groups.file,
                                            group = codes.age,
                                            parameter = c('KWRR','KWSR','AgeClassSize'))
  pd = atlantistools::load_init_weight(init = param.ls$init.nofill, fgs = param.ls$groups.file,bboxes = bboxes)
  pd = dplyr::left_join(pd,recruit.weight,by = "species")
  pd = split(pd,pd$species)

  #Calculate weight difference from one ageclass to the next
  for(i in seq_along(pd)){
    pd[[i]]$wdiff = c((pd[[i]]$rn[1] + pd[[i]]$sn[1]) - (pd[[i]]$kwrr[1] + pd[[i]]$kwsr[1]),
                      diff(pd[[i]]$rn + pd[[i]]$sn))
  }
  pd = do.call(rbind,pd)
  pd$growth_req = pd$wdiff/ (365* pd$ageclasssize)
  if( any(pd$growth_req < 0 )){
    warning("Required growth negative for some groups. Please check your initial conditions files.")
    print(unique(pd$species[which(pd$growth_req<0)]))
  }
  unique(pd$species[which(pd$growth_req<0)])

  growth.required = dplyr::select(pd, c(species,agecl, growth_req))

  # Process DietCheck -------------------------------------------------------

  if(plot.diet|plot.all|process.all){
    if(large.file==F){
      data.dietcheck.orig = atlantistools::load_dietcheck(dietcheck = param.ls$dietcheck,
                                                          fgs = param.ls$groups.file,
                                                          prm_run = param.ls$run.prm,
                                                          convert_names = T)
      #Normalize proprotions so they always sum to 1
      dietcheck.tot = data.dietcheck.orig %>%
        dplyr::group_by(time,pred,agecl)%>%
        dplyr::summarise(atoutput.tot = sum(atoutput,na.rm=T))

      data.dietcheck = data.dietcheck.orig %>%
        dplyr::left_join(dietcheck.tot,by = c("time", "pred", "agecl")) %>%
        dplyr::rename(atoutput.old = 'atoutput')%>%
        dplyr::mutate(atoutput = atoutput.old/atoutput.tot)%>%
        dplyr::select(time,pred,agecl,prey,atoutput)


      saveRDS(data.dietcheck,file = file.path(out.dir,'data_dietcheck.rds'))
    }else{


      ##NEUS only 613 diet obs per timestep
      nsteps =  365/extract_prm(prm_biol = param.ls$run.prm, variables = "toutinc")
      line.incr = 613 * nsteps
      if(system == 'windows'){
        nline.str = system(paste0('find /c /v "" ',param.ls$dietcheck),intern = T)[2]
        nline = as.numeric(strsplit(nline.str,' ')[[1]][3])
      }else{
        nline.str = system(paste0('wc -l ',param.ls$dietcheck),intern = T)
        nline = as.numeric(strsplit(nline.str,' ')[[1]][1])
      }
      line.seq = c(seq(0,nline,line.incr),nline)

      diet.agg = list()

      diet.colnames = colnames(data.table::fread(param.ls$dietcheck,nrow = 1))
      for(i in 1:(length(line.seq)-1)){

        #read chunk and aggregate by specified interval
        lines2read = line.seq[i+1]-line.seq[i]
        diet.slice = data.table::fread(param.ls$dietcheck,skip = line.seq[i]+1,nrow = lines2read)
        colnames(diet.slice) = diet.colnames

        diet.slice.dates = as.POSIXct(diet.slice$Time*86400, origin = '1964-01-01')
        if(agg.scale == 'month') {
          diet.slice$time.agg = as.numeric(factor(format(diet.slice.dates,format = '%m')))
        }else if(agg.scale == 'year'){
          diet.slice$time.agg = as.numeric(factor(format(diet.slice.dates, format = '%Y')))
        }else{
          diet.slice$time.agg = 1:nrow(diet.slice)
        }

        diet.slice = diet.slice %>%
          # dplyr::mutate(Time = Time)%>%
          dplyr::select(-Stock,-Updated)%>%
          tidyr::gather('prey','atoutput',-Time,-time.agg,-Predator,-Cohort)%>%
          dplyr::mutate(atoutput = as.numeric(atoutput))%>%
          dplyr::group_by(time.agg,Predator,Cohort,prey)%>%
          dplyr::summarise(time = floor(min(Time/365)),
                           atoutput = mean(atoutput,na.rm=T))%>%
          dplyr::ungroup()%>%
          dplyr::select(-time.agg)%>%
          dplyr::left_join(select(fgs,'Code','Name'),by = c('Predator' = 'Code'))%>%
          dplyr::select(-Predator)%>%
          dplyr::rename(Predator = 'Name')%>%
          dplyr::filter(atoutput != 0)%>%
          dplyr::left_join(select(fgs,'Code','Name'),by = c('prey' = 'Code'))%>%
          dplyr::select(-prey)%>%
          dplyr::rename(prey = 'Name',agecl = 'Cohort',pred = 'Predator')%>%
          dplyr::select(time,pred,agecl,prey,atoutput)%>%
          dplyr::arrange(time,pred,agecl,prey)

        # test = diet.slice %>%
        #   group_by(time,pred,agecl)%>%
        #   summarise(atoutput = sum(as.numeric(atoutput),na.rm=T))

        # pred.sum =apply(diet.slice[,6:ncol(diet.slice)],1,sum,na.rm=T)
        #
        # diet.slice[,6:ncol(diet.slice)] = diet.slice[,6:ncol(diet.slice)]/pred.sum
        # diet.slice = diet.slice[which(pred.sum !=0),]

        diet.agg[[i]] = diet.slice
        print(i)
      }


      data.dietcheck = dplyr::bind_rows(diet.agg)%>%
        dplyr::mutate(atoutput  = as.numeric(atoutput))

      pred.sum = data.dietcheck %>%
        dplyr::group_by(time,pred,agecl)%>%
        dplyr::summarise(atoutput.sum = sum(atoutput,na.rm=T))
      data.dietcheck = data.dietcheck %>%
        dplyr::left_join(pred.sum) %>%
        dplyr::mutate(atoutput = atoutput/atoutput.sum)%>%
        dplyr::select(-atoutput.sum)


      # data.dietcheck = atlantistools::load_dietcheck(dietcheck = paste0(atl.dir,run.prefix,'DietCheck.txt'),
      #                                                fgs = param.ls$groups.file,
      #                                                prm_run = param.ls$run.prm,
      #                                                convert_names = T)

      rm(diet.agg)
      saveRDS(data.dietcheck,file = file.path(out.dir,'data_dietcheck.rds'))
    }

    rm(data.dietcheck.orig,dietcheck.tot)
    gc()
  }


  # Main NetCDF objects -----------------------------------------------------

  #Set up biological variable groups
  group.types = dplyr::bind_rows(list(data.frame(species = groups.age,group = 'age'),
                                      data.frame(species = groups.bp, group = 'bp'))
  )
  age.vars= c('Nums','StructN','ResN','N')
  bp.vars = 'N'

  if(plot.overall.biomass|plot.biomass.timeseries|plot.biomass.box|plot.weight|plot.benthic|plot.spatial.biomass|plot.spatial.biomass.seasonal|plot.sn.rn|plot.length.age|plot.numbers.timeseries|plot.cohort|plot.spatial.overlap|plot.c.mum|plot.spatial.catch|plot.all|process.all){

    numbers = list()
    numbers.age = list()
    numbers.box = list()
    spatial.numbers = list()
    RN.box = list()
    SN.box = list()
    RN.age = list()
    SN.age = list()
    RN.age.mean = list()
    SN.age.mean = list()
    biomass.age = list()
    biomass.age.invert = list()
    spatial.biomass = list()
    spatial.numbers = list()
    spatial.biomass.stanza = list()
    biomass = list()
    biomass.box = list()
    sp.overlap = list()
    biomass.box.invert = list()
    length.age = list()

    if(large.file == F){

      vars = list('Nums','StructN','ResN','N')
      group.types = list(groups.age,groups.age,groups.age,groups.bp)
      rawdata.main = Map(atlantistools::load_nc,
                         select_variable = vars,
                         select_groups = group.types,
                         MoreArgs = list(nc = param.ls$main.nc, bps = bio.pools,
                                         fgs = param.ls$groups.file,prm_run = param.ls$run.prm,
                                         bboxes = bboxes ))


      if(plot.overall.biomass|plot.biomass.timeseries|plot.biomass.box|plot.weight|plot.benthic|plot.spatial.biomass|plot.spatial.biomass.seasonal|plot.spatial.catch|process.all|plot.all){

        spatial.biomass = atlantistools::calculate_biomass_spatial(nums = rawdata.main[[1]],
                                                                   sn = rawdata.main[[2]],
                                                                   rn = rawdata.main[[3]],
                                                                   n = rawdata.main[[4]],
                                                                   vol_dz = vol.dz,
                                                                   bio_conv = bio.conv,
                                                                   bps = bio.pools)

        #Calculate spatial overlap
        if(plot.spatial.overlap|process.all|plot.all){
          # sp.overlap = list()
          #   spatial.biomass = dplyr::bind_rows(spatial.biomass)
          #   sp.overlap = atlantistools::calculate_spatial_overlap(biomass_spatial = spatial.biomass,dietmatrix = data.diet.mat, agemat = data.age.mat )
          #   saveRDS(sp.overlap, file.path(out.dir,'spatial_overlap.rds'))
          #   rm(sp.overlap)
        }

        #Overall biomass objects
        if(plot.overall.biomass|plot.biomass.timeseries|process.all|plot.all){

          biomass <- atlantistools::agg_data(spatial.biomass,groups = c('species','time'),fun = sum)
          bind.save(biomass,'biomass',out.dir); rm('biomass')

        }

        #Biomass timeseries objects
        if(plot.biomass.timeseries|process.all|plot.all){

          biomass.age = dplyr::filter(spatial.biomass, species %in% data.age.mat$species)
          biomass.age = atlantistools::agg_data(biomass.age, groups = c('species','agecl','time'),fun = sum)

          biomass.age.invert = dplyr::filter(spatial.biomass, !(species %in% data.age.mat$species))
          biomass.age.invert = atlantistools::agg_data(biomass.age.invert,groups = c('species','time'),fun = sum)

          bind.save(biomass.age,'biomass_age',out.dir); rm('biomass.age')
          bind.save(biomass.age.invert,'biomass_age_invert',out.dir); rm('biomass.age.invert')
        }

        #Biomass Box objects
        if(plot.biomass.box|plot.spatial.catch|plot.spatial.biomass|plot.spatial.biomass.seasonal|process.all|plot.all){

          biomass.box = atlantistools::agg_data(spatial.biomass, groups = c('species','polygon','time'), fun = sum)
          bind.save(biomass.box,'biomass_box',out.dir)

          #Biomass Seasonal objects
          if(plot.spatial.biomass.seasonal|process.all|plot.all){

            biomass.box.invert = dplyr::filter(biomass.box, !(species %in% data.age.mat$species))
            bind.save(biomass.box.invert,'biomass_box_invert',out.dir); rm('biomass.box.invert')

          }

        }


        if(plot.spatial.biomass|process.all|plot.all){

          spatial.biomass.stanza = atlantistools::combine_ages(spatial.biomass,grp_col = 'species',agemat = data.age.mat)
          bind.save(spatial.biomass.stanza,'biomass_spatial_stanza',out.dir); rm('spatial.biomass.stanza')

        }

      }

      if(plot.weight|plot.all|process.all){

        spatialNumbers = rawdata.main[[1]] %>%
          dplyr::rename(numbers = atoutput)

        # filter biomass for species with 10 cohorts and convert to kilograms
        spatialBiomass2 <- spatial.biomass %>%
          dplyr::filter(species %in% unique(spatialNumbers$species)) %>%
          dplyr::rename(biomass = atoutput) %>%
          dplyr::mutate(biomass = 1E6*biomass)

        # join numbers with biomass and calculate mean weight of an individivual in age, polygon, layer, time
        weight <- spatialNumbers %>% dplyr::left_join(.,spatialBiomass2,by = c("species", "agecl", "polygon", "layer", "time")) %>%
          dplyr::filter(!is.na(biomass)) %>%
          dplyr::mutate(meanWeight = biomass/numbers)

        weight <- weight %>%
          dplyr::group_by(species, polygon,layer, agecl, time) %>%
          dplyr::summarise(maxMeanWeight = max(meanWeight),.groups="drop")

        bind.save(weight,'max_weight',out.dir); rm('weight')
        bind.save(spatialNumbers,'spatial_numbers',out.dir); rm('spatialNumbers')
        rm('spatialBiomass2')
      }

      if(plot.length.age|plot.c.mum|plot.sn.rn|plot.numbers.timeseries|process.all|plot.all){

        RN.age = atlantistools::agg_data(data = rawdata.main[[3]], groups = c('species','agecl','time'), fun = sum)
        SN.age = atlantistools::agg_data(data = rawdata.main[[2]], groups = c('species','agecl','time'), fun = sum)

        SN.age.mean = atlantistools::agg_data(data = rawdata.main[[2]], groups = c('species','time','agecl'), fun = mean)
        RN.age.mean = atlantistools::agg_data(data = rawdata.main[[3]], groups = c('species','time','agecl'), fun = mean)

        bind.save(RN.age,'RN_age',out.dir); rm('RN.age')
        bind.save(SN.age,'SN_age',out.dir); rm('SN.age')

        bind.save(SN.age.mean,'SN_age_mean',out.dir)
        bind.save(RN.age.mean,'RN_age_mean',out.dir)

        #Numbers only objects
        if(plot.numbers.timeseries|plot.cohort|plot.all|process.all){

          #numbers
          numbers = atlantistools::agg_data(data = rawdata.main[[1]], groups = c('species','time'), fun = sum)
          numbers.age = atlantistools::agg_data(data = rawdata.main[[1]], groups = c('species','agecl','time'), fun = sum)
          numbers.box = atlantistools::agg_data(data = rawdata.main[[1]], groups = c('species','polygon','time'), fun = sum)

          bind.save(numbers,'numbers',out.dir); rm('numbers')
          bind.save(numbers.age,'numbers_age',out.dir); rm('numbers.age')
          bind.save(numbers.box,'numbers_box',out.dir); rm('numbers.box')
        }

        #length-age only objects
        if(plot.length.age|plot.c.mum|process.all|plot.all){

          #Use mean RN+SN per age for each species, convert to weight, get length w/Von Bert.
          RN.SN.age = dplyr::left_join(RN.age.mean,SN.age.mean,by = c('species','agecl','time'))
          colnames(RN.SN.age) = c('species','time','agecl','RN','SN')

          biomass.age2 = dplyr::left_join(RN.SN.age, tempmat3[,2:4], by = c('species'='LongName'))
          biomass.age2$grams_N_Ind = (biomass.age2$RN+biomass.age2$SN)*5.7*20/1000
          biomass.age2$length_age = (as.numeric(as.character(biomass.age2$grams_N_Ind))/as.numeric(as.character(biomass.age2$li_a)))^(1/as.numeric(as.character(biomass.age2$li_b)))
          length.age = biomass.age2[,c('species','agecl','time','length_age')]
          colnames(length.age)[4] = 'atoutput'

          bind.save(length.age,'length_age',out.dir); rm('length.age')
        }

        #SN.RN only objects
        if(plot.sn.rn|process.all|plot.all){

          RN.box = atlantistools::agg_data(data = rawdata.main[[3]], groups = c('species','polygon','time'), fun = sum)
          SN.box = atlantistools::agg_data(data = rawdata.main[[2]], groups = c('species','polygon','time'), fun = sum)


          bind.save(RN.box,'RN_box',out.dir); rm('RN.box')
          bind.save(SN.box,'SN_box',out.dir); rm('SN.box')
        }

      }
    }

    #large file size accomodations: Pre-aggregate and then run normal routine

    if(large.file == T){

      for(i in 1:nrow(group.types)){

        if(load_fgs(param.ls$groups.file)$IsTurnedOn[which(load_fgs(param.ls$groups.file)$Name == group.types$species[i])] == 0){
          next()
        }

        #SN/RN
        if(group.types$group[i] == 'age'){

          #numbers

          rawdata.spp = get_rawdata(group = group.types$species[i],group.type = group.types$group[i])

          numbers[[i]] = agg_custom(data = rawdata.spp[[1]], groups = c('species','time'),fun = sum,agg.scale)
          numbers.age[[i]] = agg_custom(data = rawdata.spp[[1]], groups = c('species','agecl','time'), fun = sum,agg.scale)
          numbers.box[[i]] = agg_custom(data = rawdata.spp[[1]], groups = c('species','polygon','time'), fun = sum,agg.scale)

          RN.box[[i]] = agg_custom(data = rawdata.spp[[3]], groups = c('species','polygon','time'), fun = sum, agg.scale)
          SN.box[[i]] = agg_custom(data = rawdata.spp[[2]], groups = c('species','polygon','time'), fun = sum,agg.scale)
          RN.age[[i]] = agg_custom(data = rawdata.spp[[3]], groups = c('species','agecl','time'), fun = sum,agg.scale)
          SN.age[[i]] = agg_custom(data = rawdata.spp[[2]], groups = c('species','agecl','time'), fun = sum,agg.scale)

          SN.age.mean[[i]] = agg_custom(data = rawdata.spp[[2]], groups = c('species','time','agecl'), fun = mean,agg.scale)
          RN.age.mean[[i]] = agg_custom(data = rawdata.spp[[3]], groups = c('species','time','agecl'), fun = mean,agg.scale)

          #make length.age
          #Use mean RN+SN per age for each species, convert to weight, get length w/Von Bert.
          RN.SN.age = dplyr::left_join(RN.age[[i]],SN.age[[i]],by = c('species','agecl','time'))
          colnames(RN.SN.age) = c('species','agecl','time','RN','SN')

          biomass.age2 = dplyr::left_join(RN.SN.age, tempmat3[,2:4], by = c('species'='LongName'))
          biomass.age2$grams_N_Ind = (biomass.age2$RN+biomass.age2$SN)*5.7*20/1000
          biomass.age2$length_age = (as.numeric(as.character(biomass.age2$grams_N_Ind))/as.numeric(as.character(biomass.age2$li_a)))^(1/as.numeric(as.character(biomass.age2$li_b)))
          length.age[[i]] = biomass.age2[,c('species','agecl','time','length_age')]
          colnames(length.age[[i]])[4] = 'atoutput'

          spatial.biomass[[i]] =rename(rawdata.spp[[1]],nums = 'atoutput')%>%
            left_join(rename(rawdata.spp[[2]],sn = 'atoutput'))%>%
            left_join(rename(rawdata.spp[[3]],rn = 'atoutput'))%>%
            mutate(atoutput = nums*(sn+rn)*bio.conv) %>%
            select(species,agecl,polygon,layer,time,atoutput)

          # test=atlantistools::calculate_biomass_spatial(nums = rawdata.spp[[1]],
          #                                                               sn = rawdata.spp[[2]],
          #                                                               rn = rawdata.spp[[3]],
          #                                                               n = rawdata.spp[[4]],
          #                                                               vol_dz = NA,
          #                                                               bio_conv = NA,
          #                                                               bps = NA)
          spatial.numbers[[i]] = agg_custom(data = rawdata.spp[[1]], groups = c('species','agecl','polygon','layer','time'),fun= mean, agg.scale)%>%
            dplyr::rename(numbers = 'atoutput')

          spatial.biomass[[i]] = agg_custom(data = spatial.biomass[[i]],groups = c('species','agecl','polygon','layer','time'),fun = mean,agg.scale)

          #Aggregate spatial biomass based on stanzas
          spatial.biomass.stanza[[i]] = atlantistools::combine_ages(spatial.biomass[[i]],grp_col = 'species',agemat = data.age.mat)

        }else{
          blank.df = data.frame(species = fgs$LongName[which(fgs$Name == group.types$species[i])], polygon =NA, agecl = NA,layer = NA, time = 0, atoutput = NA)

          rawdata.spp = get_rawdata(group = group.types$species[i],group.type = group.types$group[i])

          spatial.biomass[[i]] = atlantistools::calculate_biomass_spatial(nums = blank.df,
                                                                          sn = blank.df,
                                                                          rn = blank.df,
                                                                          n = rawdata.spp[[1]],
                                                                          vol_dz = vol.dz,
                                                                          bio_conv = bio.conv,
                                                                          bps = bio.pools)
        }

        print(group.types$species[i])

        # if(spatial.overlap == F){
        #   rm(spatial.biomass)
        # }

        #spatial biomass

        rm(rawdata.spp)

        ##Biomass Groups and spatial overlap

        biomass[[i]] <- agg_custom(spatial.biomass[[i]],groups = c('species','time'),fun = sum,agg.scale)

        #Aggregate biomass by box
        biomass.box[[i]] = agg_custom(spatial.biomass[[i]], groups = c('species','polygon','time'), fun = sum,agg.scale)
        biomass.box.invert[[i]] = dplyr::filter(biomass.box[[i]], !(species %in% data.age.mat$species))

        #Aggregate by ageclass
        biomass.age[[i]] = dplyr::filter(spatial.biomass[[i]], species %in% data.age.mat$species)
        biomass.age[[i]] =agg_custom(biomass.age[[i]], groups = c('species','agecl','time'),fun = sum,agg.scale)

        biomass.age.invert[[i]] = dplyr::filter(spatial.biomass[[i]], !(species %in% data.age.mat$species))
        biomass.age.invert[[i]] = agg_custom(biomass.age.invert[[i]],groups = c('species','time'),fun = sum,agg.scale)


        gc()
      }

      # max age (mean) - biomass / numbers

      spatialNumbers = dplyr::bind_rows(spatial.numbers)
      # filter biomass for species with 10 cohorts and convert to kilograms
      spatialBiomass <- dplyr::bind_rows(spatial.biomass) %>%
        dplyr::filter(species %in% unique(spatialNumbers$species)) %>%
        dplyr::rename(biomass = atoutput) %>%
        dplyr::mutate(biomass = 1E6*biomass)

      # join numbers with biomass and calculate mean weight of an individivual in age, polygon, layer, time
      weight <- spatialNumbers %>% dplyr::left_join(.,spatialBiomass,by = c("species", "agecl", "polygon", "layer", "time")) %>%
        dplyr::filter(!is.na(biomass)) %>%
        dplyr::mutate(meanWeight = biomass/numbers)

      weight <- weight %>%
        dplyr::group_by(species, polygon,layer, agecl, time) %>%
        dplyr::summarise(maxMeanWeight = max(meanWeight),.groups="drop")
      # tictoc::toc()
    }
  }

  # PROD netCDF objectsspatialBiomass# PROD netCDF objects -----------------------------------------------------


  if(plot.growth.cons|plot.diet|process.all|plot.all){

    #Read in raw untransformed data from prod.nc file

    #Set up biological variable groups
    age.vars.prod = c('Eat','Growth')
    bp.vars.prod = 'Grazing'

    #Setup spaces for objects
    eat.age = list()
    grazing = list()
    growth.age = list()
    growth.rel.init = list()
    bio.consumed = list()

    if(large.file == F){
      vars = list('Eat','Grazing','Growth')
      group.types = list(groups.age,groups.bp,groups.age)
      rawdata.prod = Map(atlantistools::load_nc,
                         select_variable = vars,
                         select_groups = group.types,
                         MoreArgs = list(nc = param.ls$prod.nc, bps = bio.pools,
                                         fgs = param.ls$groups.file, prm_run = param.ls$run.prm,
                                         bboxes = bboxes))

      ##Recreate bio.consumed manually without full_join
      data_eat = dplyr::bind_rows(rawdata.prod[[1]], rawdata.prod[[2]])
      ts_eat = sort(unique(data_eat$time))
      ts_dm = sort(unique(data.dietcheck$time))
      matching = sum(ts_eat %in% ts_dm)/length(ts_eat)
      boxvol = atlantistools::agg_data(vol, groups = c('polygon','time'),out = 'vol', fun = sum)

      pred.names = unique(data.dietcheck$pred)
      bio.consumed = list()

      for(i in 1:length(pred.names)){
        consumed_bio = data_eat %>%
          dplyr::filter(species == pred.names[i])%>%
          dplyr::left_join(boxvol, by = c('polygon','time')) %>%
          dplyr::mutate_(.dots = stats::setNames(list(~atoutput * vol), "atoutput")) %>%
          dplyr::mutate_(.dots = stats::setNames(list(~atoutput * bio.conv), "atoutput"))%>%
          dplyr::full_join(dplyr::filter(data.dietcheck,pred == pred.names[i]),by = c(species = "pred","time", "agecl"))%>%
          dplyr::filter_(~time %in% ts_eat) %>%
          dplyr::rename_(.dots = c(pred = "species"))
        bio.consumed[[i]] = consumed_bio %>%
          dplyr::filter_(~!is.na(atoutput.x)) %>%
          dplyr::filter_(~!is.na(atoutput.y)) %>%
          dplyr::mutate_(.dots = stats::setNames(list(~atoutput.x * atoutput.y), "atoutput")) %>%
          dplyr::select_(.dots = names(.)[!names(.) %in% c("atoutput.x", "vol", "atoutput.y")])%>%
          dplyr::ungroup()

        print(pred.names[i])
        gc()
      }
      bio.consumed = dplyr::bind_rows(bio.consumed)
      saveRDS(bio.consumed,file.path(out.dir,'biomass_consumed.rds'))
      rm(bio.consumed)
      gc()
      bio.consumed = atlantistools::calculate_consumed_biomass(eat = rawdata.prod[[1]],
                                                               grazing = rawdata.prod[[2]],
                                                               dm = data.dietcheck,
                                                               vol = vol,
                                                               bio_conv = bio.conv)

      eat.age = atlantistools::agg_data(data = rawdata.prod[[1]], groups = c('species','time','agecl'),fun = mean)
      grazing = atlantistools::agg_data(data = rawdata.prod[[2]], groups = c('species','time'),fun = mean)
      growth.age =  atlantistools::agg_data(data = rawdata.prod[[3]], groups = c('species','time','agecl'),fun = mean)


    }else{

      for(i in 1:nrow(group.types)){

        if(load_fgs(param.ls$groups.file)$IsTurnedOn[which(load_fgs(param.ls$groups.file)$Name == group.types$species[i])] == 0){
          next()
        }

        if(group.types$group[i] == 'age'){
          prod.vars = age.vars.prod
        }else{
          prod.vars = bp.vars.prod
        }

        ## Process PROD Data by spp
        proddata.spp = Map(load_nc_temp,
                           select_variable = prod.vars,
                           select_groups = group.types$species[i],
                           MoreArgs = list(nc = param.ls$prod.nc, bps = bio.pools,
                                           fgs = param.ls$groups.file, prm_run = param.ls$run.prm,
                                           bboxes = bboxes))

        if(is.null(nrow(proddata.spp[[1]]))){

          print(i)
          next()
        }else if(group.types$group[i] != 'age'){
          grazing[[i]] =  agg_custom(proddata.spp[[1]],groups = c('species','agecl','polygon','time'),fun = mean ,agg.scale) %>%
            filter(time %in% unique(data.dietcheck$time))
        }else{
          eat.age[[i]] = agg_custom(proddata.spp[[1]],groups = c('species','agecl','polygon','time'),fun = mean ,agg.scale) %>%
            filter(time %in% unique(data.dietcheck$time))
          growth.age[[i]] = agg_custom(proddata.spp[[2]],groups = c('species','agecl','polygon','time'),fun = mean ,agg.scale) %>%
            filter(time %in% unique(data.dietcheck$time))
        }

        print(i)
      }

      grazing = dplyr::bind_rows(grazing)
      eat.age = dplyr::bind_rows(eat.age)

      vol.temp = dplyr::filter(vol,time %in% unique(data.dietcheck$time))

      bio.consumed = calculate_consumed_biomass(eat = eat.age,
                                                grazing = grazing,
                                                dm = data.dietcheck,
                                                vol = vol.temp,
                                                bio_conv = bio.conv)

    }


    growth.age = dplyr::bind_rows(growth.age)
    growth.age =  atlantistools::agg_data(data = growth.age, groups = c('species','time','agecl'),fun = mean)
    #make growth.rel.init

    growth.rel.init = dplyr::left_join(growth.age,growth.required,by = c("species", "agecl"))
    growth.rel.init = dplyr::mutate(growth.rel.init, gr_rel = (atoutput - growth_req) / growth_req)

    which.inf = which(growth.rel.init$gr_rel == 'Inf')
    growth.rel.init$gr_rel[which.inf] = 1

    saveRDS(growth.rel.init,file.path(out.dir,'growth_rel_init.rds'))
    saveRDS(growth.age,file.path(out.dir,'growth_age.rds'))

    rm(growth.age,growth.rel.init)


    #aggregate other prod objects
    grazing = atlantistools::agg_data(data = grazing, groups = c('species','time'),fun = mean)
    eat.age = atlantistools::agg_data(data = eat.age, groups = c('species','time','agecl'),fun = mean)


    #write to file and remove
    saveRDS(grazing,file.path(out.dir,'grazing.rds'))
    saveRDS(eat.age,file.path(out.dir,'eat_age.rds'))



    rm(grazing,eat.age)

  }

  # Do Recruitment ----------------------------------------------------------

  if(plot.recruits|process.all|plot.all){
    ssb.recruits = atlantistools::load_rec(yoy = param.ls$yoy, ssb = param.ls$ssb,prm_biol = param.ls$biol.prm )
    saveRDS(ssb.recruits,file.path(out.dir,'ssb_recruits.rds'))
    rm(ssb.recruits)
  }

  # Do catch -------------------------------------------------------------------

  if(plot.catch|plot.spatial.catch|process.all|plot.all){
    catch = atlantistools::load_nc(param.ls$catch,
                                   fgs = param.ls$groups.file,
                                   bps = bio.pools,
                                   select_groups = group.names,
                                   select_variable = 'Catch',
                                   prm_run = param.ls$run.prm,
                                   bboxes = bboxes,check_acronyms = F)
    totcatch = atlantistools::agg_data(catch, groups = c('species','time','agecl'), fun = sum)
    catchmt <- atlantisom::load_catch(dir=atl.dir,file_catch=paste0(run.prefix,"Catch.txt"),fgs = groups.data) %>%
      dplyr::select(species,time,atoutput) %>%
      dplyr::mutate(time = time/365)

    saveRDS(catch,file.path(out.dir,'catch.rds'))
    saveRDS(totcatch,file.path(out.dir,'totcatch.rds'))
    saveRDS(catchmt,file.path(out.dir,'catchmt.rds'))

    rm(catch,totcatch,catchmt)
  }

  if(plot.catch.fleet|process.all|plot.all){

    catch.fleet = process_catch_fleet(fishery.prm = param.ls$fishery.prm,
                                      catch = param.ls$catch,
                                      groups.file = param.ls$groups.file)

    saveRDS(catch.fleet,file.path(out.dir,'catch_fleet.rds'))

  }

  # Do mortality -------------------------------------------------------------------

  if(plot.mortality|process.all|plot.all){
    mortality <- atlantistools::load_mort(param.ls$mort,prm_run=param.ls$run.prm,fgs=param.ls$groups.file,convert_names = T) #%>%
    #   dplyr::group_by(species,time) %>%
    #   dplyr::summarise(atoutput = atoutput[source == "F"]/atoutput[source == "M"],.groups="drop") %>%
    #   dplyr::mutate(atoutput = ifelse(is.infinite(atoutput),NA,atoutput))
    saveRDS(mortality,file.path(out.dir,'mort.rds'))

    specificMortality <- atlantistools::load_spec_mort(param.ls$specificmort,prm_run=param.ls$run.prm,fgs=param.ls$groups.file,convert_names = T,removeZeros = F)
    saveRDS(specificMortality,file.path(out.dir,'specificmort.rds'))
  }
}
