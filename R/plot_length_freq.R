#' Creates plots comparing the length frequency distribution of model catch and biomass against data
#'
#' @param param.dir string. Path to location of atlantis parameter files
#' @param run.dir character vector of path to location of atlantis output files
#' @param run.name character vector of run name corresponding to run.dir
#' @param catch.ref dataframe  that contains reference catch data with format (Code|len.lower|len.upper|cum.prob)
#' @param survey.ref dataframe that contains reference biomass data  with format (Code|len.lower|len.upper|cum.prob)
#' @param ref.years numeric vector with first and last year (years from start) for comparison period
#' @param out.dir string. path to desired location of post-processed output
#' @param out.name string. name for output file prefix
#' @param param.ls list generated from get_atl_paramfiles()
#' @param speciesCodes character vector. List of species to make plots for. Default = NULL (All species)
#' @param fleetCodes character vector. List of fleets to make plots for., Default = NULL (All fleets)
#'
#'
#' @importFrom magrittr "%>%"
#'
#' @return PDF of all fleet x species combination cumulative length frequency distributions
#'
#' Author: Joseph Caracappa
#'
#' @export
#'

plot_length_freq = function(param.dir, run.dir, run.name, catch.ref, survey.ref,ref.years,bin.width = 1, out.dir,out.name, param.ls,speciesCodes = NULL, fleetCodes = NULL){

  fgs.orig = read.csv(param.ls$groups.file)
  fgs.fished = fgs.orig %>%
    dplyr::filter(isFished == 1 & IsTurnedOn == 1 & NumCohorts >2)%>%
    dplyr::arrange(LongName)

  fleet.param = read.csv(param.ls$fishery.prm)
  if(!is.null(fleetCodes)){
    fleet.param = filter(fleet.param,Code %in% fleetCodes)
  }
  fleet.names = fleet.param$Code

  #Get selectivity parameters from harvest.prm
  harvest.lines = readLines(param.ls$harvest.prm)

  sel.prm.df = data.frame(fleet.name = fleet.names,sel.lsm = NA, sel.b = NA,selcurve = NA)
  for(f in 1:length(fleet.names)){
    #Get YYYsel_lsm
    sel.lsm.line = grep(paste0(fleet.names[f],'sel_lsm'),harvest.lines,value =T)
    sel.lsm.val = strsplit(sel.lsm.line,'sel_lsm| |\t')[[1]]
    sel.lsm.val = sel.lsm.val[sel.lsm.val != '']
    sel.lsm.val = sel.lsm.val[length(sel.lsm.val)]
    sel.prm.df$sel.lsm[f] = as.numeric(sel.lsm.val)

    #Get YYYsel_b
    sel.b.line = grep(paste0(fleet.names[f],'sel_b\\b'),harvest.lines,value =T )
    sel.b.val = strsplit(sel.b.line,'sel_b| |\t')[[1]]
    sel.b.val = sel.b.val[sel.b.val != '']
    sel.b.val = sel.b.val[length(sel.b.val)]
    sel.prm.df$sel.b[f] = as.numeric(sel.b.val)

    selcurve.line = grep(paste0(fleet.names[f],'_selcurve'),harvest.lines,value =T)
    selcurve.val = strsplit(selcurve.line,' |\t')[[1]]
    selcurve.val = selcurve.val[selcurve.val != ''][2]
    sel.prm.df$selcurve[f] = as.numeric(selcurve.val)
  }

  #Get fleet targets
  fleet.target.ls = list()
  for(p in 1:length(fleet.names)){

    target.line = grep(paste0('target_',fleet.names[p]),harvest.lines)
    target.val = as.numeric(strsplit(harvest.lines[target.line+1],' |\t')[[1]])


    fleet.target.ls[[p]] = data.frame(fleet.name = fleet.names[p], Code = fgs.orig$Code,target = target.val)

  }
  fleet.target.df = bind_rows(fleet.target.ls)

  #Get fleet selcurve


  #Get model output RN and SN
  model.nc = ncdf4::nc_open(param.ls$main.nc)
  model.nc.names = names(model.nc$var)
  # model.rn = readRDS(paste0(run.dir,'/Post_Processed/Data/RN_box.rds'))
  # model.sn = readRDS(paste0(run.dir,'/Post_Processed/Data/SN_box.rds'))
  # model.sn = atlantisom::load_nc(dir = NULL,
  #                                file_nc = param.ls$main.nc,
  #                                fgs = atlantisom::load_fgs(param.dir, 'neus_groups.csv'),
  #                                bps = atlantisom::load_bps(dir = NULL, file_init = param.ls$init.file, fgs = param.ls$groups.file),
  #                                select_variable = 'StructN',
  #                                select_groups = fgs$Name)

  #Get model output catch.fleet
  model.catch = readRDS(paste0(run.dir,'/Post_Processed/Data/catch_fleet.rds'))
  catch.nc = ncdf4::nc_open(paste0(run.dir,'/neus_outputCATCH.nc'))
  model.time = catch.nc$dim$t$vals/86400/365
  which.time = which(model.time >= ref.years[1] & model.time <= ref.years[2])
  # catch.nc.names = names(catch.nc$var)
  # grep('Cod',catch.nc.names,value =T)
  # dim(ncdf4::ncvar_get(catch.nc,'Cod1_Catch'))


  #Read bio.prm lines
  bio.lines = readLines(param.ls$biol.prm)

  #get wetdry conversion
  wetdry = grep('wetdry',bio.lines,value =T)
  wetdry = strsplit(wetdry,'\t| ')[[1]]
  wetdry = as.numeric(wetdry[wetdry != ''][2])

  #Get CN conversion
  CN = grep('X_CN',bio.lines,value =T)
  CN = strsplit(CN,'\t| ')[[1]]
  CN = as.numeric(CN[CN != ''][2])

  catch.ref.names = sort(unique(catch.ref$Code))
  survey.ref.names = sort(unique(survey.ref$Code))

  plot.col.df = data.frame(color = c('red2','blue2','purple2','green2','black'), Var = c('Model Catch','Model Population','Observed Catch','Observed Population','Parameter Selectivity'))

  len.count.ls = list()
  pdf(paste0(out.dir,out.name,'.pdf'))
  for(s in 1:length(fgs.fished$Code)){

    #Get model length-weight conversion
    spp.lia = grep(paste0('li_a_',fgs.fished$Code[s]),bio.lines,value =T)
    spp.lia = strsplit(spp.lia,paste0('li_a_|',fgs.fished$Code[s],'|\t'))[[1]]
    spp.lia = as.numeric(spp.lia[spp.lia != ''][1])
    spp.lib = grep(paste0('li_b_',fgs.fished$Code[s]),bio.lines,value =T)
    spp.lib = strsplit(spp.lib,paste0('li_b_|',fgs.fished$Code[s],'|\t'))[[1]]
    spp.lib = as.numeric(spp.lib[spp.lib != ''][1])

    #Get model population

    spp.ages = fgs.fished$NumCohorts[s]

    spp.len.ls = list()
    for(a in 1:spp.ages){
      spp.sn.age = ncdf4::ncvar_get(model.nc,paste0(fgs.fished$Name[s],a,'_StructN'))[,,which.time]
      spp.rn.age = ncdf4::ncvar_get(model.nc,paste0(fgs.fished$Name[s],a,'_ResN'))[,,which.time]
      spp.num.age = ncdf4::ncvar_get(model.nc,paste0(fgs.fished$Name[s],a,'_Nums'))[,,which.time]
      spp.catch.age = ncdf4::ncvar_get(catch.nc,paste0(fgs.fished$Name[s],a,'_Catch'))[,which.time]

      spp.wgt.age = (spp.sn.age + spp.rn.age)* wetdry * CN /1000
      spp.wgt.age[spp.wgt.age == 0] = NA
      spp.wgt.age.mean = apply(spp.wgt.age,c(2,3),mean,na.rm=T)
      spp.len.age.mean = (spp.wgt.age.mean/spp.lia)^(1/spp.lib)
      spp.num.age[spp.num.age == 0] = NA
      spp.num.age.sum = apply(spp.num.age,c(2,3),sum,na.rm=T)

      spp.len.age = (spp.wgt.age/spp.lia)^(1/spp.lib)
      # spp.len.age = unlist(spp.len.age)
      # spp.len.age = spp.len.age[!is.na(spp.len.age)]

      len.bins = seq(floor(min(spp.len.age,na.rm=T)),ceiling(max(spp.len.age,na.rm=T)),bin.width)

      spp.len.age.mean.bin =ceiling(spp.len.age.mean/bin.width)*bin.width
      # spp.len.age.mean.fact = matrix(as.numeric(factor(spp.len.age.mean.bin)),nrow = nrow(spp.len.age.mean), ncol = ncol(spp.len.age.mean))

      spp.len.age.bin =ceiling(spp.len.age/bin.width)*bin.width
      # spp.len.age.fact = array(as.numeric(factor(spp.len.age.bin)),dim = dim(spp.len.age))

      spp.len.bin.df = data.frame(Code = fgs.orig$Code[s],agecl = a,bin.width = bin.width, len.lower = len.bins, len.upper = len.bins + bin.width,catch.n = NA, bio.n = NA)

      for(l in 1:length(len.bins)){
        spp.len.age.mean.l = spp.len.age.mean.bin/len.bins[l]
        spp.len.age.mean.l[spp.len.age.mean.l!=1] = 0
        spp.len.bin.df$catch.n[l] = sum(round(spp.catch.age * spp.len.age.mean.l, 0))
        spp.len.bin.df$bio.n[l] = sum(round(spp.num.age.sum * spp.len.age.mean.l, 0))
      }


      # as.data.frame(table(cut(c(spp.len.age.mean),breaks = len.bins))) %>%
      #   mutate(len.lower = spp.len.seq[-length(spp.len.seq)],
      #          len.upper = spp.len.seq[-1],
      #          rel.freq = Freq/sum(Freq),
      #          cum.prob = cumsum(rel.freq),
      #          Code = spp.names[i])
      #
      # spp.catch.prop.age = spp.catch.age/sum(spp.catch.age)

      spp.len.ls[[a]] = spp.len.bin.df
    }
    spp.len.df =  dplyr::bind_rows(spp.len.ls) %>%
      dplyr::group_by(Code,bin.width, len.lower,len.upper)%>%
      dplyr::summarise(catch.n = sum(catch.n,na.rm=T),
                    bio.n = sum(bio.n,na.rm=T))%>%
      dplyr::group_by(Code)%>%
      dplyr::mutate(catch.n.tot = sum(catch.n),
                    bio.n.tot = sum(bio.n))%>%
      dplyr::ungroup()%>%
      dplyr::mutate(catch.n.freq = catch.n/catch.n.tot,
                    bio.n.freq = bio.n/bio.n.tot,
                    catch.n.cum.prop = cumsum(catch.n.freq),
                    bio.n.cum.prop= cumsum(bio.n.freq))%>%
      dplyr::select(Code,bin.width,len.lower,len.upper, catch.n.cum.prop,bio.n.cum.prop)%>%
      dplyr::rename('Model Population' = 'bio.n.cum.prop',
                    'Model Catch' = 'catch.n.cum.prop')%>%
      tidyr::gather(Var,Value,-Code,-bin.width,-len.lower,-len.upper)

    #Get ref catch
    catch.ref.plot = catch.ref%>%
      dplyr::filter(Code == fgs.fished$Code[s])%>%
      dplyr::select(Code,len.lower,len.upper,cum.prob)%>%
      dplyr::mutate(bin.width = bin.width)%>%
      dplyr::rename('Observed Catch'= 'cum.prob')%>%
      tidyr::gather(Var,Value,-Code,-bin.width,-len.lower,-len.upper)

    survey.ref.plot = survey.ref %>%
      dplyr::filter(Code == fgs.fished$Code[s])%>%
      dplyr::select(Code,len.lower,len.upper,cum.prob)%>%
      dplyr::mutate(bin.width = bin.width)%>%
      dplyr::rename('Observed Population' = 'cum.prob')%>%
      tidyr::gather(Var,Value,-Code,-bin.width,-len.lower,-len.upper)

    data.plot = spp.len.df %>%
      bind_rows(catch.ref.plot)%>%
      bind_rows(survey.ref.plot)


    #Find which fleet catches spp
    #check if selcurve == 2
    which.fleet.target = fleet.target.df %>%
      dplyr::filter(Code == fgs.fished$Code[s] & target == 1)
    if(nrow(which.fleet.target) !=0){

      which.sel = sel.prm.df%>%
        dplyr::filter(sel.prm.df$fleet.name %in% which.fleet.target$fleet.name & selcurve == 2)

      if(nrow(which.sel)!=0){
      bin.max = ceiling((log((1/0.999)-1)/(-1*which.sel$sel.b[1]))+which.sel$sel.lsm[1])
      bin.seq = seq(0,bin.max,bin.width)
      param.len.df = data.frame(Code = fgs.fished$Code[s],
                                bin.width = bin.width,
                                len.lower = bin.seq[-length(bin.seq)],
                                len.upper = bin.seq[-1],
                                Var = 'Parameter Selectivity',
                                Value = 1/(1+exp(-which.sel$sel.b[1]*(bin.seq[-length(bin.seq)]-which.sel$sel.lsm[1])))


      )

      data.plot = data.plot %>%
        dplyr::bind_rows(param.len.df)

      }
    }

    data.plot2= data.plot %>%
      dplyr::left_join(fgs.fished)%>%
      dplyr::left_join(plot.col.df)%>%
      dplyr::mutate(Var = factor(Var))


    plot.cols = plot.col.df$color[match(levels(data.plot2$Var),plot.col.df$Var)]
    plot.labs = plot.col.df$Var[match(levels(data.plot2$Var),plot.col.df$Var)]
    #Get ref survey
    p = ggplot2::ggplot(data = data.plot2, ggplot2::aes( x = len.lower, y = Value, color = Var))+
      ggplot2::scale_color_manual(name = 'Series',values = plot.cols, labels = plot.labs)+
      ggplot2::geom_line()+
      ggplot2::ggtitle(fgs.fished$LongName[s])+
      ggplot2::xlab('Length')+
      ggplot2::ylab('Proportion Below Length')+
      ggplot2::theme(legend.position = 'bottom')

    gridExtra::grid.arrange(p)
  }
  dev.off()
}
