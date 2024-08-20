#' Creates post-processing output for Atlantis
#'
#' @param param.dir string. Path to location of atlantis parameter files
#' @param run.dirs character vector of paths to location of atlantis output files
#' @param run.names character vector of run names corresponding to run.dirs
#' @param ref.data dataframe  that contains reference spatial data with format (polygon|species|var.name|statistic|ref.value)
#' @param init.data dataframe that contains initial spatial data  with format (polygon|species|var.name|statistic|init.value)
#' @param out.dir string. path to desired location of post-processed output
#' @param out.name string. name for output file prefix
#' @param param.ls list generated from get_atl_paramfiles()
#' @param data.type which type of data is being compared: 'proportion','value'
#' @param comparison.type which type of comparison should be made: 'difference','scalar'
#' @param ref.years numeric vector with first and last year (years from start) for comparison period
#' @param plot logical. do you want to generate a plot
#'
#'
#' @importFrom magrittr "%>%"
#'
#' @return Either saves an R object or returns a list called "result"
#'
#' Author: Joseph Caracappa
#'
#' @export


compare_spatial_vars = function(param.dir,
                                run.dirs,
                                run.names,
                                ref.data,
                                init.data,
                                out.dir,
                                out.name,
                                param.ls,
                                data.type = 'proportion',
                                comparison.type = 'difference',
                                ref.years,
                                plot = T){
  #Pull in reference data

    # ref.data = read.csv(ref.file, as.ist =T)%>%
  ref.data = ref.data %>%
      dplyr::mutate(polygon = as.factor(polygon))

  init.data = init.data %>%
    dplyr::mutate(polygon = as.factor(polygon))

    ##Check if ref.data$statistic == data.type

    if(ref.data$statistic[1] != data.type){
      warning('Reference data "statistics" must be the same as data.type argument')
      stop()
    }

  #Pull in spatial data

    boxes = atlantistools::convert_bgm(param.ls$bgm.file)%>%
      dplyr::mutate(polygon = as.factor(polygon))

    # ggplot2::ggplot(boxes,ggplot2::aes(x = long, y = lat, fill = polygon,group = polygon))+
    #   ggplot2::geom_polygon()

  run.data.all.ls = list()
  #Loop through each run.dirs
    for(i in 1:length(run.dirs)){

    #Pull in run data

      if(tolower(ref.data$var.name[1]) == 'biomass'){
        run.data = readRDS(paste0(run.dirs[i],'/Post_Processed/Data/biomass_box.rds'))
        run.data.yr = run.data%>%
          dplyr::mutate(polygon = as.factor(polygon))%>%
          ##Calculate proportion by box over ref.years
          dplyr::filter( time>= ref.years[1] & time <= ref.years[2]) %>%
          ##Calculate mean by box over ref.years
          dplyr::group_by(species,polygon)%>%
          dplyr::summarise(atoutput = mean(atoutput,na.rm=T))%>%
          dplyr::group_by(species)%>%
          dplyr::mutate(model.total = sum(atoutput))%>%
          dplyr::ungroup()
      }else if(tolower(ref.data$var.name[1]) == 'numbers'){
        run.data = readRDS(paste0(run.dirs[i],'/Post_Processed/Data/numbers_box.rds'))
        run.data.yr = run.data%>%
          dplyr::mutate(polygon = as.factor(polygon))%>%
          ##Calculate proportion by box over ref.years
          dplyr::filter( time>= ref.years[1] & time <= ref.years[2]) %>%
          ##Calculate mean by box over ref.years
          dplyr::group_by(species,polygon)%>%
          dplyr::summarise(atoutput = mean(atoutput,na.rm=T))%>%
          dplyr::group_by(species)%>%
          dplyr::mutate(model.total = sum(atoutput))%>%
          dplyr::ungroup()
      }else if(tolower(ref.data$var.name[1]) == 'catch.total'){
        run.data = readRDS(paste0(run.dirs[i],'/Post_Processed/Data/catch.rds'))
        run.data.yr = run.data%>%
          dplyr::mutate(polygon = as.factor(polygon))%>%
          ##Calculate proportion by box over ref.years
          dplyr::filter( time>= ref.years[1] & time <= ref.years[2]) %>%
          ##Calculate mean by box over ref.years
          dplyr::group_by(species,agecl,polygon)%>%
          dplyr::summarise(atoutput = mean(atoutput,na.rm=T))%>%
          dplyr::group_by(species,polygon)%>%
          dplyr::summarise(atoutput = mean(atoutput,na.rm=T))%>%
          dplyr::group_by(species)%>%
          dplyr::mutate(model.total = sum(atoutput))%>%
          dplyr::ungroup()
      }else if(tolower(ref.data$var.name[1]) == 'catch_fleet'){
        run.data = readRDS(paste0(run.dirs[i],'/Post_Processed/Data/catch_fleet.rds'))
        run.data.yr = run.data%>%
          dplyr::mutate(polygon = as.factor(polygon))%>%
          ##Calculate proportion by box over ref.years
          dplyr::filter( time>= ref.years[1] & time <= ref.years[2]) %>%
          ##Calculate mean by box over ref.years
          dplyr::group_by(species,fleet,polygon)%>%
          dplyr::summarise(atoutput = mean(atoutput,na.rm=T))%>%
          dplyr::group_by(species,fleet)%>%
          dplyr::mutate(model.total = sum(atoutput))%>%
          dplyr::ungroup()

      }

      run.data.yr$polygon = as.factor(run.data.yr$polygon)

      #Calculate proportion or value for model.val
      if(data.type == 'value'){
        run.data.yr = run.data.yr %>%
          dplyr::mutate(model.val = atoutput)
      }else if(data.type == 'proportion'){
        run.data.yr = run.data.yr %>%
          dplyr::mutate(model.val = atoutput /model.total)
      }else{
        warning("data.type must be either 'absolute' or 'proportion'")
      }

      #Join with ref.data
      if(tolower(ref.data$var.name[1]) %in% c('catch','catch_fleet')){

        init.data2 = init.data %>%
          dplyr::select(species,polygon,init.value)%>%
          dplyr::mutate(polygon = as.factor(polygon))
        run.data.yr = run.data.yr %>%
          dplyr::left_join(ref.data)%>%
          dplyr::left_join(init.data2)%>%
          dplyr::mutate(var.name = ref.data$var.name[1],
                        statistic = ref.data$statistic[1])

      }else{
        run.data.yr = run.data.yr %>%
          dplyr::left_join(ref.data)%>%
          dplyr::left_join(init.data)

      }

    #Calculate comparison.type

      #Do difference
      if(comparison.type == 'difference'){
        run.data.compare = run.data.yr %>%
          dplyr::mutate(compare.val = model.val-ref.value)
      #Do scalars
      }else if(comparison.type == 'scalar'){
        run.data.compare = run.data.yr %>%
          dplyr::mutate(compare.val = (model.val-ref.value)/ref.value)
      }else{
        warning('comparison.type must be either "difference" or "scalar"')
      }

      run.data.all.ls[[i]] = run.data.compare %>%
        dplyr::mutate(run.name = run.names[i] )
    }

  #Join to final dataframe with (species|polygon|atoutput|model.val|var.name|statistic|ref.value|compare.val|run.name|data.type|comparison.type)
  run.data.all = dplyr::bind_rows(run.data.all.ls)%>%
    dplyr::mutate(data.type = data.type,
                  comparison.type = comparison.type)

  # out.name = paste(run.names,collapse = '_')
  saveRDS(run.data.all,paste0(out.dir,out.name,'.rds'))

  if(plot == T){
    #If plotting create PDF with the following plots

    fgs = read.csv(param.ls$groups.file)
    spp.names = sort(unique(run.data.all$species))
    spp.codes = fgs$Code[match(spp.names,fgs$LongName)]
    box.id = sort(unique(boxes$polygon))


    #Loop over species with 3+ plots per page (ref, model, comparisons)
    if(tolower(ref.data$var.name[1]) %in% c('catch','biomass','numbers')){
      pdf(paste0(out.dir,out.name,'.pdf'), width =6+(3*length(run.names)), height =10)
      for(s in 1:length(spp.names)){

        #Get species data

        #species ref box
        ref.data.box =boxes %>%
          dplyr::left_join(filter(ref.data,species == spp.names[s]))

        init.data.box = boxes %>%
          dplyr::left_join(filter(init.data,species == spp.names[s]))

        plot.data.ls = list()
        for(r in 1:length(run.names)){

          plot.data.spp = run.data.all %>%
            dplyr::filter(species == spp.names[s] & run.name == run.names[r])

          #get missing boxes
          missing.df = data.frame(species = spp.names[s],
                                  polygon = box.id[which(!(box.id %in% plot.data.spp$polygon))],
                                  atoutput = NA,model.total = NA,model.val = NA,
                                  var.name = plot.data.spp$var.name[1],
                                  statistic = plot.data.spp$statistic[1],
                                  compare.val = NA,
                                  run.name = run.names[r],
                                  data.type = data.type,
                                  comparison.type = comparison.type)%>%
            dplyr::left_join(ref.data)%>%
            dplyr::left_join(init.data)

          plot.data.spp = dplyr::bind_rows(plot.data.spp,missing.df)%>%
            dplyr::mutate(polygon = as.factor(polygon))

          plot.data.ls[[r]] = boxes %>%
            dplyr::left_join(plot.data.spp, by = 'polygon')

        }
        plot.data = dplyr::bind_rows(plot.data.ls)

        plot.spp.ls = list()
        #1: Maps of ref values
        p1 = ggplot2::ggplot(ref.data.box,ggplot2::aes(x= long, y = lat, group = polygon, fill = ref.value))+
          ggplot2::geom_polygon(color = 'black')+
          ggplot2::ggtitle('Reference Value')+
          ggplot2::scale_fill_gradient(low = 'white',high =  'forestgreen',name = paste0('reference\n',data.type))+
          ggplot2::theme_bw()+
          ggplot2::theme(
            plot.title = ggplot2::element_text(hjust = 0.5)
          )

        #2: Map of init values
        p2 = ggplot2::ggplot(init.data.box,ggplot2::aes(x = long,y = lat, group = polygon, fill = init.value))+
          ggplot2::geom_polygon(color = 'black')+
          ggplot2::ggtitle('Initial Conditions')+
          ggplot2::scale_fill_gradient(low = 'white',high =  'forestgreen',name = paste0('initial\n',data.type))+
          ggplot2::theme_bw()+
          ggplot2::theme(
            plot.title = ggplot2::element_text(hjust = 0.5)
          )

        #3: Maps of run values

        p3 = ggplot2::ggplot(plot.data,ggplot2::aes(x = long,y = lat, group = polygon, fill = model.val))+
          ggplot2::geom_polygon(color = 'black')+
          ggplot2::facet_wrap(~run.name)+
          ggplot2::ggtitle('Model Value')+
          ggplot2::scale_fill_gradient(low = 'white',high = 'forestgreen',name = paste0('model\n',data.type))+
          ggplot2::theme_bw()+
          ggplot2::theme(
            plot.title = ggplot2::element_text(hjust = 0.5)
          )

        #4: Maps of comparisons between runs and ref values
        p4 =ggplot2::ggplot(plot.data,ggplot2::aes(x = long,y = lat, group = polygon, fill = compare.val))+
          ggplot2::geom_polygon(color = 'black')+
          ggplot2::facet_wrap(~run.name)+
          ggplot2::ggtitle(paste0('Comparison: ',comparison.type))+
          ggplot2::scale_fill_gradient2(low = 'red4',mid = 'white',high = 'blue4',name = paste0(data.type,'\n',comparison.type),)+
          ggplot2::theme_bw()+
          ggplot2::theme(
            plot.title = ggplot2::element_text(hjust = 0.5)
          )

        plot.layout = matrix(c(1,1,rep(3,length(run.names)+1),2,2,rep(4,length(run.names)+1)),byrow = T,nrow =2)
        gridExtra::grid.arrange(p1,p2,p3,p4,nrow = 2,layout_matrix = plot.layout,top = paste0(spp.names[s]," (",spp.codes[s],"): ",ref.data$var.name[1],' ',data.type))

      }
      dev.off()

    #If Catch Fleet

    }else{

      fisheries = read.csv(param.ls$fishery.prm)

      fleet.combs = run.data.all%>%
        dplyr::distinct(species,fleet)

      pdf(paste0(out.dir,out.name,'.pdf'), width =6+(3*length(run.names)), height =10)
      for(sf in 1:nrow(fleet.combs)){

        #Get species data

        #species ref box
        ref.data.box =boxes %>%
          dplyr::left_join(filter(ref.data,species == fleet.combs$species[sf] & fleet == fleet.combs$fleet[sf]))

        init.data.box = boxes %>%
          dplyr::left_join(filter(init.data,species == fleet.combs$species[sf]))

        plot.data.ls = list()
        for(r in 1:length(run.names)){

          plot.data.spp = run.data.all %>%
            dplyr::filter(species == fleet.combs$species[sf] & fleet == fleet.combs$fleet[sf] & run.name == run.names[r])

          #get missing boxes
          missing.box = box.id[which(!(box.id %in% plot.data.spp$polygon))]
          if(length(missing.box)>0){
            missing.df = data.frame(species = fleet.combs$species[sf],
                                    fleet = fleet.combs$fleet[sf],
                                    polygon = box.id[which(!(box.id %in% plot.data.spp$polygon))],
                                    atoutput = NA,model.total = NA,model.val = NA,
                                    var.name = plot.data.spp$var.name[1],
                                    statistic = plot.data.spp$statistic[1],
                                    compare.val = NA,
                                    run.name = run.names[r],
                                    data.type = data.type,
                                    comparison.type = comparison.type)%>%
              dplyr::left_join(ref.data)%>%
              dplyr::left_join(init.data)

            plot.data.spp = dplyr::bind_rows(plot.data.spp,missing.df)%>%
              dplyr::mutate(polygon = as.factor(polygon))
          }else{
            plot.data.spp = plot.data.spp %>%
              dplyr::mutate(polygon = as.factor(polygon))
          }

          plot.data.ls[[r]] = boxes %>%
            dplyr::left_join(plot.data.spp, by = 'polygon')

        }
        plot.data = dplyr::bind_rows(plot.data.ls)

        plot.spp.ls = list()
        #1: Maps of ref values
        p1 = ggplot2::ggplot(ref.data.box,ggplot2::aes(x= long, y = lat, group = polygon, fill = ref.value))+
          ggplot2::geom_polygon(color = 'black')+
          ggplot2::ggtitle('Reference Catch')+
          ggplot2::scale_fill_gradient(low = 'white',high =  'forestgreen',name = paste0('reference\n',data.type))+
          ggplot2::theme_bw()+
          ggplot2::theme(
            plot.title = ggplot2::element_text(hjust = 0.5)
          )

        #2: Map of init values
        p2 = ggplot2::ggplot(init.data.box,ggplot2::aes(x = long,y = lat, group = polygon, fill = init.value))+
          ggplot2::geom_polygon(color = 'black')+
          ggplot2::ggtitle('Initial Biomass')+
          ggplot2::scale_fill_gradient(low = 'white',high =  'forestgreen',name = paste0('initial\n',data.type))+
          ggplot2::theme_bw()+
          ggplot2::theme(
            plot.title = ggplot2::element_text(hjust = 0.5)
          )

        #3: Maps of run values

        p3 = ggplot2::ggplot(plot.data,ggplot2::aes(x = long,y = lat, group = polygon, fill = model.val))+
          ggplot2::geom_polygon(color = 'black')+
          ggplot2::facet_wrap(~run.name)+
          ggplot2::ggtitle('Model Catch')+
          ggplot2::scale_fill_gradient(low = 'white',high = 'forestgreen',name = paste0('model\n',data.type))+
          ggplot2::theme_bw()+
          ggplot2::theme(
            plot.title = ggplot2::element_text(hjust = 0.5)
          )

        #4: Maps of comparisons between runs and ref values
        p4 =ggplot2::ggplot(plot.data,ggplot2::aes(x = long,y = lat, group = polygon, fill = compare.val))+
          ggplot2::geom_polygon(color = 'black')+
          ggplot2::facet_wrap(~run.name)+
          ggplot2::ggtitle(paste0('Comparison: ',comparison.type))+
          ggplot2::scale_fill_gradient2(low = 'red4',mid = 'white',high = 'blue4',name = paste0(data.type,'\n',comparison.type),)+
          ggplot2::theme_bw()+
          ggplot2::theme(
            plot.title = ggplot2::element_text(hjust = 0.5)
          )

        spp.match = fgs$Code[which(fgs$LongName == fleet.combs$species[sf])]
        plot.name = paste0(fleet.combs$fleet[sf],':',fleet.combs$species[sf]," (",spp.match,"): ",ref.data$var.name[1],' ',data.type)
        plot.layout = matrix(c(1,1,rep(3,length(run.names)+1),2,2,rep(4,length(run.names)+1)),byrow = T,nrow =2)
        gridExtra::grid.arrange(p1,p2,p3,p4,nrow = 2,layout_matrix = plot.layout,top = plot.name)

      }
      dev.off()
    }

  }

}
