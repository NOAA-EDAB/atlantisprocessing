#' Creates post-processing output for Atlantis
#'
#' @param param.dir string. Path to location of atlantis parameter files
#' @param run.dirs character vector of paths to location of atlantis output files
#' @param run.names character vector of run names corresponding to run.dirs
#' @param ref.data dataframe  that contains reference spatial data with format (polygon|species|var.name|statistic|ref.value)
#' @param out.dir string. path to desired location of post-processed output
#' @param param.ls list generated from get_atl_paramfiles()
#' @param data.type which type of data is being compared: 'proportion','value'
#' @param comparison.type which type of comparison should be made: 'difference','scalar'
#' @param ref.years numeric vector with first and last year (years from start) for comparison period
#' @param plot logical. do you want to generate a plot
#'
#' @inheritParams make_atlantis_diagnostic_figures
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
                                out.dir,
                                param.ls,
                                data.type = 'proportion',
                                comparison.type = 'difference',
                                ref.years,
                                plot = T){
  #Pull in reference data

    # ref.data = read.csv(ref.file, as.ist =T)%>%
  ref.data = ref.data %>%
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
      }else if(lower(ref.data$var.name[1]) == 'numbers'){
        run.data = readRDS(paste0(run.dirs[i],'/Post_Processed/Data/numbers_box.rds'))
      }else if(lower(ref.data$var.name[1]) == 'catch.total'){
        run.data = readRDS(paste0(run.dirs[i],'/Post_Processed/Data/catch.rds'))
      }else if(lower(ref.data$var.name[1]) == 'catch.fleet'){
       #TO DO
      }

      run.data.yr = run.data%>%
        dplyr::mutate(polygon = as.factor(polygon -1))%>%
        ##Calculate proportion by box over ref.years
        dplyr::filter( time>= ref.years[1] & time <= ref.years[2]) %>%
        ##Calculate mean by box over ref.years
        dplyr::group_by(species,polygon)%>%
        dplyr::summarise(atoutput = mean(atoutput,na.rm=T))%>%
        dplyr::group_by(species)%>%
        dplyr::mutate(model.total = sum(atoutput))%>%
        dplyr::ungroup()

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
      run.data.yr = run.data.yr %>%
        dplyr::left_join(ref.data)

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

  out.name = paste(run.names,collapse = '_')
  saveRDS(run.data,paste0(out.dir,out.name,'.rds'))

  if(plot == T){
    #If plotting create PDF with the following plots

    spp.names = sort(unique(run.data.all$species))
    box.id = sort(unique(boxes$polygon))

    #Loop over species with 3+ plots per page (ref, model, comparisons)

    pdf(paste0(out.dir,out.name,'.pdf'), width = 12, height = 8)
    for(s in 1:length(spp.names)){

      #Get species data

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
          dplyr::left_join(ref.data)

        plot.data.spp = dplyr::bind_rows(plot.data.spp,missing.df)%>%
          dplyr::mutate(polygon = as.factor(polygon))

        plot.data.ls[[r]] = boxes %>%
          left_join(plot.data.spp, by = 'polygon')

      }
      plot.data = dplyr::bind_rows(plot.data.ls)

      plot.spp.ls = list()
      #1: Maps of ref values
      p1 = ggplot2::ggplot(dplyr::filter(plot.data,run.name == run.names[1]),ggplot2::aes(x = long,y = lat, group = polygon, fill = ref.value))+
        ggplot2::geom_polygon(color = 'black')+
        ggplot2::ggtitle('Reference Value')+
        ggplot2::scale_fill_gradient(low = 'white',high =  'forestgreen',name = paste0('reference\n',data.type))+
        ggplot2::theme_bw()+
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5)
        )

      #2: Maps of run values

      p2 = ggplot2::ggplot(plot.data,ggplot2::aes(x = long,y = lat, group = polygon, fill = model.val))+
        ggplot2::geom_polygon(color = 'black')+
        ggplot2::facet_wrap(~run.name)+
        ggplot2::ggtitle('Model Value')+
        ggplot2::scale_fill_gradient(low = 'white',high = 'forestgreen',name = paste0('model\n',data.type))+
        ggplot2::theme_bw()+
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5)
        )

      #2: Maps of comparisons between runs and ref values
      p3 =ggplot2::ggplot(plot.data,ggplot2::aes(x = long,y = lat, group = polygon, fill = compare.val))+
        ggplot2::geom_polygon(color = 'black')+
        ggplot2::facet_wrap(~run.name,nrow =1)+
        ggplot2::ggtitle(paste0('Comparison: ',comparison.type))+
        ggplot2::scale_fill_gradient2(low = 'red4',mid = 'white',high = 'blue4',name = paste0(data.type,'\n',comparison.type),)+
        ggplot2::theme_bw()+
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5)
        )

      plot.layout = matrix(c(1,1,rep(2,length(run.names)),1,1,rep(3,length(run.names))),byrow = T,nrow =2)
      gridExtra::grid.arrange(p1,p2,p3,nrow = 2,layout_matrix = plot.layout,top = paste0(spp.names[s],":",ref.data$var.name[1],' ',data.type))

    }
    dev.off()


  }

}
