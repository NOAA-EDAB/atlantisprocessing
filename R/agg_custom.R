#'
#'
#'
#'
#'
#'
#'
#' @export

agg_custom  <-  function(data, groups, fun, agg.scale){
  out = atlantistools::agg_data(data = data, groups = groups, fun = fun) %>%
    mutate(date = as.POSIXct(time*365*86400, origin = '1964-01-01 00:00:00',tz = 'UTC'))

  if(agg.scale == 'month') {
    out$time.agg = as.numeric(factor(format(out$date,format = '%m')))
  }else if(agg.scale == 'year'){
    out$time.agg = as.numeric(factor(format(out$date, format = '%Y')))
  }else{
    out$time.agg = out$time*365
  }

  match.cols = c(groups[-which(groups == 'time')],'time.agg')
  out.agg = out %>%
    group_by_at(match.cols)%>%
    summarise(time = floor(min(time)),
              atoutput = mean(atoutput,na.rm=T))%>%
    ungroup()%>%
    dplyr::select(all_of(c(groups,'atoutput')))

  return(out.agg)
}
