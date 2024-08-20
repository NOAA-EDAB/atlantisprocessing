library(dplyr)
i = 1
# param.dir = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/currentVersion'
param.dir = 'Z:/Joe_Proj/currentVersion/'
# run.dirs = c('C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Atlantis_Runs/gfimposezerotsfullsweptarea2',
#             'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Atlantis_Runs/HER_TS_dist_ddepend3')
# run.dirs = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Atlantis_Runs/fleets_example'
run.dirs = 'Z:/Joe_Proj/Atlantis_Runs/fleets_example'
# run.names = c('HER_TS_dist_ddepend0','HER_TS_dist_ddepend3')
run.names = 'fleets_example'
out.name = 'catch_test'
run.prefix = 'neus_output'
out.dir = 'C:/Users/joseph.caracappa/Documents/'
# out.dir = paste0(run.dirs,'/Post_Processed/Data/')
param.ls= get_atl_paramfiles(param.dir = param.dir,
                             atl.dir=run.dirs[1],
                             run.prefix = run.prefix,
                             include_catch=T)
fgs = read.csv(param.ls$groups.file)
data.type = 'proportion'
comparison.type = 'difference'

ref.years = c(10,20)

# ref.val.ls = list()
# for(i in 1:nrow(fgs)){
#   ref.val = runif(30)
#   ref.val = ref.val/sum(ref.val)
#   ref.val.ls[[i]] = data.frame(polygon = as.factor(0:29),
#                           species = fgs$LongName[i],
#                           var.name = 'biomass',
#                           statistic = 'proportion',
#                           ref.value = ref.val)
# }
# ref.data = dplyr::bind_rows(ref.val.ls)
#
# ref.data = readRDS('C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/data/spatial_reference_survdat_2011_2021.rds')%>%
#   filter(statistic == 'proportion' & var.name == 'biomass')
ref.data = readRDS('C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/data/spatial_reference_landings_fleet.rds') %>%
  filter(statistic == 'proportion' & var.name == 'catch_fleet')
# ref.data = readRDS('C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/data/spatial_reference_landings_species.rds')
init.data = readRDS('C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/data/spatial_reference_initial_conditions.rds')%>%
  filter(statistic == 'proportion' & var.name == 'biomass')



write.csv(ref.data,'C:/Users/joseph.caracappa/Documents/test_ref.csv',row.names =F)


library(atlantisprocessing)
compare_spatial_vars(param.dir = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/currentVersion',
                                run.dirs = c('C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Atlantis_Runs/HER_TS_dist_ddepend0',
                                             'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Atlantis_Runs/HER_TS_dist_ddepend3'),
                                run.names = c('HER_TS_dist_ddepend0','HER_TS_dist_ddepend3'),
                                ref.file = 'C:/Users/joseph.caracappa/Documents/test_ref.csv',
                                out.dir = 'C:/Users/joseph.caracappa/Documents/',
                                param.ls =get_atl_paramfiles(param.dir = param.dir,
                                      atl.dir=run.dirs[1],
                                      run.prefix = run.prefix,
                                      include_catch=T),
                                data.type = 'proportion',
                                comparison.type = 'difference',
                                ref.years = c(45,55),
                                plot = T)
