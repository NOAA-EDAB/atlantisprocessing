#' Creates a dataframe of atlantis parameter files based on standard outut
#'
#' @param param.dir string. Path to location of atlantis parameter files
#' @param atl.dir string. Path to location of atlantis output files
#' @param run.prefix string. Name of the run. filename prefix in the atlantis output
#' @param include_catch logical. Whether to include catch output
#'
#' @return list containing parameter file name and full path to file
#'
#' @export

get_atl_paramfiles <- function(param.dir,atl.dir,run.prefix,include_catch){

  # checks to make sure formatted correctly
  param.dir <- check_string(param.dir)
  atl.dir <- check_string(atl.dir)

  #Create vector of netCDF output files
  output.files <- list.files(path = atl.dir, pattern = '*.nc')

  #Identify file names and their character length
  nc.str <- strsplit(output.files, '.nc')
  lng.str <- nchar(nc.str)

  #Generate paths to main, PROD, DietCheck, YOY, and SSB output files
  main.nc <- file.path(atl.dir,paste0(run.prefix,'.nc'))
  prod.nc <- file.path(atl.dir,paste0(run.prefix,'PROD.nc'))
  dietcheck <- file.path(atl.dir,paste0(run.prefix,'DietCheck.txt'))
  yoy <- file.path(atl.dir,paste0(run.prefix,'YOY.txt'))
  ssb <- file.path(atl.dir,paste0(run.prefix,'SSB.txt'))
  mort <- file.path(atl.dir,paste0(run.prefix,'Mort.txt'))
  specificmort <- file.path(atl.dir,paste0(run.prefix,'SpecificMort.txt'))

  #If catch is turned on all generate paths for CATCH and TOTCATCH
  if(include_catch){
    catch <- file.path(atl.dir, paste0(run.prefix,'CATCH.nc'))
    catchtot <- file.path(atl.dir, paste0(run.prefix,'TOTCATCH.nc'))
  }

  # Identify run_command file to get additional
  run.bat <- list.files(param.dir,'*.bat')
  run.con <- file(description=file.path(param.dir,run.bat),open='r')
  run.cmd <- readLines(con=run.con,n = 1,warn = F)
  close(run.con)

  # pull out name of input parameter files
  run.prm <- file.path(param.dir,run.filename(run.cmd, '-r'))
  biol.prm <- file.path(param.dir,run.filename(run.cmd, '-b'))
  force.prm <- file.path(param.dir,run.filename(run.cmd, '-f'))
  phys.prm <- file.path(param.dir,run.filename(run.cmd, '-p'))
  harvest.prm <- file.path(param.dir,run.filename(run.cmd, '-h'))
  economics.prm <- file.path(param.dir,run.filename(run.cmd, '-e'))
  fishery.prm <- file.path(param.dir,run.filename(run.cmd, '-q'))
  func.groups <- file.path(param.dir,run.filename(run.cmd, '-s'))
  init <- file.path(param.dir,run.filename(run.cmd, '-i'))


  #Get bgm file name from initial conditions file
  init.nc  <-  ncdf4::nc_open(init)
  bgm.str <-  ncdf4::ncatt_get(init.nc,0,'geometry')$value
  ncdf4::nc_close(init.nc)
  bgm <-  file.path(param.dir,bgm.str)

  #Get nofill version of initial conditions
  init.nofill <-  list.files(param.dir,'*nofill*',full.name = T)

  #Organize parameter files
  param.list <-  list(run.cmd = run.cmd,
                    run.prm = run.prm,
                    biol.prm = biol.prm,
                    force.prm = force.prm,
                    phys.prm = phys.prm,
                    harvest.prm = harvest.prm,
                    economics.prm = economics.prm,
                    fishery.prm = fishery.prm,
                    groups.file = func.groups,
                    init.file = init,
                    bgm.file = bgm,
                    init.nofill = init.nofill,
                    main.nc = main.nc,
                    prod.nc = prod.nc,
                    dietcheck = dietcheck,
                    yoy =yoy,
                    ssb = ssb,
                    catch = ifelse(include_catch,catch,NA),
                    catchtot = ifelse(include_catch,catchtot,NA),
                    mort = mort,
                    specificmort = specificmort
                    )

  return(param.list=param.list)
}
