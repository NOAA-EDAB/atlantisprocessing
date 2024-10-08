% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_atl_output.R
\name{process_atl_output}
\alias{process_atl_output}
\title{Creates post-processing output for Atlantis}
\usage{
process_atl_output(
  param.dir,
  atl.dir,
  out.dir = file.path(atl.dir, "Post_Processed/Data/"),
  run.prefix,
  param.ls,
  agg.scale = "day",
  large.file = F,
  system,
  process.all = F,
  plot.all = F,
  plot.benthic = F,
  plot.overall.biomass = F,
  plot.biomass.timeseries = F,
  plot.length.age = F,
  plot.biomass.box = F,
  plot.c.mum = F,
  plot.sn.rn = F,
  plot.recruits = F,
  plot.numbers.timeseries = F,
  plot.physics = F,
  plot.growth.cons = F,
  plot.cohort = F,
  plot.diet = F,
  plot.consumption = F,
  plot.spatial.biomass = F,
  plot.spatial.biomass.seasonal = F,
  plot.catch = F,
  plot.catch.fleet = F,
  plot.spatial.catch = F,
  plot.mortality = F,
  plot.weight = F,
  plot.spatial.overlap = F
)
}
\arguments{
\item{param.dir}{string. Path to location of atlantis parameter files}

\item{atl.dir}{string. path to location of atlantis output files}

\item{out.dir}{string. path to desired location of post-processed output}

\item{run.prefix}{string. Prefix for atlantis run output (specified in runcommand.bat)}

\item{param.ls}{list generated from get_atl_paramfiles()}

\item{agg.scale}{Scale to aggregate dietcheck biomass from (either 'raw','month', or 'year' )}

\item{large.file}{Boolean.}

\item{system}{String. "Windows" or "Linux"}

\item{process.all}{Boolean. Global option to process all components}

\item{plot.all}{Boolean. Global flag for plotting everything}

\item{plot.benthic}{logical. Benthic plots show timeseries of all benthic and epibenthic groups for one box}

\item{plot.overall.biomass}{logical. Plots showing the total biomass across all functional groups as stacked barplots}

\item{plot.biomass.timeseries}{logical. Plots showing biomass-related timeseries on various aggregations  and reference points}

\item{plot.length.age}{logical. Plots relating to the length-age relationship of age-structured groups}

\item{plot.biomass.box}{logical. Plots relating to biomass but grouped by box}

\item{plot.c.mum}{logical. Plots and tables related to tuning C and mum parameters}

\item{plot.sn.rn}{logical. Plots relating to SN and RN timeseries}

\item{plot.recruits}{logical. Plots of recruitment and SSB timeseries}

\item{plot.numbers.timeseries}{logical. Plots showing timeseries of numbers (as opposed to biomass)}

\item{plot.physics}{logical. Plots of physical statevariables as well as fluxes}

\item{plot.growth.cons}{logical. Plots relating to growth and consumption}

\item{plot.cohort}{logical. Plots showing timeseries of each cohort across age-structured groups}

\item{plot.diet}{logical. Plots showing predation of and consumption by each functional group}

\item{plot.consumption}{Boolean. Plots showing consumption}

\item{plot.spatial.biomass}{logical. Plots showing the spatial (box/level) structure of groups' biomass}

\item{plot.spatial.biomass.seasonal}{logical. Plots showing the spatial (box/level) structure of groups' biomass}

\item{plot.catch}{logical. Plots annual catch(mt) age based catch (numbers) and age based \%ages}

\item{plot.catch.fleet}{Boolean}

\item{plot.spatial.catch}{logical. Plots showing the spatial (box/level) structure of groups' catch}

\item{plot.mortality}{logical. Plots Mortality (F, M1, M2) from two output sources (Mort, SpecificMort)}

\item{plot.weight}{logical. Plots the maximum size of fish in each size class over the domain}

\item{plot.spatial.overlap}{Boolean}
}
\value{
Either saves an R object or returns a list called "result"

Author: Ryan Morse, modified by Joseph Caracappa
}
\description{
Creates post-processing output for Atlantis
}
