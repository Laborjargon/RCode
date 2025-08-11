# MAIN
# Ben Sommer adapted from Sven Ohl
# TODO 
# 1. set a fix y-axis on all plots; one-liner?!

setwd("C:/Users/somme/Desktop/Masterarbeit/")
path_wd <- setwd("C:/Users/somme/Desktop/Masterarbeit/")
path_data <- paste(path_wd,"/data/coca",sep="")
path_figures <- paste(path_wd,"/figures/",sep="")
path_script  <- paste(path_wd,"/scripts/",sep="")

# load packages -----------------------------------------------------------

source(paste(path_script,"load_packages.R",sep=""))

# load functions ----------------------------------------------------------

source(paste(path_script, "helper_functions.R",sep=""))

# load colours ------------------------------------------------------------

source(paste(path_script, "load_colours.R", sep=""))

# load data ---------------------------------------------------------------

source(paste(path_script,"load_data.R",sep=""))

# prepare data ------------------------------------------------------------

source(paste(path_script,"prep_data.R",sep=""))

# fit data ----------------------------------------------------------------

source(paste(path_script,"fit_data.R",sep=""))

# CM CI -------------------------------------------------------------------

source(paste(path_script,"CM_CI.R",sep=""))

# psychometric functions --------------------------------------------------

source(paste(path_script,"pmf_plot.R", sep=""))

# big plot ----------------------------------------------------------------

source(paste(path_script,"big_plot.R", sep=""))

# plot non parametrics ----------------------------------------------------

# source(paste(path_script,"plot_data.R",sep=""))

# nonparametric difference plot -------------------------------------------
  
# source(paste(path_script,"diff_plot.R",sep=""))

# cumulative sum difference plot ------------------------------------------

# source(paste(path_script,"sum_diff.R",sep=""))

# Imp3 Violin Plots -------------------------------------------------------

source(paste(path_script,"Imp.R",sep=""))

source(paste(path_script,"Imp2.R",sep=""))

# Imp3 Difference Plot ----------------------------------------------------

source(paste(path_script,"violin_diff.R",sep=""))

# Imp3 Stats  -------------------------------------------------------------

source(paste(path_script,"ImpSTATS.R",sep=""))

# median split ------------------------------------------------------------

source(paste(path_script,"median_split.R",sep=""))

# PSE Plot ----------------------------------------------------------------

source(paste(path_script,"PSE_plot.R",sep=""))

# adaptation difference ---------------------------------------------------

source(paste(path_script,"adaptation_difference.R",sep=""))

# summed adaptation -------------------------------------------------------

source(paste(path_script,"sum_adapt.R",sep=""))

# direction selectivity ---------------------------------------------------

source(paste(path_script,"direction_selectivity.R", sep=""))

# median split psychometric functions -------------------------------------

source(paste(path_script,"median_split_CMCI.R",sep=""))

source(paste(path_script,"median_pmf.R",sep=""))

source(paste(path_script,"median_pmf2.R",sep=""))






