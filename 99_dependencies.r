if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, beepr, stringr, BiocManager, readr, shiny, shinyWidgets, shinyjs, shinyalert)

library(BiocManager)
BiocManager::install("EBImage")
BiocManager::install("aoles/RBioFormats")

# N.B. make sure to download Java