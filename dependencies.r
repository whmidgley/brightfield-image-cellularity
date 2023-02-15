if (!require("pacman")) install.packages("pacman")
pacman::p_load(beepr, stringr, BiocManager)

library(BiocManager)
BiocManager::install("EBImage")