
file <- paste0(path, "/Chart_0.csv")
acellular <- str_detect(file, "acellular")

if (file.exists(file)) {

pixels <- read.csv(file, sep = "\t") %>%
    as.data.frame()

pixels <- pixels[2:257,] %>% strsplit(",")

for(p in 1:length(pixels)) {
  pixels[[p]] <- pixels[[p]][-1]
}
count <- pixels %>% unlist() %>% as.numeric() %>% replace_na(0) %>% sum()

if(acellular) {
  human_cellularity <- 100-((count/2)*100/1048576)
} else {
  human_cellularity <- (count/2)*100/1048576
}

cat("cellularity is", human_cellularity, "%")
} else {
  human_cellularity <- NA_real_
  cat("Report not made")
}