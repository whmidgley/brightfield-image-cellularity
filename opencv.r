library(opencv)


m_bf <- ocv_read("brightfield-images/bf5.tif")


ocv_sketch(m_bf) %>% ocv_write("ocv_sketch.tif")



sketch <- readImage("ocv_sketch.tif")


sketch2 <- gblur((sketch^3)*10, 1)