library(opencv)


m_bf <- ocv_read("brightfield-images/bf5.tif")


ocv_sketch(m_bf) %>% ocv_write("ocv_sketch.tif")



sketch <- readImage("ocv_sketch.tif")
m_bf <- readImage("brightfield-images/bf5.tif")

plot(sketch)
#plot(m_bf)
sketch2 <- gblur((sketch^3)*10, 3)

sketch2[sketch2>0.2] <- 1


plot(sketch2)


thresh(sketch2, offset = 0.0001) %>% plot()