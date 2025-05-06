`class_example.tif` is a cropped version of the 
[example file](https://landeco.umass.edu/web/may/prediction.zip) that
brad linked to in
[issue 11](https://github.com/ethanplunkett/rasterPrep/issues/11)


```
f <- "predict.tif"
r <- terra::rast(f)
r <- crop(r, ext(-70.863, -70.8628, 42.77, 42.7701))
plot(r)
writeRaster(r, "class_example.tif", overwrite = TRUE)
```
