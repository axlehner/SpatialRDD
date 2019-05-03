
rm(list = ls())
library(sf)
library(tmap)
library(ggplot2)
library(stargazer)
library(sandwich)
library(lmtest)
library(lfe)


# cut_off.sf <- st_read("data/Border_OldGoa_VillageBoundaries.gpkg")
# polygon_treated.sf <- st_read("data/Polygon_GoaOLD_VillageBoundaries.gpkg")
# if (st_crs(cut_off.sf) == st_crs(polygon_treated.sf)) {
#   print("loading successful")
# } else {
#   print("CRS not matching!")
#   NULL # or break?
# }
#
# polygon_full.sf <- st_read("data/Polygon_GoaFULL_VillageBoundaries.gpkg")

# load the data that is inside the package
data("Goa_GIS")

set.seed(1088)
#rm(points_samp.sf)
points_samp.sf <- st_sample(polygon_full.sf, 1000)
points_samp.sf <- st_sf(points_samp.sf)
points_samp.sf$id <- 1
points_samp.sf$id <- 1:nrow(points_samp.sf)
tm_shape(points_samp.sf) + tm_dots() + tm_shape(cut_off.sf) + tm_lines(col = "red")

points_samp.sf$treated <- assign_treated(points_samp.sf, polygon_treated.sf, id = "id")
tm_shape(points_samp.sf) + tm_dots("treated", palette = "Accent") + tm_shape(cut_off.sf) + tm_lines(col = "red")

# simulating data

# education

points_samp.sf$education <- 0
points_samp.sf$education[points_samp.sf$treated == 1] <- .9
points_samp.sf$education[points_samp.sf$treated == 0] <- .6
tm_shape(points_samp.sf) + tm_dots("education") + tm_shape(cut_off.sf) + tm_lines(col = "red")
summary(lm(education ~ treated, data = points_samp.sf))

NTr <- length(points_samp.sf$education[points_samp.sf$treated == 1])
NCo <- length(points_samp.sf$education[points_samp.sf$treated == 0])
points_samp.sf$education[points_samp.sf$treated == 1] <- sample(c(.7, .75), NTr, replace = T)
points_samp.sf$education[points_samp.sf$treated == 0] <- sample(c(.5, .55, .6, .65, .7, .75), NCo, replace = T)
points_samp.sf$education[points_samp.sf$treated == 1] <- rnorm(NTr, mean = 0, sd = .1) + points_samp.sf$education[points_samp.sf$treated == 1]
points_samp.sf$education[points_samp.sf$treated == 0] <- rnorm(NCo, mean = 0, sd = .1) + points_samp.sf$education[points_samp.sf$treated == 0]
tm_shape(points_samp.sf) + tm_dots("education", palette = "RdYlGn", size = .1) + tm_shape(cut_off.sf) + tm_lines()
summary(lm(education ~ treated, data = points_samp.sf))

# distance to cutoff
points_samp.sf$dist2cutoff <- as.numeric(st_distance(points_samp.sf, cut_off.sf))
qplot(points_samp.sf$dist2cutoff)
tm_shape(points_samp.sf[points_samp.sf$dist2cutoff < 3000, ]) + tm_dots("education", palette = "RdYlGn", size = .1) + tm_shape(cut_off.sf) + tm_lines()
summary(lm(education ~ treated, data = points_samp.sf[points_samp.sf$dist2cutoff < 3000, ]))



points_samp.sf$segment <- border_segment(points_samp.sf, cut_off.sf, 10)
summary(lm(education ~ treated + factor(segment), data = points_samp.sf[points_samp.sf$dist2cutoff < 3000, ]))



options(digits = 3)
lm1 <- lm(education ~ treated + factor(segment), data = points_samp.sf[points_samp.sf$dist2cutoff < 3000, ])
stargazer(coeftest(lm1, vcov = vcovHC))
coeftest(lm1, vcov = vcovHC)
coeftest(lm1, vcov = vcovCL, cluster = ~ segment)

# lfe way

# 1 formula | 2 factor to be projected out | 3 IV ... 0 if not needed | 4 cluster (clu1 + clu2 for multi)
summary(felm(education ~ treated | factor(segment) | 0 | segment, data = points_samp.sf[points_samp.sf$dist2cutoff < 3000, ]))
summary(felm(education ~ treated | factor(segment) | 0 | 0, data = points_samp.sf[points_samp.sf$dist2cutoff < 3000, ]))

summary(felm(education ~ treated | factor(segment) | 0 | 0, data = points_samp.sf[points_samp.sf$dist2cutoff < 3000, ]), robust = T)

# RDnaive
#======================
library(rdrobust)
points_samp.sf$distrunning <- points_samp.sf$dist2cutoff
points_samp.sf$distrunning[points_samp.sf$treated == 1] <- -1 * points_samp.sf$distrunning[points_samp.sf$treated == 1]
summary(rdrobust(points_samp.sf$education, points_samp.sf$distrunning, c = 0))

ggplot(data = points_samp.sf, aes(x = distrunning, y = education)) + geom_point() + geom_vline(xintercept = 0, col = "red")


# rddapp
library(rddapp)
plot(rd_est(education ~ distrunning, data = points_samp.sf, t.design = "g"))

# rdd
library(rdd)
plot(RDestimate(education ~ distrunning, data = points_samp.sf, bw = 5000))
abline(v = 0, col = "red")

# RD
#=======================
borderpoints.sf <- discretise_border(cutoff = cut_off.sf, n = 50)
tm_shape(points_samp.sf[points_samp.sf$dist2cutoff < 3000, ]) + tm_dots("education", palette = "RdYlGn", size = .1) +
  tm_shape(cut_off.sf) + tm_lines() +
  tm_shape(borderpoints.sf) + tm_symbols(shape = 4, size = .3)


results <- SpatialRD(y = "education", data = points_samp.sf, cutoff.points = borderpoints.sf, treated = "treated")


# plotting
#-------------------------------------------------
# series first
GRDD <- ggplot(data = results,
               mapping = aes(x = rep(1:nrow(results)), y = Estimate)) + #, ymin = Rob_CIl, ymax = Rob_CIu)) +
  #geom_errorbar(color = "grey") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_point(aes(colour = cut(pvalC, c(-Inf, .11, Inf))), size = 1, shape = 19) +
  scale_color_manual(values = c("palegreen2", "lightcoral")) +
  # Here comes the styling
  theme_bw() + # needs to go before any other individual styling, otherwise it overwrites it
  theme(text = element_text(family = "Courier New"), plot.title = element_text(hjust = 0.5), legend.position = "none") + # center title, omit legend
  ggtitle(paste("GRDDseries,")) +
  labs(y = "Estimate (rdrobust)", x = "#Boundarypoint (> 10 observations)")

GRDDfix <- ggplot(data = results,
                  mapping = aes(x = rep(1:nrow(results)), y = Estimate)) + #, ymin = Rob_CIl_fix, ymax = Rob_CIu_fix)) +
  #geom_errorbar(color = "grey") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_point(aes(colour = cut(pvalC, c(-Inf, .11, Inf))), size = 1, shape = 19) +
  scale_color_manual(values = c("palegreen2", "lightcoral")) +
  # Here comes the styling
  theme_bw() + # needs to go before any other individual styling, otherwise it overwrites it
  theme(text = element_text(family = "Courier New"), plot.title = element_text(hjust = 0.5), legend.position = "none") + # center title, omit legend
  ggtitle(paste("[forced bandwith = 20km],")) +
  labs(y = "Estimate (rdrobust)", x = "#Boundarypoint (> 10 observations)")

library(cowplot)
plot_grid(GRDD, GRDDfix, align = "v", nrow = 2)

#result should be sf object so we can immediately plot all the points


# RASTERS ===========================================================
library(raster)

raster_template = raster(extent(points_samp.sf), resolution = 1000,
                         crs = st_crs(points_samp.sf)$proj4string)
plot(rasterize(points_samp.sf, raster_template, field = 1, fun = "count"))
raster_mean <- rasterize(points_samp.sf, raster_template, field = "education", fun = mean)
plot(rasterize(points_samp.sf, raster_template, field = "education", fun = mean))
lines(as(cut_off.sf, "Spatial"))


# interpolation with focal (using a simple MANUAL weighting function, based on 3 cells around and a weight of 1)
#--------------------------------------------------------------
plot(focal(raster_mean, matrix(1, nc = 3, nr = 3), fun = mean, NAonly = T, na.rm = T, pad = T))
#points(points.sp)
lines(as(cut_off.sf, "Spatial"))



# PLACEBO BORDERS ======================================

tm_shape(cut_off.sf) + tm_lines()

border_sfc <- st_geometry(cut_off.sf)
border_shift <- border_sfc + c(0, 2000) # units are in metres
border <- st_set_geometry(cut_off.sf, border_shift)

tm_shape(cut_off.sf) + tm_lines() + tm_shape(border) + tm_lines(col = "red")


