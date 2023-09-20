
setwd(file.path(Sys.getenv("WorkSpace"),"2023-08-04_paleospace" ))

# NO S2!!!!

library(paleospace)
library(tinytest)


# do random trials of breakup
iter <- 50

# test with the Natural earth
ne <- fetch("NaturalEarth", verbose=FALSE, datadir="data/chronosphere")

# calculate 1
x11()
par(mfrow=c(3, 1))
expect_silent(ar1 <- areas(ne$geometry, lat=c(10, 90), plot=TRUE))
expect_silent(ar2 <- areas(ne$geometry, lat=c(-40, 10), plot=TRUE))
expect_silent(ar3 <- areas(ne$geometry, lat=c(-90, -40), plot=TRUE))

# based on S2 and non-projected
sf_use_s2(FALSE)
arFull <- st_area(st_union(ne$geometry))

# difference is lower than one square km
expect_true(abs(as.numeric(arFull-(ar1+ar2+ar3)))<1e6)

################################################################################
# II. Paleocoastlines

# try it with the paleocoastlines
pc <- fetch("paleomap", "paleocoastlines", verbose=FALSE, datadir="data/chronosphere")

# Do an exhaustive check (for shelf area)!
dir.create("paleospace/tests/export", showWarnings=FALSE)
pdf("paleospace/tests/export/areas_pc.pdf", width=20, height=30)
# for every reconstruction

# the calculated areas
pcArea <- matrix(NA, ncol=2, nrow=nrow(pc))
colnames(pcArea) <- c("land", "shelf")
rownames(pcArea) <- rownames(pc)

for(i in 1:nrow(pc)){
	# separate objects
	coast <- pc[i, "coast"]
	margin <- pc[i, "margin"]

	# the coastlines first
	par(mfrow=c(3,2))
	expect_silent(coast1 <- areas(coast, lat=c(10, 90), plot=TRUE))
	expect_silent(margin1 <- areas(margin, lat=c(10, 90), plot=TRUE))

	expect_silent(coast2 <- areas(coast, lat=c(-40, 10), plot=TRUE))
	expect_silent(margin2 <- areas(margin, lat=c(-40, 10), plot=TRUE))

	expect_silent(coast3 <- areas(coast, lat=c(-90, -40), plot=TRUE))
	expect_silent(margin3 <- areas(margin, lat=c(-90, -40), plot=TRUE))

	sf_use_s2(FALSE) # 
	expect_silent(coastFull <- st_area(st_union(coast)))
	expect_silent(marginFull <- st_area(st_union(margin)))

	# difference is lower than one square km
	expect_true(abs(as.numeric(coastFull-(coast1+coast2+coast3)))<1e6)
	expect_true(abs(as.numeric(marginFull-(margin1+margin2+margin3)))<1e6)

	# get shelf area
	pcArea[i,"land"] <- as.numeric(coastFull)
	pcArea[i,"shelf"] <- as.numeric(marginFull)-as.numeric(coastFull)

	# the
	cat(i, "\r")
	flush.console()
}
dev.off()


sumArea <- 510065622000000
pcArea <- pcArea/sumArea

# compare
ars <- fetch("paleomap", "areas", datadir="data/chronosphere")

# compare wiht pre-calculated
plot(pcArea[,"land"],ars$land_sum)
plot(pcArea[,"shelf"],ars$shelf_sum)

# the difference
# there is something off with the area calculation of some of the area calculation
expect_true(all((pcArea[,"land"]-ars$land_sum)<0.001))
expect_true(all((pcArea[,"shelf"]-ars$shelf_sum)<0.001))











