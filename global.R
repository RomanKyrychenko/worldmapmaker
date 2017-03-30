library(shiny)
library(openxlsx)
library(lattice)
library(readxl)
library(sp)
library(maptools)
library(ggplot2)
library(dplyr)
library(raster)
library(readxl)
library(rgeos)
library(rgdal)
library(maps)
library(geosphere)
library(plyr)
#install.packages(c("maps", "mapproj"))
load("mapdata.RData")
#world.map <- readShapePoly("TM_WORLD_BORDERS_SIMPL-0.3.shp")
#world.ggmap <- fortify(world.map, region = "NAME")

#fortify.SpatialPolygonsDataFrame <- function(model, data, region = NULL, ...) {
#  attr <- as.data.frame(model)
#  # If not specified, split into regions based on polygons
#  if (is.null(region)) {
#    coords <- ldply(model@polygons,fortify)
#    message("Regions defined for each Polygons")
#  } else {
#    cp <- sp::polygons(model)
#    #require("maptools")
#    # Union together all polygons that make up a region
#    unioned <- maptools::unionSpatialPolygons(cp, attr[, region])
#    coords <- fortify(unioned)
#    coords$order <- 1:nrow(coords)
#  }
#  coords
#}
#
#fortify.SpatialPolygons <- function(model, data, ...) {
#  ldply(model@polygons, fortify)
#}
#
#fortify.Polygons <- function(model, data, ...) {
#  subpolys <- model@Polygons
#  pieces <- ldply(seq_along(subpolys), function(i) {
#    df <- fortify(subpolys[[model@plotOrder[i]]])
#    df$piece <- i
#    df
#  })
#  within(pieces,{
#    order <- 1:nrow(pieces)
#    id <- model@ID
#    piece <- factor(piece)
#    group <- interaction(id, piece)
#  })
#}
#
#fortify.Polygon <- function(model, data, ...) {
#  df <- as.data.frame(model@coords)
#  names(df) <- c("long", "lat")
#  df$order <- 1:nrow(df)
#  df$hole <- model@hole
#  df
#}
#
#fortify.SpatialLinesDataFrame <- function(model, data, ...) {
#  ldply(model@lines, fortify)
#}
#
#fortify.Lines <- function(model, data, ...) {
#  lines <- model@Lines
#  pieces <- ldply(seq_along(lines), function(i) {
#    df <- fortify(lines[[i]])
#    df$piece <- i
#    df
#  })
#  within(pieces,{
#    order <- 1:nrow(pieces)
#    id <- model@ID
#    piece <- factor(piece)
#    group <- interaction(id, piece)
#  })
#}
#
#fortify.Line <- function(model, data, ...) {
#  df <- as.data.frame(model@coords)
#  names(df) <- c("long", "lat")
#  df$order <- 1:nrow(df)
#  df
#}
#
#RegroupElements <- function(df, longcol, idcol){
#  g <- rep(1, length(df[,longcol])) 
#  if (diff(range(df[,longcol])) > 300) {
#    d <- df[,longcol] > mean(range(df[,longcol]))
#    g[!d] <- 1
#    g[d] <- 2
#  }
#  g <- paste(df[, idcol], g, sep=".")
#  df$group.regroup <- g
#  df
#}
#
#ClosePolygons <- function(df, longcol, ordercol){
#  if (df[1,longcol] != df[nrow(df),longcol]) {
#    tmp <- df[1,]
#    df <- rbind(df,tmp)
#  }
#  o <- c(1: nrow(df)) # rassign the order variable
#  df[,ordercol] <- o
#  df
#}
#
#
##Load data
#speciestrax<-matrix(0,11,5)
#colnames(speciestrax)<-c("slong","slat","elong","elat","number")
#speciestrax[,1]<-c(153.1590,134.4637,120.7381,119.6376,120.2324,130.1927,130.2522,124.3630,121.7456,120.6749,141.5546)
#speciestrax[,2]<-c(-27.178338,7.485653,18.468052,26.189403,36.111755,46.200918,49.591650,46.914750,40.906617,33.232855,-2.815979)
#speciestrax[,3]<-c(134.4637,120.7381,119.6376,120.2324,130.1927,130.2522,124.3630,121.7456,120.6749,141.5546,143.9936)
#speciestrax[,4]<-c(7.485653,18.468052,26.189403,36.111755,46.200918,49.591650,46.914750,40.906617,33.232855,-2.815979,-13.939959)
#speciestrax[,5]<-1:11
#rownames(speciestrax)<-c("stage1","stage2","stage3","stage4","stage5","stage6","stage7","stage8","stage9","stage10","stage11")
#
#
##Plotting stuff
#center=160 #describe where you want the centre of the map to be
#
##data<-speciestrax
#data<-data.frame(as.matrix(speciestrax))
#
## shift coordinates to recenter great circles
#data$slong.recenter <- ifelse(data$slong < center - 180 , data$slong + 360, data$slong)
#data$elong.recenter <- ifelse(data$elong < center - 180 , data$elong + 360, data$elong)
#
## shift coordinates to recenter worldmap
#worldmap2 <- map_data("world")
#worldmap2$long.recenter <- ifelse(worldmap2$long < center - 180 , worldmap2$long + 360, worldmap2$long)
#
## now regroup
#worldmap2.rg <- ddply(worldmap2, .(group), RegroupElements, "long.recenter", "group")
#
## close polys
#worldmap2.cp <- ddply(worldmap2.rg, .(group.regroup), ClosePolygons, "long.recenter", "order") # use the new grouping var

