shinyServer(function(input, output, session){
  output$text <- renderPrint({
    s <- "Кнопки регулювання нахилу варто застосовувати тільки при проекції ortho. Регулювати ширину не варто (ця опція для розробника). Довжину можна."
    s  
  })
  
  df <- reactive({
    inFile <- input$file1
    
    if(is.null(inFile))
      return(NULL)
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
    df <-read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1,col_names = FALSE)
    #df <- data.frame(input$tbl)
    names(df)[1:2] <- c("X0","X1")
    df$X0 <- as.character(df$X0)
    df$X1 <- as.numeric(df$X1)
    zubat <- function(df){
      test_df <- data_frame(X0=unique(world.ggmap$id),X1=NA)
      df <- rbind(df[1:2],test_df[1:2])
      df <- df %>% distinct(X0,.keep_all=T)
      world.ggmap <- left_join(world.ggmap,df,by=c("id"="X0"))
      #df$X1[is.na(df$X1)] <- 0
      centroids <- setNames(do.call("rbind.data.frame", by(world.ggmap, world.ggmap$group, function(x) {Polygon(x[c('long', 'lat')])@labpt})), c('long', 'lat')) 
      centroids$label <- world.ggmap$id[match(rownames(centroids), world.ggmap$group)]
      centroids <- centroids %>% distinct(label,.keep_all = T)
      centroids <- left_join(centroids,df,by=c("label"="X0"))
      #centroids$lat[10] <- centroids$lat[10]-0.4
      #centroids$long[16] <- centroids$long[16]+0.35
      
      centroids <- centroids[!is.na(centroids$X1),]
      
      x1 <- mean(world.ggmap[!is.na(world.ggmap$X1),1])
      #x2 <- m(world.ggmap[!is.na(world.ggmap$X1),1])
      y1 <- mean(world.ggmap[!is.na(world.ggmap$X1),2])
      #y2 <- max(world.ggmap[!is.na(world.ggmap$X1),2])
      
      ggplot(world.ggmap, aes(map_id = id,group=group)) +
        scale_fill_gradient(low=input$mincol, high=input$maxcol,na.value = "#E5E0DE") +
        geom_polygon(aes(long,lat,fill = X1,group=group), data =world.ggmap, color = "grey",size=0.1)+
        {if(input$ckeck==T)with(centroids, annotate(geom="text", x = long, y=lat-1, label = label, size = 5,family="PT Sans"))} +
        {if(input$ckeck==T)with(centroids,annotate(geom="point",x = long, y=lat+3,color="#3bdd6d",fill="#3bdd6d",size=10))} +
        {if(input$ckeck2==T)with(centroids, annotate(geom="text", x = long, y=lat+3, label = X1, size = 6, color="white",family="PT Sans"))} +
        #expand_limits(x = world.ggmap$long, y = world.ggmap$lat) + 
        theme_void() + theme(
          legend.position = "none"
        ) + #coord_map(xlim = c(x1,x2),ylim = c(y1,y2),"gilbert") 
        #coord_map("ortho", orientation = c(y1,x1,0))
        ggplot2::coord_map("ortho",orientation = c(input$coordlat,input$coordlon,0),xlim=c(input$minlon,input$maxlon),ylim=c(input$minlat,input$maxlat))
    }
    zubat(df)
  })
  
  df2 <- reactive({
    inFile <- input$file1
    
    if(is.null(inFile))
      return(NULL)
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
    df <-read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1,col_names = FALSE)
    #df <- data.frame(input$tbl)
    names(df)[1:2] <- c("X0","X1")
    zubat <- function(df){
      test_df <- data_frame(X0=unique(world.ggmap$id),X1=NA)
      df <- rbind(df[1:2],test_df[1:2])
      df <- df %>% distinct(X0,.keep_all=T)
      world.ggmap <- left_join(world.ggmap,df,by=c("id"="X0"))
      #df$X1[is.na(df$X1)] <- 0
      centroids <- setNames(do.call("rbind.data.frame", by(world.ggmap, world.ggmap$group, function(x) {Polygon(x[c('long', 'lat')])@labpt})), c('long', 'lat')) 
      centroids$label <- world.ggmap$id[match(rownames(centroids), world.ggmap$group)]
      centroids <- centroids %>% distinct(label,.keep_all = T)
      centroids <- left_join(centroids,df,by=c("label"="X0"))
      #centroids$lat[10] <- centroids$lat[10]-0.4
      #centroids$long[16] <- centroids$long[16]+0.35
      
      centroids <- centroids[!is.na(centroids$X1),]
      
      x1 <- mean(world.ggmap[!is.na(world.ggmap$X1),1])
      #x2 <- m(world.ggmap[!is.na(world.ggmap$X1),1])
      y1 <- mean(world.ggmap[!is.na(world.ggmap$X1),2])
      #y2 <- max(world.ggmap[!is.na(world.ggmap$X1),2])
      
      ggplot() +
        scale_fill_gradient(low=input$mincol, high=input$maxcol,na.value = "#E5E0DE") +
        geom_polygon(aes(long,lat,fill = X1,group=group,id=id), data =world.ggmap, color = "grey",size=0.1)+
        {if(input$ckeck==T)with(centroids, annotate(geom="text", x = long, y=lat-1, label = label, size = 5))} +
        {if(input$ckeck==T)with(centroids,annotate(geom="point",x = long, y=lat+3,color="#3bdd6d",fill="#3bdd6d",size=10))} +
        {if(input$ckeck2==T)with(centroids, annotate(geom="text", x = long, y=lat+3, label = X1, size = 6, color="white"))} +
        #expand_limits(x = world.ggmap$long, y = world.ggmap$lat) + 
        theme_void() + theme(
          legend.position = "none"
        ) + #coord_map(xlim = c(x1,x2),ylim = c(y1,y2),"gilbert") 
        #coord_map("ortho", orientation = c(y1,x1,0))
        ggplot2::coord_map("gilbert",xlim=c(input$minlon,input$maxlon),ylim=c(input$minlat,input$maxlat))
    }
    zubat(df)
  })
  
  df3 <- reactive({
    inFile <- input$file1
    
    if(is.null(inFile))
      return(NULL)
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
    df <-read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1,col_names = FALSE)
    #df <- data.frame(input$tbl)
    names(df)[1:2] <- c("X0","X1")
    zubat <- function(df){
      test_df <- data_frame(X0=unique(world.ggmap$id),X1=NA)
      df <- rbind(df[1:2],test_df[1:2])
      df <- df %>% distinct(X0,.keep_all=T)
      world.ggmap <- left_join(world.ggmap,df,by=c("id"="X0"))
      #df$X1[is.na(df$X1)] <- 0
      centroids <- setNames(do.call("rbind.data.frame", by(world.ggmap, world.ggmap$group, function(x) {Polygon(x[c('long', 'lat')])@labpt})), c('long', 'lat')) 
      centroids$label <- world.ggmap$id[match(rownames(centroids), world.ggmap$group)]
      centroids <- centroids %>% distinct(label,.keep_all = T)
      centroids <- left_join(centroids,df,by=c("label"="X0"))
      
      centroids <- centroids[!is.na(centroids$X1),]
      
      x1 <- mean(world.ggmap[!is.na(world.ggmap$X1),1])
      #x2 <- m(world.ggmap[!is.na(world.ggmap$X1),1])
      y1 <- mean(world.ggmap[!is.na(world.ggmap$X1),2])
      #y2 <- max(world.ggmap[!is.na(world.ggmap$X1),2])
      
      ggplot(world.ggmap, aes(map_id = id,group=group)) +
        scale_fill_gradient(low=input$mincol, high=input$maxcol,na.value = "#E5E0DE") +
        geom_polygon(aes(long,lat,fill = X1,group=group), data =world.ggmap, color = "grey",size=0.1)+
        {if(input$ckeck==T)with(centroids, annotate(geom="text", x = long, y=lat-1, label = label, size = 5))} +
        {if(input$ckeck==T)with(centroids,annotate(geom="point",x = long, y=lat+3,color="#3bdd6d",fill="#3bdd6d",size=10))} +
        {if(input$ckeck2==T)with(centroids, annotate(geom="text", x = long, y=lat+3, label = X1, size = 6, color="white"))} +
        #expand_limits(x = world.ggmap$long, y = world.ggmap$lat) + 
        theme_void() + theme(
          legend.position = "none"
        ) + #coord_map(xlim = c(x1,x2),ylim = c(y1,y2),"gilbert") 
        #coord_map("ortho", orientation = c(y1,x1,0))
        ggplot2::coord_map(xlim=c(input$minlon,input$maxlon),ylim=c(input$minlat,input$maxlat))
    }
    zubat(df)
  })
  
  output$plot <- renderPlot({
    if(input$typ=="ortho"){
      tryCatch(df())
    }
    else if(input$typ=="gilbert"){
      tryCatch(df2())
    }
    else {
      tryCatch(df3())
    }
  })
  
  output$downloadPlot <-  downloadHandler(
    filename = function(){paste0("worldmap_",Sys.Date(),".pdf") },
    content = function(file) {
      cairo_pdf(file, width=15.5, height=10.3)
      print(if(input$typ=="ortho"){
        tryCatch(df())
      }
      else if(input$typ=="gilbert"){
        tryCatch(df2())
      }
      else {
        tryCatch(df3())
      })
      dev.off()
    }
  )
  
  output$download<-  downloadHandler(
    filename = function(){paste0("worldmap_",Sys.Date(),".png") },
    content = function(file) {
      png(file, width=1550, height=1030)
      print(if(input$typ=="ortho"){
        tryCatch(df())
      }
      else if(input$typ=="gilbert"){
        tryCatch(df2())
      }
      else {
        tryCatch(df3())
      })
      dev.off()
    }
  )
})