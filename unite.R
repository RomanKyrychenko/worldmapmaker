Sys.setlocale(,"UK_ua")

library(shinydashboard)
library(shiny)
library(openxlsx)
library(lattice)
library(readxl)
library(sp)
library(rhandsontable)
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
library(shinyTable)
library(mapproj)
library(colourpicker)
load("mapdata.RData")

ui <- dashboardPage(skin = "yellow",
                    title="Corestone maps",
                    dashboardHeader(
                      title="Corestone maps",
                      tags$li(class = "dropdown",
                              tags$a(href="http://corestone.expert/", target="_blank", 
                                     tags$img(height = "20px", alt="Corestone", src="http://corestone.expert/static/icons/ic-navbar-logo.svg")
                              )
                      ),
                      dropdownMenuOutput("sys"),
                      tags$li(class = "dropdown",
                              tags$a(href = "https://github.com/RomanKyrychenko",
                                     target = "_blank",
                                     tags$img(height = "20px", 
                                              src = "https://raw.githubusercontent.com/oraza/sectarianviolencePK/master/www/github.png")
                              )
                      )
                    ),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Карта світу", tabName = "world",icon = icon("newspaper-o")),
                        menuItem("Карта України", tabName = "ua", icon = icon("vcard-o")),
                        #menuItem("System", tabName = "sys2", icon = icon("line-chart")),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        hr(),
                        menuItem("Documentation", icon = icon("file-text-o"), 
                                 href = "https://github.com/RomanKyrychenko/worldmapmaker/blob/master/README.md"),
                        menuItem("Feedback & suggestion", icon = icon("envelope-o"),
                                 href = "mailto:?Roman.Kyrychenko@corestone.expert?subject=Feedback on Corestone work tools app"),
                        menuItem("Source code", icon = icon("file-code-o"), 
                                 href = "https://github.com/RomanKyrychenko/worldmapmaker"),
                        menuItem("Fork me @ github", icon = icon("code-fork"), 
                                 href = "https://github.com/RomanKyrychenko") 
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "world",
                                fluidPage(
                                fluidRow(
                                  column(2,
                                h2("Карта світу"),
                                fileInput('file1', 'Завантажте дані',accept = c(".xlsx")),
                                downloadButton('downloadPlot',"Результат в pdf!"),
                                downloadButton('download',"Результат в png!")),
                                column(2,
                                       colourInput("mincol", label = "Колір для мінімуму:", value = "#f0f0f0"),
                                       #textInput("mincol","Колір для мінімуму","#f0f0f0"),
                                       colourInput("maxcol", label = "Колір для максимуму:", value = "#4d738a"),
                                       #textInput("maxcol","Колір для максимуму","#4d738a"),
                                       colourInput("nacol", label = "Колір для NA:", value = "#E5E0DE")
                                
                                ),
                                column(2,
                                sliderInput("minlon","Мінімум широти",-180,180,-180),
                                sliderInput("maxlon","Максимум широти",-180,180,180)),
                                column(2,
                                sliderInput("minlat","Мінімум довготи",-60,90,-60),
                                sliderInput("maxlat","Максимум довготи",-60,90,90)),
                                column(2,
                                       sliderInput("coordlat","Нахил в довготі",-180,180,60),
                                       sliderInput("coordlon","Нахил в широті",-90,90,-20)),
                                       column(2,
                                              checkboxInput("ckeck","З пимпочками",T),
                                              checkboxInput("ckeck2","З назвами",T),
                                              selectInput("typ","Тип мапи",choices = rev(c(
                                                "ortho",
                                                "gilbert",
                                                "mercator"
                                              )))
                                       )),
                                plotOutput('plot', width = "100%", height = "750px"))
                        )#,
                        #tabItem(tabName = "Infoflow",
                        #        h2("Infoflow"),
                        #        fileInput('file1', 'Завантажте файл з даними',
                        #                  accept = c(".xlsx")),
                        #        tags$hr(),
                        #        downloadButton('down',"Завантажити в pdf!"),
                        #        downloadButton('do',"Завантажити в png!"),
                        #        plotOutput('plot', width = "3666px", height = "2358px")
                        )#,
                        #   tabItem(tabName = "sys2",
                        #           h2("System"),
                        #           actionButton("goButton", "Go!"),
                        #           verbatimTextOutput("nText"))
))

server <- function(input,output,server,session){
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
        scale_fill_gradient(low=input$mincol, high=input$maxcol,na.value = input$nacol) +
        geom_polygon(aes(long,lat,fill = X1,group=group), data =world.ggmap, color = "grey",size=0.1)+
        {if(input$ckeck2==T)with(centroids, annotate(geom="text", x = long, y=lat-1, label = label, size = 5,family="PT Sans"))} +
        {if(input$ckeck==T)with(centroids,annotate(geom="point",x = long, y=lat+3,color="#3bdd6d",fill="#3bdd6d",size=10))} +
        {if(input$ckeck==T)with(centroids, annotate(geom="text", x = long, y=lat+3, label = X1, size = 6, color="white",family="PT Sans"))} +
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
        scale_fill_gradient(low=input$mincol, high=input$maxcol,na.value = input$nacol) +
        geom_polygon(aes(long,lat,fill = X1,group=group,id=id), data =world.ggmap, color = "grey",size=0.1)+
        {if(input$ckeck2==T)with(centroids, annotate(geom="text", x = long, y=lat-1, label = label, size = 5))} +
        {if(input$ckeck==T)with(centroids,annotate(geom="point",x = long, y=lat+3,color="#3bdd6d",fill="#3bdd6d",size=10))} +
        {if(input$ckeck==T)with(centroids, annotate(geom="text", x = long, y=lat+3, label = X1, size = 6, color="white"))} +
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
        scale_fill_gradient(low=input$mincol, high=input$maxcol,na.value = input$nacol) +
        geom_polygon(aes(long,lat,fill = X1,group=group), data =world.ggmap, color = "grey",size=0.1)+
        {if(input$ckeck2==T)with(centroids, annotate(geom="text", x = long, y=lat-1, label = label, size = 5))} +
        {if(input$ckeck==T)with(centroids,annotate(geom="point",x = long, y=lat+3,color="#3bdd6d",fill="#3bdd6d",size=10))} +
        {if(input$ckeck==T)with(centroids, annotate(geom="text", x = long, y=lat+3, label = X1, size = 6, color="white"))} +
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
}

options(shiny.trace=TRUE)
shinyApp(ui,server)