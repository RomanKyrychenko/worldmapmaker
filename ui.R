#install.packages("mapproj")
#library(mapproj)
shinyUI(fluidPage(
  titlePanel("Карта світу"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Завантажте файл з даними',
                accept = c(".xlsx")),
      colourInput("mincol", label = "Колір для мінімуму:", value = "#f0f0f0"),
      #textInput("mincol","Колір для мінімуму","#f0f0f0"),
      colourInput("maxcol", label = "Колір для максимуму:", value = "#4d738a"),
      #textInput("maxcol","Колір для максимуму","#4d738a"),
      colourInput("nacol", label = "Колір для NA:", value = "#E5E0DE"),
      checkboxInput("ckeck","З пимпочками",T),
      checkboxInput("ckeck2","З назвами",T),
      sliderInput("coordlat","Нахил в довготі",-180,180,60),
      sliderInput("coordlon","Нахил в широті",-90,90,-20),
      selectInput("typ","Тип мапи",choices = rev(c(
        "ortho",
        "gilbert",
        "mercator"
      ))),
      sliderInput("minlon","Мінімум широти",-180,180,-180),
      sliderInput("maxlon","Максимум широти",-180,180,180),
      sliderInput("minlat","Мінімум довготи",-60,90,-60),
      sliderInput("maxlat","Максимум довготи",-60,90,90),
      tags$hr(),
      downloadButton('downloadPlot',"Завантажити мапу в pdf!"),
      downloadButton('download',"Завантажити мапу в png!")
    ),
    mainPanel(tabsetPanel(
      tabPanel("Карта",plotOutput('plot', width = "100%", height = "750px")),
      tabPanel("Таблиця",htable("tbl", colHeaders="provided")),
      tabPanel("Інструкція",textOutput("text"))
    )
    )
  )
))