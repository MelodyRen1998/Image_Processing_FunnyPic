library("shiny")
library("markdown")
library(shinyjs)

shinyUI(fluidPage(
  useShinyjs(),
  title = "FunnyPic: Play With Your Image!",
  theme = "bootstrap.min.css",

  # css hacks for responsive images
  tags$head(tags$style(
    type="text/css",
    "#original_image img {max-width: 100%; width: 100%; height: auto}"
  )),

  tags$head(tags$style(
    type="text/css",
    "#svd_image img {max-width: 100%; width: 100%; height: auto}"
  )),

  tags$head(
    tags$style(HTML(".shiny-output-error-validation {color: red;}"))
    ),
  
  titlePanel("FunnyPic: Play With Your Image!"),

  tags$hr(),

  fluidRow(
    column(
      width = 6,
      fileInput("img_selfie", accept=c("image/jpeg", "image/png"), "Upload your selfie(PNG/JPEG)")
    ),
    column(
      width = 6,
      sliderInput("intk", "Slide to choose exposure value:",
                  min = 0, max = 1, value = 0.2)
    )
  ),

  tags$hr(),

  fluidRow(
    column(
      width = 6,
      selectInput("img_bg", "Choose your background(PNG/JPEG)",
                  choices = list.files('funny_img_bg/'),
                  selected = list.files('funny_img_bg/')[1])
    ),
    column(
      width = 6,
      textInput("text", "Type your favorite words here:", value = "")
    )
  ),

  tags$hr(),

  fluidRow(
    column(
      width = 6,
      h4("Original Image"),
      tags$hr(),
      tabsetPanel(
        # tabPanel('selfie', uiOutput("original_image_selfie")),
        tabPanel('selfie', HTML('<div id="original_image_selfie" style="width:100%"></div>')),
        tabPanel('background', imageOutput("original_image_bg", width = "100%", height = "200px"))
      )
    ),
    column(
      width = 6,
      h4("Funny Image"),
      tags$hr(),
      plotOutput("funny_image", width = "100%", height = "200px")
    )
  )
))
