library("shiny")
library("jpeg")
library("png")
library(httr)
library(downloader)
library(markdown)
library(EBImage)
library(curl)
source('request.R')

Sys.setlocale("LC_CTYPE", "UTF-8")

mydeploy <- function(){
  oldRncoding = getOption('encoding')
  options(encoding='UTF-8')
  options(repos=BiocManager::repositories())
  rsconnect::deployApp(account = 'yimeng', appName = 'imgfunny-yimeng')
  options(encoding=oldRncoding)
}

DEFAULT_SELFIE = "demo.jpg"
DEFAULT_SELFIE_URL = UploadMyfile(DEFAULT_SELFIE)

shinyServer(function(input, output, session) {
  # 设置默认上传的图片为 DEFAULT_SELFIE_URL
  shinyjs::runjs(sprintf("document.getElementById('original_image_selfie').innerHTML = \"<img style='width:100%%' class='thumb' src='%s'>\"",
        DEFAULT_SELFIE_URL))

  # 上传新图片时，直接展示
  shinyjs::runjs("
     function handleFileSelect(evt) {
       var selfie = document.getElementById('original_image_selfie')
       selfie.innerHTML = ''
       var files = evt.target.files; // FileList object
       console.log('files: ')
       console.log(evt.target.files)
       for (var i = 0, f; f = files[i]; i++) {
       // Only process image files.
       if (!f.type.match('image.*')) {
       continue;
       }
       var reader = new FileReader();
       // Closure to capture the file information.
       reader.onload = (function(theFile) {
       return function(e) {
       var span = document.createElement('span');
       span.innerHTML = ['<img style=\"width:100%\" class=\"thumb\" src=\"', e.target.result,
       '\" title=\"', escape(theFile.name), '\"/>'].join('');
       selfie.insertBefore(span, null);
       };
       })(f);
       // Read in the image file as a data URL.
       reader.readAsDataURL(f);
     }}
     document.getElementById('img_selfie').addEventListener('change', handleFileSelect, false);
  ")

  run_img_selfie = reactive({
    imgtype = input$img_selfie$type
    if (is.null(imgtype)) {
      img_selfie_path = DEFAULT_SELFIE
      imgtype = "image/jpeg"
    } else {
      validate(need(imgtype %in% c("image/jpeg", "image/png"), 
                    "Only JPEG and PNG images are supported"))
      img_selfie_path = input$img_selfie$datapath
    }
    img_selfie_url <- UploadMyfile(img_selfie_path)
    validate(need(!is.null(img_selfie_url), 
                  'Failed to upload your selfie, please try again later.'))
    list(local_path=img_selfie_path, online_path=img_selfie_url)
  })

  output$original_image_bg = renderImage({
    list(src = file.path('funny_img_bg', input$img_bg), style="width:100%")
  }, deleteFile = FALSE)

  run_funny_it = reactive({
    # 自拍
    selfie = run_img_selfie()
    resp = requestFace(selfie$online_path)
    validate(need(length(resp) > 0, 'Does not detect face in you picture'),
             need(is.null(resp$error), paste0('Detect face errr: ', resp$error$message)))
    imgFace = cutFace(selfie$local_path, resp[[1]]$faceLandmarks, input$intk)
    imgFace
  })

  output$funny_image = renderPlot({
    withProgress(message='Wait, I am trying...', {
      img_bg_file <- input$img_bg
      face_data <- run_funny_it()
      setProgress(value=0.3, message = '1. Detect face: done')

      # 融合背景
      funny_data <- mergeFace(file.path('funny_img_bg', img_bg_file), face_data)
      setProgress(value=0.6, message = '2. Make funny image: done')

      # 文字
      text_info <- input$text
      img_bg_file = input$img_bg
      cat('img_bg_file: ', img_bg_file, '\n')
      if (text_info == "") {
        if (img_bg_file == "panda.jpg") {
          text_info <- "老哥，稳"
        } else if (img_bg_file == "panda2.jpg"){
          text_info <- "你看看人家王祎帆"
        } else if (img_bg_file == "panda3.png"){
          text_info <- "全宇宙我最帅"
        } else if (img_bg_file == "panda4.png"){
          text_info <- "那你很棒棒哦"
        } else if (img_bg_file == "panda5.png"){
          text_info <- "你看看人家王祎帆"
        }
      }

      # 背景位置
      bg_config = bgParameter[[img_bg_file]]
      p <- plot(funny_data) + text(x=bg_config$text_x, y=bg_config$text_y, label=text_info,
                                   col="black", cex=2)
      setProgress(value=0.9, message = '3. Add text: done')
    })
    list(
      src = p,
      title = "Send it to your friends!"
    )
  })

})
