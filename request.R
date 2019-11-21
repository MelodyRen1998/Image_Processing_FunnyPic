#' 加载不同背景图的参数设置
source('bg_config.R')

#' 每个 key 最多用7天，而且每分钟限制20条请求，每次随机使用其中一个 key
availableKeys = c('36f206204a094503984317b2f300fea4',
                  '74122c0cd91548c5bcaf15af1cd6940e',
                  '00f6addfbf274dcf8e15e87fc0f2a30c')


#' 每个图片 url 对应的人脸位置是固定的，所以做个缓存
#' 同样的 url 不必重复请求，最多缓存`maxCacheLength`(1000)条
maxCacheLength = 1000
cacheRdsFile = 'face.cache.rds'
if (file.exists(cacheRdsFile)){
  urlFaceCacheData <<- readRDS(cacheRdsFile)
  message(sprintf('read %s cached image face data.', length(urlFaceCacheData)))
}else{
  urlFaceCacheData <<- list()
}

addFaceCacheData <- function(url, data){
  # 添加缓存 & 存到本地供下次读取
  urlFaceCacheData[[url]] <<- data
  nlen = length(urlFaceCacheData)
  if (nlen > maxCacheLength){
    urlFaceCacheData <<- urlFaceCacheData[(nlen-maxCacheLength+1):nlen]
  }
  saveRDS(urlFaceCacheData, cacheRdsFile)
  message(sprintf('save to file: %s; length: %s; ', cacheRdsFile, length(urlFaceCacheData)))
}

#' 微软接口探测人脸位置
#' 优先读取缓存
#' doc: https://eastasia.dev.cognitive.microsoft.com/docs/services/563879b61984550e40cbbe8d/operations/563879b61984550f30395236/console
requestFace <- function(fileurl) {
  if (!is.null(urlFaceCacheData[[fileurl]])){
    return(urlFaceCacheData[[fileurl]])
  }
  faceURL = "https://westcentralus.api.cognitive.microsoft.com/face/v1.0/detect?returnFaceId=true&returnFaceLandmarks=true&returnFaceAttributes=age"
  faceKEY = sample(availableKeys, 1)
  mybody = list(url = fileurl)
  faceResponse = httr::POST(
    url = faceURL,
    content_type('application/json'), add_headers(.headers = c('Ocp-Apim-Subscription-Key' = faceKEY)),
    body = mybody,
    encode = 'json'
  )
  resp = httr::content(faceResponse)
  if (is.null(resp$error) && (length(resp) > 0)){
    addFaceCacheData(fileurl, resp)
  }
  resp
}

#' 每个图片上传一次后就有 url 了，不必重复上传
cacheImageUrl = 'img.cache.rds'
if (file.exists(cacheImageUrl)){
  imgUrlCacheData <<- readRDS(cacheImageUrl)
  message(sprintf('read %s cached image to url.', length(imgUrlCacheData)))
}else{
  imgUrlCacheData <<- list()
}
addUrlCacheData <- function(file_md5, online_file_url){
  # 添加缓存 & 存到本地供下次读取
  imgUrlCacheData[[file_md5]] <<- online_file_url
  nlen = length(imgUrlCacheData)
  if (nlen > maxCacheLength){
    imgUrlCacheData <<- imgUrlCacheData[(nlen-maxCacheLength+1):nlen]
  }
  saveRDS(imgUrlCacheData, cacheImageUrl)
  message(sprintf('save to file: %s; length: %s; ', cacheImageUrl, length(imgUrlCacheData)))
}

UploadMyfile <- function(filepath){
  file_md5 = tools::md5sum(filepath)
  if (!is.null(imgUrlCacheData[[file_md5]])){
    return(imgUrlCacheData[[file_md5]])
  }
  api_url = 'https://sm.ms/api/upload'
  img_file <- httr::upload_file(filepath)
  response <- httr::POST(api_url, body=list(smfile=img_file)) # status of upload
  img_url <- content(response)$data$url
  print(paste0('img_url: ', img_url, '; code: ', content(response)$code))
  if (content(response)$code != 'success'){
    # stop('Failed to upload your selfie, please try again later.')
    return(NULL)
  }
  addUrlCacheData(file_md5, img_url)
  return(img_url)
}




#' 从接口返回的结果中截取人脸
#' @param localImgFile: 上传的图片的本地路径
#' @param faceLandmarks: 接口返回的人脸位置数据
#' @param k: 什么分位数？
cutFace <- function(localImgFile, faceLandmarks, k){
  pp<-as.data.frame(as.matrix(faceLandmarks))
  # head(pp)

  img=readImage(localImgFile)
  # display(img)
  # get original x y
  all.x = unlist(pp[,1])[seq(1,54,by=2)]
  all.y = unlist(pp[,1])[seq(1,54,by=2)+1]
  # get organs x y
  nm.x = as.numeric(all.x[c(3:5,20:27)])
  nm.y = as.numeric(all.y[c(3:5,20:27)])
  eye.x = as.numeric(all.x[c(6:17)])
  eye.y = as.numeric(all.y[c(6:17)])
  # rotate
  y.dist <- eye.y[6] - eye.y[9]  # + will rotate clockwise
  x.dist <- abs(eye.x[6] - eye.x[9])
  rt_index <- atan(y.dist/x.dist)
  img.rt = rotate(img, atan(y.dist/x.dist)*180/pi, bg.col = "white")

  # display(img.rt)
  theta <- abs(rt_index) %% pi  # turn + after rotate
  x <- nrow(img)
  y <- ncol(img)
  x1 <- all.x
  y1 <- all.y
  # new x y after rotate
  if (rt_index > 0){
    new_x <- y*sin(theta)+x1*cos(theta)-y1*sin(theta)
    new_y <- y1*cos(theta) + x1*sin(theta)
  } else {
    new_x <- x1*cos(theta) + y1*sin(theta)
    new_y <- x*sin(theta) + y1*cos(theta) - x1*sin(theta)
  }
  eye.new.x = new_x[c(6:17)]
  eye.new.y = new_y[c(6:17)]
  nm.new.x = new_x[c(3:5,20:27)]
  nm.new.y = new_y[c(3:5,20:27)]

  # **not run in shiny**
  # img=readImage(img.url)
  # img.mark=img.rt
  # for (i in 1:27){
  #   # loc = unlist(pp[i,1])
  #   x = new_x[i]
  #   y = new_y[i]
  #   img.mark@.Data[((x-2):(x+2)),((y-2):(y+2)),1]=1
  #   img.mark@.Data[((x-2):(x+2)),((y-2):(y+2)),2]=0
  #   img.mark@.Data[((x-2):(x+2)),((y-2):(y+2)),3]=0
  # }
  # display(img.mark)

  # make it gray
  img.gray=channel(img.rt, "gray")
  mean = quantile(img.rt[(min(new_x):max(new_x)),(min(new_y):max(new_y)),]@.Data,k)
  display(img.gray)
  # enhance constrast
  img.gray@.Data[which(img.gray@.Data>mean)]=1
  img.gray@.Data[which(img.gray@.Data<mean)]=0
  # clear image
  img.gray@.Data[,(0:min((eye.new.y[which(names(eye.new.y) == "eyebrowLeftInner.y")]),(eye.new.y[which(names(eye.new.y) == "eyebrowLeftOuter.y")])))]=1
  img.gray@.Data[0:min(nm.new.x[which(names(nm.new.x) == "mouthLeft.x")],nm.new.x[which(names(nm.new.x) == "noseLeftAlarOutTip.x")]),
                 (eye.new.y[which(names(eye.new.y) == "eyeLeftBottom.y")]+5):ncol(img.gray)]=1
  img.gray@.Data[max(nm.new.x[which(names(nm.new.x) == "mouthRight.x")],nm.new.x[which(names(nm.new.x) == "noseRightAlarOutTip.x")]):nrow(img.gray),
                 (eye.new.y[which(names(eye.new.y) == "eyeRightBottom.y")]+5):ncol(img.gray)]=1
  # display(img.gray)

  # cut face
  img.face = img.gray[(min(new_x):max(new_x)),(min(new_y):max(new_y))]
  return(img.face)
}


#' 融合抽取的人脸和背景图
#' TODO: 根据背景图自动找到填充人脸和文字的位置?
mergeFace <- function(bg_pic, img.face){
  panda = EBImage::readImage(bg_pic)
  panda = channel(panda, "gray")
  bg_config = bgParameter[[basename(bg_pic)]]
  right <- bg_config$right
  left <- bg_config$left
  up <- bg_config$up
  down <- bg_config$down
  shape_x <- (right - left + 1)
  shape_y <- (down - up + 1)
  img.pp <- resize(img.face, shape_x, shape_y)
  panda.hh <- panda
  # combine face with bg
  panda.hh@.Data[(left:right),(up:down)]=img.pp@.Data
  # par(family=getBaseFont())
  # p <- plot(panda.hh) +
  #   text(x=250,y=400,label = text,col="black",cex=2)
  return(panda.hh)
}


#' 融合图片，为避免重复计算，shiny 应用中没用到这个函数
#' @param fileurl: 人脸照片的网络链接
#' @param bg_pic: 背景图的本地路径
#' @return 融合后的照片数据
FunnyPic <- function(fileurl, bg_pic){
  resp = requestFace(fileurl)
  if (length(resp) == 0){  # without face in image
    message(paste0('Function `requestFace` does not detect face in ', fileurl))
    return(list(picture = NULL, info = 'Fail'))
  }
  if (!is.null(resp$error)){  # error
    message(paste0('Function `requestFace` error:', resp$error$message))
    return(list(picture = NULL, info = 'Fail'))
  }
  faceLandmarks = resp[[1]]$faceLandmarks
  imgFace = cutFace(localImgFile, faceLandmarks)
  funnyFaceData = mergeFace(imgFace, bg_pic)
  funnyFaceData
}
