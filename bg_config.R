#' 为不同背景图设置不同的参数
#' text_x, text_y: 放文字的位置
#' up, down, left, right: 放置人脸的位置
bgParameter <- list(
  'panda.jpg' = list(text_x = 250, text_y = 400,
                     right = 320, left = 140, up = 110, down = 290),
  'panda2.jpg' = list(text_x = 500, text_y = 950,
                      right = 470, left = 250, up = 280, down = 500),
  'panda3.png'= list(text_x = 500, text_y = 900,
                     right = 580, left = 320, up = 360, down = 600),
  'panda4.png' = list(text_x = 500, text_y = 860,
                      right = 530, left = 400, up = 400, down = 520),
  'panda5.png' = list(text_x = 500, text_y = 860,
                      right = 480, left = 270, up = 360, down = 550)
)