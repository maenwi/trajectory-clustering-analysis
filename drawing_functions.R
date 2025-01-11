library(ggplot2)
library(gridExtra)
library(tidyverse)
library(raster)
library(OpenStreetMap)
#library(hms) #time similarity 그림 그리는데 필요
library(lubridate)
library(sf)
library(hms)

df <- readRDS("path/to/your/rds.rds")

#등산객들의 등산 바운더리 확인
eastest <- 0
westest <- 10000
northest <- 0
southest <- 90
for (i in seq(1,length(df))){
  east <- max(df[[i]]$longitude)
  west <- min(df[[i]]$longitude)
  north <- max(df[[i]]$latitude)
  south <- min(df[[i]]$latitude)
  
  if (east > eastest) {eastest <- east}
  if (west < westest) {westest <- west}
  if (north > northest) {northest <- north}
  if (south < southest) {southest <- south}
}


#월악산 지도 확인
map <- openmap(c(northest, westest), c(southest, eastest), 
               zoom = 10, 
               type = "osm", 
               mergeTiles = TRUE)
map_p <- openproj(map)

# 두 trajectory의 유사도 시각화 함수 -------------------------------------------------------------------------
similarity_visualize <- function(num1, num2, indivList) {
  # Spatial ----------------------------------------------------------------
  spatial <- OpenStreetMap::autoplot.OpenStreetMap(map_p) +
    geom_point(data = data.frame(indivList[[num1]]),
               aes(x = longitude, y = latitude), col = "red") +
    geom_point(data = data.frame(indivList[[num2]]),
               aes(x = longitude, y = latitude), col = "blue") +
    labs(title = paste("Spatial Similarity of",num1,"vs",num2, sep =" "), 
         x = "longitude",
         y = "latitude")
  

  # Velocity ----------------------------------------------------------------
  df1 <- data.frame(value = indivList[[num1]]$spd3D[-1], group = as.character(num1))
  df2 <- data.frame(value = indivList[[num2]]$spd3D[-1], group = as.character(num2))
  df <- rbind(df1, df2)
  
  velocity <- ggplot(df, aes(x = value, y = ..density.., fill = group)) +
    geom_histogram(position = "identity", alpha = 0.5, binwidth = 1) +
    scale_fill_manual(values = c("red", "blue")) +
    labs(title = paste("Velocity Overlapping of",num1,"vs",num2, sep =" "),
         x = "spd3D",
         y = "Frequency") +
    theme_minimal()
  
  # Time ----------------------------------------------------------------
  time1 <- format(indivList[[num1]]$time, '%H:%M:%S')
  time1 <- as_hms(time1)
  df1 <- data.frame(time = time1, y = 1, group = as.character(num1))
  time2 <- format(indivList[[num2]]$time, '%H:%M:%S')
  time2 <- as_hms(time2)
  df2 <- data.frame(time = time2, y = 2, group = as.character(num2))
  
  df <- rbind(df1, df2)
  
  time <- ggplot(df, aes(x = time, y = y, color = group)) +
    geom_line(linewidth = 1.2) +
    scale_color_manual(values = c("red", "blue")) +
    labs(title = paste("Time Overlapping of",num1,"vs",num2, sep =" "),
         x = "Time",
         y = "Group") +
    theme_minimal() +
    coord_cartesian(ylim = c(-10,10)) + 
    theme(axis.text.y = element_text(size = 12)) #y축 글자크기 조정
  
  # p <- grid.arrange(spatial, velocity, time, nrow = 1, ncol = 3)
  # return(p)
  plot_list <- list(spatial = spatial,
                    velocity = velocity,
                    time = time)
  
  return(plot_list)
}

# 월악산 지도 그리는 함수 -------------------------------------------------------------------------
# 위에서 확인한 map_p를 통해,
# 월악산 지도를 그리는 함수.
# 투명도를 input으로 받아, 지도의 투명도를 결정정
draw_map <- function(alpha){
  p <- OpenStreetMap::autoplot.OpenStreetMap(map_p) +
    labs(x = "longitude", y = "latitude") +
    annotate(
      "rect",
      xmin = westest, xmax = eastest,   # 지도 범위에 맞춰 좌표 설정
      ymin = southest, ymax = northest,
      fill = "white", alpha = alpha    # 반투명 하얀색
    )
  
  return(p)
}

# 등산객의 등산 경로를 그려주는 함수 -------------------------------------------------------------------------
# indivList의 num번째 사람의 등산 경로를,
# alpha의 투명도를 가지는 color로 그림.
draw_route <- function(num, indivList, color, alpha){
  p <- geom_point(data = data.frame(indivList[[num]]),
                  aes(x = longitude, y = latitude), 
                  col = color, 
                  size = 1, 
                  alpha = alpha)
  
  return(p)
}