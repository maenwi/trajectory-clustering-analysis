## 6. Velocity -------------------------------------------------------------
library(overlapping)
similarity_velocity <- function(obj1, obj2, plot = FALSE){
  X <- list(X1 = obj1$spd3D[-1],
            X2 = obj2$spd3D[-1])
  velocity_similarity <- overlap(X, type="1", plot = plot)$OV
  return(velocity_similarity)
}

## 7. Time -----------------------------------------------------------------
#install.packages("lubridate")
library(lubridate)

#obj1 <- individualWorak[[i]]
#obj2 <- individualWorak[[j]]
convert_to_seconds <- function(HOUR, MIN, SECOND){
  #시간을 초(numeric)로 바꾸어주는 함수
  return(HOUR * 60 * 60 + MIN * 60 + SECOND)
}

convert_to_time <- function(secondsInNumeric){
  #초(numeric)를 시간으로 바꾸어 주는 함수
  HOUR <- secondsInNumeric %/% 3600
  secondsInNumeric <- secondsInNumeric %% 3600
  MIN <- secondsInNumeric %/% 60
  secondsInNumeric <- secondsInNumeric %% 60
  SECOND <- secondsInNumeric
  return(paste(HOUR,MIN,SECOND, sep=":"))
}

similarity_time <- function(obj1, obj2){
  #두 obj의 time_similarity 계산
  
  #계산을 위한 사전 준비
  #계산의 IDEA
  # 1) 어차피 날짜는 큰 상관이 없음
  # 2) 시간 자료에서 시간:분:초 만 추출
  # 3) obj1의 등산 시작 시간 ~ 종료 시간과 obj2의 등산 시작 ~ 종료 시간의
  #    겹치는 넓이 확인
  # +) 이때 obj1과 obj2의 총 등산 시간에 따라, 겹치는 부분의 비율이 달라짐
  #    그래서 similarity_obj1 과 similarity_obj2를 따로 계산
  #    spatial similarity를 계산할 때 최솟값을 return 한 것과 같이
  #    여기서도 최솟값을 return
  
  obj1_start <- obj1$time[1]
  obj1_end <- obj1$time[length(obj1$time)]
  
  adjust <- 0
  if (day(obj1_start) != day(obj1_end)){
    adjust <- 24
  }
  
  obj1_start <- convert_to_seconds(hour(obj1_start), minute(obj1_start), second(obj1_start))
  obj1_end <- convert_to_seconds(hour(obj1_end)+adjust, minute(obj1_end), second(obj1_end))
  
  obj1_times <- seq(obj1_start, obj1_end)
  
  obj2_start <- obj2$time[1]
  obj2_end <- obj2$time[length(obj2$time)]
  
  adjust <- 0
  if (day(obj2_start) != day(obj2_end)){
    adjust <- 24
  }
  
  obj2_start <- convert_to_seconds(hour(obj2_start), minute(obj2_start), second(obj2_start))
  obj2_end <- convert_to_seconds(hour(obj2_end)+adjust, minute(obj2_end), second(obj2_end))
  
  obj2_times <- seq(obj2_start, obj2_end)
  
  timeOverlap <- intersect(obj1_times, obj2_times)
  
  if (length(timeOverlap) > 0 ){ #겹치는 시간이 있는 경우만 체크
    #겹치기 시작한 시간과, 겹치는게 끝난 시간
    overlapStart <- convert_to_time(timeOverlap[1])
    overlapEnd <- convert_to_time(timeOverlap[length(timeOverlap)])
    
    timeOverlap <- timeOverlap[length(timeOverlap)] - timeOverlap[1]
    #겹치는 시간이 얼마나 길었는지?
    howlong <- convert_to_time(timeOverlap)
    
    similarity_time_1 <- timeOverlap/length(obj1_times)
    similarity_time_2 <- timeOverlap/length(obj2_times)
    return(list(similarity = min(similarity_time_1,similarity_time_2),
                similarity_obj1 = similarity_time_1,
                similarity_obj2 = similarity_time_2,
                timeOverlap = c(overlapStart, overlapEnd, howlong)))
  }
  
  else{
    return(list(similarity = 0,
                similarity_obj1 = 0,
                similarity_obj2 = 0,
                timeOverlap = c("0", "0", "0")))
  }
}