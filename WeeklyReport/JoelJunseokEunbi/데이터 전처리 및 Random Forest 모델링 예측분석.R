################################################################################
### Set Library
################################################################################
set.seed(2020)
library(readxl)
library(tidyr)
library(MLmetrics)
library(purrr)
library(tidyverse)
library(plotly)
library(caret)
library(reshape2)
library(randomForest)
library(rpart)
library(ROCR)
library(xgboost)
library(readr)
library(foreign)
library(httr)
library(xml2)
library(tidyverse)
library(stringr)
library(corrplot)
library(MLmetrics)
options(scipen = 1000)
setwd("C:/Users/user/Desktop/영천시 빅데이터 공모전")

################################################################################
### Ver1 Data Setting - Farm Datas and PNU Code Info Data
################################################################################
# 해당 데이터는 영천시 팜맵 데이터로 코드 정보를 통해 농업 형태의 정보 제공
df <- read_csv("young_chun.csv")

head(df$PNU_LNM_CD)
# PNU코드에서 앞에 10자리는 행정구역 코드를 의미함
# 그 외는 세부 주소 -> 그런데 세부주소는 따른 변수에 직접적으로 언급됨
# 행정구역 코드 -> 실제 주소명으로 변환 + 세부 주소 내용 병합 -> 실제 주소 확보
df$법정동코드 <- substr(df$PNU_LNM_CD, 1,10)

# 해당 데이터는 대한민국 행정구역 코드 정보 제공
# 해당 데이터를 활용하여 PNU 코드 기준 실제 주소지 값 확보
df2 <- read_excel("KIKcd_B.20210701.xlsx")

# 행정구역 코드 기준 실제 주소 풀네임 변수 생성
df2 %>% filter(시군구명 == "영천시") %>% 
  select(법정동코드, 시도명, 시군구명, 읍면동명, 동리명) -> df2
df2$동리명 <- ifelse(is.na(df2$동리명), "", df2$동리명)
df2$주소 <- paste0(df2$시도명, " ", df2$시군구명, " ", df2$읍면동명, " ", df2$동리명)
df2 <- df2 %>% select(법정동코드, 주소)

# 데이터 병합을 통해 실제 주소 추가
df <- left_join(df, df2, by = "법정동코드") 
df$searching_index <- paste0(df$주소, " ", df$LNM)

# 데이터 프레임을 저장
# 크롤링의 경우 시간복잡도가 더 효율적인 파이썬에서 진행
# 실제 주소를 기반으로 위도 경도 데이터를 확보
#write_csv(df, file = "fram_before_find_latlon.csv")
rm(df, df2)

################################################################################
### Ver1 Data Setting - Reservoir Data and Factory Data
################################################################################
# 영천시 저수지 데이터 로드
# 저수지 데이터의 경우 소재지에 세부 주소가 있기 때문에 바로 크롤링 진행
reservoir <- read_excel("reservoir.xlsx")
reservoir <- reservoir[-1,]
#write_csv(reservoir, file = "reservoir_before_find_latlon.csv")

# 영천시 공장 데이터 로드
# 공장 데이터의 또한 공장대표주소_번지에 세부 주소가 있기 때문에 바로 크롤링 진행
factory <- read_csv("factory.csv")
#write_csv(factory, file = "factory_before_find_latlon.csv")

rm(reservoir, factory)

################################################################################
### Ver2 Data Setting
################################################################################
# 크롤링 작업 이후 위경도 데이터가 확보된 데이터 셋 로드
df <- read_csv("df.csv")
공장_final <- read_csv("공장_final.csv")
저수지_final <- read_csv("저수지_final.csv")

# 추가로 각 행정구역 면읍동, 리에 병합할 인구 데이터 로드
people <- read_excel("people.xlsx")
people[c(4,6),seq(6,53, 3)] -> people
names(people) <- people[1,]
people[1, ] <- lapply(people[1, ], function(x) gsub(",", "", x))
지역 <- as.character(people[1,])
인구수 <- as.character(people[2,])
people <- data_frame(지역, 인구수)
people$인구수 <- as.numeric(gsub(",","", people$인구수))

# 추가로 각 행정구역 면읍동, 리에 병합할 물 사용 데이터 로드
water <- read_csv("water.csv")
water %>% 
  filter(년도 == 2020) %>% 
  mutate(물_사용 = 가정용물사용량_합계+영업용물사용량_합계) %>% 
  select(읍면동, 리, 물_사용) -> water

water <- water %>%
  mutate_all(~ifelse(is.na(.), "", as.character(.)))

water$주소 <- paste0("경상북도 영천시 ", water$읍면동, " ", water$리)
water %>% select(주소, 물_사용) -> water

################################################################################
### Ver2 Data Merge and Set Final Data
################################################################################
df %>% 
  filter(is.na(위도) == FALSE & is.na(경도) == FALSE) -> df
df %>% spread(key = INTPR_NM, value = AREA) -> df
df <- df %>%
  mutate_all(~ifelse(is.na(.), 0, as.character(.)))

df$과수 <- as.numeric(df$과수)
df$논 <- as.numeric(df$논)
df$밭 <- as.numeric(df$밭)
df$시설 <- as.numeric(df$시설)
df$area = df$과수+df$논+df$밭+df$시설
df %>% 
  group_by(주소) %>% 
  summarise(area = mean(area), 과수 = mean(과수, na.rm = T), 논 = mean(논, na.rm = T), 밭 = mean(밭, na.rm = T), 시설 = mean(시설, na.rm = T), count = n()) -> df

공장_final %>% select(공장대표주소_지번, 용지면적, 건축면적) -> 공장_final
공장_final$주소 <- str_sub(공장_final$공장대표주소_지번,1,16)
공장_final %>% 
  select(주소, 용지면적, 건축면적) %>% 
  group_by(주소) %>% 
  summarise(용지면적 = mean(용지면적, na.rm = T), 건축면적= mean(건축면적, na.rm = T), 공장_count = n())-> 공장_final

저수지_final$주소2 <- str_sub(저수지_final$searching_index,1,12)
저수지_final %>% 
  group_by(주소2) %>% 
  summarise(유효저수량_km3 = mean(유효저수량_km3, na.rm = T), 관개면적_ha= mean(관개면적_ha,na.rm = T), 저수지_count = n()) -> 저수지_final

people$주소2 <- paste0("경상북도 영천시 ", people$지역)

df$주소2 <- str_sub(df$주소, 1, 12)

df <- left_join(df, 저수지_final, by = "주소2")
df <- left_join(df, people, by = "주소2")
df <- left_join(df, 공장_final, by = "주소")
df <- left_join(df, water, by = "주소")

df$유효저수량_km3 <- ifelse(is.na(df$유효저수량_km3) == TRUE, 0, df$유효저수량_km3)
df$관개면적_ha <- ifelse(is.na(df$관개면적_ha) == TRUE, 0, df$관개면적_ha)
df$용지면적 <- ifelse(is.na(df$용지면적) == TRUE, 0, df$용지면적)
df$건축면적 <- ifelse(is.na(df$건축면적) == TRUE, 0, df$건축면적)
df$공장_count <- ifelse(is.na(df$공장_count) == TRUE, 0, df$공장_count)

df %>% filter(is.na(물_사용) == FALSE) -> df
df$리 <- str_sub(df$주소, 10, 16)

df$count <- as.numeric(df$count)
df$인구수 <- gsub(",", "",df$인구수)
df$인구수 <- as.numeric(df$인구수)
df$물_사용 <- as.numeric(df$물_사용)
df$주소 <- as.factor(df$주소)
df$주소2 <- as.factor(df$주소2)
df$지역 <- as.factor(df$지역)
df$리 <- as.factor(df$리)
str(df)

#write.csv(df, file = "final_df.csv")
rm(공장_final, 저수지_final, people, water)
################################################################################
### Modeling
################################################################################
names(df) <- c("address","area","Fruit","crops","crops_bat","Factiliy","count",
               "address2","store_km3","water_area_ha", "res_num","Region","people",
               "fact_area","fact_bulid_area", "fact_num","water","li")

object <- c("신녕면 가천리","신녕면 화성리","신녕면 신덕리",
            "화산면 대안리","화산면 용평리","화산면 덕암리",
            "청통면 용천리")

# 상관관계 플롯
df[, -c(1,8,12,18)] -> df_cormat
cor_matrix <- cor(df_cormat)
corrplot(cor_matrix, method = "number", 
         tl.col = "black", tl.srt = 45)
df %>% 
  filter(li %in% object) %>% ungroup()-> test
df %>% 
  filter(!li %in% object) %>% ungroup()-> train

# 학습 데이터(영화지구가 아닌 영천시 데이터)를 활용한 모델 성능 확인
set.seed(2023)
train_idx <- sample(nrow(train[, -c(1,8,12,18)]), nrow(train[, -c(1,8,12,18)])*0.9) 
t_train_data <- train[, -c(1,8,12,18)][train_idx, ]
t_test_data <- train[, -c(1,8,12,18)][-train_idx, ]

t_train_water <- t_train_data$water
t_test_water<- t_test_data$water

model <- randomForest(x = t_train_data, y = t_train_water, 
                      ntree=400, importance=T, localImp=T, nodesize =3)
t_test_data$pred <- predict(model, newdata = t_test_data)

t_test_data %>% ggplot(aes(pred, t_test_water)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  scale_x_continuous(limits = c(0, 300),
                     breaks = seq(0, 300, 50)) +
  scale_y_continuous(limits = c(0, 300),
                     breaks = seq(0, 300, 50))


print(paste0("rmse : ", RMSE(t_test_data$pred, t_test_water)))
print(paste0("mape : ", MAPE(t_test_data$pred, t_test_water)))

# 학습 데이터 및 예측 데이터(영화지구)를 활용한 예측
set.seed(2022)
train_data <- train[, -c(1,8,12,18)]
test_data <- test[, -c(1,8,12,18)]

train_water <- train_data$water
test_water <- test_data$water

model <- randomForest(x = train_data,
                      y = train_water, ntree=500, importance=FALSE,nodesize =2)
test_data$pred <- predict(model, newdata = test_data)

test_data %>% ggplot(aes(test_water, pred)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  scale_x_continuous(limits = c(0, 400),
                     breaks = seq(0, 400, 100)) +
  scale_y_continuous(limits = c(0, 400),
                     breaks = seq(0, 400, 100))

print(paste0("rmse : ", RMSE(test_data$pred, test_water)))
print(paste0("mape : ", MAPE(test_data$pred, test_water)))

test$pred_water <- test_data$pred
test$gap <- test$water - test$pred_water
test$gap_per <- test$water/test$pred_water
test$water_label <- ifelse(test$gap_per < 0.7, 3,
                           ifelse(test$gap_per < 1, 2, 1))

################################################################################
### Ver2 Data Resetting for Gird Analysis
################################################################################
# 크롤링 작업 이후 위경도 데이터가 확보된 데이터 셋 로드
df <- read_csv("df.csv")
df$세부주소 <- substr(df$주소, 10, 17)
df %>% filter(세부주소 %in% object) -> df

공장_final <- read_csv("공장_final.csv")
저수지_final <- read_csv("저수지_final.csv")
################################################################################
### 격자 분석
################################################################################
# 대한민국 영천시의 위경도 범위
x_min <- 128.6
x_max <- 129.2
y_min <- 35.8
y_max <- 36.15

# 격자 크기 설정 (500m x 500m)
cell_size <- 500

# 경위도 간 거리 계산 함수 (Haversine formula)
haversine <- function(lon1, lat1, lon2, lat2) {
  # convert decimal degrees to radians
  lon1 <- lon1 * (pi/180)
  lat1 <- lat1 * (pi/180)
  lon2 <- lon2 * (pi/180)
  lat2 <- lat2 * (pi/180)
  
  # haversine formula
  dlon <- lon2 - lon1
  dlat <- lat2 - lat1
  a <- sin(dlat / 2)^2 + cos(lat1) * cos(lat2) * sin(dlon / 2)^2
  c <- 2 * asin(sqrt(a))
  r <- 6371  # radius of the earth in kilometers
  return(c * r * 1000)  # return distance in meters
}

# 격자 생성 함수
generate_grid <- function(x_min, x_max, y_min, y_max, cell_size) {
  x_range <- haversine(x_min, y_min, x_max, y_min)
  y_range <- haversine(x_min, y_min, x_min, y_max)
  
  n_cells_x <- as.integer(x_range / cell_size)
  n_cells_y <- as.integer(y_range / cell_size)
  
  x_breaks <- vector("numeric", n_cells_x + 1)
  y_breaks <- vector("numeric", n_cells_y + 1)
  
  for (i in 1:(n_cells_x + 1)) {
    x <- x_min + (i * cell_size / x_range) * (x_max - x_min)
    x_breaks[i] <- x
  }
  
  for (i in 1:(n_cells_y + 1)) {
    y <- y_min + (i * cell_size / y_range) * (y_max - y_min)
    y_breaks[i] <- y
  }
  
  polygons <- data.frame(
    id = numeric(n_cells_x * n_cells_y),
    x_min = numeric(n_cells_x * n_cells_y),
    y_min = numeric(n_cells_x * n_cells_y),
    x_max = numeric(n_cells_x * n_cells_y),
    y_max = numeric(n_cells_x * n_cells_y)
  )
  
  ids <- numeric(n_cells_x * n_cells_y)
  
  index <- 1
  for (i in 1:n_cells_y) {
    for (j in 1:n_cells_x) {
      polygon <- data.frame(
        id = index,
        x_min = x_breaks[j],
        y_min = y_breaks[i],
        x_max = x_breaks[j + 1],
        y_max = y_breaks[i + 1]
      )
      polygons[index, ] <- polygon
      ids[index] <- index
      index <- index + 1
    }
  }
  
  grid <- data.frame(
    id = ids,
    x_min = polygons$x_min,
    y_min = polygons$y_min,
    x_max = polygons$x_max,
    y_max = polygons$y_max
  )
  return(grid)
}

# 격자 생성
grid <- generate_grid(x_min, x_max, y_min, y_max, cell_size)

add_center_coordinates <- function(grid) {
  grid$lon_center <- (grid$x_min + grid$x_max) / 2
  grid$lat_center <- (grid$y_min + grid$y_max) / 2
  return(grid)
}

# 중앙 좌표 변수 추가
grid <- add_center_coordinates(grid)



# Haversine 함수 정의
haversine <- function(lon1, lat1, lon2, lat2) {
  # convert decimal degrees to radians
  lon1 <- lon1 * (pi/180)
  lat1 <- lat1 * (pi/180)
  lon2 <- lon2 * (pi/180)
  lat2 <- lat2 * (pi/180)
  
  # haversine formula
  dlon <- lon2 - lon1
  dlat <- lat2 - lat1
  a <- sin(dlat / 2)^2 + cos(lat1) * cos(lat2) * sin(dlon / 2)^2
  c <- 2 * asin(sqrt(a))
  r <- 6371  # radius of the earth in kilometers
  return(c * r * 1000)  # return distance in meters
}

# 필요한 패키지 로드
library(dplyr)

# 각 격자의 중심 좌표를 기준으로 1km 반경 내의 저수지들을 탐색하고 물 평균 저장량 계산
for (i in 1:nrow(grid)) {
  center_lon <- grid$lon_center[i]
  center_lat <- grid$lat_center[i]
  
  # 반경 1km 내의 저수지 필터링
  within_radius <- 저수지_final %>%
    filter(haversine(center_lon, center_lat, 위도, 경도) <= 1000)
  
  # 물 평균 저장량 계산
  if (nrow(within_radius) > 0) {
    avg_유효저수량_km3 <- mean(within_radius$유효저수량_km3, na.rm = T)
    avg_관개면적_ha <- mean(within_radius$관개면적_ha, na.rm = T)
    num_of_dam <- nrow(within_radius)
  } else {
    avg_유효저수량_km3 <- NA
    avg_관개면적_ha <- NA
    num_of_dam <- NA
  }
  
  # 결과를 grid 데이터에 추가
  grid$avg_유효저수량_km3[i] <- avg_유효저수량_km3
  grid$avg_관개면적_ha[i] <- avg_관개면적_ha
  grid$num_of_dam[i] <- num_of_dam
}

# 각 격자의 중심 좌표를 기준으로 1km 반경 내의 공장들을 탐색하고 물 평균 저장량 계산
for (i in 1:nrow(grid)) {
  center_lon <- grid$lon_center[i]
  center_lat <- grid$lat_center[i]
  
  # 반경 1km 내의 저수지 필터링
  within_radius <- 공장_final %>%
    filter(haversine(center_lon, center_lat, 위도, 경도) <= 1000)
  
  # 물 평균 저장량 계산
  if (nrow(within_radius) > 0) {
    avg_용지면적 <- mean(within_radius$용지면적, na.rm = T)
    avg_건축면적 <- mean(within_radius$건축면적, na.rm = T)
    num_of_fact <- nrow(within_radius)
  } else {
    avg_용지면적 <- NA
    avg_건축면적 <- NA
    num_of_fact <- NA
  }
  
  # 결과를 grid 데이터에 추가
  grid$avg_용지면적[i] <- avg_용지면적
  grid$avg_건축면적[i] <- avg_건축면적
  grid$num_of_fact[i] <- num_of_fact
}


################################################################################
### Farm Data Processing
################################################################################
names(df)
df <- df %>%mutate(id = NA_integer_)

test[,c(13,16:22)] -> mini_test
names(mini_test) <- c("people","fact_num","water","세부주소","pred_water","gap","gap_per","water_label")
df <- left_join(df, mini_test, by = "세부주소")

# 데이터의 위도와 경도에 해당하는 격자 ID 부여
for (i in 1:nrow(df)) {
  longitude <- df$위도[i]
  latitude <- df$경도[i]
  
  grid_index <- which(
    grid$x_min <= longitude & grid$x_max > longitude &
      grid$y_min <= latitude & grid$y_max > latitude
  )
  
  if (length(grid_index) > 0) {
    df$id[i] <- grid$id[grid_index]
  }
}
# 농작물 위치 수
df %>% group_by(id, INTPR_NM) %>% summarise(n = n()) -> crops_number_info
crops_number_info %>% spread(key = "INTPR_NM", value = "n") -> crops_number_info
crops_number_info %>% mutate(across(everything(), ~ifelse(is.na(.), 0, .))) -> crops_number_info
crops_number_info$전체 <- crops_number_info$과수+crops_number_info$논+crops_number_info$밭+crops_number_info$시설

# 농작물 전체 면적
df %>% group_by(id, INTPR_NM) %>% summarise(area_sum = sum(AREA, na.rm = T)) -> crops_sum_area_info
crops_sum_area_info %>% spread(key = "INTPR_NM", value = "area_sum") -> crops_sum_area_info
crops_sum_area_info %>% mutate(across(everything(), ~ifelse(is.na(.), 0, .))) -> crops_sum_area_info
names(crops_sum_area_info) <- c("id","총_과수_면적","총_논_면적","총_밭_면적","총_시설_면적")
crops_sum_area_info$전체_면적_합 <- crops_sum_area_info$총_과수_면적+crops_sum_area_info$총_논_면적+crops_sum_area_info$총_밭_면적+crops_sum_area_info$총_시설_면적

# 물사용량 & 인구 정보 정리
df %>% group_by(id) %>% summarise(people = mean(people, na.rm = T), level = mean(water_label, na.rm = T), pred_water = mean(pred_water, na.rm = T), water = mean(water, na.rm = T)) -> crops_water_people_info

crops_info <- left_join(crops_number_info, crops_sum_area_info, by = "id")
crops_info <- left_join(crops_number_info, crops_water_people_info, by = "id")

################################################################################
##### 최종 병합
################################################################################
# 그리드 정보를 포함한 데이터 병합
merged_data <- grid %>%
  left_join(crops_info, by = "id")

merged_data %>%
  filter(rowSums(is.na(.)) <= 7) -> merged_data

merged_data[is.na(merged_data)] <- 0
write_csv(merged_data, file = "merge_data_500.csv")

grid_data <- merged_data

x_min <- 128.3
x_max <- 129.3
y_min <- 35.8
y_max <- 36


# 저수지 필터링
저수지_final <- 저수지_final %>% select(유효저수량_km3, 관개면적_ha, searching_index, 위도, 경도)
names(저수지_final) <- c("유효저수량_km3","관개면적_ha","주소","위도","경도")
저수지_final <- 저수지_final %>% mutate(id = NA_integer_)

# 데이터의 위도와 경도에 해당하는 격자 ID 부여
for (i in 1:nrow(저수지_final)) {
  longitude <- 저수지_final$위도[i]
  latitude <- 저수지_final$경도[i]
  
  grid_index <- which(
    grid$x_min <= longitude & grid$x_max > longitude &
      grid$y_min <= latitude & grid$y_max > latitude
  )
  
  if (length(grid_index) > 0) {
    저수지_final$id[i] <- grid$id[grid_index]
  }
}

저수지_final %>% filter(id %in% grid_data$id) -> 저수지_final


# 공장
공장_final %>% select(c("용지면적","건축면적","지역","위도","경도")) -> 공장_final

공장_final <- 공장_final %>%
  mutate(id = NA_integer_)

# 데이터의 위도와 경도에 해당하는 격자 ID 부여
for (i in 1:nrow(공장_final)) {
  longitude <- 공장_final$위도[i]
  latitude <- 공장_final$경도[i]
  
  grid_index <- which(
    grid$x_min <= longitude & grid$x_max > longitude &
      grid$y_min <= latitude & grid$y_max > latitude
  )
  
  if (length(grid_index) > 0) {
    공장_final$id[i] <- grid$id[grid_index]
  }
}

공장_final %>% filter(id %in% grid_data$id) -> 공장_final

# 지도와 격자 데이터를 함께 시각화합니다.
ggplot() +
  geom_rect(data = grid_data, aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max, fill = level),
            color = "black", size = 1.5) +
  geom_point(data = df, aes(x = 위도, y = 경도), color = "gray",  alpha = 0.7) +
  geom_point(data = 저수지_final, aes(x = 위도, y = 경도),  color = "red", size = 2) +
  geom_point(data = 공장_final, aes(x = 위도, y = 경도),  color = "yellow", size = 2) +
  theme_minimal()