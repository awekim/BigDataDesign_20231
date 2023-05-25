# 팜맵 데이터 geo 좌표 병합
# 공공데이터 활용
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
options(scipen = 1000)
setwd("~/Downloads/contest")

people <- read_excel("people.xlsx")
reservoir <- read_excel("reservoir.xlsx")
factory <- read_csv("factory.csv")
crops_rice <- read.dbf("crops_rice.dbf")
################################################################################
# 영천시 영화지구 인구수
################################################################################
# 데이터 오버뷰
head(people)

# 영화지구에 해당되는 지역들 추출 및 NA 변수 제거
object_people <- people[c(7:9),-2]
names(object_people) <- c("지역", "총인구수", "남인구수", "여인구수", "남녀구성비", "남구성비", "여구성비", "성비", "세대수", "세대당인구수")

# 필터링 이후 데이터 오버뷰
head(object_people)

# 적용 방안
# 다른 데이터와 병합을 진행할 때, 지역별로 총인구수, 남인구수, 여인구수, 성비, 세대수, 서대당인구수 변수를 활용하여 병합
object_people %>% select(지역, 총인구수, 남인구수, 여인구수, 성비, 세대수, 세대당인구수) -> object_people
convert_to_numeric <- function(x) {
  x <- gsub(",", "", x)
  return(as.numeric(x))
}

# 열 데이터 타입 전처리
str(object_people)
object_people$지역 -> region
object_people %>% select(-지역) %>% mutate_all(convert_to_numeric) -> object_people
object_people$지역 <- region
rm(region, convert_to_numeric)
object_people %>% select(지역, 총인구수, 남인구수, 여인구수, 성비, 세대수, 세대당인구수) -> object_people


# 영화지구 면,읍별 인구수 시각화화
pastel_colors <- c("#FFD6A5", "#FFCAD4", "#C3F6C8")
object_people %>%
  ggplot(aes(x = 지역, y = 총인구수, fill = 지역)) +
  geom_col(width = 0.5, color = "black", size = 1) +
  scale_fill_manual(values = pastel_colors) +
  theme_bw() +
  ylab("명") +
  xlab(NULL) +
  ggtitle("영천시 영화지구 면,읍별 인구수") + 
  theme(plot.title = element_text(size = 17, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14, face = "bold")) +
  geom_text(aes(label = paste0(총인구수)), vjust = -0.5, size = 6) +
  scale_y_continuous(limits = c(0, 12000))


# 영화지구 면,읍별 남녀인구수 시각화화
pastel_colors <- c("#A8E6CF", "#FFD6A5", "#D0F0C0")
object_people %>% 
  gather(남인구수, 여인구수, key = "key", value = "value") %>% 
  mutate(성별 = key) %>% 
  ggplot(aes(지역,value, fill = 성별)) +
  geom_col(position = "dodge", width = 0.6, color = "black", size = 0.5) +
  scale_fill_manual(values = pastel_colors) +
  theme_bw() +
  ylab("명") +
  xlab(NULL) +
  ggtitle("영천시 영화지구 면,읍별 남녀 인구수") +
  theme(plot.title = element_text(size = 17, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14, face = "bold")) +
  geom_text(aes(label = value, group = 성별),
            position = position_dodge(width = 0.6), vjust = -0.5, size = 4.5) +
  scale_y_continuous(limits = c(0, 7000))

# 영화지구 인구 정보
object_people

# 눈에 띄게 식별되는 인사이트는 없음
# 그러나 농경지 비율에 따라 성비에 차이가 있을 수 있다는 추측이 듦
# 인구수 == 물 사용량은 비례하지만
# 만약 반대로 인구가 적은 지역에 농경지, 공장 등의 시설들이 더 많다면 다른 변수가 될 수도 있음

non_object_people <- people[c(5,9:21),-2]
names(non_object_people) <- c("지역", "총인구수", "남인구수", "여인구수", "남녀구성비", "남구성비", "여구성비", "성비", "세대수", "세대당인구수")
# 열 데이터 타입 전처리
non_object_people %>% select(지역, 총인구수, 남인구수, 여인구수, 성비, 세대수, 세대당인구수) -> non_object_people
convert_to_numeric <- function(x) {
  x <- gsub(",", "", x)
  return(as.numeric(x))
}
str(non_object_people)
non_object_people$지역 -> region
non_object_people %>% select(-지역) %>% mutate_all(convert_to_numeric) -> non_object_people
non_object_people$지역 <- region
rm(region, convert_to_numeric)
non_object_people %>% select(지역, 총인구수, 남인구수, 여인구수, 성비, 세대수, 세대당인구수) -> non_object_people

non_object_people %>% summarise(type = "non_object", 평균인구수 = mean(총인구수), 평균성비 = mean(성비),
                                평균세대수 = mean(세대수), 평균세대당인구수 = mean(세대당인구수)) -> summary_non_object_people
object_people %>% summarise(type = "object",평균인구수 = mean(총인구수), 평균성비 = mean(성비),
                            평균세대수 = mean(세대수), 평균세대당인구수 = mean(세대당인구수)) -> summary_object_people
rbind(summary_non_object_people, summary_object_people)

# 인구가 상대적으로 부족한 지역에 진행되는 사업으로 여겨짐

################################################################################
# 영천시 영화지구 저수지 정보
################################################################################
head(reservoir)
reservoir <- reservoir[-1,]
names(reservoir) <- c("저수지명", "한발빈도", "유역면적_ha", "유효저수량_km3", "댐형식",
                      "관개면적_ha", "순관개면적", "관배수면적", "구역외급수면적", "소재지")

# 한발 뜻이란? : 공사에서는 “한발빈도”라는 말을 사용하고 있는데 예를 들어 
#                어떤 저수지의 설계 시 적용된 한발 빈도가 10년이라면 10년에 
#                한 번 올 정도의 가뭄이 발생하더라도 주변 지역에 물을 충분히 
#                 공급할 수 있을 정도로 저수지를 설계했다는 뜻

# 순관개면적 뜻이란? : 순관개면적은 토지의 총 면적에서 길, 건물, 물이나 계곡 등 
#                      농작물을 재배할 수 없는 부분을 제외한 부분. 이 면적은 
#                      농작물의 생산량을 추정하거나 농업 계획을 세울 때 중요한 
#                      지표로 사용됨

# 관배수면적 뜻이란? : 농업용 물을 공급하거나 배수하는 시설, 특히 관개와 배수를 
#                      동시에 수행하는 시설에서 사용되는 토지의 면적을 의미. 
#                      관배수는 농작물의 생육에 필요한 물 공급 및 물 관리를 
#                      위해 설치되며, 여기에 포함된 토지는 농작물 재배에 적합한 
#                      환경을 유지하는 데 도움이 됨

# 구역외급수면적 뜻이란? : 구역 외 급수 면적이란, 특정 수원 지역에서 공급되는 
#                          물이 해당 지역의 경계를 넘어 다른 지역으로 공급되는 
#                          토지의 면적을 의미. 이는 특정 지역의 물 공급 시설이 
#                          다른 지역의 농작물 재배, 가축 사육, 생활용수 등에 
#                          사용되기 때문에 발생하는 현상


# NA 변수 제거
reservoir <- reservoir[,-c(8,9)]

# NA 변수 제거
reservoir$유역면적_ha <- as.numeric(reservoir$유역면적_ha)
reservoir$유효저수량_km3 <- as.numeric(reservoir$유효저수량_km3)
reservoir$관개면적_ha <- as.numeric(reservoir$관개면적_ha)
reservoir$순관개면적 <- as.numeric(reservoir$순관개면적)
reservoir$댐형식 <- as.factor(reservoir$댐형식)

# 영화지구 이외에 해당되는 지역 필터링
reservoir %>% filter(str_detect(소재지,"가천리") == FALSE &  
                       str_detect(소재지,"화성리") == FALSE &
                       str_detect(소재지,"신덕리") == FALSE &  
                       str_detect(소재지,"대안리") == FALSE & 
                       str_detect(소재지,"용평리") == FALSE &
                       str_detect(소재지,"덕암리") == FALSE &
                       str_detect(소재지,"용천리") == FALSE) -> non_object_reservoir

# 영화지구에 해당되는 지역 필터링
reservoir %>% filter(str_detect(소재지,"가천리") == TRUE |  
                       str_detect(소재지,"화성리") == TRUE |
                       str_detect(소재지,"신덕리") == TRUE |  
                       str_detect(소재지,"대안리") == TRUE | 
                       str_detect(소재지,"용평리") == TRUE |
                       str_detect(소재지,"덕암리") == TRUE |
                       str_detect(소재지,"용천리") == TRUE) -> object_reservoir

# 지역명 내용 추가(추후 병합에 활용)
object_reservoir$지역 <- if_else(str_detect(object_reservoir$소재지, "신녕면") == TRUE, "신녕면",
                               if_else(str_detect(object_reservoir$소재지, "화산면") == TRUE, "화산면", "청통면"))

non_object_reservoir %>% 
  summarise(type = "non_object", 한발빈도 = mean(한발빈도, na.rm = TRUE), 유역면적_ha = mean(유역면적_ha, na.rm = TRUE),
            유효저수량_km3 = mean(유효저수량_km3, na.rm = TRUE), 관개면적_ha = mean(관개면적_ha, na.rm = TRUE),
            순관개면적 = mean(순관개면적, na.rm = TRUE)) -> summary_non_object_reservoir
object_reservoir %>% 
  summarise(type = "object", 한발빈도 = mean(한발빈도, na.rm = TRUE), 유역면적_ha = mean(유역면적_ha, na.rm = TRUE),
            유효저수량_km3 = mean(유효저수량_km3, na.rm = TRUE), 관개면적_ha = mean(관개면적_ha, na.rm = TRUE),
            순관개면적 = mean(순관개면적, na.rm = TRUE)) -> summary_object_reservoir

rbind(summary_non_object_reservoir, summary_object_reservoir) -> compare_dam

compare_dam

# 영화지구의 저수지들이 다른 영천시 내 저수지들보다 유효저수량이 상대적으로 부족
pastel_colors <- c("#FFD6A5", "#FFCAD4")
compare_dam %>% 
  ggplot(aes(type, 유효저수량_km3, fill = type)) +
  scale_fill_manual(values = pastel_colors) +
  geom_col(width = 0.5, color = "black", size = 1) +
  theme_bw() +
  ylab("유효저수량km3") +
  xlab(NULL) +
  ggtitle("영천시 영화지구 면,읍별 인구수") + 
  theme(plot.title = element_text(size = 17, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14, face = "bold")) +
  geom_text(aes(label = paste0(round(유효저수량_km3,2))), vjust = -0.5, size = 6) +
  scale_y_continuous(limits = c(0, 40)) 

################################################################################
# 영천시 영화지구 농경지 정보
################################################################################
head(crops_rice)

str(crops_rice)

crops_rice$지역 <- if_else(str_detect(crops_rice$emd_name, "신녕면") == TRUE, "신녕면",
                         if_else(str_detect(crops_rice$emd_name, "화산면") == TRUE, "화산면", 
                                 if_else(str_detect(crops_rice$emd_name, "청통면") == TRUE,"청통면", "영화지구이외지역")))
table(crops_rice$지역)

crops_rice %>% filter(지역 == "영화지구이외지역") -> non_object_crops_rice
crops_rice %>% filter(지역 != "영화지구이외지역") -> object_crops_rice

# 평균 면적은 영화지구가 더 적으나
mean(non_object_crops_rice$area)
sum(non_object_crops_rice$area)

# 전체 면적은 압도적으로 차이가 남
mean(object_crops_rice$area)
sum(object_crops_rice$area)

# 순수 저수지 & 댐 개수 논 대비 면적은 어케 될까?
sum(non_object_crops_rice$area)/nrow(non_object_reservoir)
sum(object_crops_rice$area)/nrow(object_reservoir)

# 유효저수지량 대비 논 재배 면적은 어케 될까?
sum(non_object_crops_rice$area)/sum(non_object_people$총인구수)
sum(object_crops_rice$area)/sum(object_people$총인구수)

# 인구수 대비 논 재배 면적은 어케 될까?
sum(non_object_crops_rice$area)/sum(non_object_reservoir$유효저수량_km3, na.rm = T)
sum(object_crops_rice$area)/sum(object_reservoir$유효저수량_km3, na.rm = T)

# 오, 영화지구에서는 저수지 개수 대비 벼 재배 면적이 더 넓음..! 
# 다만 인구수 인구 대비 재배 면적은 확실히 좁긴 함
# 그러나 인프라가 부족한 것이 일부 입증
# 해당 데이터는 논 재배가지고만 확인한 것이기에, 다른 과수, 휴경지 등의 농업 정보도 활용해야함

################################################################################
# 영천시 영화지구 공장 정보
################################################################################
head(factory)
names(factory) <- c("순번","단지명","회사명","대표자명","공장대표주소_도로명","공장대표주소_지번", 
                    "대표업종번호","업종명","전화번호","생산품","용지면적","건축면적" )

factory$지역 <- if_else(str_detect(factory$공장대표주소_지번, "신녕면") == TRUE, "신녕면",
                      if_else(str_detect(factory$공장대표주소_지번, "화산면") == TRUE, "화산면", 
                              if_else(str_detect(factory$공장대표주소_지번, "청통면") == TRUE,"청통면", "영화지구이외지역")))
table(factory$지역)

factory %>% filter(지역 == "영화지구이외지역") -> non_object_factory
factory %>% filter(지역 != "영화지구이외지역") -> object_factory

# 공장 수 대비 인구수는 어떻게 될까?
sum(non_object_people$총인구수)/nrow(non_object_factory)
sum(object_people$총인구수)/nrow(object_factory)

# 영화지구 지역 이외의 공장대비 인구 수가 더 많음
# 궁극적으로 물 소비와 관련되는 학교, 공공시설 등 인프라 위치 정보를 조금 더 확보할 필요가 있음

output_df <- data.frame()
for (i in 1:nrow(object_crops_rice)) {
  object_crops_rice[i,1] -> fram_map_id
  
  url <- paste0("https://apis.data.go.kr/B552895/getFarmmapService/getIdBasedFarmmapInfo?serviceKey=7U1DN0vlDmzWJzFnL55aHBi3UpdMeTb%2BX%2Bp4XLMdXGCoSpJuZ0J6izERjPgm5cGcpShT3xshqjpswkMWXdwNFA%3D%3D&type=xml&id=",fram_map_id)
  
  response <- GET(url)
  content <- content(response, as = "text", encoding = "UTF-8")
  content <- gsub("\n", "", content)  # \n 제거
  
  # 필요한 내용 추출
  fmapInnb <- sub('.*<fmapInnb>(.*?)</fmapInnb>.*', '\\1', content)
  fmapBdcrd <- sub('.*<fmapBdcrd>(.*?)</fmapBdcrd>.*', '\\1', content)
  vdptYr <- sub('.*<vdptYr>(.*?)</vdptYr>.*', '\\1', content)
  pnuLnmCd <- sub('.*<pnuLnmCd>(.*?)</pnuLnmCd>.*', '\\1', content)
  lglEmdCd <- sub('.*<lglEmdCd>(.*?)</lglEmdCd>.*', '\\1', content)
  lglEmdNm <- sub('.*<lglEmdNm>(.*?)</lglEmdNm>.*', '\\1', content)
  chgCfcd <- sub('.*<chgCfcd>(.*?)</chgCfcd>.*', '\\1', content)
  chgCfnm <- sub('.*<chgCfnm>(.*?)</chgCfnm>.*', '\\1', content)
  lnm <- sub('.*<lnm>(.*?)</lnm>.*', '\\1', content)
  intprCd <- sub('.*<intprCd>(.*?)</intprCd>.*', '\\1', content)
  intprNm <- sub('.*<intprNm>(.*?)</intprNm>.*', '\\1', content)
  invdCfcd <- sub('.*<invdCfcd>(.*?)</invdCfcd>.*', '\\1', content)
  invdCfnm <- sub('.*<invdCfnm>(.*?)</invdCfnm>.*', '\\1', content)
  itpinpDe <- sub('.*<itpinpDe>(.*?)</itpinpDe>.*', '\\1', content)
  rnhstCd <- sub('.*<rnhstCd>(.*?)</rnhstCd>.*', '\\1', content)
  rnhstNm <- sub('.*<rnhstNm>(.*?)</rnhstNm>.*', '\\1', content)
  mapdmcNo <- sub('.*<mapdmcNo>(.*?)</mapdmcNo>.*', '\\1', content)
  
  # coordinates 추출
  coordinate <- sub('.*<coordinates>(.*?)</coordinates>.*', '\\1', fmapBdcrd)
  
  # <gml:coordinates>와 </gml:coordinates> 사이에 있는 숫자들만 추출
  coordinates_extracted <- str_extract_all(coordinate, "\\d+\\.\\d+")
  
  # 추출된 숫자들을 공백으로 연결하여 문자열로 변환
  coordinates_final <- sapply(coordinates_extracted, paste, collapse = " ")
  
  # 데이터 프레임 생성
  df <- data.frame(
    fmapInnb,
    fmapBdcrd,
    coordinates_final,
    vdptYr,
    pnuLnmCd,
    lglEmdCd,
    lglEmdNm,
    chgCfcd,
    chgCfnm,
    lnm,
    intprCd,
    intprNm,
    invdCfcd,
    invdCfnm,
    itpinpDe,
    rnhstCd,
    rnhstNm,
    mapdmcNo
  )
  
  # coordinates_final을 묶음별로 분할하여 리스트로 저장
  coordinates_split <- strsplit(coordinates_final, " ")
  
  centroid_lat <- sapply(coordinates_split, function(coords) mean(as.numeric(coords[seq(1, length(coords), 2)])))
  centroid_lon <- sapply(coordinates_split, function(coords) mean(as.numeric(coords[seq(2, length(coords), 2)])))
  
  # 중심점 좌표 포맷팅
  centroid_lat <- sprintf("%.10f", centroid_lat)
  centroid_lon <- sprintf("%.10f", centroid_lon)
  
  # 데이터 프레임에 중심점 열 추가
  df$centroid_lat <- centroid_lat
  df$centroid_lon <- centroid_lon
  
  df %>% select(centroid_lat, centroid_lon) -> mini_df
  output_df <- rbind(output_df, mini_df)
}

object_crops_rice_df <- cbind(object_crops_rice, output_df)

object_crops_rice
object_factory$region <- ifelse(str_detect(object_factory$공장대표주소_지번, "신녕면"))

object_crops_rice$region <- if_else(str_detect(object_factory$공장대표주소_지번, "신녕면") == TRUE, "신녕면",
                                    if_else(str_detect(object_factory$공장대표주소_지번, "화산면") == TRUE, "화산면", 
                                            if_else(str_detect(object_factory$공장대표주소_지번, "청통면") == TRUE,"청통면", "영화지구이외지역")))


object_factory$리 <- substr(object_factory$공장대표주소_지번, 14, 16) 
object_factory %>% group_by(리) %>% summarise(count = n(), 평균용지면적 = mean(용지면적), 평균건축면적 = mean(건축면적)) -> output_object_factory
object_reservoir$리 <- substr(object_reservoir$소재지, 14, 16)
object_reservoir %>% group_by(리) %>% summarise(count = n(), 평균관개면적 = mean(관개면적_ha, na.rm = T), 평균순관개면적 = mean(순관개면적, na.rm = T)) -> output_oobject_reservoir

objective_df <- left_join(object_crops_rice_df, object_people, by = "지역")
objective_df$리 <- substr(objective_df$emd_name, 14, 16) 

names(output_object_factory) <- c("리", "Factory_Count", "평균용지면적", "평균건축면적")
names(output_oobject_reservoir) <- c("리", "Reservoir_Count", "평균관개면적", "평균순관개면적")
objective_df <- left_join(objective_df, output_object_factory, by = "리")
objective_df <- left_join(objective_df, output_oobject_reservoir, by = "리")


objective_df$평균건축면적[is.na(objective_df$평균건축면적)] <- 0
objective_df$평균관개면적[is.na(objective_df$평균관개면적)] <- 0
objective_df$평균용지면적[is.na(objective_df$평균용지면적)] <- 0
objective_df$평균순관개면적[is.na(objective_df$평균순관개면적)] <- 0
objective_df$Factory_Count[is.na(objective_df$Factory_Count)] <- 0
objective_df$Reservoir_Count[is.na(objective_df$Reservoir_Count)] <- 0
