# ds.total이라는 큰 데이터? 만들기
#reopen with encoding: CP949

library(ggplot2)
library(ggmap)
library(readxl)


files <- c("201512","201606","201612","201706","201712")
columns <- c( "상가업소번호", "상호명", "상권업종대분류명", "상권업종중분류명", 
              "상권업종소분류명", "시군구명", "행정동명", "경도", "위도")     
ds.total <- NULL

#ds$수집연월
#1: 201512
#2: 201606
#3: 201612
#4: 201706
#5: 201712
for (i in 1:length(files)) {
  filename <- paste("seoul_",files[i],".xlsx", sep="")
  cat("read ", filename, "...\n")         # 읽을 파일 이름 출력
  
  ds <- read_excel(filename)
  ds <- data.frame(ds)
  ds <- ds[,columns]
  ds$수집연월 <- rep(i, nrow(ds))
  ds.total <- rbind(ds.total,ds)
}

head(ds.total)
tail(ds.total)

