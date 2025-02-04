#### Q9: 역삼1동 상권분석: 대분류에 따라 상점의 위치를 지도에 나타냄
#### Q9: 역삼1동의 상권을 분석하시오: 역삼1동 상점의 위치를 업종별(대분류 기준)로 지도에 나타냄
ds.yeoksam <- subset(ds.total, ds.total$수집연월==5&
                       ds.total$행정동명=="역삼1동")
register_google(key = ) # 개인 구글 api키 사용  
cen <- c(mean(ds.yeoksam$경도),mean(ds.yeoksam$위도))
map <- get_googlemap(center = cen,          # 마커 없는 지도 가져오기
                     maptype="roadmap",
                     size=c(640,640),
                     zoom=15)

gmap <- ggmap(map)                              # 지도를 저장
gmap+geom_point(data = ds.yeoksam,
                aes(x=경도, y=위도, col=상권업종대분류명), 
                size=2, alpha=0.7) +
  labs(x = "Longitude", y = "Latitude",
       title="역삼1동 업종별 점포", color = "업종")





#### Q10: 커피전문점/카페/다방 업종 분석
#### Q10: 커피 점포만 지도에 표시(소분류: 커피전문점/카페/다방)
ds.yeoksam2 <- subset(ds.yeoksam, ds.yeoksam$상권업종소분류명=="커피전문점/카페/다방")
gmap+geom_point(data = ds.yeoksam2,
                aes(x=경도, y=위도),size=2,alpha=0.5, col="red") +
  labs(x = "Longitude", y = "Latitude",
       title="역삼1동 커피점")

