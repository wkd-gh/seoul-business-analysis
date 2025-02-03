#### Q5: 수집연월별(1:5) 업종별(대분류) 점포수의 변화를 분석하시오.
store.change <- aggregate(ds.total[,1],
                          by=list(연월=ds.total$수집연월,
                                  업종대분류=ds.total$상권업종대분류명),
                          FUN=length)

head(store.change)
names(store.change)[3] <- c("count")

ggplot(store.change, aes(x=연월, y=count,col=업종대분류,group=업종대분류)) + 
  geom_line() + 
  geom_point(size=6, shape=19, alpha=0.5) +
  ggtitle("업종별 점포수 변화 (대분류)") +
  ylab("점포수") + 
  scale_x_continuous(breaks=1:5, 
                     labels=files) +
  theme(plot.title = element_text(color="black", size=14, face="bold"))

ds.total %>% group_by(수집연월, 상권업종대분류명) %>% summarise(count=n())




#### Q6: 201512, 201712 두 기간에 점포수 변화가 큰 상위 10개 업종(소분류체계)을 알아내시오
store.tmp <- aggregate(ds.total[,1], by=list(연월=ds.total$수집연월,
                                             업종소분류=ds.total$상권업종소분류명),
                       FUN=length)
names(store.tmp)[3] <- c("count")

store.201512 <- store.tmp[store.tmp$연월==1,]
names(store.201512)[3] <- c("cnt_2015")

store.201712 <- store.tmp[store.tmp$연월==5,]
names(store.201712)[3] <- c("cnt_2017")

store.diff <- merge(store.201512[,2:3], store.201712[,2:3])
store.diff$diff <- abs(store.diff$cnt_2015-store.diff$cnt_2017)
store.diff <- store.diff[order(by=store.diff$diff, decreasing = T),]
top10 <- store.diff[1:10,1]
top10 # 가장 많이 변화가 있었던 업종명?
head(store.diff)

#점포수 변화 top10 업종(소분류) 그래프 그리기 - 맨위에 있다고 탑이 아니다 착각하지마
store.change <- subset(store.tmp, store.tmp$업종소분류 %in% top10)
ggplot(store.change, aes(x=연월, y=count, col=업종소분류, group=업종소분류)) +
  geom_line() + 
  geom_point(size=6, shape=19, alpha=0.5) +
  ggtitle("점포수 변화 Top 10 업종(소분류)") +
  ylab("점포수") + 
  scale_x_continuous(breaks=1:5, 
                     labels=files) +
  theme(plot.title = element_text(color="black", size=14, face="bold"))






#### Q7: 구별 점포수의 변화를 분석하시오
#Q7: 5개년도 구별 점포수의 변화를 분석하시오
store.gu <- aggregate(ds.total[,1],
                      by=list(연월=ds.total$수집연월,
                              구이름=ds.total$시군구명),
                      FUN=length)

names(store.gu)[3] <- c("count")
ggplot(store.gu, aes(x=연월, y=count, colour=구이름, group=구이름)) +
  geom_line() + 
  geom_point(size=6, shape=20, alpha=0.5) +
  ggtitle("구별 점포수 변화 (대분류)") +
  ylab("점포수") + 
  scale_x_continuous(breaks=1:5, 
                     labels=files) +
  theme(plot.title = element_text(color="black", size=14, face="bold"))







#### Q8: 201512,201712 두기간에 점포수 변화가 큰 상위 10개 행정동을 확인하시오
store.tmp <- aggregate(ds.total[,1],
                       by=list(연월=ds.total$수집연월,
                               동이름=ds.total$행정동명),
                       FUN=length) 
  
names(store.tmp)[3] <- c("count")

store.dong.201512 <- store.tmp[store.tmp$연월==1,]
names(store.dong.201512)[3] <- c("cnt_2015")

store.dong.201712 <- store.tmp[store.tmp$연월==5,]
names(store.dong.201712)[3] <- c("cnt_2017")

store.diff <- merge(store.dong.201512[,2:3],store.dong.201712[,2:3]) #중요?
store.diff$diff <- abs(store.diff$cnt_2015-store.diff$cnt_2017)
store.diff <- store.diff[order(by=store.diff$diff, decreasing = T),]
top10 <- store.diff[1:10,1]
top10

store.change <- subset(store.tmp, store.tmp$동이름%in%top10) # 중요?
ggplot(store.change, aes(x=연월, y=count, colour=동이름, group=동이름)) +
  geom_line() + 
  geom_point(size=6, shape=19, alpha=0.5) +
  ggtitle("점포수 변화 Top 10 동") +
  ylab("점포수") + 
  scale_x_continuous(breaks=1:5, 
                     labels=files) +
  theme(plot.title = element_text(color="black", size=14, face="bold"))

