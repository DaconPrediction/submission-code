library(lubridate) #날짜 연산 package
library(kernlab)
library(e1071)
library(randomForest)
setwd("c:/AI/submission")
submission <- read.csv("sample_submission.csv",header=TRUE)
tr <- read.csv("train.csv")
tr2 <- read.csv("C:/AI/public_data/test_files/test_2020-11-05.csv")
tr <- rbind(tr,tr2)
tr3 <- read.csv("C:/AI/private_data/private_data.csv")
tr[1771,1] <- "2020-11-05"
for(i in 3:44){
  tr[1771,i] <- mean(tr[1770,i],tr3[1,i])
}
tr <- rbind(tr,tr3)
tr$date <- as.Date(tr$date)
tr <- tr[,-2]
######train데이터 준비######
#n=1~21-> 품목 개수 (1주뒤 예측) 모델 return 
RF <- function(n,wk){
  #train할 날짜
  date <- c(seq(as.Date('2017/09/28','%Y/%m/%d'), 
                as.Date('2017/12/01','%Y/%m/%d'),1),
            seq(as.Date('2018/09/28','%Y/%m/%d'), 
                as.Date('2018/12/01','%Y/%m/%d'),1),
            seq(as.Date('2019/09/28','%Y/%m/%d'), 
                as.Date('2019/12/01','%Y/%m/%d'),1),
            seq(as.Date('2020/09/28','%Y/%m/%d'),
                as.Date('2020/12/01','%Y/%m/%d'),1)
  )
  tr_date <- matrix(rep(0,len=10400),nrow=40,ncol=260) #날짜data포함
  tr_date <- as.data.frame(tr_date)
  colnames(tr_date) <- date
  #train할 날짜데이터를 가지는 DF->tr_date
  for(i in 1:260){
    a <- c()
    day <- c()
    day <- date[i]
    a <- date[i]+7*wk #예측할 label값(wk주뒤)
    a <- c(a,day-years(1)) #1년전 데이터
    a <- c(a, day-seq(75,112,1))
    tr_date[,i] <- a
  }
  rownames(tr_date) <- c('label','1year',75:112)
  tr_date <- t(tr_date)
  tr_date <- as.data.frame(tr_date)
  tr_price <- matrix(rep(0,len=10400),nrow=260,ncol=40) #가격데이터를 담은 DF
  tr_price <- as.data.frame(tr_price)
  #train할 가격데이터를 가지는 DF->tr_price
  for(i in 1:260){
    for(j in 1:40){
      tr_price[i,j] <- tr[tr_date[i,j]==tr$date,1+2*n]
    }
  }
  subset <- subset(tr,tr[,1+2*n]!=0)
  laplace <- mean(subset[,1+2*n]) #원하는 농작물행의 평균값
  for(i in 1:260){
    for(j in 1:40){
      #0인 값들을 각 농작물의 평균으로 대체
      if(tr_price[i,j]==0){
        tr_price[i,j] <- laplace
      }
    }
  }
  colnames(tr_price) <-c('label','1year',75:112)
  rownames(tr_price) <- date
  
  subset1 <- subset(tr_price$label,tr_price$label!=0)
  laplace1 <- mean(subset1)
  for(i in 1:length(tr_price$label)){
    if(tr_price$label[i]==0){
      tr_price$label[i] <- laplace1
    }
  }
  set.seed(300)
  attach(tr_price)
  
  model <- randomForest(label~.,  data = tr_price, mtry = floor(sqrt(30)), ntree = 500) #랜덤 포레스트 모델 생성
  return(model)
}

######test데이터 준비######
library(psych)#산포도와 상관관계, 히스토그램 등을 모두 보여주는 함수
library(MASS)
date2 <- as.Date(format(Sys.Date(),'%Y/%m/%d')) #test할 데이터의 날짜
prediction <- function(n,wk){
  ts_date <- matrix(rep(0,len=1520),nrow=40,ncol=1) #날짜data포함
  ts_date <- as.data.frame(ts_date)
  colnames(ts_date) <- date2
  b <- c()
  b <- date2+7*wk #예측할 label값(wk주뒤)
  b <- c(b,date2-years(1)) #1년 전 데이터
  b <- c(b,date2-seq(75,112,1))
  ts_date[,1] <- b
  
  rownames(ts_date) <- c('label','1year',75:112)
  ts_date <- t(ts_date)
  
  ts_price <- matrix(rep(0,len=1520),nrow=1,ncol=40) #가격데이터를 담은 DF
  ts_price <- as.data.frame(ts_price)
  for(j in 2:40){
    ts_price[1,j] <- tr[ts_date[1,j]==tr$date,1+2*n]
  }
  
  subset <- subset(tr,tr[,1+2*n]!=0)
  laplace <- mean(subset[,1+2*n])
  for(j in 2:40){
    #값이 0인 부분을 평균값으로 대체
    if(ts_price[1,j]==0){
      ts_price[1,j] <- laplace
    }
  }
  colnames(ts_price) <-c('label','1year',75:112)
  rownames(ts_price) <- date2
  ts_price
  
  ml <- RF(n,wk)#rf모델
  pred <- predict(ml,ts_price)
  return(pred)
} #예측값을 반환해주는 함수
actual <- matrix(rep(0,len=63),nrow=3,ncol=21) #날짜data포함
actual <- as.data.frame(actual)
for(a in 1:21){
  actual[1,a] <- prediction(a,1) #1주뒤 a번째 농산물의 예측 가격들
  actual[2,a] <- prediction(a,2) #2주뒤 a번째 농산물의 예측 가격들
  actual[3,a] <- prediction(a,4) #4주뒤 a번째 농산물의 예측 가격들
}
colnames(actual) <-c("배추_가격(원/kg)","무_가격(원/kg)",
                     "양파_가격(원/kg)","건고추_가격(원/kg)","마늘_가격(원/kg)",
                     "대파_가격(원/kg)","얼갈이배추_가격(원/kg)","양배추_가격(원/kg)",
                     "깻잎_가격(원/kg)","시금치_가격(원/kg)","미나리_가격(원/kg)",
                     "당근_가격(원/kg)","파프리카_가격(원/kg)","새송이_가격(원/kg)",
                     "팽이버섯_가격(원/kg)","토마토_가격(원/kg)","청상추_가격(원/kg)",
                     "백다다기_가격(원/kg)","애호박_가격(원/kg)","캠벨얼리_가격(원/kg)",
                     "샤인마스캇_가격(원/kg)")
rownames(actual) <- c(paste(as.character(Sys.Date()),"+1week",sep=""),
                      paste(as.character(Sys.Date()),"+2week",sep=""),
                      paste(as.character(Sys.Date()),"+4week",sep=""))

colnames(submission) <-c("예측대상일자","배추_가격(원/kg)","무_가격(원/kg)",
                         "양파_가격(원/kg)","건고추_가격(원/kg)","마늘_가격(원/kg)",
                         "대파_가격(원/kg)","얼갈이배추_가격(원/kg)","양배추_가격(원/kg)",
                         "깻잎_가격(원/kg)","시금치_가격(원/kg)","미나리_가격(원/kg)",
                         "당근_가격(원/kg)","파프리카_가격(원/kg)","새송이_가격(원/kg)",
                         "팽이버섯_가격(원/kg)","토마토_가격(원/kg)","청상추_가격(원/kg)",
                         "백다다기_가격(원/kg)","애호박_가격(원/kg)","캠벨얼리_가격(원/kg)",
                         "샤인마스캇_가격(원/kg)")
rownames(actual) <- c(paste(as.character(Sys.Date()),"+1week",sep=""),
                      paste(as.character(Sys.Date()),"+2week",sep=""),
                      paste(as.character(Sys.Date()),"+4week",sep="")
)
date <-c(paste(as.character(Sys.Date()),"+1week",sep=""),
         paste(as.character(Sys.Date()),"+2week",sep=""),
         paste(as.character(Sys.Date()),"+4week",sep=""))

count <- 0
for(d in date){
  count=count+1
  for(i in 1:21){
    submission[d==submission[,1],i+1] <- actual[count,i]
  }
}

write.csv(submission,paste("c:/AI/submission/submission_",format(Sys.Date(),"%m%d"),".csv",sep=""))
