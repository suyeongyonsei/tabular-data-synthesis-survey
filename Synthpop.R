setwd("C:/Users/suyeong/OneDrive - 연세대학교 (Yonsei University)/R_code")
setwd("/Users/kimsuyeong/Library/CloudStorage/OneDrive-연세대학교(YonseiUniversity)/R_code")
setwd("C:/Users/김수영/OneDrive - 연세대학교 (Yonsei University)/R_code")
setwd("/Users/kimsuyeong/Documents/R_code")

adult <-read.csv("original_data/adult.csv")
#creditcard <- read.csv("original_data/creditcard.csv")
house_16H <- read.csv("original_data/house_16H2.csv")
insurance <- read.csv("original_data/insurance.csv")
wilt <- read.csv('original_data/wilt.csv')

install.packages("synthpop")
library(synthpop)

for (i in 1:30) {
  syn_adult <- syn(adult,method = "cart", m=1 , smoothing = "density"
                   , proper = TRUE,cart.minbucket = 10, minnumlevels = 2, cart.cp=0.01)
  write.csv(syn_adult$syn, file = paste0("synthpop/adult", i, "_synthpop.csv"))
}
#write.csv(syn_adult$syn[[1]],file="synthpop/adult1_synthpop.csv")
#write.csv(syn_adult$syn[[2]],file="synthpop/adult2_synthpop.csv")
#write.csv(syn_adult$syn[[3]],file="synthpop/adult3_synthpop.csv")
#write.csv(syn_adult$syn[[4]],file="synthpop/adult4_synthpop.csv")
#write.csv(syn_adult$syn[[5]],file="synthpop/adult5_synthpop.csv")


for (i in 1:30) {
  syn_wilt <- syn(wilt,method = "cart", m=1, smoothing = "spline", proper = TRUE,
                  cart.minbucket=10, minnumlevels = 2, cart.cp = 0.01)
  write.csv(syn_wilt$syn, file = paste0("synthpop/wilt", i, "_synthpop.csv"))
}
#write.csv(syn_wilt$syn[[1]],file="synthpop/wilt1_synthpop.csv")
#write.csv(syn_wilt$syn[[2]],file="synthpop/wilt2_synthpop.csv")
#write.csv(syn_wilt$syn[[3]],file="synthpop/wilt3_synthpop.csv")
#write.csv(syn_wilt$syn[[4]],file="synthpop/wilt4_synthpop.csv")
#write.csv(syn_wilt$syn[[5]],file="synthpop/wilt5_synthpop.csv")


for (i in 1:30) {
  syn_insurance <- syn(insurance,method = "cart", m=1, smoothing = "density",
                       cart.minbucket=10, minnumlevels=2, cart.cp = 0.01,proper = TRUE)
  write.csv(syn_insurance$syn, file = paste0("synthpop/insurance", i, "_synthpop.csv"))
}
#write.csv(syn_insurance$syn[[1]],file="synthpop/insurance1_synthpop.csv")
#write.csv(syn_insurance$syn[[2]],file="synthpop/insurance2_synthpop.csv")
#write.csv(syn_insurance$syn[[3]],file="synthpop/insurance3_synthpop.csv")
#write.csv(syn_insurance$syn[[4]],file="synthpop/insurance4_synthpop.csv")
#write.csv(syn_insurance$syn[[5]],file="synthpop/insurance5_synthpop.csv")

#house_16H[,1] <- lapply(house_16H[,1],as.character)
#sapply(house_16H,class)

for (i in 1:30) {
  syn_house_16H <- syn(house_16H,method = "cart", m=1, smoothing = list(P1="density",P5p1 ="density", P11p4 ="density", P14p9="density", P15p1="density"
                                                                        , P16p2="density", P27p4="density", H2p2="density",H10p1="density", H13p1="density",
                                                                        H18pA="density", H40p4="density", price="density"),
                       cart.minbucket=10, minnumlevels=2, cart.cp = 0.01,proper = TRUE)
  write.csv(syn_house_16H$syn, file = paste0("synthpop/house_16H", i, "_synthpop.csv"))
}
#syn_house_16H <- syn(house_16H,method = "cart", m=5, smoothing = 'density',
            #         cart.minbucket=10, minnumlevels=2, cart.cp = 0.1,proper = TRUE)

p6p2<-adult$capital.gain
sub_p <- sub("0+$", "", as.character(p6p2))
str <- strsplit(sub_p,".",fixed=TRUE)

p15p3<-house_16H$P15p3
sub_p3 <- sub("0+$", "", as.character(p15p3))
str3 <- strsplit(sub_p3,".",fixed=TRUE)

p5p1<-house_16H$P5p1
sub_p5 <- sub("0+$", "", as.character(p5p1))
str5 <- strsplit(sub_p5,".",fixed=TRUE)[[1]][[2]]

p15p1<-house_16H$P15p1
sub_p15 <- sub("0+$", "", as.character(p15p1))
str15 <- strsplit(sub_p15,".",fixed=TRUE)[[1]][[2]]

summary(syn_house_16H)
write.csv(syn_house_16H$syn[[1]],file="synthpop/house_16H1_synthpop.csv")
write.csv(syn_house_16H$syn[[2]],file="synthpop/house_16H2_synthpop.csv")
write.csv(syn_house_16H$syn[[3]],file="synthpop/house_16H3_synthpop.csv")
write.csv(syn_house_16H$syn[[4]],file="synthpop/house_16H4_synthpop.csv")
write.csv(syn_house_16H$syn[[5]],file="synthpop/house_16H5_synthpop.csv")
str(house_16H)
# 요약 통계 확인
house_16H[,3] <- as.numeric(house_16H[,3])
summary(house_16H)
# 변수별로 결측값 확인
sapply(house_16H, function(x) sum(is.na(x)))
house_16H <- lapply(house_16H, as.numeric)
house_16H
syn.smooth(syn_house_16H$syn[[1]], house_16,control=list(stop.iterations=20), smoothing = 'density')

library(synthpop)
synthpop::synthpop_options(proper = TRUE,
                           control = list(stop.iterations = 20,
                                          cart.minbucket = 5,
                                          stop.complexity = 0.1),
                           smoothing = 'spline')

# 합성 데이터 생성
synthetic_data <- syn(house_16H, method = "cart", m = 5
                      , smoothing = 'density', proper=TRUE)
synthetic_data <- syn(house_16H,
                      proper = TRUE,
                      method = "cart",
                      control = list(stop.iterations = 20),  # 반복 횟수 설정
                      ctree.minbucket = 5,  # 각 노드의 최소 관측치 개수 설정
                      cart.cp = 0.1,  # 최대 복잡도 설정
                      smoothing = 'spline')

syn1.default <- syn(adult, seed=1000)
syn2.default <- syn(adult, seed=2000)
syn3.default <- syn(adult, seed=3000)
syn4.default <- syn(adult, seed=4000)
syn5.default <- syn(adult, seed=5000)

adult1 <- syn1.default$syn
adult2 <- syn2.default$syn
adult3 <- syn3.default$syn
adult4 <- syn4.default$syn
adult5 <- syn5.default$syn

write.csv(adult1,file="adult1_synthpop2.csv")
write.csv(adult2,file="adult2_synthpop2.csv")
write.csv(adult3,file="adult3_synthpop2.csv")
write.csv(adult4,file="adult4_synthpop2.csv")
write.csv(adult5,file="adult5_synthpop2.csv")
#-------------------------------------------------------------
#creditcard

syn1.default <- syn(creditcard, seed=1001)
syn2.default <- syn(creditcard, seed=2001)
syn3.default <- syn(creditcard, seed=3001)
syn4.default <- syn(creditcard, seed=4001)
syn5.default <- syn(creditcard, seed=5001)

creditcard1 <- syn1.default$syn
creditcard2 <- syn2.default$syn
creditcard3 <- syn3.default$syn
creditcard4 <- syn4.default$syn
creditcard5 <- syn5.default$syn

write.csv(creditcard1,file="creditcard1_synthpop.csv")
write.csv(creditcard2,file="creditcard2_synthpop.csv")
write.csv(creditcard3,file="creditcard3_synthpop.csv")
write.csv(creditcard4,file="creditcard4_synthpop.csv")
write.csv(creditcard5,file="creditcard5_synthpop.csv")

#------------------------------------------------------------------
#house_16H

syn1.default <- syn(house_16H, seed=1002)
syn2.default <- syn(house_16H, seed=2002)
syn3.default <- syn(house_16H, seed=3002)
syn4.default <- syn(house_16H, seed=4002)
syn5.default <- syn(house_16H, seed=5002)

house_16H1 <- syn1.default$syn
house_16H2 <- syn2.default$syn
house_16H3 <- syn3.default$syn
house_16H4 <- syn4.default$syn
house_16H5 <- syn5.default$syn

write.csv(house_16H1,file="house_16H1_synthpop.csv")
write.csv(house_16H2,file="house_16H2_synthpop.csv")
write.csv(house_16H3,file="house_16H3_synthpop.csv")
write.csv(house_16H4,file="house_16H4_synthpop.csv")
write.csv(house_16H5,file="house_16H5_synthpop.csv")

#---------------------------------------------------------
#insurance

syn1.default <- syn(insurance, seed=1003)
syn2.default <- syn(insurance, seed=2003)
syn3.default <- syn(insurance, seed=3003)
syn4.default <- syn(insurance, seed=4003)
syn5.default <- syn(insurance, seed=5003)

insurance1 <- syn1.default$syn
insurance2 <- syn2.default$syn
insurance3 <- syn3.default$syn
insurance4 <- syn4.default$syn
insurance5 <- syn5.default$syn

write.csv(insurance1,file="insurance1_synthpop.csv")
write.csv(insurance2,file="insurance2_synthpop.csv")
write.csv(insurance3,file="insurance3_synthpop.csv")
write.csv(insurance4,file="insurance4_synthpop.csv")
write.csv(insurance5,file="insurance5_synthpop.csv")




# 히스토그램 비교
library(ggplot2)
install.packages("gridExtra")
library(gridExtra)

# 변수 목록 (여기서는 예시로 age와 income 두 변수를 사용)
variables <- c("fnlwgt")


# 히스토그램 비교
for (var in variables) {
  p1 <- ggplot(adult, aes_string(var)) +
    geom_histogram(binwidth=1000, fill="blue", alpha=0.5) +
    ggtitle(paste("Original:", var))
  
  p2 <- ggplot(adult1, aes_string(var)) +
    geom_histogram(binwidth=1000, fill="red", alpha=0.5) +
    ggtitle(paste("Synthesized:", var))
  
  grid.arrange(p1, p2, ncol=2)
}

# 밀도 플롯 비교
for (var in variables) {
  p1 <- ggplot(adult, aes_string(var)) +
    geom_density(fill="blue", alpha=0.5) +
    ggtitle(paste("Original:", var))
  
  p2 <- ggplot(adult1, aes_string(var)) +
    geom_density(fill="red", alpha=0.5) +
    ggtitle(paste("Synthesized:", var))
  
  grid.arrange(p1, p2, ncol=2)
}

library(rpart)
m_dt <- rpart(income ~., data=adult)
m_dt$control


