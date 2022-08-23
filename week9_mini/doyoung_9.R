# 라이브 러리
library(readxl)
library(stringr)
library(reshape2)
library(dplyr)
library(tidyverse)
library(gganimate)
library(nord)
library(viridis)
library(shiny)
library(ggplot2)
library(plotly)
library(av)

## 기대수명

# 파일 로딩 (기대수명)
df_age <- read_excel('project_data.xlsx', sheet = '기대수명')
str(df_age)

# 합계 열 추출
v <- c()
for (i in c(1, seq(2, 130, 3))) v <- c(v, df_age[, i])

# 프레임화
df_age <- data.frame(v)
df_age <- df_age[-1, ]
head(df_age, 10)

# 컬럼명 처리
v <- c()
for (i in colnames(df_age)) {
  j <- str_replace(i, '\\...', ' ')  # ... 제거 
  v <- c(v, strsplit(j, ' ')[[1]][1])  # 숫자 제거
}

colnames(df_age) <- v

# 대륙 처리
rem <- c('아시아', '북아메리카', '남아메리카', '유럽', '오세아니아')

u <- c()
for (i in colnames(df_age)) {
  for (j in rem) {
    if (i==j) u <- c(u, which(colnames(df_age) == i))  # 대륙 인덱스 찾기
  }
}

df_age <- df_age[, -u]

# 인덱스를 정리, 형 변환
rownames(df_age) <- 1:dim(df_age)[1]

for (i in colnames(df_age)[2:dim(df_age)[2]]) {
  df_age[[i]] <- as.numeric(df_age[[i]])
}

# melt 함수로 원하는 형태 만들기
df_ages <- melt(df_age, id.vars = '시점')

dim(df_ages)

# 나라별 대륙 설정
asia_ <- c('대한민국', '이스라엘', '일본', '튀르키예')
norame_ <- c('캐나다','멕시코','미국')
souame_ <- c('칠레','콜롬비아','코스타리카')
eu_ <- c('오스트리아','벨기에','체코','덴마크','에스토니아','핀란드',
         '프랑스','독일','그리스','헝가리','아이슬란드','아일랜드',
         '이탈리아','라트비아','리투아니아','룩셈부르크','네덜란드',
         '노르웨이','폴란드','포르투갈','슬로바키아','슬로베니아',
         '스페인','스웨덴','스위스','영국')
oce_ <- c('오스트레일리아','뉴질랜드')

# 대륙에 해당 하는 나라 인덱스 받기
asia_idx <- c()
norame_idx <- c()
souame_idx <- c()
eu_idx <- c()
oce_idx <- c()

for (i in df_ages$variable) {
  if (i %in% asia_) asia_idx <- c(asia_idx, which(df_ages$variable == i))
  else if (i %in% norame_) norame_idx <- c(norame_idx, which(df_ages$variable == i))
  else if (i %in% souame_) souame_idx <- c(souame_idx, which(df_ages$variable == i))
  else if (i %in% eu_) eu_idx <- c(eu_idx, which(df_ages$variable == i))
  else if (i %in% oce_) oce_idx <- c(oce_idx, which(df_ages$variable == i))
} 

# 대륙에 해당 하는 나라 인덱스 중복 제거
asia_idx <- unique(asia_idx)
norame_idx <- unique(norame_idx)
souame_idx <- unique(souame_idx)
eu_idx <- unique(eu_idx)
oce_idx <- unique(oce_idx)

# 대륙 넣기
continent <- 1:dim(df_ages)[1]

continent[asia_idx] <- '아시아'
continent[norame_idx] <- '북아메리카'
continent[souame_idx] <- '남아메리카'
continent[eu_idx] <- '유럽'
continent[oce_idx] <- '오세아니아'

df_ages$대륙 <- continent

# 국가 형 변환
df_ages$variable <- as.character(df_ages$variable)

str(df_ages)


## 부양비

# 데이터 로딩(부양비, 노령화지수)
df_buno <- read_excel('project_data.xlsx', sheet = '부양비_노령화지수')

# 열 추출
v <- c()
for (i in c(1, seq(6, 345, 8))) v <- c(v, df_buno[, i])

# 프레임화
df_buyang <- data.frame(v)
df_buyang <- df_buyang[-c(1,2), ]

# 컬럼명 처리
v <- c()
for (i in colnames(df_buyang)) {
  j <- str_replace(i, '\\...', ' ')  # ... 제거 
  v <- c(v, strsplit(j, ' ')[[1]][1])  # 숫자 제거
}

colnames(df_buyang) <- v

# 대륙 처리
rem <- c('아시아', '북아메리카', '남아메리카', '유럽', '오세아니아')

u <- c()
for (i in colnames(df_buyang)) {
  for (j in rem) {
    if (i==j) u <- c(u, which(colnames(df_buyang) == i))  # 대륙 인덱스 찾기
  }
}

df_buyang <- df_buyang[, -u]

# 인덱스를 정리, 형 변환
rownames(df_buyang) <- 1:dim(df_buyang)[1]

for (i in colnames(df_buyang)[2:dim(df_buyang)[2]]) {
  df_buyang[[i]] <- as.numeric(df_buyang[[i]])
}

# melt 함수로 원하는 형태 만들기
df_buyangs <- melt(df_buyang, id.vars = '시점')
str(df_buyangs)


## 노령화지수

# 열 추출
v <- c()
for (i in c(1, seq(9, 345, 8))) v <- c(v, df_buno[, i])

# 프레임화
df_aging <- data.frame(v)
df_aging <- df_aging[-c(1,2), ]
df_aging

# 컬럼명 처리
v <- c()
for (i in colnames(df_aging)) {
  j <- str_replace(i, '\\...', ' ')  # ... 제거 
  v <- c(v, strsplit(j, ' ')[[1]][1])  # 숫자 제거
}

colnames(df_aging) <- v

# 대륙 처리
rem <- c('아시아', '북아메리카', '남아메리카', '유럽', '오세아니아')

u <- c()
for (i in colnames(df_aging)) {
  for (j in rem) {
    if (i==j) u <- c(u, which(colnames(df_aging) == i))  # 대륙 인덱스 찾기
  }
}

df_aging <- df_aging[, -u]

# 인덱스를 정리, 형 변환
rownames(df_aging) <- 1:dim(df_aging)[1]

for (i in colnames(df_aging)[2:dim(df_aging)[2]]) {
  df_aging[[i]] <- as.numeric(df_aging[[i]])
}

# melt 함수로 원하는 형태 만들기
df_agings <- melt(df_aging, id.vars = '시점')
str(df_agings)


## 장래인구

# 파일 로딩 (장래인구)
df_pop <- read_excel('project_data.xlsx', sheet = '장래인구')
df_pop <- data.frame(df_pop)

# 대륙 처리 (리히텐슈타인 도)
rem_ <- c('아시아', '북아메리카', '남아메리카', '유럽', '오세아니아', '리히텐슈타인')

u <- c()
for (i in colnames(df_pop)) {
  for (j in rem_) {
    if (i==j) u <- c(u, which(colnames(df_pop) == i))  # 대륙 인덱스 찾기
    
  }
}

df_pop <- df_pop[, -u]

# 형 변환
for (i in colnames(df_pop)[1:2]) {
  df_pop[[i]] <- as.numeric(df_pop[[i]])
}

str(df_pop)

# 5년 끊어서 평균 처리
fiv <- c()
for (i in seq(0, 145, 5)) {
  fiv <- c(fiv, apply(df_pop[seq(i+1, 5+i), ], MARGIN = 2, mean, na.rm = T))
}

# 연도 담기
w <- c()
w <- c(w, fiv[seq(1, length(fiv), 39)] + 3)


# 나머지 데이터도 담기
w3 <- c(w)
for (i in 2:39) {
  w2 <- c()
  w2 <- c(w2, fiv[seq(i, length(fiv), 39)])
  w3 <- c(w3, w2)
}
length(w3)

df_population <- data.frame(w3[1:30], w3[31:60], w3[61:90], w3[91:120],
                            w3[121:150], w3[151:180], w3[181:210], w3[211:240],
                            w3[241:270], w3[271:300], w3[301:330], w3[331:360],
                            w3[361:390], w3[391:420], w3[421:450], w3[451:480],
                            w3[481:510], w3[511:540], w3[541:570], w3[571:600],
                            w3[601:630], w3[631:660], w3[661:690], w3[691:720],
                            w3[721:750], w3[751:780], w3[781:810], w3[811:840],
                            w3[841:870], w3[871:900], w3[901:930], w3[931:960],
                            w3[961:990], w3[991:1020], w3[1021:1050], w3[1051:1080],
                            w3[1081:1110], w3[1111:1140], w3[1141:1170])

colnames(df_population) <- colnames(df_pop)

# melt 함수로 원하는 형태 만들기
df_populations <- melt(df_population, id.vars = '시점')
str(df_populations)
dim(df_populations)


### 하나의 프레임으로 합치기 
DF <- cbind('연도'=df_ages$시점, '대륙'=df_ages$대륙, '나라'=df_ages$variable,
            '장래인구'=df_populations$value, '기대수명'=df_ages$value,
            '노령화지수'=df_agings$value, '부양비'=df_buyangs$value)
DF <- data.frame(DF)

# 형 변환
DF$연도 <- as.numeric(DF$연도)
DF$장래인구 <- as.numeric(DF$장래인구)
DF$기대수명 <- as.numeric(DF$기대수명)
DF$노령화지수 <- as.numeric(DF$노령화지수)
DF$부양비 <- as.numeric(DF$부양비)

str(DF)

############################## 그래프 ######################################
# bubble chart
ggplot(DF, aes(기대수명, 노령화지수, size = 장래인구, colour = 대륙)) +
  geom_point(alpha = 0.7) +
  scale_colour_manual(values = c('#9370DB', '#F4A460','#FF69B4','#98FB98','#87CEFA')) +
  scale_size(range = c(2, 10)) +
  scale_x_log10() +
  facet_wrap(~대륙) +
  theme_bw() +
  labs(title = 'Year: {round(frame_time, 0)}', x = '기대수명', y = '노령화지수') +
  transition_time(연도) +
  shadow_wake(wake_length = 0.1, alpha = FALSE) +
  ease_aes('linear')

# anim_save("scatter.gif")


# boxplot
ggplot(DF, aes(x = 대륙, y = 기대수명, fill = 대륙))+
  geom_boxplot()+
  scale_fill_viridis(option='viridis', discrete=TRUE)+
  theme_minimal()+
  transition_states(연도,
                    transition_length=0.5,
                    state_length=0.5) +
  ggtitle('Year : {closest_state}',
          subtitle = 'Frame {frame} of {nframes}')

# anim_save("boxplot.gif")


# barplot
ggplot(DF, aes(x=대륙, y=장래인구, fill=노령화지수))+
  geom_col(show.legend=T)+
  theme_minimal()+
  transition_states(연도,
                    transition_length=1.5,
                    state_length=0.5) +
  ggtitle('Year : {closest_state}',
          subtitle = 'Frame {frame} of {nframes}')

# anim_save("col.gif")

# shiny
ui <- fluidPage(
  
  tags$h1('1조 김도영'),
  tags$h3('OECD 국가 인구 데이터'),
  tags$h5('출처 : 통계청'),
  
  mainPanel(
    tabsetPanel(
      tabPanel('interactive1', h1('interactive graph1'), plotlyOutput('mytable2')),
      tabPanel('interactive2', h1('interactive graph2'), plotOutput('mytable1', click = 'plot_click'), tableOutput('data')),
      tabPanel('boxplot', h1('boxplot'), img(src='https://media1.giphy.com/media/BtElBEIZg8WQB2YEnj/giphy.gif?cid=790b7611338c4a6e9ac27f62ce26734671055979dd705e83&rid=giphy.gif&ct=g')),
      tabPanel('barplot', h1('barplot'), img(src='https://media0.giphy.com/media/XYovHVfPkT5XDkOtuj/giphy.gif?cid=790b761128b37c1fd42521d3c0a74e3b77a543558b43b51a&rid=giphy.gif&ct=g')),
      tabPanel('bubble chart', h1('bubble chart'), img(src='https://media4.giphy.com/media/dnTd38ePOT51n4MZ7r/giphy.gif?cid=790b7611d18ca14e59ff3dd9aca91da3140f421b14ef5b49&rid=giphy.gif&ct=g')),
    )  
  )
)

server <- function (input, output) {
  output$mytable1 <- renderPlot({
    ggplot(DF,
           aes(x = 노령화지수, y = 부양비)) +
      geom_point(aes(color = 대륙, size = 장래인구), alpha = 0.5) +
      scale_x_log10() +
      geom_smooth(se = F) +
      theme_classic()
  })
  output$mytable2 <- renderPlotly({
    ggplot(DF,
           aes(x = 노령화지수, y = 부양비)) +
      geom_point(aes(color = 대륙, size = 장래인구), alpha = 0.5) +
      scale_x_log10() +
      geom_smooth(se = F) +
      theme_classic()
  })
  output$data <- renderTable({
    req(input$plot_click)
    nearPoints(DF, input$plot_click)
  })
  
}



shinyApp(ui, server)

