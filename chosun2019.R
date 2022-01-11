install.packages('RSelenium')
install.packages("rJava")
install.packages("wordcloud")
install.packages("multilinguer")
install.packages('tm')
install.packages('slam')
install.packages('Sejong')
install.packages('hash')
install.packages('tau')
install.packages('devtools')
install.packages('RSQLite')
install.packages('remotes')
remotes::install_github('haven-jeon/KoNLP',
                        upgrade = 'never',
                        INSTALL_opts = c('--no-multiarch'))

setwd("D:/computatuional_sta_assignment")
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.0_301")


# 패키지 로딩
library(rJava) 
library(slam) 
library(RSQLite)
library(httr)
library(XML)
library(multilinguer)
library(KoNLP) # 세종사전 
library(tm) # 영문 텍스트 마이닝 
library(wordcloud) # RColorBrewer()함수 제공
library(RSelenium)
library(curl)

install.packages("rvest")
install.packages("XML")
install.packages("httr") #httr, XML소스 가져올 수 있는 함수를 포함하는 라이브러리 
library(XML)
library(httr)
library(rvest)





basic_url<-"https://search.naver.com/search.naver?where=news&sm=tab_pge&query=%EB%B6%80%EB%8F%99%EC%82%B0&sort=0&photo=3&field=0&pd=3&ds=2019.01.01&de=2019.12.31&cluster_rank=10&mynews=1&office_type=1&office_section_code=1&news_office_checked=1023&nso=so:r,p:from20190101to20191231,a:all&start="
urls<-NULL
for(index in 0:19){
  urls[index+1] <- paste0(basic_url, index*10+1)
}
links <- NULL
for(url in urls){
  html <- read_html(url)
  links <- c(links, html  %>% html_nodes('a') %>% html_attr('href') %>% unique())
}
links
news <-links[grep("news.naver.com/main/read.naver", links)]
news #목록 확인

#본문 내용 크롤링

#cd C:\Users\Administrator\Documents\Rworkingdir\module
#java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-4.0.0-alpha-1.jar -port 4445

remDr = remoteDriver(
  remoteServerAddr="localhost",
  port=4445,
  browserName="chrome")

remDr$open()
body_raw<-NULL
for(news_content in news){
  remDr$navigate(news_content)
  body_raw<-c(body_raw,remDr$getPageSource(news_content)[[1]] %>% 
                read_html() %>% html_nodes("div#articleBodyContents") %>% html_text())
  Sys.sleep(time = 0.5)    
}

body_raw


body_raw <- gsub('[\r\n\t]', '', body_raw) 
body_raw <- gsub('[a-z]','',body_raw)
body_raw <- gsub('[A-Z]','',body_raw)
body_raw <- gsub('\\s+',' ',body_raw)
body_raw <- gsub('[[:cntrl:]]','',body_raw)
body_raw <- gsub('[[:punct:]]','',body_raw)
body_raw <- gsub('\\d+',' ',body_raw)
body_raw

#1차 전처리
library(stringr)
body<-str_replace_all(body_raw,"\n","") %>% str_replace_all("\t","") %>% 
  str_replace_all("오류를 우회하기 위한 함수 추가","") %>% 
  str_replace_all("function _flash_removeCallback","") %>%  str_replace_all("\\{\\}","") %>% 
  str_replace_all("//","") %>% str_replace_all("\\(\\)","") %>% 
  str_replace_all("\\(\\)","") %>% 
  str_replace_all("네이버에서 조선일보 받아보기 당신의 맞춤뉴스 뉴스레터 신청하세요 조선일보 로그인하고 영화 공짜로 보자","") %>%
  str_replace_all("조선일보   무단 전재 및 재배포 금지 네이버 메인에서 조선일보 받아보기조선닷컴 바로가기조선일보 구독신청하기","") %>%
  str_replace_all("조선일보   무단 전재 및 재배포 금지 네이버에서 조선일보 받고 경품도 받기조선닷컴 바로가기조선일보 구독신청하기","") %>%
  str_replace_all("조선일보   무단 전재 및 재배포 금지 네이버에서 조선일보 받고 경품도 받기조선닷컴 바로가기조선일보 구독신청하기","")

body <- gsub('문제','',body)
body <- gsub('문재인','',body)
body <- gsub('기자','',body)
body <- gsub('하기','',body)
body <- gsub('하게','',body)
body <- gsub('때문','',body)
body <- gsub('들이','',body)
body <- gsub('이상','',body)
body <- gsub('지난해','',body)
body <- gsub('올해','',body)
body <- gsub('내년','',body)
body <- gsub('이후','',body)
body <- gsub('층지상','',body)
body <- gsub('만원','',body)
body <- gsub('관련','',body)
body <- gsub('경우','',body)
body


#텍스트 추출
library(tm) # 영문 텍스트 마이닝
library(KoNLP) # 세종사전 
write.table(body,file="2019_Chosun_house.txt",fileEncoding = "UTF-8")
body_file<-file("2019_Chosun_house.txt",encoding = "UTF-8")
body_data <- readLines(body_file)
body_data_corpus <- VCorpus(VectorSource(body_data))
useSejongDic()
exNouns <- function(x) { paste(extractNoun(as.character(x)), collapse=" ")}
body_nouns <- sapply(body_data_corpus, exNouns)
body_corpus<-VCorpus(VectorSource(body_nouns))

#2차 전처리
body_corpus <- tm_map(body_corpus, content_transformer(removePunctuation)) # 문장부호 제거
body_corpus <- tm_map(body_corpus, content_transformer(removeNumbers)) # 수치 제거
body_corpus <- tm_map(body_corpus, content_transformer(tolower))# 소문자 변경
myStopwords = c(stopwords('english'), "네이버","일보","조선","사용","이용", "하기","하게","chosun","com","들이","이상","이후","때문")
body_corpus <-tm_map(body_corpus, content_transformer(removeWords), myStopwords) # 불용어제거

#빈도수 정리
body_corpus_term <- TermDocumentMatrix(body_corpus, control=list(wordLengths=c(2,Inf)))
body_term.df <- as.data.frame(as.matrix(body_corpus_term)) 
dim(body_term.df) 
body_term.df


#단어 구름 생성
library(wordcloud)# RColorBrewer()함수 제공
wordResult <- sort(rowSums(body_term.df), decreasing=TRUE) # 빈도수로 내림차순 정렬
myName <- names(wordResult) # 단어 이름 생성 -> 빈도수의 이름 
word.df <- data.frame(word=myName, freq=wordResult)
word.df

pal <- brewer.pal(12,"Paired") # 12가지 색상 pal <- brewer.pal(9,"Set1") # Set1~ Set3
# 폰트 설정세팅 : "맑은 고딕", "서울남산체 B"
windowsFonts(malgun=windowsFont("맑은 고딕"))  #windows

# 단어 구름 시각화 - 별도의 창에 색상, 빈도수, 글꼴, 회전 등의 속성 적용 
#x11( ) # 별도의 창을 띄우는 함수

wordcloud(word.df$word, word.df$freq, 
          scale=c(6,1), min.freq=20, random.order=F, 
          rot.per=.0, colors=pal, family="malgun")


#연관어 분석=================================================================================================
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_301')

#줄단위 단어 추출
lword <- Map(extractNoun, body_data)  
length(lword)
lword
#중복 제거
lword<-unique(lword)
lword <- sapply(lword, unique) # 중복제거2 (줄 단위 대상) 
length(lword) 
lword
# (4) 연관어 분석을 위한 전처리 

# 단어 필터링 함수 정의 (길이 2~4사이 한글 단어 추출)
filter1 <- function(a){
  nchar(a) <= 4 && nchar(a) >= 2 && is.hangul(a)
}

filter2 <- function(b){
  Filter(filter1, b)
}

lword <- sapply(lword, filter2)
lword

install.packages('arules')
library(arules) 
library(KoNLP)
useSejongDic()
detach(package:tm, unload=TRUE)

wordtran <- as(lword, "transactions") # lword에 중복데이터가 있으면 error발생
wordtran 
tranrules <- apriori(wordtran, parameter=list(supp=0.25, conf=0.05)) #지지도 0.25 신뢰도 0.5
tranrules <- apriori(wordtran, parameter=list(supp=0.25, conf=0.8)) #지지도 0.25 신뢰도 0.8
tranrules <- apriori(wordtran, parameter=list(supp=0.3, conf=0.05)) #지지도 0.3 신뢰도 0.05

inspect(tranrules)

rules <- labels(tranrules, ruleSep=" ")  
rules
class(rules)

rules <- sapply(rules, strsplit, " ",  USE.NAMES=F) 
rules
class(rules) 

rulemat <- do.call("rbind", rules)
rulemat
class(rulemat)

# 연관어 시각화를 위한 igraph 패키지 설치
install.packages('igraph')
library(igraph)   
library(ggplot2)
? graph.edgelist
# edgelist보기 - 연관단어를 정점 형태의 목록 제공 
ruleg <- graph.edgelist(rulemat[c(21:75),], directed=F) # [1,]~[11,] "{}" 제외
ruleg
#  edgelist 시각화
#X11()
plot.igraph(ruleg, vertex.label=V(ruleg)$name,
            vertex.label.cex=1.3, vertex.label.color='black',
            vertex.size=20, vertex.color='green', vertex.frame.color='blue',layout=layout_on_sphere)
