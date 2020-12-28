library(RColorBrewer)
library(tidyverse)
library(data.table)
library(jsonlite)
library(dplyr)

library(lubridate) 

#----------個別US TW 40天的資料結合在同一個CSV------------------------------------
multmerge = function(mypath){
  filenames=list.files(path=mypath, full.names=TRUE)
  datalist = lapply(filenames, function(x){fread(file=x,header=T,encoding = 'UTF-8')})
  Reduce(function(x,y) {merge(x,y,all = TRUE)}, datalist)
}

tw = multmerge("R dataset/final/DATASET/TW")

us = multmerge("R dataset/final/DATASET/US")

#-----------------------------------------------------------------------


#na值檢查
sapply(tw, function(x) sum(ifelse(is.na(x), 1, 0)))




#CPM計算公式是參考以下網址
#https://www.bannertag.com/youtube-video-cpm-rates-2020/
tw$income = tw$view_count / 1000 * 0.59


us$income = us$view_count / 1000 * 0.38



#計算tag個數
tw$tag_count = lengths(strsplit(tw$tags, "|", fixed = TRUE))
us$tag_count = lengths(strsplit(us$tags, "|", fixed = TRUE))
tw$tag_count<- ifelse(tw$tags=="[none]", 0,lengths(strsplit(tw$tags, "|", fixed = TRUE)))
us$tag_count<- ifelse(us$tags=="[none]", 0,lengths(strsplit(us$tags, "|", fixed = TRUE)))

#計算desc長度
us$desc_length = nchar(us$description)
tw$desc_length = nchar(tw$description)




#新增每部影片上幾天熱門的欄位
tw = group_by(tw, video_id) %>% mutate(trending_days = n())
us = group_by(us, video_id) %>% mutate(trending_days = n())
#將每部影片最終的資料獨立出來，避免重複統計。
tw_mostViews = group_by(tw, video_id) %>% filter(view_count == max(view_count))
us_mostViews = group_by(us, video_id) %>% filter(view_count == max(view_count))






#把爬到台灣資料 影片長度json檔導入
tw_id_json<- fromJSON("R dataset/final/scrape/tw_id.json")
tw_time_json<- fromJSON("R dataset/final/scrape/tw_numeric_times.json")

#把爬到美國資料 影片長度json檔導入
us_id_json<- fromJSON("R dataset/final/scrape/us_id.json")
us_time_json<- fromJSON("R dataset/final/scrape/us_numeric_times.json")

#把影片長度合併到CSV
tw_Time<-data.frame(video_id=(tw_id_json),duration=(tw_time_json))
tw_duration<- dplyr::inner_join(tw_mostViews,tw_Time,by="video_id")

us_Time<-data.frame(video_id=(us_id_json),duration=(us_time_json))
us_duration<- dplyr::inner_join(us_mostViews,us_Time,by="video_id")






write.csv(tw_duration,file="tw.csv",row.names = F)
write.csv(us_duration,file="us.csv",row.names = F)

