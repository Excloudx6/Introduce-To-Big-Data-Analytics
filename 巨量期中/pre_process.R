#############################################################################

## 資料集說明 :
# - 來源 : [kaggle](https://www.kaggle.com/datasnaek/youtube-new)
# - 資料集 : youtube 發燒影片列表資料，有 US、CA、DE、FR、GB、IN、JP、KR、MX、RU，共 10 個國家的統計資料。
# - 資料集大小 : 40949 筆資料、16個欄位
# - 時間 : 2017-11-14 ~ 2018-06-14

## 欄位說明 :
# - [欄位說明](https://techpostplus.com/youtube-video-categories-list-faqs-and-solutions/)
# - video_id : 影片 ID
# - trending_date : 發燒日期
# - title : 影片標題
# - channel_title : 頻道標題
# - category_id : 類別 ID
# - publish_time : 影片發布時間
# - tags : 標籤
# - views : 觀看數
# - likes : 喜歡數
# - dislikes : 倒讚數
# - comment_count : 評論數
# - thumbnail_link : 影片縮圖連結
# - comments_disabled : 是否允許評論
# - ratings_disabled : 是否允許評分
# - video_error_or_removed : 影片錯誤或移除
# - description : 影片描述

## 關鍵指標
# 1. YouTube 參與度指標 (觀看次數、喜歡人數、不喜歡人數、訂閱數) 代表觀眾與影片或頻道互動的次數。這些指標可以視為衡量影片或頻道整體熱門程度的重要指標。([Youtube](https://support.google.com/youtube/answer/2991785?hl=zh-Hant)) <br><br>
# 2. 發燒影片會將許多指標納入參考，以下為其中幾項： ([Youtube](https://support.google.com/youtube/answer/7239739?hl=zh-Hant))
  # - 觀看次數
  # - 影片產生觀看次數的速度 (即「熱度」)
  # - 觀看次數的來源 (包括 YouTube 以外的來源)
  # - 影片已發布多久
  # - 與同頻道最近上傳的其他影片相比，影片的表現如何
# 3. 平均收益 (https://reurl.cc/avgWb9)
  # - 根據美國富比士商業雜誌：頂尖的創作者平均 1000 觀看能夠創造 5 美元收入

## 報告內容
# 1. 每組共有 15 分鐘 (10分鐘報告與 5 分鐘 Q & A)。
  # - 資料來源
  # - 研究背景
  # - 分析結果
  # - 遭遇的困難等重點
  # - 預期至少要有敘述性分析的結果，也請不要花時間說明原始碼與實作細節。
# 2.報告 slides 上請加上"組號"、"參與組員姓名與學號"、 "slides numbers"，如果組員沒出現或參與過程，請勿列出。

############################################################################

# setwd("~/GitHub/bigdata_team_project/巨量期中")
rm(list=ls()) # clean env
pacman::p_load("data.table", "tidyverse", "sqldf", "jsonlite", "corrplot", "d3heatmap") # load packages

yt = read_csv("./USvideos.csv") # read csv
sapply(yt, function(x) sum(ifelse(is.na(x), 1, 0))) # check NA，discription 有 NA
yt[is.na(yt)] = "" # fill NA，將 discription NA 填為空字串

# load category JSON file，讀取類別 JSON 檔
cjson = fromJSON("./US_category_id.json"); cid = cjson$items$id; ctable = as.data.frame(cid); ctable$category = cjson$items$snippet$title
# add category name to youtube data frame，新增類別名稱欄位
youtube = merge(yt, ctable, by.x = "category_id", by.y = "cid") 
# trending_date to Date type，資料型態轉換
youtube$trending_date = as.Date(youtube$trending_date, format = "%y.%d.%m")
# 日期範圍 : 2017-11-14 ~ 2018-06-14
cat("開始日期:", format(min(youtube$trending_date)), "結束日期:", format(max(youtube$trending_date)))
# remove 清理環境
rm(list=c("cjson", "ctable", "yt", "cid")) 
# add trending_days column，新增每部影片上幾天熱門的欄位
youtube = group_by(youtube, video_id) %>% mutate(trending_days = n())
# add average profit : views / 1000 * 5
youtube$avg_profit = youtube$views / 1000 * 5
# most views of each video，將每部影片最終的資料獨立出來，避免重複統計。
mostViews = group_by(youtube, video_id) %>% filter(views == max(views))


save(youtube, mostViews, file = "./pre_process.rdata")

