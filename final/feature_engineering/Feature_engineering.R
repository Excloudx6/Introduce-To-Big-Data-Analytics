# setwd("~/GitHub/bigdata/巨量期末/特徵工程")
rm(list=ls())
pacman::p_load(tidyverse, data.table, jsonlite, corrplot, mltools)

# load tw.csv
tw = fread("./tw.csv", fill = T, stringsAsFactors = F, encoding = "UTF-8", drop = c("title", "publishedAt", "channelId", "channelTitle", "trending_date", "tags", "thumbnail_link", "comments_disabled", "ratings_disabled", "description", "view_count"))
# load category JSON file，讀取類別 JSON 檔
cjson = fromJSON("./TW_category_id.json"); cid = cjson$items$id; ctable = as.data.frame(cid); ctable$category = cjson$items$snippet$title
# add category name to youtube data frame，新增類別名稱欄位
tw$categoryId = as.factor(tw$categoryId)
tw = merge(tw, ctable, by.x = "categoryId", by.y = "cid")
tw$categoryId = NULL
tw$category = as.factor(tw$category)
# 新增喜歡比欄位
tw$likes_ratio = ifelse(tw$dislikes == 0, 0, tw$likes / tw$dislikes)
rm(list=c("cjson", "ctable", "cid"))

#######################################################################

## 相關係數圖
tw_cor = cor(tw[, -c(1, 10)])
# 與 income 高度正相關 : likes、dislikes、comment_count
# 與 likes_ration 高度正相關 : 無
corrplot(tw_cor, method = "number", type = "upper")

## 預測目標 1 : income, 預測目標 2 : likes_ratio
y1 = tw$income; tw$income = NULL
y2 = tw$likes_ratio; tw$likes_ratio = NULL
ids = tw$video_id; tw$video_id = NULL # video_id
# tw$category = as.numeric(tw$category) # label encoding
tw = one_hot(as.data.table(tw)) # one hot encoding

## cor.test()
# income p-value < 0.05 : "likes" "dislikes" "comment_count" "desc_length" "trending_days" "duration" "category"
shapiro.test(y1) # p-value < 0.05, income is not normal
y1_pvalue = sapply(tw, function(x) cor.test(x, y1, method = "spearman", exact = F)$p.value)
names(y1_pvalue[y1_pvalue < 0.05])
# likes_ratio p-value < 0.05 : "likes" "comment_count" "desc_length" "trending_days" "duration"  
shapiro.test(y2) # p-value < 0.05, likes_ratio is not normal
y2_pvalue = sapply(tw, function(x) cor.test(x, y2, method = "spearman", exact = F)$p.value)
names(y2_pvalue[y2_pvalue < 0.05])


# Min-max scaling
normalize = function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
tw_nor = as.data.frame(lapply(tw, normalize))
tw_nor$income = y1
tw_nor$likes_ratio = y2
tw_nor$video_id = ids
tw_nor = tw_nor[c(24, 1:23)]

## output new_tw2.csv (after scaling)
write.table(tw_nor, "./new_tw2.csv", sep = ",", row.names = F)

