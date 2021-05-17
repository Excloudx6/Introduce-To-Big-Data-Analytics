rm(list=ls())
pacman::p_load(tidyverse, data.table, jsonlite, corrplot)

tw = fread("./tw.csv", fill = T, stringsAsFactors = F, encoding = "UTF-8", drop = c("title", "publishedAt", "channelId", "channelTitle", "trending_date", "tags", "thumbnail_link", "comments_disabled", "ratings_disabled", "description", "view_count"))
tw$likes_ratio = ifelse(tw$dislikes == 0, 0, tw$likes / tw$dislikes)
tw_edu = tw %>% filter(categoryId == 27)

shapiro.test(tw$income)
y1_pvalue = sapply(tw_edu[, -c(1, 2, 6)], function(x) cor.test(x, tw_edu$income, method = "spearman")$p.value)
names(y1_pvalue[y1_pvalue < 0.05])

tw_cor = cor(tw_edu[, -c(1, 2)])
corrplot(tw_cor, method = "number", type = "upper")
