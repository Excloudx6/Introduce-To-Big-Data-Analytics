# setwd("~/GitHub/bigdata_team_project/巨量期中")
rm(list=ls()) # clean env
pacman::p_load("data.table", "tidyverse", "sqldf", "jsonlite", "corrplot", "d3heatmap") # load packages
load("./pre_process.rdata") # load pre process data : youtube and mostViews

# category dataframe，新增類別資料框，針對類別作分析
C = group_by(mostViews, category) %>% summarise(likes = mean(likes), dislikes = mean(dislikes), comment_count = mean(comment_count), views = mean(views), trending_days = mean(trending_days))
# C = C %>% mutate(likes_rate = likes / sum(likes), dislikes_rate = dislikes / sum(dislikes), comment_rate = comment_count / sum(comment_count))

### 單變數分析
# 什麼類別的影片上熱門的次數最多 : Entertainment、Music、Howto & Style、Comedy、People & Blogs
# Entertainment 上發燒影片最多次，且差距很大，娛樂是觀眾接受度最高且熱度最高的類別。
group_by(youtube, category) %>% summarise(n = n()) %>% arrange(desc(n)) %>% ggplot(aes(x = reorder(category, n), y = n, fill = category)) + geom_bar(stat = "identity") + coord_flip()
# 什麼類別的影片總觀看數最多 : Music、Entertainment、Film & Animation、Comedy、People & Blogs
group_by(mostViews, category) %>% summarise(total_views = sum(views)) %>% ggplot(aes(x = reorder(category, total_views), y = total_views, fill = category)) + geom_bar(stat = "identity") + coord_flip()
# 什麼類型的影片觀眾參與度最高(喜歡數 + 倒讚數 + 觀看數 + 評論數) : Music、Film & Animation、Nonprofits & Activism、Gaming、Entertainment
group_by(C, category) %>% summarise(engagement = sum(likes + dislikes + views + comment_count)) %>% arrange(desc(engagement)) %>% ggplot(aes(x = reorder(category, engagement), y = engagement, fill = category)) + geom_bar(stat = "identity") + coord_flip()
# Min-max scaling
normalize = function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

df_nor = as.data.frame(lapply(C[2:5], normalize))
df_nor$category = C$category

dfplot = df_nor[, c(1:5)] %>% gather(key, value, -category)
ggplot(dfplot, aes(x = category, y = value, fill = key)) + geom_bar(position = "fill", stat = "identity") + labs(y = "proportion") + theme(axis.text.x = element_text(angle = 90))

# 什麼類別的影片平均上熱門時間最久 : Shows、Gaming、Music、Film & Animation、Howto & Style
group_by(youtube, category) %>% summarise(days = mean(trending_days)) %>% ggplot(aes(x = reorder(category, days), y = days, fill = category)) + geom_bar(stat = "identity") + coord_flip()

### 雙變數分析
ggplot(C, aes(x = likes, y = dislikes)) + geom_point() + geom_smooth(method = "lm", se = F)

ggplot(C, aes(x = log10(likes), y = log10(comment_count))) + geom_point() + geom_smooth(method = "lm", se = F)
ggplot(C, aes(x = log10(likes), y = log10(views))) + geom_point() + geom_smooth(method = "lm", se = F)
ggplot(C, aes(x = likes, y = trending_days)) + geom_point() + geom_smooth(method = "lm", se = F)

ggplot(C, aes(x = log10(dislikes), y = log10(comment_count))) + geom_point() + geom_smooth(method = "lm", se = F)
ggplot(C, aes(x = dislikes, y = views)) + geom_point() + geom_smooth(method = "lm", se = F)
ggplot(C, aes(x = dislikes, y = trending_days)) + geom_point() + geom_smooth(method = "lm", se = F)

# 相關係數 > 0.8 高度正相關 0.5 ~ 0.8 顯著正相關 0.3 ~ 0.5 低度正相關
# likes 與 views、comment_count 有顯著正相關，對於喜歡影片的人來說是蠻正常的
# 但可以發現 dislikes 與 comment_count 有最高的正相關，似乎討人厭的影片評論區能夠引發觀眾論戰。
category_cor = cor(C[, -1])
corrplot(category_cor, method="number", type="upper")

# 每個類別平均上熱門的天數 : shows 平均上熱門天數最多
ggplot(C, aes(x = reorder(category, trending_days), y = trending_days, fill = category)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90))

# heatmap
top10 = names(head(sort(table(mostViews$category),decreasing = T), 10))
table(format(mostViews[mostViews$category %in% top10, ]$publish_time,"%H"), mostViews[mostViews$category %in% top10, ]$category) %>% as.data.frame.matrix %>% d3heatmap(F, F, col = colorRamp(c("seagreen", "lightyellow", "red")))


### 多變數分析
# 各類別喜歡數、不喜歡數、評論數走勢
dfplot = C[, c(1:4)] %>% gather(key, value, -category)
ggplot(dfplot, aes(x = category, y = value, group = key, color = key)) + geom_line() + theme(axis.text.x = element_text(angle = 90))
# bubble plot
# 可以發現影片倒讚數多，即使喜歡數多，觀看數卻不會提升
ggplot(C, aes(x = likes, y = views, size = dislikes, color = category)) + geom_point(alpha = 0.7) + scale_size(range = c(.1, 24), name="dislikes")
# 喜歡比(likes / dislikes)與評論比(comment_count / views)
# 可以看出 Pets & Animals 有很高的喜歡比，大部分人都很喜歡動物
# Nonprofits & Activism (非營利組織和行動主義) e.g. "TED x Talks"
# Nonprofits & Activism 喜歡比低且評論比高，此類別影片可能較多爭議，很多觀眾討論
mutate(C, likes_prop = likes / dislikes, comment_prop = 1000 * (comment_count / views)) %>% ggplot(aes(x = category)) + geom_col(aes(y = likes_prop), size = 1, color = "darkblue", fill = "white") + geom_line(aes(y = comment_prop), size = 1.5, color = "red", group = 1) + scale_y_continuous(sec.axis = sec_axis(~./100, name = "comment_prop")) + labs(y = "likes / dislikes")  + theme(axis.text.x = element_text(angle = 90))

