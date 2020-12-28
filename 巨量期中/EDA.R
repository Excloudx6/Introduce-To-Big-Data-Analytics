# setwd("~/GitHub/bigdata_team_project/巨量期中")
rm(list=ls()) # clean env
pacman::p_load("data.table", "tidyverse", "sqldf", "jsonlite", "corrplot", "d3heatmap") # load packages
load("./pre_process.rdata") # load pre process data : youtube and mostViews

# channel of most frequently appearing in trending videos : 前 10 最常上熱門的頻道
group_by(youtube, channel_title) %>% summarise(n = n()) %>% arrange(desc(n)) %>% head(10) %>% ggplot(aes(x = reorder(channel_title, n), y = n, fill = channel_title)) + geom_bar(stat = "identity") + coord_flip()

# 觀看數前 10 名影片, 音樂 8 : 2 娛樂 
arrange(mostViews, desc(views)) %>% head(10) %>% select(channel_title, title, views, category) %>% group_by(category) %>% summarise(n = n()) %>% ggplot(aes(x = category, y = n, fill = category)) + geom_bar(stat = "identity")

# video publish time : 下午 4 點左右為影片發布熱點
mutate(mostViews, hour = hour(publish_time)) %>% group_by(hour) %>% summarise(n = n()) %>% ggplot(aes(x = hour, y = n)) + geom_line() + scale_x_continuous(breaks=c(0:23)) + labs(x = "hour", y = "number of videos") + geom_vline(xintercept = 16 , color = "#ff2244", size = 1)

# Number of videos by weekdays : 週三、週二影片發布量最多
mutate(mostViews, weekday = weekdays(publish_time)) %>% group_by(weekday) %>% summarise(n = n()) %>% ggplot(aes(x = reorder(weekday, n), y = n, fill = weekday)) + geom_bar(stat = "identity") + labs(y = "number of videos")

# videos published heat map : 每周二到四下午四點左右為影片發布熱點, Sunday is 0.
table(format(mostViews$publish_time,"%H"), format(mostViews$publish_time,"%w")) %>% as.data.frame.matrix %>% d3heatmap(F, F, col = colorRamp(c("seagreen", "lightyellow", "red")))

# 哥要的圖
mostViews = mutate(mostViews, time = format(publish_time, "%Y-%m"))
breaks = append(paste("2017-", c(11, 12), sep =""), paste("2018-0", c(1:6), sep =""))
group_by(mostViews, category, time) %>% summarise(total_views = log10(sum(views))) %>% ggplot(aes(x = time, y = total_views, group = category, color = category)) + geom_line() + xlim(breaks) + theme(axis.text.x = element_text(angle = 90)) 
group_by(mostViews, category, time) %>% summarise(total_views = log10(sum(views))) %>% ggplot(aes(x = time, y = total_views, group = category)) + geom_line() + facet_wrap(~ category) 

# 一天最多 200 支影片上熱門
nrow(youtube[format(youtube$trending_date) == "2017-11-14", ])

# 影片上熱門天數集中在 1 ~ 7 天
breaks = as.character(c(1:30))
mean(mostViews$trending_days) # 平均上熱門 6.44 天
group_by(mostViews, trending_days) %>% summarise(n = n()) %>% ggplot(aes(x = trending_days, y = n, fill = trending_days)) + geom_bar(stat = "identity") + xlim(breaks)

