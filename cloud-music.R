#### R语言期末大作业--yq ####

# 基于R语言的网易云音乐用户精细化运营分析
# 探索网易云音乐用户群体基本特征；
# 如何利用用户活动数据构建用户属性，并对用户细分；
# 怎样对用户活跃度进行量化分箱，并基于此构建用户活跃度判别模型；
# 如何为用户推荐优质的相似用户，以提升用户的社交属性与平台粘性；
# 如何基于用户的曲风数据进行曲风推荐；
# 实验结果综合展示：用户个性化面板数据

#### 1.数据清洗 ####
setwd("D:/文档/2023-1/R/期末综合实验")
music <- read.csv("./music.csv", stringsAsFactors = FALSE, fileEncoding = 'GBK',quote = "")
View(music)
summary(music)
table(is.na(music))
psych::describe(music)

## 去除爬虫导致的不可利用数据--缺失值处理
music = na.omit(music)
table(is.na(music))
View(music)
summary(music)
psych::describe(music)

## 删除无用变量：用户名、更新时间
music <- music[,-2]
music <- music[,-14]
str(music)

## 是否新用户与是否召回用户的数据转换
music$user_newUser[music$user_newUser==FALSE]<-0
music$user_newUser[music$user_newUser==TRUE]<-1
music$user_recallUser[music$user_recallUser==FALSE]<-0
music$user_recallUser[music$user_recallUser==TRUE]<-1

## 个性签名转换为（0，1）
music$user_introduce[music$user_introduce != ""]<-1
music$user_introduce[music$user_introduce == ""]<-0
table(music$user_introduce)

## 用户标签的预处理
library(stringr)
pattern = "\'(.*?)\'"
music$user_tags =gsub("\'","",str_extract_all(music$user_tags,pattern))

## 用户评论处理
# 统计用户评论数
library(dplyr)
groupbyuserid = aggregate(music$user_id, by=list(type=music$user_id),length)
names(groupbyuserid) <- c("user_id", "comment_count")
music<-merge(music,groupbyuserid,by="user_id",all=T)
# 评论文本预处理：得到去重后的评论文本
comment_set <- unique(music[, 19])

## 用户去重
music_set <- music %>% distinct(user_id, .keep_all = T)
music_set <- music_set[,-19]
View(music_set)

#### 2.用户基本特征分析 ####
# 性别占比
user_gender_group = table(music_set$user_gender)
names(user_gender_group) <- c('未知','男','女')
piepercent <- round(100 * user_gender_group / sum(user_gender_group), 1)
piepercent <- paste(piepercent, "%", sep = "")
piepercent

# 年龄分布:人口金字塔
library(ggplot2)
music_set$user_age <- as.integer(music_set$user_age)
user_gender_age_group = as.data.frame(table(
  music_set$user_gender[which(music_set$user_age >= 9)],
  music_set$user_age[which(music_set$user_age >= 9)]))

user_gender_age_group = user_gender_age_group[which(
  user_gender_age_group$Var1!=0),]

names(user_gender_age_group) = c('sex','age','count')
user_gender_age_group$sex = as.integer(as.character(user_gender_age_group$sex))
user_gender_age_group$age = as.integer(as.character(user_gender_age_group$age))
user_gender_age_group$sex[user_gender_age_group$sex=='1'] <- "male"
user_gender_age_group$sex[user_gender_age_group$sex=='2'] <- "female"
# install.packages("ggcharts")

library(ggcharts)
pyramid_chart(user_gender_age_group, age, count, group = sex, bar_colors = 
                c("#845EC2", "#FF8066"),)

# 不同类型用户占比
user_usertype_group <- music_set[which(music_set$user_usertype < 300),] %>% 
  group_by(user_usertype)%>%
  summarise(n = n())
library("echarts4r")
library(tidyverse)
user_usertype_group$user_usertype = c('普通用户','欧美歌单达人、图文达人、欧美音乐自媒体',
                                      '网易音乐人','网易云音乐评论大赛专用账号',
                                      '华语歌单达人、图文达人','Mlog达人',
                                      '声音达人、图文达人','Mlog达人、声音达人、图文达人')
user_usertype_group %>% 
  as.data.frame() %>%
  e_charts(user_usertype) %>% 
  e_pie(n,radius = c('50%','70%')) %>% 
  e_title("用户类型占比")

# 不同类型消费用户占比
user_vipType_group <- music_set %>% 
  group_by(user_vipType)%>%
  summarise(n = n())
user_vipType_group$user_vipType = c('普通用户','黑胶会员','普通会员')
user_vipType_group %>% 
  as.data.frame() %>%
  e_charts(user_vipType) %>% 
  e_pie(n,radius = c('40%','70%')) %>% 
  e_color(c("#2766A8","#5C948F","#EEE8A9")) %>%
  e_title("会员用户占比")

# 用户地域分布
library(hchinamap)
library(magrittr)
library(dplyr)
music_set$user_city <- trunc(music_set$user_city/10000)
music_set$user_city[which(music_set$user_city > 99)] <- 0
user_city_group <- music_set[which(music_set$user_city != 0),] %>% 
  group_by(user_city)%>%
  summarise(n = n())
as.factor(user_city_group$user_city)
user_city_group$user_city = c('北京', '天津', '河北', '山西', '内蒙古', '辽宁', 
                              '吉林','黑龙江','上海','江苏', '浙江', '安徽',
                              '福建','江西', '山东', '河南', '湖北','湖南', 
                              '广东','广西', '海南', '重庆', '四川', '贵州',
                              '云南', '西藏','陕西', '甘肃','青海','宁夏',
                              '新疆', '台湾','香港', '澳门')
hchinamap(
  name = user_city_group$user_city, 
  value = user_city_group$n,
  width = "100%", 
  height = "500px",
  title = "网易云音乐用户地域分布", 
  region = "China",
  minColor = "#FFEFD5",
  maxColor = "#FF3030")

# 新用户与召回用户占比
music_set$user_createDays <- as.integer(music_set$user_createDays)
str(music_set$user_createDays)
user_count = nrow(music_set)
user_newuser_count = nrow(music_set[which(music_set$user_createDays <= 30),])
user_recalluser_count = nrow(music_set[which(music_set$user_recallUser == 1),])
# 新用户占比
round(100 * user_newuser_count / user_count, 3) %>%
  paste("%", sep = "")
# 召回用户占比
round(100 * user_recalluser_count / user_count, 3) %>%
  paste("%", sep = "")

# 创建天数分布
user_createDays_dist <- music_set[which(music_set$user_gender != 0),][c('user_gender','user_vipType','user_createDays')]
user_createDays_dist$user_gender[user_createDays_dist$user_gender=='2'] <- "female"
user_createDays_dist$user_gender[user_createDays_dist$user_gender=='1'] <- "male"
user_createDays_dist$user_vipType[user_createDays_dist$user_vipType==10] <- "黑胶会员"
user_createDays_dist$user_vipType[user_createDays_dist$user_vipType==11] <- "普通会员"
user_createDays_dist$user_vipType[user_createDays_dist$user_vipType==0] <- "普通用户"
ggplot(user_createDays_dist , aes(x = user_createDays)) +
  geom_histogram(bins = 60, fill = "#FFC37E", colour = "#93B40C") +
  facet_grid(vars(user_gender), vars(user_vipType),scales="free")

# 听歌数分布
library(ggExtra)
p <- ggplot(music_set[which((music_set$user_listenSongs < 75000)&(music_set$user_gender !=0)),]) +
  geom_point(aes(x = user_listenSongs, y = user_createDays, color = user_gender), alpha = 0.6, shape = 16) +  # alpha 调整点的透明度；shape 调整点的形状
  theme_bw() +
  theme(legend.position = "bottom") + # 图例置于底部
  labs(x = "user_listenSongs", y = "user_createDays") # 添加x，y轴的名称
ggMarginal(p, type = "density", groupColour = TRUE, groupFill = TRUE)
# ggMarginal(p, type = "boxplot", groupColour = TRUE, groupFill = TRUE)

# 曲风喜好统计
pattern = "\"(.*?)\""
tags_list<-list()
for (i in 1:nrow(music_set)){
  tags_list[[i]]= gsub("\"","",gsub("\\\\\\\\xa0"," ",str_extract_all(music_set$user_tags[i],pattern)[[1]]))
}
tags <- unlist(tags_list)
tags_freq <- as.data.frame(table(tags))
tags_freq$tag_type <- NA
type_lag <- c("欧美", "日语", "韩语", "小语种","华语,”粤语")
type_style <- c("流行", "摇滚", "民谣", "电子", "舞曲", "说唱", "轻音乐", "爵士",
                "乡村", "R&B/Soul", "古典", "民族", "英伦", "金属", "朋克",
                "蓝调", "雷鬼", "世界音乐", "拉丁", "New Age", "古风", "后摇",
                "Bossa Nova", "另类/独立", "性感")
type_scene <- c("清晨", "夜晚", "学习", "工作", "午休", "下午茶", "地铁", "驾车", 
                "运动", "旅行", "散步",  "酒吧")
type_emotion <- c("怀旧", "清新", "浪漫", "伤感", "治愈", "放松", "孤独", 
                  "感动", "兴奋", "快乐", "安静", "思念")
type_theme <- c("综艺", "影视原声", "ACG", "儿童", "校园", "游戏", "70后", "80后",
                "90后", "网络歌曲", "KTV", "经典", "翻唱", "吉他", "钢琴", "器乐", 
                "榜单", "00后", "音乐剧")
tags_freq$tags <- as.character(tags_freq$tags)
for (i in 1:nrow(tags_freq)){
  if (any(grepl(tags_freq$tags[i], type_lag))){
    tags_freq$tag_type[i] <- "语种"
  }
  if (any(grepl(tags_freq$tags[i], type_style))){
    tags_freq$tag_type[i] <- "风格"
  }
  if (any(grepl(tags_freq$tags[i], type_scene))){
    tags_freq$tag_type[i] <- "场景"
  }
  if (any(grepl(tags_freq$tags[i], type_emotion))) {
    tags_freq$tag_type[i] <- "情感"
  }
  if (any(grepl(tags_freq$tags[i], type_theme))) {
    tags_freq$tag_type[i] <- "主题"
  }
}
library(sunburstR)
library(d3r)
# https://zhuanlan.zhihu.com/p/148537340
tag_seq <- data.frame(path = paste(tags_freq$tag_type,tags_freq$tags,sep = "-"),
                      Freq = tags_freq$Freq)
head(tag_seq)
sund2b(tag_seq)

# 评论文本分析,词云
library(jiebaR)
cutter <- worker(type = "tag", stop_word = "stopwords.txt")
seg_word <- list()
for(i in 1:length(comment_set)){
  seg_word[[i]] <- segment(comment_set[i], cutter)
}
head(seg_word, 40)
n_word <- sapply(seg_word, length)
index <- rep(1:length(seg_word), n_word)
nature <- unlist(sapply(seg_word, names))
result <- data.frame(index, unlist(seg_word), nature)
colnames(result) <- c("id", "word","nature")
head(result)
n_word <- sapply(split(result,result$id), nrow)
index_word <- sapply(n_word, seq_len)
index_word <- unlist(index_word)  
result$index_word <- index_word
head(result)
is_n <- subset(result, grepl("n", result$nature), "id")
result <- result[result$id %in% is_n$id, ]
library(wordcloud2)
word.frep <- table(result$word)
word.frep <- sort(word.frep, decreasing = TRUE)
word.frep <- data.frame(word.frep)
head(word.frep)
wordcloud2(word.frep[1:100,], color = "random-light",backgroundColor = 'black',shape = 'circle')

# 变量相关性矩阵
corr_vars <- music_set %>% select(user_followeds, user_follows, user_createDays, 
                                  user_eventCount, user_listenSongs, user_playlistCount, 
                                  user_playlistBeSubscribedCount, user_other_create,
                                  praise, comment_count)
corr_vars$user_follows = as.integer(corr_vars$user_follows) 
corr_vars$user_eventCount = as.integer(corr_vars$user_eventCount)
library(corrgram)
corrgram(corr_vars,
         lower.panel=panel.shade,
         upper.panel=panel.cor,
         diag.panel=panel.density,
         cor.method = "pearson")

#### 3.用户细分 ####
# 数据转换
# write.csv(music_set, "music_set.csv", row.names = FALSE)
music_cluster <- music_set %>% select(user_id,user_level,user_createDays,user_followeds,
                                      user_follows,user_eventCount,comment_count,
                                      user_introduce,praise, user_playlistBeSubscribedCount,
                                      user_listenSongs,user_playlistCount,user_other_create,
                                      user_tags)
psych::describe(music_cluster)
str(music_cluster)
music_cluster$user_level <- as.numeric(music_cluster$user_level)
music_cluster$user_createDays <- as.numeric(music_cluster$user_createDays)
music_cluster$user_follows <- as.numeric(music_cluster$user_follows)
music_cluster$user_introduce <- as.numeric(music_cluster$user_introduce)
music_cluster$user_eventCount <- as.numeric(music_cluster$user_eventCount)
str(music_cluster)
psych::describe(music_cluster)
music_cluster[,2:13] <- scale(music_cluster[,2:13])

# 用户使用时长:user_level用户等级,user_createDays创建天数
music_cluster$user_usage_z <- scale(music_cluster$user_level + music_cluster$user_createDays)[,1]

# 用户社交属性
music_cluster$user_social_z <- scale(music_cluster$user_followeds + music_cluster$user_follows +
  music_cluster$user_eventCount + music_cluster$comment_count + music_cluster$user_introduce +
  music_cluster$praise + music_cluster$user_playlistBeSubscribedCount)[,1]

# 用户音乐多度
music_cluster$user_listenSongs_num_z <- scale(music_cluster$user_listenSongs +
  music_cluster$user_playlistCount + music_cluster$user_other_create)[,1]

# 用户音乐广度
music_cluster$user_listenSongs_range_z <- NA 
pattern = "\"(.*?)\""
for (i in 1:nrow(music_cluster)){
  music_cluster$user_listenSongs_range_z[i] <- length(
    unique(
      gsub("\"","",
           gsub("\\\\\\\\xa0"," ",
                str_extract_all(music_cluster$user_tags[i],pattern)[[1]]
                )
          )
          )
      )
}
music_cluster$user_listenSongs_range_z <- music_cluster$user_listenSongs_range/74
music_cluster$user_listenSongs_range_z <- scale(music_cluster$user_listenSongs_range_z)[,1]

music_cluster_z = music_cluster[c(1,c(15:18))]
str(music_cluster_z)

# 最优聚类数确定
library(NbClust)
par(mar = c(2,2,2,2))
nc <- NbClust(music_cluster_z[1:200,2:5], min.nc = 2, max.nc = 7, method = "kmeans")
nc
table(nc$Best.nc[1, ])
par(mfrow = c(1,1))
barplot(table(nc$Best.nc[1, ]), xlab = "聚类数", ylab = "准则数", main = "由26个准则选择的聚类数")

# K-means聚类方法
k <- kmeans(music_cluster_z[2:5], 3)
k$cluster
k$size
k$centers
plot(music_cluster_z[2:5], col = k$cluster, pch = 19, frame = FALSE,
     main = "K-means with k = 3")

# 聚类结果可视化：雨云图
music_cluster_z$cluster <- k$cluster
data <- data.frame(
  var = c(rep("usage",nrow(music_cluster_z)),rep("social",nrow(music_cluster_z)),
          rep("listensongs_num",nrow(music_cluster_z)),rep("listensongs_range",nrow(music_cluster_z))),
  cluster = rep(as.factor(music_cluster_z$cluster),4),
  value = c(as.factor(music_cluster_z$user_usage_z),as.factor(music_cluster_z$user_social_z),
            as.factor(music_cluster_z$user_listenSongs_num_z),as.factor(music_cluster_z$user_listenSongs_range_z))
)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(grid)
"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}
color<-brewer.pal(7,"Set2")[c(1,2,4,5)]
geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                             position = "dodge", trim = TRUE, scale = "area",
                             show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}
GeomFlatViolin <-
  ggproto("GeomFlatViolin", Geom,
          setup_data = function(data, params) {
            data$width <- data$width %||%
              params$width %||% (resolution(data$x, FALSE) * 0.9)
            
            data %>%
              group_by(group) %>%
              mutate(ymin = min(y),
                     ymax = max(y),
                     xmin = x,
                     xmax = x + width / 2)
            
          },
          
          draw_group = function(data, panel_scales, coord) {
            data <- transform(data, xminv = x,
                              xmaxv = x + violinwidth * (xmax - x)) 
            
            newdata <- rbind(plyr::arrange(transform(data, x = xmaxv), -y),plyr::arrange(transform(data, x = xminv), y))
            newdata_Polygon <- rbind(newdata, newdata[1,])
            newdata_Polygon$colour<-NA
            
            newdata_Path <- plyr::arrange(transform(data, x = xmaxv), -y)
            
            ggplot2:::ggname("geom_flat_violin", grobTree(
              GeomPolygon$draw_panel(newdata_Polygon, panel_scales, coord),
              GeomPath$draw_panel(newdata_Path, panel_scales, coord))
            )
          },
          
          draw_key = draw_key_polygon,
          
          default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
                            alpha = NA, linetype = "solid"),
          
          required_aes = c("x", "y")
  )
findParams <- function(mu, sigma, skew, kurt) {
  value <- .C("JohnsonMomentFitR", as.double(mu), as.double(sigma),
              as.double(skew), as.double(kurt - 3), gamma = double(1),
              delta = double(1), xi = double(1), lambda = double(1),
              type = integer(1), PACKAGE = "SuppDists")
  
  list(gamma = value$gamma, delta = value$delta,
       xi = value$xi, lambda = value$lambda,
       type = c("SN", "SL", "SU", "SB")[value$type])
}

data$value<-as.numeric(as.character(data$value)) 
ggplot(sample_n(data, 2000), aes(x = var, y = value, fill = cluster)) +
  geom_flat_violin(aes(fill = cluster),position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = var, y = value, colour = cluster),position = position_jitter(width = .05), size = 1, shape = 20)+
  geom_boxplot(aes(x = var, y = value, fill = cluster),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ylim(-2,3.5)


#### 用户活跃度量化分箱与判别 ####
# 用户活跃度规则定义与分箱
# 变量选取user_level, user_followeds, user_followes, user_eventCount, 
# user_createDays,user_listenSongs, user_playlistCount, user_other_create, comment_count
music_set$user_level <- as.numeric(music_set$user_level)
music_set$user_follows <- as.numeric(music_set$user_follows)
music_set$user_eventCount <- as.numeric(music_set$user_eventCount)
str(music_set)
music_set$activation = scale(music_set$user_level)[,1]+scale(music_set$user_followeds)[,1]+
  scale(music_set$user_follows)[,1]+ scale(music_set$user_eventCount)[,1]+scale(music_set$user_createDays)[,1]+
  scale(music_set$user_playlistCount)[,1]+scale(music_set$user_other_create)[,1]+
  scale(music_set$user_listenSongs)[,1]+scale(music_set$comment_count)[,1]
quantile(music_set$activation,c(0,0.3,0.8,1))
music_set$activation_type <- NA
# cut(music_set$activation,c(-8.081792, -2.039485, 2.537931, 59.028320))
music_set$activation_type[which((music_set$activation < -2.03948) & (-8.081793 <= music_set$activation))] <- "0"
music_set$activation_type[which((music_set$activation <= 2.537931) & (-2.039485 <= music_set$activation))] <- "1"
music_set$activation_type[which((music_set$activation <= 59.028321) & (2.537931 < music_set$activation))] <- "2"
# 0-低活跃度;1-中;2-高``
table(music_set$activation_type)

# 用户活跃度判别
# 特征提取
# 数据转换
charges <- music_set[c(c(2:4),c(6:9),11,c(13:17),19,23,25)]
str(charges)

# 特征相关性
charges <- model.matrix(activation_type ~ . -1, charges) %>% as.data.frame()
charges$activation_type <- as.numeric(music_set$activation_type)
library(corrplot)
corr1 <- cor(charges[, "activation_type"], charges)
library(dplyr)
par(mar=c(7,3,2,1) + 0.1)
var_names <- c('user_age','user_gender0','user_gender1','user_gender2','user_level',
               'user_vipType','user_followeds','user_follows','user_eventCount',
               'user_introduce1','user_createDays','user_listenSongs','user_playlistCount',
               'user_playlistBeSubscribedCount','user_other_create','praise',
               'comment_count','activation_type')
corr1 <- data.frame(feature = var_names,value = corr1[1,]) %>% select(feature,value) %>% arrange(desc(value))
barplot(corr1$value, col = "blue" ,las =2,names.arg=corr1$feature)

# 去除变量&标准化
charges <- music_set[c(c(2:4),c(6:9),11,c(13:17),19,23,25)]
str(charges)
charges$user_gender <- as.numeric(charges$user_gender)
charges$user_introduce <- as.numeric(charges$user_introduce)
charges$activation_type <- as.numeric(charges$activation_type)
charges <- charges[,-2]
charges <- charges[,-4]
charges <- charges[,-10]
charges$user_age <- scale(charges$user_age, center = TRUE, scale = TRUE)[,1]
charges$user_level <- scale(charges$user_level, center = TRUE, scale = TRUE)[,1]
charges$user_follows <- scale(charges$user_follows, center = TRUE, scale = TRUE)[,1]
charges$user_eventCount <- scale(charges$user_eventCount, center = TRUE, scale = TRUE)[,1]
charges$user_createDays <- scale(charges$user_createDays, center = TRUE, scale = TRUE)[,1]
charges$user_listenSongs <- scale(charges$user_listenSongs, center = TRUE, scale = TRUE)[,1]
charges$user_playlistCount <- scale(charges$user_playlistCount, center = TRUE, scale = TRUE)[,1]
charges$user_other_create <- scale(charges$user_other_create, center = TRUE, scale = TRUE)[,1]
charges$praise <- scale(charges$praise, center = TRUE, scale = TRUE)[,1]
charges$comment_count <- scale(charges$comment_count, center = TRUE, scale = TRUE)[,1]

ggplot(charges,aes(x = as.factor(user_vipType),y=user_listenSongs,fill=as.factor(activation_type)))+
  geom_boxplot(width=0.5,alpha=0.8)+
  labs(fill = "活跃度类型") +
  ylim(-1,15)

ggplot(charges,aes(x = as.factor(user_vipType),y=user_createDays,fill=as.factor(activation_type)))+
  geom_boxplot(width=0.5,alpha=0.8)+
  labs(fill = "活跃度类型") 

# 异常值处理
deal_Outlier <- function(Dat,n){
  Dat[, n] <- as.numeric(as.character(Dat[, n]))
  outlier_limup <-3 * IQR(Dat[, n], na.rm = TRUE)+ quantile(Dat[, n], 3 / 4,na.rm = TRUE, names = FALSE)
  outlier_limdown <-
    quantile(Dat[, n], 1 / 4,na.rm = TRUE, names = FALSE) - 3 * IQR(Dat [, n],na.rm = TRUE)
  Dat <- Dat[!Dat[, n] >= outlier_limup & ! Dat[, n] <= outlier_limdown, ]
  return(Dat)
}
charges = deal_Outlier(charges,8)
charges = deal_Outlier(charges,9)

# 分类变量转独热编码
charges$user_vipType <- factor(charges$user_vipType, labels = c(0,1,2))

# LightGBM模型训练
# 数据转换与训练测试集划分
library(tidymodels)
library(bonsai)
tidymodels_prefer()
charges$activation_type <- factor(charges$activation_type)
set.seed(123)
charges_split <- initial_split(charges, prop = 0.80)
train <- training(charges_split)
test  <-  testing(charges_split)

# 设置超参数搜索空间
lgb_spec <-boost_tree(tree_depth = tune(),
                      trees = tune(), 
                      learn_rate = tune(), 
                      min_n = tune(), 
                      loss_reduction = tune()) %>% 
  set_engine('lightgbm') %>%
  set_mode('classification')
lgb_spec

# 设置随机网格搜索
lgb_grid <- grid_random(tree_depth(),
                        trees(), 
                        learn_rate(), 
                        min_n(), 
                        loss_reduction())
lgb_grid

# 5折交叉验证
set.seed(123)
lgb_folds <- vfold_cv(train,5) # 5折交叉验证
lgb_folds

# 建立工作流
lgb_wflow <-
  workflow() %>% 
  add_model(lgb_spec) %>% 
  add_formula(factor(activation_type) ~ .)

lgb_res <-lgb_wflow %>% 
  tune_grid(resamples = lgb_folds,
            grid = lgb_grid
  )
lgb_res

autoplot(lgb_res)
lgb_res %>% 
  collect_metrics()

best_lgb <- lgb_res %>%
  select_best("accuracy")
best_lgb
show_best(lgb_res,"accuracy")

# 最优参数下的模型工作流
charges_rec<-
  recipe(activation_type ~., data = train)

lgb_best <-
  boost_tree(tree_depth = 10,
             trees = 1509, 
             learn_rate = 0.00423, 
             min_n = 38, 
             loss_reduction = 0.139) %>%
  set_engine('lightgbm') %>%
  set_mode('classification')

# 建立工作流
lgb_wflow <- 
  workflow() %>% 
  add_model(lgb_best) %>% 
  add_recipe(charges_rec)
#模型拟合
lgb_fit <- lgb_wflow %>% fit(data=train)
#预测分类
lgb_pred <- predict(lgb_fit,new_data = test)
#预测概率
lgb_pred_prob <- predict(lgb_fit,new_data =test,type = "prob")
# 预测数据整理
lgb_test_res <- bind_cols(lgb_pred,
                          test %>% select(activation_type))
lgb_test_prob <- bind_cols(lgb_pred_prob,
                           test %>% select(activation_type))
#混淆矩阵
lgb_table <- table(actual = test$activation_type,predict = lgb_pred$.pred_class)
lgb_table

# 准确率
lgb_test_res %>% accuracy(activation_type,.pred_class)

# kappa值
lgb_test_res %>% kap(activation_type,.pred_class)
# F1-score
lgb_test_res %>% f_meas(activation_type,.pred_class)
# AUC值
lgb_test_prob %>% roc_auc(activation_type,.pred_0,.pred_1,.pred_2,estimator = 'macro')
#ROC曲线
lgb_test_prob %>% 
  roc_curve(activation_type,.pred_0,.pred_1,.pred_2,estimator = 'multiclass') %>% 
  autoplot()

# 模型解释：SHAP值
library(DALEXtra)
lgb_explainer <- explain_tidymodels(lgb_fit,
                                    data=train[,-14],
                                    y = train$activation_type)
# SHAP
lgb_shap <- predict_parts(lgb_explainer,
                          type="shap", 
                          new_observation = train[3,],
                          B=50)
# 其中横轴为SHAP值,纵轴是样本各个特征的取值
# 箭头向左,SHAP值减小,代表该特征对预测有负向影响;箭头向右,SHAP值增,代表该特征对预测有正向影响。
plot(lgb_shap)


#### 相似用户推荐&&曲风推荐  ####
# 第1列是用户ID，第2列是曲风名，第3列是出现次数
music_recommend <- music_set
music_recommend$tags_num <- NA
pattern = "\"(.*?)\""
# 获取标签数
for (i in 1:nrow(music_recommend)){
    music_recommend$tags_num[i] <- length(unique(
        gsub("\"","",gsub("\\\\\\\\xa0"," ",str_extract_all(music_recommend$user_tags[i],pattern)[[1]]))))
}
table(is.na(music_recommend$tags_num))
# 标签列表
tags_list <- list()
for (i in 1:nrow(music_recommend)){
  tags_list[[i]]= as.character(data.frame(table(gsub("\"","",gsub("\\\\\\\\xa0"," ",str_extract_all(music_recommend$user_tags[i],pattern)[[1]]))))$Var1)
}
tags <- unlist(tags_list)
# 单用户标签数列表
single_tags_list <- list()
for (i in 1:nrow(music_recommend)){
  single_tags_list[[i]]= data.frame(table(gsub("\"","",gsub("\\\\\\\\xa0"," ",str_extract_all(music_recommend$user_tags[i],pattern)[[1]]))))$Freq
}
single_tags <- unlist(single_tags_list)

data1 <- data.frame(
  user_id <- rep(music_recommend$user_id,music_recommend$tags_num),
  tags,
  single_tags
)
names(data1) = c('user_id','tags','count')
library(reshape)
library(recommenderlab)
data1 <- cast(data1, user_id ~ tags, value = "count")
class(data1) <- "data.frame"
data = column_to_rownames(data1,'user_id')
data <- base::as.matrix(data)
class(data)
data <- as(data, "realRatingMatrix")
data

# 用户推荐
similar_user =  similarity(data[2, ],data[-2, ], method = "cosine")
sort(similar_user[,],decreasing=TRUE)[0:5]
#music_recommend[which(music_recommend$user_id==1291552478),]

# 曲风推荐
user_recommModel <- Recommender(data[1:3200], method = "UBCF")
user_recommModel
user_predict1 <- predict(user_recommModel, data[3201], n = 5) 
user_predict1
as(user_predict1, "list")

# 模型评估
model.eval <- evaluationScheme(data[1:2000], method = "split", train = 0.9, given = 15, goodRating = 5)
model.eval

model.random <- Recommender(getData(model.eval, "train"), method = "RANDOM")
model.ubcf <- Recommender(getData(model.eval, "train"), method = "UBCF")
model.ibcf <- Recommender(getData(model.eval, "train"), method = "IBCF")
predict.random <- predict(model.random, getData(model.eval, "known"), type = "ratings")
predict.ubcf <- predict(model.ubcf, getData(model.eval, "known"), type = "ratings")
predict.ibcf <- predict(model.ibcf, getData(model.eval, "known"), type = "ratings")
error <- rbind(calcPredictionAccuracy(predict.random, getData(model.eval, "unknown")), 
               calcPredictionAccuracy(predict.ubcf, getData(model.eval, "unknown")), 
               calcPredictionAccuracy(predict.ibcf, getData(model.eval, "unknown")))
rownames(error) <- c("RANDOM", "UBCF", "IBCF")
error
# UBFC效果最佳


#### 用户画像个性化展示 ####
# 用户属性-雷达图
User_portrait <- music_set %>% select(user_id,user_age,user_gender,user_level,
                                      user_usertype, user_vipType,user_createDays,
                                      user_listenSongs,user_newUser,user_recallUser,
                                      activation, activation_type,user_tags)
User_portrait<-merge(User_portrait,music_cluster_z,by="user_id",all=T)
View(User_portrait)
# devtools::install_github("ricardo-bion/ggradar")
library(ggplot2)
library(ggradar)
User_portrait$user_usage_z <- User_portrait$user_usage_z + 2.75
User_portrait$user_social_z <- User_portrait$user_social_z + 0.87
User_portrait$user_social_z[which(User_portrait$user_social_z >= 3)] <- 3
User_portrait$user_listenSongs_num_z <- User_portrait$user_listenSongs_num_z + 0.83
User_portrait$user_listenSongs_num_z[which(User_portrait$user_listenSongs_num_z >= 3)] <- 3
User_portrait$user_listenSongs_range_z <- User_portrait$user_listenSongs_range_z + 0.83
User_portrait$user_listenSongs_range_z[which(User_portrait$user_listenSongs_range_z >= 3)] <- 3
User_portrait[,15:17] <- (User_portrait[,15:17] *5)/3
User_portrait$activation <- User_portrait$activation + 8.09
User_portrait$activation[which(User_portrait$activation >= 16)] <- 16
User_portrait[,11] <- (User_portrait[,11] *5)/16
psych::describe(User_portrait[,c(11,14:17)])
radar_data <- User_portrait[,c(1,11,14:17)]
names(radar_data) <- c('用户ID','活跃度','使用时长','社交属性','音乐多度','音乐广度')
ggradar(plot.data =radar_data[2,],
        grid.max = 5,
        grid.mid = 1.5,
        axis.label.size = 3,
        background.circle.colour="skyblue",
)



# 曲风-voronoi树状图
# devtools::install_github('uRosConf/voronoiTreemap')
library(voronoiTreemap)
pattern = "\"(.*?)\""
single_tags <- gsub("\"","",gsub("\\\\\\\\xa0"," ",str_extract_all(User_portrait$user_tags[2],pattern)[[1]]))
single_tags_freq <- as.data.frame(table(single_tags))
single_tags_freq$tag_type <- NA
single_tags_freq$color <- NA
type_lag <- c("欧美", "日语", "韩语", "小语种","华语,”粤语")
type_style <- c("流行", "摇滚", "民谣", "电子", "舞曲", "说唱", "轻音乐", "爵士",
                "乡村", "R&B/Soul", "古典", "民族", "英伦", "金属", "朋克",
                "蓝调", "雷鬼", "世界音乐", "拉丁", "New Age", "古风", "后摇",
                "Bossa Nova", "另类/独立", "性感")
type_scene <- c("清晨", "夜晚", "学习", "工作", "午休", "下午茶", "地铁", "驾车", 
                "运动", "旅行", "散步",  "酒吧")
type_emotion <- c("怀旧", "清新", "浪漫", "伤感", "治愈", "放松", "孤独", 
                  "感动", "兴奋", "快乐", "安静", "思念")
type_theme <- c("综艺", "影视原声", "ACG", "儿童", "校园", "游戏", "70后", "80后",
                "90后", "网络歌曲", "KTV", "经典", "翻唱", "吉他", "钢琴", "器乐", 
                "榜单", "00后", "音乐剧")
single_tags_freq$single_tags <- as.character(single_tags_freq$single_tags)
for (i in 1:nrow(single_tags_freq)){
  if (any(grepl(single_tags_freq$single_tags[i], type_lag))){
    single_tags_freq$tag_type[i] <- "语种"
    single_tags_freq$color[i] <- "#99E2D8"
  }
  if (any(grepl(single_tags_freq$single_tags[i], type_style))){
    single_tags_freq$tag_type[i] <- "风格"
    single_tags_freq$color[i] <- "#ABDBF3"
  }
  if (any(grepl(single_tags_freq$single_tags[i], type_scene))){
    single_tags_freq$tag_type[i] <- "场景"
    single_tags_freq$color[i] <- "#AFE0B9"
  }
  if (any(grepl(single_tags_freq$single_tags[i], type_emotion))) {
    single_tags_freq$tag_type[i] <- "情感"
    single_tags_freq$color[i] <- "#D4D8A7"
  }
  if (any(grepl(single_tags_freq$single_tags[i], type_theme))) {
    single_tags_freq$tag_type[i] <- "主题"
    single_tags_freq$color[i] <- "#D5D0FC"
  }
}
single_tags_freq$code <- single_tags_freq$single_tags



data_int = vt_input_from_df(single_tags_freq,
                            hierachyVar1 = "tag_type",
                            hierachyVar2 = "single_tags",
                            colorVar = "color",
                            weightVar = "Freq",
                            labelVar = "code")
# 绘图
vt_d3(vt_export_json(data_int),
      legend= F, # 是否显示图例
      label = T  # 是否显示文字标签
)



# ggplot树状图
#if(!require(treemap))
#  install.packages("treemap")
#if(!require(treemapify))
#  devtools::install_github("wilkox/treemapify")
library(ggplot2)
library(treemapify)
treeMapCoordinates <- treemapify(single_tags_freq, area = "Freq", subgroup = "tag_type",
)
Class_Label1 <- aggregate(cbind(xmin, ymin) ~ tag_type, treeMapCoordinates, min)
Class_Label2 <- aggregate(cbind(xmax, ymax) ~ tag_type, treeMapCoordinates, max)
Class_Label <- cbind(Class_Label1, Class_Label2[c("xmax", "ymax")])
treeMapCoordinates$Area <- (treeMapCoordinates$xmax - treeMapCoordinates$xmin) *
  (treeMapCoordinates$ymax - treeMapCoordinates$ymin)
treeMapCoordinates$label <- treeMapCoordinates$single_tags
ggplot(treeMapCoordinates) + geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin,
  ymax = ymax, fill = tag_type), colour = "black") + geom_text(aes(x = xmin + (xmax -
  xmin)/2, y = ymin + (ymax - ymin)/4, label = label, size = Area)) + scale_size(range = c(2,
  5)) + geom_label(aes(x = (xmin + xmax)/2, y = (ymin + ymax)/2, label = tag_type),
  data = Class_Label, size = 5, fill = "white", alpha = 0.5) + theme_void() + theme(legend.position = "none")


# 相似用户
t = which(data1$user_id == User_portrait[2,1])
similar_user =  similarity(data[t,],data[-t,], method = "cosine")
sort(similar_user[,],decreasing=TRUE)[0:5]
# 推荐曲风
user_predict1 <- predict(user_recommModel, data[1], n = 5) 
user_predict1
as(user_predict1, "list")                                    
                                        
     


# 接口 
library(patchwork)                                  
display <- function(id){
  a = which(radar_data$用户ID == id)
  if (a>nrow(radar_data)){
    return("other error")
  }
  p1<-ggradar(plot.data =radar_data[a,],
                grid.max = 5,
                grid.mid = 1.5,
                axis.label.size = 3,
                background.circle.colour="skyblue",
  )
  print(p1)
  j = which(User_portrait$user_id == id)
  pattern = "\"(.*?)\""
  single_tags <- gsub("\"","",gsub("\\\\\\\\xa0"," ",str_extract_all(User_portrait$user_tags[j],pattern)[[1]]))
  single_tags_freq <- as.data.frame(table(single_tags))
  single_tags_freq$tag_type <- NA
  single_tags_freq$color <- NA
  type_lag <- c("欧美", "日语", "韩语", "小语种","华语,”粤语")
  type_style <- c("流行", "摇滚", "民谣", "电子", "舞曲", "说唱", "轻音乐", "爵士",
                  "乡村", "R&B/Soul", "古典", "民族", "英伦", "金属", "朋克",
                  "蓝调", "雷鬼", "世界音乐", "拉丁", "New Age", "古风", "后摇",
                  "Bossa Nova", "另类/独立", "性感")
  type_scene <- c("清晨", "夜晚", "学习", "工作", "午休", "下午茶", "地铁", "驾车", 
                  "运动", "旅行", "散步",  "酒吧")
  type_emotion <- c("怀旧", "清新", "浪漫", "伤感", "治愈", "放松", "孤独", 
                    "感动", "兴奋", "快乐", "安静", "思念")
  type_theme <- c("综艺", "影视原声", "ACG", "儿童", "校园", "游戏", "70后", "80后",
                  "90后", "网络歌曲", "KTV", "经典", "翻唱", "吉他", "钢琴", "器乐", 
                  "榜单", "00后", "音乐剧")
  single_tags_freq$single_tags <- as.character(single_tags_freq$single_tags)
  for (i in 1:nrow(single_tags_freq)){
    if (any(grepl(single_tags_freq$single_tags[i], type_lag))){
      single_tags_freq$tag_type[i] <- "语种"
      single_tags_freq$color[i] <- "#99E2D8"
    }
    if (any(grepl(single_tags_freq$single_tags[i], type_style))){
      single_tags_freq$tag_type[i] <- "风格"
      single_tags_freq$color[i] <- "#ABDBF3"
    }
    if (any(grepl(single_tags_freq$single_tags[i], type_scene))){
      single_tags_freq$tag_type[i] <- "场景"
      single_tags_freq$color[i] <- "#AFE0B9"
    }
    if (any(grepl(single_tags_freq$single_tags[i], type_emotion))) {
      single_tags_freq$tag_type[i] <- "情感"
      single_tags_freq$color[i] <- "#D4D8A7"
    }
    if (any(grepl(single_tags_freq$single_tags[i], type_theme))) {
      single_tags_freq$tag_type[i] <- "主题"
      single_tags_freq$color[i] <- "#D5D0FC"
    }
  }
  
  treeMapCoordinates <- treemapify(single_tags_freq, area = "Freq", subgroup = "tag_type",
  )
  Class_Label1 <- aggregate(cbind(xmin, ymin) ~ tag_type, treeMapCoordinates, min)
  Class_Label2 <- aggregate(cbind(xmax, ymax) ~ tag_type, treeMapCoordinates, max)
  Class_Label <- cbind(Class_Label1, Class_Label2[c("xmax", "ymax")])
  treeMapCoordinates$Area <- (treeMapCoordinates$xmax - treeMapCoordinates$xmin) *
    (treeMapCoordinates$ymax - treeMapCoordinates$ymin)
  treeMapCoordinates$label <- treeMapCoordinates$single_tags
  p2 <- ggplot(treeMapCoordinates) + geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin,
        ymax = ymax, fill = tag_type), colour = "black") + geom_text(aes(x = xmin + (xmax -
        xmin)/2, y = ymin + (ymax - ymin)/4, label = label, size = Area)) + scale_size(range = c(2,
        5)) + geom_label(aes(x = (xmin + xmax)/2, y = (ymin + ymax)/2, label = tag_type),
        data = Class_Label, size = 5, fill = "white", alpha = 0.5) + theme_void() + theme(legend.position = "none")
  print(p2)
  t = which(data1$user_id == id)
  similar_user =  similarity(data[t,],data[-t,], method = "cosine")
  rownames = row.names(as.data.frame(sort(similar_user[,],decreasing=TRUE)[0:3]))
  # 推荐曲风
  user_predict1 <- predict(user_recommModel, data[t], n = 3)
  as(user_predict1, "list")
  p1 + p2 +
    plot_annotation(
      title = paste("用户ID：",id),
      subtitle = paste(" 相似用户：",rownames[1],"、",rownames[2],"、",rownames[3],
                       "\n","推荐曲风：",as(user_predict1,"list")[[1]][1],"、",
                       as(user_predict1,"list")[[1]][2],"、",
                       as(user_predict1,"list")[[1]][3]),
      caption = "@网易云音乐"
    )
}
                             
display("101589779")
