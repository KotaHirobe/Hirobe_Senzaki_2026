#####
#音量減衰の確認
Soundlevel <- read.csv("c:/Users/kouch/OneDrive/デスクトップ/研究室関連/修士研究/データ/dbalv.csv")
print(head(Soundlevel))

library(ggplot2)
library(tidyr)

long_data <- Soundlevel %>%
  pivot_longer(
    cols = c(d1609, d1616, d1648, d1636, d1628, h1403, h1803, h201001, h201604, whitenoise, no_sounds),
    names_to = "variable",
    values_to = "value"
  )

# Plot the data
ggplot(long_data, aes(x = dist, y = value, color = variable, group = variable)) +
  geom_line() +
  labs(title = "Sound Levels Over Distance", x = "Distance", y = "Sound Level") +
  theme_bw(base_size = 11)



#####
#パッケージの読み込み
library(lubridate)#日時データを読み込むためのパッケージ

#データの読み込みと処理
FIDdata <- read.csv("C:/Users/kouch/OneDrive/デスクトップ/研究室関連/修士研究/データ/FIDdata.csv")
print(head(FIDdata))

#NAを消す
FIDdata <- na.omit(FIDdata)

#lightを数値として読み込み
FIDdata$light <- as.numeric(FIDdata$light)

#日付が数字として読み込まれるので日時のデータに変換
FIDdata$sunrise <- as.POSIXct(paste(FIDdata$day, FIDdata$sunrise))
FIDdata$sunset <- as.POSIXct(paste(FIDdata$day, FIDdata$sunset))
FIDdata$time <- as.POSIXct(paste(FIDdata$day, FIDdata$time))

#日付を経過日数に変換
min_date <- min(FIDdata$day, na.rm = TRUE)
FIDdata$day_count <- as.numeric(difftime(FIDdata$day, min_date, units = "days")) +1
head(FIDdata)

#昼と夜を分ける
FIDdata$day_or_night <- ifelse(FIDdata$time >= FIDdata$sunrise & FIDdata$time <= FIDdata$sunset,
                               "day",
                               ifelse(FIDdata$time > FIDdata$sunset | FIDdata$time < FIDdata$sunrise,
                                      "night", NA))

print(head(FIDdata))
table(FIDdata$day_or_night)

#時間データをGLMMで使えるよう小数にする
FIDdata$time_num <- hour(FIDdata$time) + minute(FIDdata$time)/60



###
#プロット位置データの読み込み
site1 <- read.csv("C:/Users/kouch/OneDrive/デスクトップ/研究室関連/修士研究/データ/site202408.csv")
site2 <- read.csv("C:/Users/kouch/OneDrive/デスクトップ/研究室関連/修士研究/データ/site20241006.csv")
site3 <- read.csv("C:/Users/kouch/OneDrive/デスクトップ/研究室関連/修士研究/データ/site20241013.csv")
site4 <- read.csv("C:/Users/kouch/OneDrive/デスクトップ/研究室関連/修士研究/データ/site20241127.csv")
all_sites <- rbind(site1, site2, site3, site4)
print(head(all_sites))

# "名前"列から数字部分だけを抽出し、新しい列 "名前番号" を作成
all_sites$名前番号 <- as.numeric(gsub("[^0-9]", "", all_sites$名前))



###
#データの結合
# "名前番号"列と "No"列で結合
merged_data <- merge(all_sites, FIDdata, by.x = "名前番号", by.y = "No")
print(head(merged_data))

#座標データの抽出
merged_data$longitude <- as.numeric(sub("POINT \\(([^ ]+) .*", "\\1", merged_data$WKT))
merged_data$latitude <- as.numeric(sub("POINT \\([^ ]+ ([^ ]+)\\)", "\\1", merged_data$WKT))

#不要な行を削除
# dplyrパッケージを読み込み
library(dplyr)
merged_data <- merged_data %>% select(-説明, -照度1, -照度2, -照度3, -X, -X.1, soundNo.)



###
#site_numberの設定
# 必要なパッケージを読み込み
library(geosphere)

# 新しい列 "site_number" を追加し、初期値をNAに設定
merged_data$site_number <- NA

# 半径150m以内の座標に番号を付与
for (i in 1:nrow(merged_data)) {
  current_coords <- c(merged_data$longitude[i], merged_data$latitude[i])
  distances <- distHaversine(current_coords, 
                             cbind(merged_data$longitude, merged_data$latitude))
  within_radius <- which(distances <= 150)
  merged_data$site_number[within_radius] <- i
}

# 重複を解消して連続した番号にする
unique_sites <- unique(na.omit(merged_data$site_number))
merged_data$site_number <- match(merged_data$site_number, unique_sites)


###
#150m離れていなくて1週間あけてとっていないデータを削除
# 名前番号が昇順になるように並び替え
merged_data$day <- as.Date(merged_data$day)
merged_data <- merged_data[order(merged_data$site_number, merged_data$名前番号), ]

#同じ種で150m離れていないかつ7日以上日付が空いていないデータの片方を省けるようにする
merged_data <- merged_data %>%
  arrange(site_number, species, day_count) %>% # site_numberとspeciesでグループ化しdayで並べ替え
  group_by(site_number, species) %>%
  mutate(diff_days = day_count - lag(day_count, default = first(day_count))) %>%
  filter(!(diff_days < 7 & row_number() == 2)) %>%
  select(-diff_days) %>%
  ungroup()

head(merged_data)
table(merged_data$species)






###
#種ごとに分割
Deer <- subset(merged_data, species == "Deer")
Red_fox <-  subset(merged_data, species == "Red fox")

ggplot(Deer, aes(x = site_number, fill = cues))+
  geom_bar(stat = "count")

###
#種ごとの行動圏にあわせて番号を設定
#シカ
# 半径1800m以内の座標に番号を付与
#Laneng et al. 2023を参照
for (i in 1:nrow(Deer)) {
  # 現在の座標の緯度と経度を取得
  Deer_coords <- c(Deer$longitude[i], Deer$latitude[i])
  
  # 現在の座標から他の全ての座標までの距離を計算
  Deer_distances <- distHaversine(Deer_coords, 
                             cbind(Deer$longitude, Deer$latitude))
  
  # 200m以内の座標のインデックスを取得
  Deer_within_radius <- which(Deer_distances <= 1800)
  
  # "site_number" 列に番号を付与（現在の座標も含む）
  Deer$site_number[Deer_within_radius] <- i
}


# 重複を解消して連続した番号にする
unique_sites <- unique(na.omit(Deer$site_number))
Deer$site_number <- match(Deer$site_number, unique_sites)



###
#GLMM
library(lme4)
library(Matrix)
library(ggplot2)

#シカ
# lmer()でGLMMを構築
Deer_model <- lmer(
  log(FID) ~ cues + day_or_night * log(light + 1)  + day_count + flock + noise + log(SD) + MaxWind + (1 | site_number),
  data = Deer
)

# 結果の確認
summary(Deer_model)

library(car)
vif(lm(FID ~ cues + day_or_night*log(light + 1) + day_count + flock + noise + SD, data = Deer))


# 推定値の信頼区間を計算
conf_intervals_Deer <- confint(Deer_model)
# 不必要な行を除外
conf_intervals_Deer <- conf_intervals_Deer[!rownames(conf_intervals_Deer) %in% c(".sig01", ".sigma"), ]

# 推定値を取得
estimates <- summary(Deer_model)$coefficients

# データフレームに変換
results <- data.frame(
  term = rownames(estimates),
  estimate = estimates[, "Estimate"],
  lwr = conf_intervals_Deer[, 1],
  upr = conf_intervals_Deer[, 2]
)

# NAの行を削除
results <- na.omit(results)

# 推定値と信頼区間のプロット
# termを因子型にして逆順に設定
results$term <- factor(results$term, levels = rev(unique(results$term)))

ggplot(results, aes(x = estimate, y = term)) +
  geom_point(size = 3) +  # 推定値の点
  scale_y_discrete() +
  geom_errorbar(aes(xmin = lwr, xmax = upr), width = 0.2) +  # 信頼区間
  labs(title = "Estimated Coefficients with Confidence Intervals",
       x = "Predictor Variables",
       y = "Estimated Values") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  coord_cartesian(xlim = c(-1, 1)) +
  theme_classic()



###
#相関の図示
# 数値データのみを抽出
numeric_data <- merged_data[sapply(merged_data, is.numeric)]

# 相関行列を計算
cor_matrix <- cor(numeric_data, use = "complete.obs")
print(cor_matrix)


library(corrplot)

# 相関行列のプロット
corrplot(cor_matrix, method = "circle")



#相関の図示
# 数値データのみを抽出
deer_data_night$dif <- deer_data_night$SD - deer_data_night$FID
numeric_data_deer <- deer_data_night[sapply(deer_data_night, is.numeric)]

# 相関行列を計算
cor_deer <- cor(numeric_data_deer, use = "complete.obs")
print(cor_deer)



library(ggplot2)
ggplot(merged_data, aes(x = as.factor(cues), y = FID)) +
  geom_boxplot() +
  labs(title = "Comparison of FID by Cues",
       x = "Cues",
       y = "FID") +
  theme_bw(base_size = 18)



###
#説明変数の分布をプロット
ggplot(data = Deer, aes(x=cues, y=FID)) +
  geom_boxplot()

ggplot(data = Deer, aes(x=cues, y=light)) +
  geom_boxplot(outliers = FALSE) +
  geom_jitter()

ggplot(data = Deer, aes(x = cues, y = flock)) +
  geom_boxplot(outliers = FALSE) +
  geom_jitter()

ggplot(data = Deer, aes(x = cues, y = noise)) +
  geom_boxplot(outliers = FALSE) +
  geom_jitter()

ggplot(data = Deer, aes(x = cues, y = time_num)) +
  geom_boxplot(outliers = FALSE) +
  geom_jitter()

ggplot(data = Deer, aes(x = cues, y = day_count)) +
  geom_boxplot(outliers = FALSE) +
  geom_jitter()

ggplot(data = Deer, aes(x = cues, y = SD)) +
  geom_boxplot(outliers = FALSE) +
  geom_jitter()

ggplot(data = Deer, aes(x = cues, y = site_number)) +
  geom_boxplot(outliers = FALSE) +
  geom_jitter()
