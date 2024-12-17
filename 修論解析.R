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

#時間データをGLMMで使えるよう小数にする#時間データをGLMMで使えるよう小数にする
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

#季節を追加
merged_data <- merged_data %>%
  mutate(
    season = case_when(
      format(day, "%m") %in% c("08", "09") ~ "Summer",
      format(day, "%m") %in% c("10", "11") ~ "Fall",
      format(day, "%m") %in% c ("12") ~ "Winter"
    )
  )

print(head(merged_data))





###
#種ごとに分割
Deer <- subset(merged_data, species == "Deer")


###
#行動圏にあわせて番号を設定
#冬半径1800m、夏半径1100m以内の座標に番号を付与
#コアエリアは冬700m、夏400mでよさそう
#Laneng et al. 2023を参照

library(geosphere) # distHaversine関数のため

# サイト番号列を初期化
# 季節ごとに適用
Deer$site_number_Winter <- NA
Deer$site_number_nonWinter <- NA

# 最大半径（700m）でクラスタリング
for (i in 1:nrow(Deer)) {
  # 現在の座標
  Deer_coords <- c(Deer$longitude[i], Deer$latitude[i])
  
  # 距離計算（700m以内）
  Deer_distances <- distHaversine(Deer_coords, cbind(Deer$longitude, Deer$latitude))
  Deer_within_radius <- which(Deer_distances <= 700)
  
  # サイト番号を割り当て
  if (is.na(Deer$site_number_Winter[i])) {
    Deer$site_number_Winter[Deer_within_radius] <- i
  }
}

# 最大半径（400m）でクラスタリング
for (i in 1:nrow(Deer)) {
  # 現在の座標
  Deer_coords <- c(Deer$longitude[i], Deer$latitude[i])
  
  # 距離計算（400m以内）
  Deer_distances <- distHaversine(Deer_coords, cbind(Deer$longitude, Deer$latitude))
  Deer_within_radius <- which(Deer_distances <= 400)
  
  # サイト番号を割り当て
  if (is.na(Deer$site_number_nonWinter[i])) {
    Deer$site_number_nonWinter[Deer_within_radius] <- i
  }
}

# 重複を解消して連続した番号にする
unique_sites <- unique(na.omit(Deer$site_number_Winter))
Deer$site_number_Winter <- match(Deer$site_number_Winter, unique_sites)

unique_sites <- unique(na.omit(Deer$site_number_nonWinter))
Deer$site_number_nonWinter <- match(Deer$site_number_nonWinter, unique_sites)

ggplot(Deer, aes(x = site_number_Winter, fill = cues))+
  geom_bar(stat = "count")

ggplot(Deer, aes(x = site_number_nonWinter, fill = cues))+
  geom_bar(stat = "count")

###
#GLMM
library(lme4)
library(Matrix)
library(ggplot2)

#シカ
# lmer()でGLMMを構築
Deer_model <- lmer(
  FID ~ cues + log(light + 1) + noise + SD  + flock + MaxWind + season +
    (1 | site_number_nonWinter:season) + (1 | site_number_Winter:season),
  data = Deer
)

# 結果の確認
summary(Deer_model)

library(car)
vif(lm(FID ~ cues + log(light + 1) + flock + noise + SD + MaxWind + season, data = Deer))


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
  labs(title = "Flight Initiation Distance without outliers",
       x = "Predictor Variables",
       y = "Estimated Values") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  coord_cartesian(xlim = c(-1, 1)) +
  theme_classic()


#AD
Deer_model_AD <- lmer(
  AD ~ cues + log(light + 1) +  flock + noise + SD + MaxWind + season +
    (1 | site_number_nonWinter:season) + (1 | site_number_Winter:season),
  data = Deer
)

# 結果の確認
summary(Deer_model_AD)

# 推定値の信頼区間を計算
conf_intervals_Deer_AD <- confint(Deer_model_AD)
# 不必要な行を除外
conf_intervals_Deer_AD <- conf_intervals_Deer_AD[!rownames(conf_intervals_Deer_AD) %in% c(".sig01", ".sigma"), ]

# 推定値を取得
estimates_AD <- summary(Deer_model_AD)$coefficients

# データフレームに変換
results_AD <- data.frame(
  term = rownames(estimates_AD),
  estimate_AD = estimates_AD[, "Estimate"],
  lwr = conf_intervals_Deer_AD[, 1],
  upr = conf_intervals_Deer_AD[, 2]
)

# NAの行を削除
results_AD <- na.omit(results_AD)

# 推定値と信頼区間のプロット
# termを因子型にして逆順に設定
results_AD$term <- factor(results_AD$term, levels = rev(unique(results_AD$term)))

ggplot(results_AD, aes(x = estimate_AD, y = term)) +
  geom_point(size = 3) +  # 推定値の点
  scale_y_discrete() +
  geom_errorbar(aes(xmin = lwr, xmax = upr), width = 0.2) +  # 信頼区間
  labs(title = "Alart Distance",
       x = "Predictor Variables",
       y = "Estimated Values") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  coord_cartesian(xlim = c(-1, 1)) +
  theme_classic()





###
#説明変数の分布をプロット
ggplot(data = Deer, aes(x = cues, fill = day_or_night)) +
  geom_bar(stat = "count")

table(Deer$day_or_night, by = Deer$cues)

ggplot(data = Deer, aes(x=cues, y=FID)) +
  geom_boxplot()

ggplot(data = Deer, aes(x=cues, y=AD)) +
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

ggplot(data = Deer, aes(x = cues, y = time)) +
  geom_boxplot(outliers = FALSE) +
  geom_jitter()

ggplot(data = Deer, aes(x = cues, y = day_count)) +
  geom_boxplot(outliers = FALSE) +
  geom_jitter()

ggplot(data = Deer, aes(x = cues, y = SD)) +
  geom_boxplot(outliers = TRUE)

ggplot(data = Deer, aes(x = cues, y = site_number)) +
  geom_boxplot(outliers = FALSE) +
  geom_jitter()


library(sf) # 地理データの操作用
library(maps)

# 日本地図の取得
japan_map <- map_data("world", region = "Japan")

ggplot() +
  # 背景地図
  geom_polygon(
    data = japan_map,
    aes(x = long, y = lat, group = group),
    fill = "lightgray",
    color = "black"
  ) +
  # site_number のプロット
  geom_point(
    data = Deer,
    aes(x = longitude, y = latitude),
    color = "black"
  ) +
  # ラベルの追加
  geom_text(
    data = Deer,
    aes(x = longitude, y = latitude, label = seasonal_site_number),
    hjust = -0.2, vjust = -0.2, color = "blue"
  ) +
  # 表示範囲の指定
  coord_cartesian(
    xlim = c(141.7, 142),
    ylim = c(42.5, 42.8)
  ) +
  # テーマとタイトル
  theme_minimal() +
  labs(
    title = "Site Number Distribution",
    x = "Longitude",
    y = "Latitude"
  )
