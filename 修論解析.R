#####
#音量減衰の確認
Soundlevel <- read.csv("c:/Users/kouch/OneDrive/デスクトップ/研究室関連/修士研究/データ/dbalv.csv")
print(head(Soundlevel))

library(ggplot2)
library(tidyr)

long_data <- Soundlevel %>%
  pivot_longer(
    cols = c(d1609, d1616, d1648, d1636, d1628, h1403, h1604, h1803, h201001, h201604, whitenoise, no_sounds),
    names_to = "variable",
    values_to = "value"
  )

# Plot the data
ggplot(long_data, aes(x = dist, y = value, color = variable, group = variable)) +
  geom_line() +
  labs(title = "Sound Levels Over Distance", x = "Distance(m)", y = "Sound Level(dBA)") +
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
      format(day, "%m") %in% c("08", "09", "12") ~ "NonBreeding",
      format(day, "%m") %in% c("10", "11") ~ "Breeding"
    )
  )

print(head(merged_data))





###
#種ごとに分割
Deer <- subset(merged_data, species == "Deer")


###
#行動圏にあわせて番号を設定

library(geosphere) 

# サイト番号列
Deer$site_number_core <- NA
Deer$site_number_home <- NA

library(sf)
# Deerをsfオブジェクトに変換
Deer_sf <- st_as_sf(Deer, coords = c("longitude", "latitude"), crs = 4326)
Deer_utm <- st_transform(Deer_sf, crs = 32654)

# Deerデータのバウンディングボックスを取得
bbox <- st_bbox(Deer_utm)

# 5000m x 5000mのグリッドを作成
grid <- st_make_grid(
  st_as_sfc(bbox), 
  cellsize = c(5000, 5000), 
  what = "polygons"
) %>% st_as_sf()

# グリッドにIDを付与
grid <- grid %>% mutate(grid_id = row_number())

# ポイントをグリッドに割り当て
Deer_with_grid <- st_join(Deer_utm, grid)

unique_sites <- unique(na.omit(Deer_with_grid$grid_id))
Deer_with_grid$grid_id <- match(Deer_with_grid$grid_id, unique_sites)

# グリッドIDを site_number_home に割り当て
Deer <- Deer %>% mutate(site_number_home = Deer_with_grid$grid_id)
Deer_with_grid <- Deer_with_grid %>% mutate(site_number_home = Deer_with_grid$grid_id)

# 結果の確認
print(Deer)

# プロット
ggplot() +
  geom_sf(data = grid, fill = NA, color = "gray") + # グリッド
  geom_sf(data = Deer_with_grid, aes(color = as.factor(site_number_home)), size = 3) + # ポイント
  labs(color = "Site Number", title = "5000x5000m Grid Clustering") +
  theme_minimal()



###
#GLMM
library(lme4)
library(Matrix)
library(ggplot2)

Deer <- subset(Deer, FID <= 150)


#シカ
# lmer()でGLMMを構築
#現時点ではcoreのランダム効果がない方が説明しやすい結果
Deer_model <- lmer(
  FID ~ cues + log(light + 1) + noise + SD  + flock + AvgWind + season +
    (1 | site_number_home),
  data = Deer
)

# 結果の確認
summary(Deer_model)

library(performance)

# 決定係数の確認
model_r2 <- r2_nakagawa(Deer_model)

print(model_r2)

# AICの確認
AIC(Deer_model)
null_model <- lmer(FID ~ (1 | site_number_home),
                   data = Deer)
AIC(null_model)


library(car)
vif(lm(FID ~ cues + log(light + 1) + flock + noise + SD + AvgWind + season, data = Deer))

#全変数の相関を確認
vif(lm(FID ~ cues + weather + cloud + flock + AvgWind + MaxWind + noise + log(light + 1) + moon + season, data = Deer))

# emmeans
library(emmeans)

# 多重比較
emmeans_FID <- emmeans(Deer_model, pairwise ~ cues)
summary(emmeans_FID)
plot(emmeans_FID)
pwpp(emmeans_FID, sort = FALSE)

library(multcompView)
library(multcomp)
cld_result_FID <- cld(emmeans_FID)
print(cld_result_FID)

library(dplyr)
# グループ名を数字からアルファベットに置き換え
cld_result_FID <- cld_result_FID %>%
  mutate(.group = case_when(
    .group == " 1 " ~ "a",      
    .group == "  2" ~ "b",       
    .group == " 12" ~ "ab"
  ))
print(cld_result_FID)

# 推定値の大きい順に並べ替える
cld_result_FID <- cld_result_FID[order(-cld_result_FID$emmean), ]
cld_result_FID$cues <- factor(cld_result_FID$cues, levels = cld_result_FID$cues)

library(ggplot2)
# プロット作成
ggplot(cld_result_FID, aes(x = cues, y = emmean)) +
  geom_point(size = 2) +                                # 平均値の点
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.2) + # エラーバー
  geom_text(aes(label = .group), hjust = -0.5, size = 5) +  # グループラベルを追加
  labs(
    x = "Cues", 
    y = "Estimated Means", 
    title = "Estimated FID Means"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # x軸ラベルを45度傾ける



#AD
Deer_model_AD <- lmer(
  AD ~ cues + log(light + 1) +  flock + noise + SD + AvgWind + season +
    (1 | site_number_home),
  data = Deer
)

# 結果の確認
summary(Deer_model_AD)

# 決定係数の確認
model_AD_r2 <- r2_nakagawa(Deer_model_AD)

print(model_AD_r2)

# AICの確認
AIC(Deer_model_AD)
null_model_AD <- lmer(AD ~ (1 | site_number_home),
                   data = Deer)
AIC(null_model_AD)

# emmeans
library(emmeans)

# 多重比較
emmeans_AD <- emmeans(Deer_model_AD, pairwise ~ cues)
summary(emmeans_AD)
plot(emmeans_AD)
pwpp(emmeans_AD, sort = FALSE)

library(multcompView)
library(multcomp)
cld_result_AD <- cld(emmeans_AD)
print(cld_result_AD)

# グループ名を数字からアルファベットに置き換え
cld_result_AD <- cld_result_AD %>%
  mutate(.group = case_when(
    .group == " 1 " ~ "a",      
    .group == "  2" ~ "b",       
    .group == " 12" ~ "ab"
  ))
print(cld_result_AD)

# 推定値の大きい順に並べ替える
cld_result_AD <- cld_result_AD[order(-cld_result_AD$emmean), ]
cld_result_AD$cues <- factor(cld_result_AD$cues, levels = cld_result_AD$cues)

library(ggplot2)
# プロット作成
ggplot(cld_result_AD, aes(x = cues, y = emmean)) +
  geom_point(size = 2) +                                # 平均値の点
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.2) + # エラーバー
  geom_text(aes(label = .group), hjust = -0.5, size = 5) +  # グループラベルを追加
  labs(
    x = "Cues", 
    y = "Estimated Means", 
    title = "Estimated AD Means"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # x軸ラベルを45度傾ける






;###
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

ggplot(data = Deer, aes(x = season, y = FID)) +
  geom_boxplot(outliers = FALSE) +
  geom_jitter()


#相関をプロット
ggplot(data = Deer, aes(x = light, y = FID)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()

ggplot(data = Deer, aes(x = noise, y = FID)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()

ggplot(data = Deer, aes(x = flock, y = FID)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()

ggplot(data = Deer, aes(x = AvgWind, y = FID)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()


library(sf) # 地理データの操作用
library(maps)

# 日本地図の取得
japan_map <- map_data("world", region = "Japan")

# 日本地図のバウンディングボックス作成
bbox <- st_bbox(c(xmin = 141.7, ymin = 42.5, xmax = 142.0, ymax = 42.8), crs = st_crs(4326))

# バウンディングボックスを sf オブジェクトに変換
bbox_sf <- st_as_sfc(bbox)

# 10km² のグリッド作成
grid <- st_make_grid(
  bbox_sf,
  cellsize = c(0.032, 0.044),  # 緯度・経度での10kmに相当する値（約0.1度）
  what = "polygons"
) %>%
  st_as_sf()  # sf オブジェクトに変換


# プロット
ggplot() +
  # 背景地図
  geom_polygon(
    data = japan_map,
    aes(x = long, y = lat, group = group),
    fill = "lightgray",
    color = "black"
  ) +
  # グリッド
  geom_sf(
    data = grid,
    fill = NA,
    color = "red",
    linetype = "dotted"
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
    aes(x = longitude, y = latitude, label = site_number_home),
    hjust = -0.2, vjust = -0.2, color = "blue"
  ) +
  # テーマとタイトル
  theme_minimal() +
  labs(
    title = "Site Number Distribution with 10km² Grid",
    x = "Longitude",
    y = "Latitude"
  )

###
#相関の図示
# 数値データのみを抽出
numeric_data <- Deer[sapply(Deer, is.numeric)]

# 相関行列を計算
cor_matrix <- cor(numeric_data, use = "complete.obs")
print(cor_matrix)


library(corrplot)

# 相関行列のプロット
corrplot(cor_matrix, method = "circle")

# 必要なパッケージをロード
library(dplyr)
library(ggcorrplot)

# 必要なパッケージをロード
library(dplyr)
library(ggcorrplot)

# データの確認
str(Deer)

# カテゴリ変数のOne-hotエンコーディング
cues_dummy <- model.matrix(~ cues - 1, data = Deer)
weather_dummy <- model.matrix(~ weather - 1, data = Deer)
day_or_night_dummy <- model.matrix(~ day_or_night - 1, data = Deer)
season_dummy <- model.matrix(~ season - 1, data = Deer)

# 対象列の選択と結合
continuous_columns <- Deer %>% 
  select(cloud, flock, FID, AD, SD, AvgWind, MaxWind, noise, light, moon)

# 連続変数とエンコードされたカテゴリ変数を結合
combined_data <- cbind(continuous_columns, cues_dummy, weather_dummy, day_or_night_dummy, season_dummy)

# 相関行列の計算
correlation_matrix <- cor(combined_data, use = "pairwise.complete.obs")

# 相関行列の可視化
ggcorrplot(correlation_matrix, lab = TRUE, lab_size = 3, title = "Correlation Matrix")

# 必要なパッケージを読み込み
library(sf)
save_dir <- "c:/Users/kouch/OneDrive/デスクトップ/研究室関連/修士研究/データ" # フォルダパスを設定

# 元のデータフレームをコピーして新しいオブジェクトに
Deer_sf <- Deer

# FIDという列が存在するか確認
if ("FID" %in% names(Deer_sf)) {
  names(Deer_sf)[names(Deer_sf) == "FID"] <- "original_FID"
}

Deer_sf <- st_as_sf(Deer_sf, coords = c("longitude", "latitude"), crs = 4326)

# 既存のGeoPackageファイルを削除
if (file.exists("Deer.gpkg")) {
  unlink("Deer.gpkg")
}

save_path <- file.path(save_dir, "Deer.gpkg")

# GeoPackage形式で保存
st_write(Deer_sf, save_path, layer = "Deer_layer", delete_dsn = TRUE)


###
# 予想結果図の作成

# データフレームを作成
# 推定値と信頼区間の値を手動で設定します
data <- data.frame(
  Pattern = c("Pattern3", "Pattern2", "Pattern1"),
  Estimate = c(80, 70, 60),
  LowerCI = c(76, 64, 54),
  UpperCI = c(84, 78, 68),
  Group = c("b", "ab", "a")
)

# ggplot2ライブラリを読み込み
library(ggplot2)

# プロットを作成
ggplot(data, aes(y = Estimate, x = Pattern, ymin = LowerCI, ymax = UpperCI)) +
  geom_pointrange(size = 1) + # 推定値と信頼区間を描画
  geom_text(aes(label = Group), hjust = -1, size = 5) + # グループ記号を推定値の真下に表示
  theme_bw(base_size = 16) +
  labs(
    title = "Predicting results",
    y = "Estimate",
    x = NULL
  ) +
  theme(
    legend.position = "none" 
  )
