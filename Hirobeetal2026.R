# 音量減衰の確認 ####
Soundlevel <- read.csv("https://raw.githubusercontent.com/KotaHirobe/Hirobe_et_al_2026/refs/heads/main/Hirobeetal2026AcousticAttenuation.csv")

print(head(Soundlevel))

library(ggplot2)
library(tidyr)

long_data <- Soundlevel %>%
  pivot_longer(
    cols = c(d1609, d1616, d1648, d1636, d1628, h1403, h1604, h1803, h201001, h201604, whitenoise, no_sounds),
    names_to = "variable",
    values_to = "value"
  )

ggplot(long_data, aes(x = dist, y = value, color = variable, group = variable)) +
  geom_line() +
  labs(title = "Sound Levels Over Distance", x = "Distance(m)", y = "Sound Level(dBA)") +
  theme_classic(base_size = 22)


# Loaing the data ####
merged_data <- read.csv("https://raw.githubusercontent.com/KotaHirobe/Hirobe_et_al_2026/refs/heads/main/Hirobeetal2026.csv")
# ローカルから読み込む用
merged_data <- read.csv("Hirobeetal2026.csv", colClasses = c(actime = "character"))

# NAを消す
merged_data <- na.omit(merged_data)

# lightを数値として読み込み
merged_data$light <- as.numeric(merged_data$light)

# 日付(数値)を日時のデータに変換
merged_data$sunrise <- as.POSIXct(paste(merged_data$day, merged_data$sunrise))
merged_data$sunset <- as.POSIXct(paste(merged_data$day, merged_data$sunset))
merged_data$time <- as.POSIXct(paste(merged_data$day, merged_data$time))

# 日付を経過日数に変換
min_date <- min(merged_data$day, na.rm = TRUE)
merged_data$day_count <- as.numeric(difftime(merged_data$day, min_date, units = "days")) +1
head(merged_data)

# 昼と夜を分ける
merged_data$day_or_night <- ifelse(merged_data$time >= merged_data$sunrise & merged_data$time <= merged_data$sunset,
                               "day",
                               ifelse(merged_data$time > merged_data$sunset | merged_data$time < merged_data$sunrise,
                                      "night", NA))

print(head(merged_data))
table(merged_data$day_or_night)

#時間データを小数にする
library(lubridate)
merged_data$time_num <- hour(merged_data$time) + minute(merged_data$time)/60




# site_numberの設定 ####
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
#同じ種で150m離れていないかつ7日以上日付が空いていないデータの片方を省く
library(dplyr)
merged_data$day <- as.Date(merged_data$day)
merged_data <- merged_data[order(merged_data$site_number, merged_data$number), ]

merged_data <- merged_data %>%
  arrange(site_number, species, day_count) %>% 
  group_by(site_number, species) %>%
  mutate(diff_days = day_count - lag(day_count, default = first(day_count))) %>%
  filter(!(diff_days < 7 & row_number() == 2)) %>%
  dplyr::select(-diff_days) %>%
  ungroup()

head(merged_data)
table(merged_data$species)

#季節を追加
merged_data <- merged_data %>%
  mutate(
    season = case_when(
      format(day, "%m") %in% c("08", "09", "12", "05") ~ "NonMating",
      format(day, "%m") %in% c("10", "11") ~ "Mating"
    )
  )

print(head(merged_data))




#行動圏にあわせて番号を設定
library(geosphere) 

# サイト番号列
merged_data$site_number_core <- NA
merged_data$site_number_home <- NA

library(sf)
# Deerをsfオブジェクトに変換
Deer_sf <- st_as_sf(merged_data, coords = c("longitude", "latitude"), crs = 4326)
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
merged_data <- merged_data %>% mutate(site_number_home = Deer_with_grid$grid_id)
Deer_with_grid <- Deer_with_grid %>% mutate(site_number_home = Deer_with_grid$grid_id)

# 結果の確認
print(merged_data)

# プロット
library(ggplot2)
ggplot() +
  geom_sf(data = grid, fill = NA, color = "gray") + # グリッド
  geom_sf(data = Deer_with_grid, aes(color = as.factor(site_number_home)), size = 3) + # ポイント
  labs(color = "Location ID", title = NULL) +
  theme_classic()

library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

# 日本の都道府県レベル（admin-1）を取得
jp_pref <- rnaturalearth::ne_states(country = "Japan", returnclass = "sf")

# 北海道だけ抽出（name が "Hokkaido" のはず）
hokkaido <- subset(jp_pref, name == "Hokkaidō")

# CRS をまず揃える（grid を基準に）
hokkaido2       <- st_transform(hokkaido, st_crs(grid))
Deer_with_grid2 <- st_transform(Deer_with_grid, st_crs(grid))

# bbox を「数値」で取り出す（ここがポイント）
bb <- st_bbox(grid)

ggplot() +
  geom_sf(data = hokkaido2, fill = "gray95", color = "gray40", linewidth = 0.4) +
  geom_sf(data = grid,      fill = NA,      color = "gray70", linewidth = 0.3) +
  geom_sf(data = Deer_with_grid2, aes(color = as.factor(site_number_home)), size = 3) +
  labs(color = "Location ID", title = NULL) +
  theme_classic() +
  coord_sf(
    xlim = c(as.numeric(bb["xmin"]), as.numeric(bb["xmax"])),
    ylim = c(as.numeric(bb["ymin"]), as.numeric(bb["ymax"])),
    expand = TRUE
  )  

Deer_with_grid3 <- subset(Deer_with_grid2, FID <= 150)
Deer_with_grid3 <- Deer_with_grid3 %>%
  mutate(grid_id = match(grid_id, sort(unique(grid_id))))

ggplot() +
  geom_sf(data = hokkaido2, fill = "gray95", color = "gray40", linewidth = 0.4) +
  geom_sf(data = grid,      fill = NA,      color = "gray70", linewidth = 0.3) +
  geom_sf(data = Deer_with_grid3, aes(color = as.factor(grid_id)), size = 3) +
  labs(color = "Location ID", title = NULL) +
  theme_classic() +
  coord_sf(
    xlim = c(as.numeric(bb["xmin"]), as.numeric(bb["xmax"])),
    ylim = c(as.numeric(bb["ymin"]), as.numeric(bb["ymax"])),
    expand = TRUE
  )  


# 相関係数
cor_vars <- Deer %>%
  dplyr::select(where(is.numeric))

cor_matrix <- cor(cor_vars, use = "complete.obs", method = "pearson")
print(cor_matrix)


# LMM ####
library(lme4)
library(Matrix)

Deer <- subset(merged_data, FID <= 150)
Deer$log_light <- log((Deer$light)+1)

# cuesを分解して個別の変数に
Deer <- Deer %>%
  mutate(
    human_visual = factor(ifelse(cues %in% c(
      "human_vi", "human_vi_ac", "human_vi_dog_ac", "human_vi_no",
      "human_vi_dog_vi", "human_vi_ac_dog_vi", "human_vi_dog_vi_ac",
      "human_vi_no_dog_vi", "human_vi_dog_vi_cover", "human_vi_dog_vi_ac_cover"
    ), 1, 0)),
    
    dog_visual = factor(ifelse(cues %in% c(
      "human_vi_dog_vi", "human_vi_ac_dog_vi",
      "human_vi_dog_vi_ac", "human_vi_no_dog_vi"
    ), 1, 0)),
    
    blinddog_visual = factor(ifelse(cues %in% c(
      "human_vi_dog_vi_cover", "human_vi_dog_vi_ac_cover"
    ), 1, 0)),
    
    human_acoustic = factor(ifelse(cues %in% c(
      "human_vi_ac", "human_vi_ac_dog_vi"
    ), 1, 0)),
    
    dog_acoustic = factor(ifelse(cues %in% c(
      "human_vi_dog_vi_ac", "human_vi_dog_ac", "human_vi_dog_vi_ac_cover"
    ), 1, 0)),
    
    noise_acoustic = factor(ifelse(cues %in% c(
      "human_vi_no", "human_vi_no_dog_vi"
    ), 1, 0)),
    
    no_acoustic = factor(ifelse(cues %in% c(
      "human_vi", "human_vi_dog_vi", "human_vi_dog_vi_cover"
    ), 1, 0))
  )


# 予備解析：プレイバックの有音時間の違いによるFIDの変化がないか検証
deer_prep <- Deer[Deer$human_acoustic == 1,]
deer_prep$actime <- as.numeric(deer_prep$actime)

prep_deer_model <- lm(
  FID ~ log_light + SD + flock + AvgWind + season + actime,
  data = deer_prep
)
# 結果の確認
summary(prep_deer_model)

prep_AD <- lm(
  AD ~ log_light + SD + flock + AvgWind + season + actime,
  data = deer_prep
)
# 結果の確認
summary(prep_AD)

library(dplyr)

Deer <- Deer %>%
  mutate(
    dog_visual      = factor(dog_visual,      levels = c(0, 1)),
    blinddog_visual = factor(blinddog_visual, levels = c(0, 1)),
    human_acoustic  = factor(human_acoustic,  levels = c(0, 1)),
    dog_acoustic    = factor(dog_acoustic,    levels = c(0, 1)),
    noise_acoustic  = factor(noise_acoustic,  levels = c(0, 1))
  )

Deer$season <- factor(Deer$season)  


# lmer()でLMMを構築
Deer_model <- lmer(
  FID ~ 
    dog_visual + 
    blinddog_visual + 
    human_acoustic + 
    dog_acoustic + 
    dog_visual:human_acoustic + 
    dog_visual:dog_acoustic +
    noise_acoustic + 
    log_light +
    SD + 
    flock +
    AvgWind + 
    season + 
    (1 | site_number_home),
  data = Deer
)

# 結果の確認
summary(Deer_model)
signif(summary(Deer_model)$coefficients, 3)

library(lmerTest)

Deer_model_p <- lmer(
  FID ~ dog_visual + blinddog_visual + human_acoustic + dog_acoustic +
    dog_visual:human_acoustic + dog_visual:dog_acoustic + noise_acoustic +
    log_light + SD + flock + AvgWind + season + (1 | site_number_home),
  data = Deer
)

summary(Deer_model_p)

library(performance)
# 決定係数の確認
model_r2 <- r2_nakagawa(Deer_model)
print(model_r2)

# 相関も確認する
library(car)
vif(lm(FID ~ 
         dog_visual + 
         blinddog_visual + 
         human_acoustic + 
         dog_acoustic + 
         dog_visual:human_acoustic + 
         dog_visual:dog_acoustic +
         noise_acoustic + 
         log_light +
         SD + 
         flock +
         AvgWind + 
         season,
       data = Deer))


# 信頼区間を計算
conf_intervals_Deer <- confint(Deer_model)
print(conf_intervals_Deer)

conf_intervals_Deer <- conf_intervals_Deer[!rownames(conf_intervals_Deer) %in% c(".sig01", ".sig02",  ".sigma"), ]
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
print(results)
signif(results$lwr, 3)
signif(results$upr, 3)


# FIDの推定値出す
library(emmeans)

emmeans_FID <- emmeans(
  Deer_model,
  ~ dog_visual * blinddog_visual * human_acoustic * dog_acoustic * noise_acoustic)

plot(emmeans_FID)
emm_df <- as.data.frame(emmeans_FID)


sub_8 <- subset(
  emm_df,
  (dog_visual == "0" & blinddog_visual == "0" & human_acoustic == "0" &
     dog_acoustic == "0" & noise_acoustic == "0") |
    # 2) dog_visual1: dog_visual="1"のみ
    (dog_visual == "1" & blinddog_visual == "0" & human_acoustic == "0" &
       dog_acoustic == "0" & noise_acoustic == "0") |
    # 3) blinddog_visual1
    (dog_visual == "0" & blinddog_visual == "1" & human_acoustic == "0" &
       dog_acoustic == "0" & noise_acoustic == "0") |
    # 4) human_acoustic1
    (dog_visual == "0" & blinddog_visual == "0" & human_acoustic == "1" &
       dog_acoustic == "0" & noise_acoustic == "0") |
    # 5) dog_acoustic1
    (dog_visual == "0" & blinddog_visual == "0" & human_acoustic == "0" &
       dog_acoustic == "1" & noise_acoustic == "0") |
    # 6) noise_acoustic1
    (dog_visual == "0" & blinddog_visual == "0" & human_acoustic == "0" &
       dog_acoustic == "0" & noise_acoustic == "1") |
    # 7) dog_visual1:human_acoustic1
    (dog_visual == "1" & blinddog_visual == "0" & human_acoustic == "1" &
       dog_acoustic == "0" & noise_acoustic == "0") |
    # 8) dog_visual1:dog_acoustic1
    (dog_visual == "1" & blinddog_visual == "0" & human_acoustic == "0" &
       dog_acoustic == "1" & noise_acoustic == "0") )

library(dplyr)

sub_8 <- sub_8 %>%
  mutate(
    scenario = case_when(
      dog_visual == "0" & blinddog_visual == "0" & human_acoustic == "0" &
        dog_acoustic == "0" & noise_acoustic == "0" ~ "Intercept",
      dog_visual == "1" & blinddog_visual == "0" & human_acoustic == "0" &
        dog_acoustic == "0" & noise_acoustic == "0" ~ "dog_visual1",
      blinddog_visual == "1" & dog_visual == "0" & human_acoustic == "0" &
        dog_acoustic == "0" & noise_acoustic == "0" ~ "blinddog_visual1",
      human_acoustic == "1" & dog_visual == "0" & blinddog_visual == "0" &
        dog_acoustic == "0" & noise_acoustic == "0" ~ "human_acoustic1",
      dog_acoustic == "1" & dog_visual == "0" & blinddog_visual == "0" &
        human_acoustic == "0" & noise_acoustic == "0" ~ "dog_acoustic1",
      noise_acoustic == "1" & dog_visual == "0" & blinddog_visual == "0" &
        human_acoustic == "0" & dog_acoustic == "0" ~ "noise_acoustic1",
      dog_visual == "1" & human_acoustic == "1" &
        blinddog_visual == "0" & dog_acoustic == "0" & noise_acoustic == "0" ~
        "dog_visual1:human_acoustic1",
      dog_visual == "1" & dog_acoustic == "1" &
        blinddog_visual == "0" & human_acoustic == "0" & noise_acoustic == "0" ~
        "dog_visual1:dog_acoustic1"    ),
    scenario = factor(
      scenario,
      levels = c("Intercept",
                 "dog_visual1",
                 "blinddog_visual1",
                 "human_acoustic1",
                 "dog_acoustic1",
                 "noise_acoustic1",
                 "dog_visual1:human_acoustic1",
                 "dog_visual1:dog_acoustic1")
    )
  )

print(sub_8)

sub_8 <- sub_8 %>%
  mutate(
    group = case_when(
      scenario %in% c("blinddog_visual1", "noise_acoustic1") ~ "Control",
      scenario == "human_acoustic1"                   ~ "Human",
      scenario %in% c("dog_visual1",
                      "dog_acoustic1")                ~ "Dog",
      scenario %in% c("dog_visual1:human_acoustic1",
                      "dog_visual1:dog_acoustic1"
                      )    ~ "Interaction",
      scenario == "Intercept" ~ "Baseline" 
    ),
    group = factor(group, levels = c("Baseline", "Human", "Dog", "Interaction", "Control")),
    
    # x軸ラベル用
    scenario_lab = case_when(
      scenario == "Intercept"                    ~ "Baseline (Approaching surveyor only)",
      scenario == "dog_visual1"                  ~ "Dog decoy",
      scenario == "blinddog_visual1"             ~ "Covered dog decoy",
      scenario == "human_acoustic1"              ~ "Human voice",
      scenario == "dog_acoustic1"                ~ "Dog barking",
      scenario == "noise_acoustic1"              ~ "White noise",
      scenario == "dog_visual1:human_acoustic1"  ~ "Dog decoy + Human voice",
      scenario == "dog_visual1:dog_acoustic1"    ~ "Dog decoy + Dog barking"
    ),
    scenario_lab = factor(
      scenario_lab,
      levels = c(
        "Baseline (Approaching surveyor only)",
        "Human voice",
        "Dog decoy",
        "Dog barking",
        "Dog decoy + Human voice",
        "Dog decoy + Dog barking",
        "Covered dog decoy",
        "White noise"
      )
    )
  )


library(ggplot2)

ggplot(sub_8, aes(x = scenario_lab, y = emmean)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2, linewidth = 1) +
  xlab(NULL) +
  ylab("Predicted FID (m)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(t = 5, r = 5, b = 5, l = 50)) 




# AD ####
Deer_model_AD <- lmer(
  AD ~ dog_visual + 
    blinddog_visual + 
    human_acoustic + 
    dog_acoustic + 
    dog_visual:human_acoustic + 
    dog_visual:dog_acoustic +
    noise_acoustic + 
    log_light +
    SD + 
    flock +
    AvgWind + 
    season +  
    (1 | site_number_home),
  data = Deer
)
# 結果の確認
summary(Deer_model_AD)

library(lmerTest)

Deer_model_AD_p <- lmer(
  AD ~ dog_visual + blinddog_visual + human_acoustic + dog_acoustic +
    dog_visual:human_acoustic + dog_visual:dog_acoustic + noise_acoustic +
    log_light + SD + flock + AvgWind + season + (1 | site_number_home),
  data = Deer
)

summary(Deer_model_AD_p)

library(performance)
# 決定係数の確認
model_r2_AD <- r2_nakagawa(Deer_model_AD)
print(model_r2_AD)

# 信頼区間を計算 
conf_intervals_Deer_AD <- confint(Deer_model_AD)
print(conf_intervals_Deer_AD)

conf_intervals_Deer_AD <- conf_intervals_Deer_AD[!rownames(conf_intervals_Deer_AD) %in% c(".sig01", ".sig02",  ".sigma"), ]
estimates_AD <- summary(Deer_model_AD)$coefficients

# データフレームに変換
results_AD <- data.frame(
  term = rownames(estimates_AD),
  estimate = estimates_AD[, "Estimate"],
  lwr = conf_intervals_Deer_AD[, 1],
  upr = conf_intervals_Deer_AD[, 2]
)
# NAの行を削除
results_AD <- na.omit(results_AD)
print(results_AD)

# ADの推定値出す
library(emmeans)

emmeans_AD <- emmeans(
  Deer_model_AD,
  ~ dog_visual * blinddog_visual * human_acoustic * dog_acoustic * noise_acoustic)

plot(emmeans_AD)
emm_df_AD <- as.data.frame(emmeans_AD)


sub_AD <- subset(
  emm_df_AD,
  (dog_visual == "0" & blinddog_visual == "0" & human_acoustic == "0" &
     dog_acoustic == "0" & noise_acoustic == "0") |
    # 2) dog_visual1: dog_visual="1"のみ
    (dog_visual == "1" & blinddog_visual == "0" & human_acoustic == "0" &
       dog_acoustic == "0" & noise_acoustic == "0") |
    # 3) blinddog_visual1
    (dog_visual == "0" & blinddog_visual == "1" & human_acoustic == "0" &
       dog_acoustic == "0" & noise_acoustic == "0") |
    # 4) human_acoustic1
    (dog_visual == "0" & blinddog_visual == "0" & human_acoustic == "1" &
       dog_acoustic == "0" & noise_acoustic == "0") |
    # 5) dog_acoustic1
    (dog_visual == "0" & blinddog_visual == "0" & human_acoustic == "0" &
       dog_acoustic == "1" & noise_acoustic == "0") |
    # 6) noise_acoustic1
    (dog_visual == "0" & blinddog_visual == "0" & human_acoustic == "0" &
       dog_acoustic == "0" & noise_acoustic == "1") |
    # 7) dog_visual1:human_acoustic1
    (dog_visual == "1" & blinddog_visual == "0" & human_acoustic == "1" &
       dog_acoustic == "0" & noise_acoustic == "0") |
    # 8) dog_visual1:dog_acoustic1
    (dog_visual == "1" & blinddog_visual == "0" & human_acoustic == "0" &
       dog_acoustic == "1" & noise_acoustic == "0"))

library(dplyr)

sub_AD <- sub_AD %>%
  mutate(
    scenario = case_when(
      dog_visual == "0" & blinddog_visual == "0" & human_acoustic == "0" &
        dog_acoustic == "0" & noise_acoustic == "0" ~ "Intercept",
      dog_visual == "1" & blinddog_visual == "0" & human_acoustic == "0" &
        dog_acoustic == "0" & noise_acoustic == "0" ~ "dog_visual1",
      blinddog_visual == "1" & dog_visual == "0" & human_acoustic == "0" &
        dog_acoustic == "0" & noise_acoustic == "0" ~ "blinddog_visual1",
      human_acoustic == "1" & dog_visual == "0" & blinddog_visual == "0" &
        dog_acoustic == "0" & noise_acoustic == "0" ~ "human_acoustic1",
      dog_acoustic == "1" & dog_visual == "0" & blinddog_visual == "0" &
        human_acoustic == "0" & noise_acoustic == "0" ~ "dog_acoustic1",
      noise_acoustic == "1" & dog_visual == "0" & blinddog_visual == "0" &
        human_acoustic == "0" & dog_acoustic == "0" ~ "noise_acoustic1",
      dog_visual == "1" & human_acoustic == "1" &
        blinddog_visual == "0" & dog_acoustic == "0" & noise_acoustic == "0" ~
        "dog_visual1:human_acoustic1",
      dog_visual == "1" & dog_acoustic == "1" &
        blinddog_visual == "0" & human_acoustic == "0" & noise_acoustic == "0" ~
        "dog_visual1:dog_acoustic1"    ),
    scenario = factor(
      scenario,
      levels = c("Intercept",
                 "dog_visual1",
                 "blinddog_visual1",
                 "human_acoustic1",
                 "dog_acoustic1",
                 "noise_acoustic1",
                 "dog_visual1:human_acoustic1",
                 "dog_visual1:dog_acoustic1")
    )
  )

print(sub_AD)

sub_AD <- sub_AD %>%
  mutate(
    group = case_when(
      scenario %in% c("blinddog_visual1", "noise_acoustic1") ~ "Control",
      scenario == "human_acoustic1"                   ~ "Human",
      scenario %in% c("dog_visual1",
                      "dog_acoustic1")                ~ "Dog",
      scenario %in% c("dog_visual1:human_acoustic1",
                      "dog_visual1:dog_acoustic1")    ~ "Interaction",
      scenario == "Intercept" ~ "Baseline" 
    ),
    group = factor(group, levels = c("Baseline", "Human", "Dog", "Interaction", "Control")),
    
    # x軸ラベル用
    scenario_lab = case_when(
      scenario == "Intercept"                    ~ "Baseline (Approaching surveyor only)",
      scenario == "dog_visual1"                  ~ "Dog decoy",
      scenario == "blinddog_visual1"             ~ "Covered dog decoy",
      scenario == "human_acoustic1"              ~ "Human voice",
      scenario == "dog_acoustic1"                ~ "Dog barking",
      scenario == "noise_acoustic1"              ~ "White noise",
      scenario == "dog_visual1:human_acoustic1"  ~ "Dog decoy + Human voice",
      scenario == "dog_visual1:dog_acoustic1"    ~ "Dog decoy + Dog barking"
    ),
    scenario_lab = factor(
      scenario_lab,
      levels = c(
        "Baseline (Approaching surveyor only)",
        "Human voice",
        "Dog decoy",
        "Dog barking",
        "Dog decoy + Human voice",
        "Dog decoy + Dog barking",
        "Covered dog decoy",
        "White noise"
      )
    )
  )


library(ggplot2)

ggplot(sub_AD, aes(x = scenario_lab, y = emmean)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2, linewidth = 1) +
  xlab(NULL) +
  ylab("Predicted AD (m)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(t = 5, r = 5, b = 5, l = 50)) 



# FID単一キュー
Deer_unimodal <- subset(Deer, dog_visual == 0 & blinddog_visual == 0 &
                        human_acoustic == 0 & dog_acoustic == 0 & noise_acoustic == 0)
View(Deer_unimodal)
# lmer()でLMMを構築
Deer_model_unimodal <- lm(
  FID ~ 
    log_light +
    SD + 
    flock +
    AvgWind + 
    season #+ 
    #(1 | site_number_home),
  ,data = Deer_unimodal
)

# 結果の確認
summary(Deer_model_unimodal)
signif(summary(Deer_model_unimodal)$coefficients, 3)

library(performance)
# 決定係数の確認
model_r2 <- r2_nakagawa(Deer_model_unimodal)
print(model_r2)


# multimodal
Deer_multimodal <- subset(Deer, dog_visual == 0 & blinddog_visual == 0 &
                          dog_acoustic == 0 & noise_acoustic == 0)
View(Deer_multimodal)
# lmer()でLMMを構築
Deer_model_multimodal <- lm(
  FID ~ 
    human_acoustic +
    log_light +
    SD + 
    flock +
    AvgWind + 
    season #+ 
    #(1 | site_number_home),
  ,data = Deer_multimodal
)

# 結果の確認
summary(Deer_model_multimodal)
signif(summary(Deer_model_unimodal)$coefficients, 3)

library(performance)
# 決定係数の確認
model_r2 <- r2_nakagawa(Deer_model_unimodal)
print(model_r2)

# イヌ単一キュー
Deer_unimodal_dog <- subset(Deer, dog_visual == 1 & blinddog_visual == 0 &
                          human_acoustic == 0 & dog_acoustic == 0 & noise_acoustic == 0)
View(Deer_unimodal_dog)
# lmer()でLMMを構築
Deer_model_unimodal_dog <- lm(
  FID ~ 
    log_light +
    SD + 
    flock +
    AvgWind + 
    season 
    #(1 | site_number_home),
  ,data = Deer_unimodal_dog
)

# 結果の確認
summary(Deer_model_unimodal_dog)
signif(summary(Deer_model_unimodal_dog)$coefficients, 3)

library(performance)
# 決定係数の確認
model_r2 <- r2_nakagawa(Deer_model_unimodal)
print(model_r2)

# イヌ複数キュー
Deer_multimodal_dog <- subset(Deer, dog_visual == 1 & blinddog_visual == 0 &
                          human_acoustic == 0 & noise_acoustic == 0)
View(Deer_multimodal_dog)
# lmer()でLMMを構築
Deer_model_multimodal_dog <- lm(
  FID ~ 
    dog_acoustic +
    log_light +
    SD + 
    flock +
    AvgWind + 
    season,
    #(1 | site_number_home),
  data = Deer_multimodal_dog
)

# 結果の確認
summary(Deer_model_multimodal_dog)
signif(summary(Deer_model_multimodal_dog)$coefficients, 3)

library(performance)
# 決定係数の確認
model_r2 <- r2_nakagawa(Deer_model_unimodal)
print(model_r2)


# AD単一キュー
Deer_unimodal <- subset(Deer, dog_visual == 0 & blinddog_visual == 0 &
                          human_acoustic == 0 & dog_acoustic == 0 & noise_acoustic == 0)
View(Deer_unimodal)
# lmer()でLMMを構築
Deer_model_unimodal <- lm(
  AD ~ 
    log_light +
    SD + 
    flock +
    AvgWind + 
    season #+ 
  #(1 | site_number_home),
  ,data = Deer_unimodal
)

# 結果の確認
summary(Deer_model_unimodal)
signif(summary(Deer_model_unimodal)$coefficients, 3)

library(performance)
# 決定係数の確認
model_r2 <- r2_nakagawa(Deer_model_unimodal)
print(model_r2)


# multimodal
Deer_multimodal <- subset(Deer, dog_visual == 0 & blinddog_visual == 0 &
                            dog_acoustic == 0 & noise_acoustic == 0)
View(Deer_multimodal)
# lmer()でLMMを構築
Deer_model_multimodal <- lm(
  AD ~ 
    human_acoustic +
    log_light +
    SD + 
    flock +
    AvgWind + 
    season #+ 
  #(1 | site_number_home),
  ,data = Deer_multimodal
)

# 結果の確認
summary(Deer_model_multimodal)
signif(summary(Deer_model_unimodal)$coefficients, 3)

library(performance)
# 決定係数の確認
model_r2 <- r2_nakagawa(Deer_model_unimodal)
print(model_r2)

# イヌ単一キュー
Deer_unimodal_dog <- subset(Deer, dog_visual == 1 & blinddog_visual == 0 &
                              human_acoustic == 0 & dog_acoustic == 0 & noise_acoustic == 0)
View(Deer_unimodal_dog)
# lmer()でLMMを構築
Deer_model_unimodal_dog <- lm(
  AD ~ 
    log_light +
    SD + 
    flock +
    AvgWind + 
    season 
  #(1 | site_number_home),
  ,data = Deer_unimodal_dog
)

# 結果の確認
summary(Deer_model_unimodal_dog)
signif(summary(Deer_model_unimodal_dog)$coefficients, 3)

library(performance)
# 決定係数の確認
model_r2 <- r2_nakagawa(Deer_model_unimodal)
print(model_r2)

# イヌ複数キュー
Deer_multimodal_dog <- subset(Deer, dog_visual == 1 & blinddog_visual == 0 &
                                human_acoustic == 0 & noise_acoustic == 0)
View(Deer_multimodal_dog)
# lmer()でLMMを構築
Deer_model_multimodal_dog <- lm(
  AD ~ 
    dog_acoustic +
    log_light +
    SD + 
    flock +
    AvgWind + 
    season,
  #(1 | site_number_home),
  data = Deer_multimodal_dog
)

# 結果の確認
summary(Deer_model_multimodal_dog)
signif(summary(Deer_model_multimodal_dog)$coefficients, 3)

library(performance)
# 決定係数の確認
model_r2 <- r2_nakagawa(Deer_model_unimodal)
print(model_r2)


#####
#説明変数の分布をプロット
ggplot(data = Deer, aes(x = cues, fill = day_or_night)) +
  geom_bar(stat = "count") +
  xlab(NULL) +
  ylab("Counts") +
  theme_classic() +
  labs(fill = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


ggplot(data = Deer, aes(x=cues, y=FID)) +
  geom_boxplot(outlier.colour = NA) +
  geom_jitter(width = 0.2) +
  xlab(NULL) +
  ylab("FID (m)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = Deer, aes(x=cues, y=AD)) +
  geom_boxplot(outlier.colour = NA) +
  geom_jitter(width = 0.2) +
  xlab(NULL) +
  ylab("AD (m)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = Deer, aes(x=cues, y=light)) +
  geom_boxplot(outlier.colour = NA) +
  geom_jitter(width = 0.2) +
  xlab(NULL) +
  ylab("Ambient light level (lx)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(data = Deer, aes(x = cues, y = flock)) +
  geom_boxplot(outlier.colour = NA) +
  geom_jitter(width = 0.2) +
  xlab(NULL) +
  ylab("Group size") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = Deer, aes(x = cues, y = noise)) +
  geom_boxplot(outlier.colour = NA) +
  geom_jitter(width = 0.2) +
  xlab(NULL) +
  ylab("Ambient noise level (dBA)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(data = Deer, aes(x = cues, y = SD)) +
  geom_boxplot(outlier.colour = NA) +
  geom_jitter(width = 0.2) +
  xlab(NULL) +
  ylab("Starting distance (SD) (m)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = Deer, aes(x = cues, fill = season)) +
  geom_bar(stat = "count")+
  xlab(NULL) +
  ylab("Season (rutting vs non-rutting)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = Deer, aes(x = cues, y = AvgWind)) +
  geom_boxplot(outlier.colour = NA) +
  geom_jitter(width = 0.2) +
  xlab(NULL) +
  ylab("Mean wind speed (m/s)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = Deer, aes(x = cues, y = MaxWind)) +
  geom_boxplot(outlier.colour = NA) +
  geom_jitter(width = 0.2) +
  xlab(NULL) +
  ylab("Maximum wind speed (m/s)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

d <- density(Deer$FID)
op <- par(mar = c(4.5, 4.5, 1.5, 1), las = 1)  
plot(d,
     main = NULL,
     xlab = "FID (m)",
     ylab = "Density",
     lwd  = 2,
     col  = "black")

d2 <- density(Deer$AD)
op <- par(mar = c(4.5, 4.5, 1.5, 1), las = 1)  
plot(d2,
     main = NULL,
     xlab = "AD (m)",
     ylab = "Density",
     lwd  = 2,
     col  = "black")
