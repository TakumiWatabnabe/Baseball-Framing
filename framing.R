library(baseballr)
library(tidyverse)

### Statcastから2021年シーズンの投球データを取得 ###
data<-statcast_search("2022-04-01","2022-04-01")
data_2<-data%>%
  ###　フレーミングの分析に必要なカラムを選択
  filter(description=="ball"|description=="called_strike")%>%
  select("pitch_type","game_date","release_speed","release_pos_x","release_pos_z",
         "batter","pitcher","description","zone","stand","p_throws","home_team",
         "away_team","type","balls","strikes","pfx_x","pfx_z","plate_x","plate_z",
         "outs_when_up","inning","inning_topbot","fielder_2","vx0","vy0","vz0",
         "ax","ay","az")
data_2
write.csv(data_2,
          "baseballr/2021-11-02.csv",
          row.names=FALSE,
          fileEncoding="shift-jis",
          na="")

### 2021年投球データの読み込み ###
getwd()
setwd("C:/Users/tdm60/OneDrive/ドキュメント/framing/baseballr/")
file_list<-list.files("C:/Users/tdm60/OneDrive/ドキュメント/framing/baseballr/",pattern=".csv")
bind_data<-NULL
length(file_list)
for (i in 1:length(file_list)){
  csv_data<-read_csv(file_list[i])
  
  bind_data<-rbind(bind_data,csv_data)
}

 
### ストライクゾーンはどこか ###
plate_width<-17+2*(9/pi)
crcblue<-"#2905a1"
k_zone_plot <- ggplot(NULL, aes(x = plate_x, y = plate_z)) +
  geom_rect(xmin = -(plate_width/2)/12,
            xmax = (plate_width/2)/12,
            ymin = 1.5,
            ymax = 3.6, color = crcblue, alpha = 0) +
  coord_equal() +
  scale_x_continuous("水平位置（フィート）（Horizontal location (ft.)）",
                     limits = c(-2, 2)) +
  scale_y_continuous("垂直位置（フィート）（Vertical location (ft.)）",
                     limits = c(0, 5))

### 5000球を抽出しプロット ###
k_zone_plot%+%sample_n(bind_data,5000)+
  aes(color=type)+
  geom_point(alpha=0.1)+
  scale_color_manual(values=c(crcblue,"black"))

taken<-bind_data%>%
  filter(type!="X")

zones<-taken%>%
  group_by(zone)%>%
  summarize(
    N=n(),
    right_edge=min(1.5,max(plate_x)),
    left_edge=max(-1.5,min(plate_x)),
    top_edge=min(5,quantile(plate_z,0.95,na.rm=TRUE)),
    bottom_edge=max(0,quantile(plate_z,0.05,na.rm=TRUE)),
    strike_pct=sum(type=="S")/n(),
    plate_x=mean(plate_x),
    plate_z=mean(plate_z)
  )

### それぞれのゾーンの領域及びゾーン内でストライクと判定されるであろう確率をプロット ###
library(ggrepel)
k_zone_plot %+% zones +
  geom_rect(aes(xmax = right_edge, xmin = left_edge,
                ymax = top_edge, ymin = bottom_edge,
                fill = strike_pct, alpha = strike_pct),
            color = "lightgray") +
  geom_text_repel(size = 3, aes(label = round(strike_pct, 2),
                                color = strike_pct < 0.5)) +
  scale_fill_gradient(low = "gray70", high = crcblue) +
  scale_color_manual(values = c("white", "black")) +
  guides(color = FALSE, alpha = FALSE)

library(mgcv)
strike_mod <- gam(type == "S" ~ s(plate_x, plate_z),
                  family = binomial, data = taken)

### 推定結果の可視化 ###
install.packages("broom")
library(broom)
hats <- strike_mod %>%
  augment(type.predict = "response")
k_zone_plot %+% sample_n(hats, 50000) +
  geom_point(aes(color = .fitted), alpha = 0.1) +
  scale_color_gradient(low = "gray70", high = crcblue)

### 推定した平面の可視化 ###
install.packages("modelr")
library(modelr)
grid <- taken %>%
  data_grid(plate_x = seq_range(plate_x, n = 100),
            plate_z = seq_range(plate_z, n = 100))

grid_hats <- strike_mod %>%
  augment(type.predict = "response", newdata = grid)

tile_plot <- k_zone_plot %+% grid_hats +
  geom_tile(aes(fill = .fitted), alpha = 0.7) +
  scale_fill_gradient(low = "gray92", high = crcblue)
tile_plot

### 利き腕の調整 ###
hand_mod <- gam(type == "S" ~ p_throws + stand + s(plate_x, plate_z),
                family = binomial, data = bind_data)
hand_grid <- bind_data %>%
  data_grid(plate_x = seq_range(plate_x, n = 100),
            plate_z = seq_range(plate_z, n = 100),
            p_throws, stand)
hand_grid_hats <- hand_mod %>%
  augment(type.predict = "response", newdata = hand_grid)

tile_plot %+% hand_grid_hats +
  facet_grid(p_throws ~ stand)
diffs <- hand_grid_hats %>%
  group_by(plate_x, plate_z) %>%
  summarize(N = n(), .fitted = sd(.fitted))
tile_plot %+% diffs

### フレーミングのモデルを作成 ###
sc_taken<-bind_data%>%
  sample_n(10000)%>%
  mutate(strike_prob=predict(strike_mod,newdata=.,
                             type="response"))

###　一般化線形混合モデルの作成 ###
install.packages("lme4")
library(lme4)
mod_a <- glmer(type == "S" ~ strike_prob + (1|fielder_2),
               data = sc_taken, family = binomial)

install.packages("broom.mixed")
library(broom.mixed)

### 固定効果に関する情報 ###
tidy(mod_a, effects = "fixed")
### ランダム効果の推定値 ###
tidy(mod_a, effects = "ran_pars")


c_effects <- mod_a %>%
  ranef() %>%
  as_tibble() %>%
  transmute(id = as.numeric(levels(grp)),
            effect = condval)
c_effects<-c_effects%>%
  arrange(desc(effect))
c_effects%>%head()
c_effects %>% tail()
playername_lookup(457763)

mlb_people(person_ids = 641598)
playerid<-mlb_people(person_ids = NULL)
head(playerid)

### 一般化線形混合モデルの再構築 ###
mod_b <- glmer(type == "S" ~ strike_prob + (1|fielder_2) +
                 (1|batter) + (1|pitcher),
               data = sc_taken, family = binomial)
tidy(mod_b, effects = "ran_pars")
c_effects <- mod_b %>%
  ranef() %>%
  as_tibble() %>%
  filter(grpvar == "fielder_2") %>%
  transmute(id = as.numeric(as.character(grp)),
            effect = condval)
c_effects <- c_effects %>%
  arrange(desc(effect))
c_effects %>% head()
c_effects %>% tail()
playername_lookup(521692)
