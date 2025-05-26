# --- ライブラリ読み込み ---
if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)

# --- データ読み込み ---
data <- read.csv("data.csv", fileEncoding = "utf-8")  # エラー時は "cp932" に
print(data)

# --- 線形モデル y = ax + b ---
linear_model <- lm(収穫量 ~ 日照時間, data = data)
summary(linear_model)

# --- 2次モデル y = ax^2 + bx + c ---
quad_model <- lm(収穫量 ~ poly(日照時間, 2, raw = TRUE), data = data)
summary(quad_model)

# --- グラフ出力（線形＝青、2次＝赤） ---
ggplot(data, aes(x = 日照時間, y = 収穫量)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "blue", linewidth = 1.2) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE), se = FALSE, color = "red", linewidth = 1.2) +
  labs(title = "日照時間と収穫量の関係", x = "日照時間 (hr)", y = "収穫量 (kg/ha)") +
  theme_minimal()
write.csv(data, "data_R.csv", row.names = FALSE) #CSVファイルを保存
