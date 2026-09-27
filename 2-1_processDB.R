# 必要なパッケージの読み込み
library(tidyverse)
library(lubridate)

# 1. csvをRに読み込む
# "primaryyear2026.csv" をデータフレームとして読み込み
df <- read_csv("primaryyear2026.csv")

# データの整形（Time列を日付型に変換、Flowを列に展開）
# 実際のCSVの列名（REF_AREA, TIME_PERIOD等）に合わせて適宜変更してください
df_wide <- df %>%
  mutate(Date = ym(TIME_PERIOD)) %>% 
  filter(UNIT_MEASURE == "KBBL") |> 
  select(REF_AREA, ENERGY_PRODUCT, FLOW_BREAKDOWN, Date, OBS_VALUE) %>%
  mutate(OBS_VALUE = as.numeric(OBS_VALUE)) |> 
  pivot_wider(names_from = FLOW_BREAKDOWN, values_from = OBS_VALUE, values_fill = NA)

# 2. 国ごとの在庫水準変動を寄与度分解する
# 一次製品の定義式に基づく要因（InflowとOutflow）を計算
df_decomp <- 
  df_wide %>%
  mutate(
    # プラス要因（在庫増）
    In_Production = INDPROD,
    In_Imports = TOTIMPSB,
    In_Other = OSOURCES + TRANSBAK,
    # マイナス要因（在庫減：寄与度プロットのためマイナス反転）
sOut_Exports = -TOTEXPSB,
    Out_Refinery = -REFINOBS,
    Out_DirectUse = -DIRECUSE,
    Out_StatDiff = -STATDIFF,
    # 実際の在庫変動
    Actual_Stock_Change = STOCKCH
  ) %>%
  select(REF_AREA, ENERGY_PRODUCT, Date, starts_with("In_"), starts_with("Out_"), Actual_Stock_Change, CLOSTLV) %>%
  pivot_longer(cols = starts_with("In_") | starts_with("Out_"), 
               names_to = "Factor", values_to = "Contribution")

# 3. 結果を国ごと、製品ごとにプロットしてpdfとして保存する
# 寄与度分解の積み上げ棒グラフを作成
plot_decomp <- 
  ggplot(df_decomp, aes(x = Date, y = Contribution, fill = Factor)) +
  geom_bar(stat = "identity") +
  geom_line(aes(y = Actual_Stock_Change, color = "Net Stock Change"), size = 1) +
  facet_grid(ENERGY_PRODUCT ~ REF_AREA, scales = "free_y") +
  scale_fill_brewer(palette = "Set3") +
  scale_color_manual(values = c("Net Stock Change" = "black")) +
  theme_minimal() +
  labs(title = "Stock Change Decomposition by Country and Product",
       x = "Date", y = "Volume", fill = "Flow Factors", color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# PDFとして保存
ggsave("stock_decomposition.pdf", plot = plot_decomp, width = 16, height = 10)

# 4. 在庫水準が各年の平均的な需要の何か月分に相当するかを計算し、国を横並びで比較可能なヒートマップを描く
# ※一次製品の場合は需要をRefinery intake (+ Direct use)とみなして計算
df_months_demand <- df_wide %>%
  mutate(Year = year(Date),
         Total_Demand = REFINOBS + DIRECUSE) %>%
  group_by(REF_AREA, ENERGY_PRODUCT, Year) %>%
  mutate(Avg_Monthly_Demand = mean(Total_Demand, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Months_of_Demand = if_else(Avg_Monthly_Demand > 0, 
                                    CLOSTLV / Avg_Monthly_Demand, 
                                    NA_real_)) |> 
  filter(ENERGY_PRODUCT == "TOTCRUDE") 
  

# ヒートマップの作成
upper_limit <- quantile(df_months_demand$Months_of_Demand, probs = 0.95, na.rm = TRUE)

plot_heatmap <- 
  df_months_demand |> 
  na.omit() |> 
  ggplot(aes(x = as.factor(Date), y = REF_AREA, fill = Months_of_Demand)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(
    colors = c("red", "yellow", "green"),
    limits = c(0, upper_limit), # 上限を動的に設定
    oob = scales::squish,       # 上限を超える外れ値は緑に固定
    na.value = "grey90",
    name = "Months"
  ) +
  # facet_wrap(~ Year, ncol = 1) +
  theme_minimal() +
  labs(title = "Closing Stocks Coverage (Months of Average Demand)",
       x = "Country", y = "Product") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 必要に応じてヒートマップも保存
ggsave("stock_months_coverage_heatmap.pdf", plot = plot_heatmap, width = 12, height = 8)