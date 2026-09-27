# 必要なパッケージの読み込み
library(comtradr)
library(tidyverse)

# ==========================================
# 1. 初期設定とデータのダウンロード
# ==========================================

# APIキーの設定（取得した自身のAPIキーに置き換えてください）
set_primary_comtrade_key("5f1791d2356e4129aa0a17f93ff33904")

# 対象とするHSコード（6桁）の設定
# 270900: 原油 (Petroleum oils and oils obtained from bituminous minerals, crude)
# 271012, 271019, 271020, 271091, 271099: 石油製品（揮発油、灯油、軽油、重油など）
hs_codes <- c("270900", "271012", "271019", "271020", "271091", "271099")

# --- 2015年から2025年までの年次データを取得 ---
# ※API制限を避けるため、年ごとに取得して結合します。
# ※「すべて」の国を指定すると膨大になるため、主要国を指定するか、少しずつ取得することを推奨します。
years <- 2015:2025
yearly_data_list <- list()

for (y in years) {
  cat(y, "年のデータを取得中...\n")
  # 輸出入データ（フロー: Import, Export）を取得
  # 実際にはデータ量が多いため、主要なレポーター国を指定するなどの工夫が必要です
  temp_data <- ct_get_data(
    reporter = "all_countries",
    partner = "all_countries",
    flow_direction = c("Import", "Export"),
    frequency = "A", # Annual
    start_date = as.character(y),
    end_date = as.character(y),
    commodity_code = hs_codes
  )
  yearly_data_list[[as.character(y)]] <- temp_data
  Sys.sleep(10) # API制限回避のための待機
}

df_yearly <- 
  bind_rows(yearly_data_list)

# --- 2026年以降の月次データを取得 ---
months_2026 <- c("2026-01", "2026-02", "2026-03", "2026-04", "2026-05", "2026-06") # 必要に応じて拡張
monthly_data_list <- list()

for (m in months_2026) {
  cat(m, "のデータを取得中...\n")
  temp_data <- ct_get_data(
    reporter = "all",
    partner = "all",
    flow_direction = c("Import", "Export"),
    frequency = "M", # Monthly
    start_date = m,
    end_date = m,
    commodity_code = hs_codes
  )
  monthly_data_list[[m]] <- temp_data
  Sys.sleep(10)
}

df_monthly <- bind_rows(monthly_data_list)

# データの保存（CSVまたはRDS）
write_rds(df_yearly,  "../4_input/comtrade_yearly.rds")
write_rds(df_monthly, "../4_input/comtrade_monthly.rds")
