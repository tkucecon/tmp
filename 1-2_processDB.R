
# 必要なパッケージの読み込み
library(tidyverse)
library(igraph)
library(networkD3)
library(ggraph)
library(scales)
library(countrycode)

# ==========================================
# 1. データの再読み込み
# ==========================================

df_yearly <- 
  read_rds("../4_input/comtrade_yearly.rds")

# 2024年の輸出データに絞り込み、国間の総取引量（重量：qty）を集計
data_2024_import <- 
  df_yearly %>%
  filter(ref_year == 2024, flow_code == "M") %>%
  # まずは原油だけを残す
  filter(cmd_code != 270900) |> 
  # "World"（全世界）などの集計値を除外
  filter(partner_desc != "World") %>%
  # 表示単位がkgの取引だけを残す
  filter(qty_unit_abbr == "kg") |> 
  group_by(reporter_desc, partner_desc) %>%
  summarise(trade_qty = sum(qty, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(trade_qty)) |> 
  mutate(
    partner_region  = countrycode(partner_desc,  origin = "country.name", destination = "region", warn = FALSE)
  )
  
# ==========================================
# 2. 2024年の輸出入ネットワーク可視化（平時）
# ==========================================

links <- data_2024_import %>%
  # 左右を区別するために名前の後ろに (Export) / (Import) を付ける
  mutate(
    source_name = paste0(partner_desc, " (Export)"),
    target_name = paste0(reporter_desc, " (Import)")
  ) %>%
  # 取引額が少なすぎるフローを除外（線が多すぎて黒く潰れるのを防ぐため）
  # ※ここでは例として、全体の平均額以上のフローに絞っています
  filter(trade_qty > sum(trade_qty, na.rm = TRUE) * 0.01)

# ノード（すべての地域名のリスト）を作成
nodes <- data.frame(
  name = unique(c(links$source_name, links$target_name))
)

# networkD3では、ノードのIDが「0」から始まるインデックスである必要があります。
# 文字列の地域名を、0始まりのID番号に変換します。
links$source <- match(links$source_name, nodes$name) - 1
links$target <- match(links$target_name, nodes$name) - 1

sankeyNetwork(
  Links = links, 
  Nodes = nodes,
  Source = "source",      # リンク元（輸出）のID
  Target = "target",      # リンク先（輸入）のID
  Value = "trade_qty",  # 線の太さ（取引額）
  NodeID = "name",        # ノードのラベル（地域名）
  fontSize = 14,          # 文字サイズ
  nodeWidth = 60,         # ノード（四角いブロック）の幅
  nodePadding = 5,       # ノード間の縦のすき間
  height = 800,
  width = 1000,
  sinksRight = TRUE,      # 最終地点を右端に揃える
  colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);") # 色のテーマ
)

# ==========================================
# 3. 取引先集中度の可視化（地域別）
# ==========================================

# calculate HHI
df_concentration <- 
  data_2024_import %>%
  group_by(reporter_desc, partner_region) %>%
  summarise(trade_qty = sum(trade_qty, na.rm = TRUE), .groups = 'drop') %>%
  group_by(reporter_desc) %>%
  mutate(
    total_import = sum(trade_qty, na.rm = TRUE),
    share = trade_qty / total_import,
    share_sq = share^2
  ) %>%
  summarise(
    hhi = sum(share_sq, na.rm = TRUE),
    total_import = first(total_import),
    num_ = n(),
    top_origin = partner_region[which.max(share)],
    top_share = max(share, na.rm = TRUE),
    .groups = "drop"
  )

# keep main players
df_main_hhi <- 
  df_concentration %>%
  # イスラエルはデータがないので落とす
  filter(reporter_desc != "Israel") |> 
  top_n(30, total_import) %>%
  # HHIが高い（集中している）順に並び替え
  arrange(desc(hhi)) %>%
  # グラフの並び順を固定するためのファクタ化
  mutate(reporter_desc = factor(reporter_desc, levels = rev(reporter_desc))) |> 
  # 相手先が中東か否かのフラグを立てる
  mutate(flag_me = ifelse(top_origin == "Middle East, North Africa, Afghanistan & Pakistan", "Middle East", "Other"))

# plot
ggplot(df_main_hhi, aes(x = reporter_desc, y = hhi, fill = flag_me)) +
  geom_col(color = "white", linewidth = 0.2) +
  # 中東への依存度が最も高い地域は赤、それ以外はグレー
  scale_fill_manual(values = c("Middle East" = "#E63946", "Other" = "#B0B0B0")) +
  # 最大の輸出先と、その国へのシェアを棒の横にテキスト表示（例: "USA (85%)"）
  geom_text(aes(label = paste0(top_origin, " (", percent(top_share, accuracy = 1), ")")), 
            hjust = -0.1, size = 3, color = "gray20") +
  coord_flip() + # 横向きの棒グラフにする
  # y軸の範囲を少し広げてテキストが切れないようにする
  scale_y_continuous(limits = c(0, max(top_importers$hhi) * 1.3), breaks = seq(0, 1, 0.2)) +
  labs(
    title = "原油主要輸入国の輸出先集中度（HHI）の比較",
    subtitle = "棒が長い（赤い）ほど、特定の少数の国に輸出を依存している\nテキストは最大の輸入先地域とそのシェア",
    x = "輸入国",
    y = "集中度 HHI (0〜1)",
    fill = "HHI"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(size = 10, face = "bold")
  )

# ==========================================
# 4. 取引先
# ==========================================
