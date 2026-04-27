# assignmentを読み込む
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())
source("HW2.R")

check <- function(cond, msg_ok, msg_ng) {
  if (isTRUE(cond)) {
    cat(msg_ok, "\n")
    return(1)
  } else {
    cat(msg_ng, "\n")
    return(0)
  }
}
score <- 0

# ------------------------------
# Q1 df3が正しく作成されているか
# ------------------------------
score <- score + check(
  exists("df3") &&
    is.data.frame(df3) &&
    nrow(df3) == 10 &&
    all(c("id","name","gender","age","score","department","salary") %in% names(df3)),
  "Q1 正解：df3は正しく作成されています",
  "Q1 不正解：df3が存在しない、または構造が誤っています"
)

# データ型のチェック（重要）
score <- score + check(
    is.integer(df3$id) &&
    is.character(df3$name) &&
    is.character(df3$gender) &&
    is.numeric(df3$age) &&
    is.numeric(df3$score) &&
    is.character(df3$department) &&
    is.numeric(df3$salary),
  "Q1（型）正解：データ型は正しいです",
  "Q1（型）不正解：データ型が要件を満たしていません"
)

# ------------------------------
# Q2 salary_newの作成
# ------------------------------
score <- score + check(
  "salary_new" %in% names(df3) &&
    is.numeric(df3$salary_new) &&
    isTRUE(all.equal(df3$salary_new, df3$salary * 1.1)),
  "Q2 正解：salary_newは正しく作成されています",
  "Q2 不正解：salary_newが正しくありません"
)

# ------------------------------
# Q3 high_scoreの作成
# ------------------------------
score <- score + check(
  "high_score" %in% names(df3) &&
    all(df3$high_score %in% c(0,1)) &&
    all(df3$high_score == ifelse(df3$score >= 80, 1, 0)),
  "Q3 正解：high_scoreは正しく作成されています",
  "Q3 不正解：high_scoreが正しくありません"
)

# ------------------------------
# Q4 df_highの作成
# ------------------------------
score <- score + check(
  exists("df_high") &&
    is.data.frame(df_high) &&
    all(df_high$score >= 80),
  "Q4 正解：df_highは正しく作成されています",
  "Q4 不正解：df_highが正しくありません"
)

# ------------------------------
# Q5 df_devの作成
# ------------------------------
score <- score + check(
  exists("df_dev") &&
    is.data.frame(df_dev) &&
    all(df_dev$department == "開発"),
  "Q5 正解：df_devは正しく作成されています",
  "Q5 不正解：df_devが正しくありません"
)

# ------------------------------
# Q6 mean_scoreの計算
# ------------------------------
score <- score + check(
  exists("mean_score") &&
    is.numeric(mean_score) &&
    abs(mean_score - mean(df3$score)) < 1e-6,
  "Q6 正解：mean_scoreは正しく計算されています",
  "Q6 不正解：mean_scoreが正しくありません"
)

# ------------------------------
# Q7 ボーナスの反映
# ------------------------------
expected_salary_new <- df3$salary * 1.1
bonus_index <- df3$score >= 80
expected_salary_new[bonus_index] <- expected_salary_new[bonus_index] * 1.05

score <- score + check(
  isTRUE(all.equal(df3$salary_final, expected_salary_new)),
  "Q7 正解：ボーナスは正しく反映されています",
  "Q7 不正解：ボーナス計算が正しくありません"
)
# ------------------------------
# 最終結果
# ------------------------------
cat("===== 採点終了 =====\n")
cat("総得点：", score, "/8\n")

