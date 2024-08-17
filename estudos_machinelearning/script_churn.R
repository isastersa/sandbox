library(dplyr)
library(h2o)
library(highcharter)
library(readr)

# Preparando objetos
ml <- list() # Em uma lista vazia é possível colocar varios tipos de objetos, como df, vetor, gráficos, etc
colors <- c("#e8e9ed", "#e89978", "#4a57a6", "#4192b5")

# Iniciando h2o cluster
# h2o é um motor de machine learning
h2o::h2o.init()

# Criando uma sublista data dentro da lista ml para ler o dados
ml$data$raw <- readr::read_csv(
  "https://raw.githubusercontent.com/wrprates/open-data/master/telco_customer_churn.csv"
) |>
  dplyr::mutate(across(where(is.character), as.factor)) #entre todas as colunas, transformar o que é caracter em fator

# Definindo variáveis
ml$vars$y <- "Churn" #y - variável dependente, variável que faremos a predição
ml$vars$discard <- "customerID" # discard = variável para descarte. Tiramos o ID para não ter problema de overfitting, além de que no modelo cada linha é um registro
ml$vars$x <- setdiff(names(ml$data$raw), c(ml$vars$y, ml$vars$discard)) #x - variável independente, o que vai explicar o y
# na variavel x ele está pedindo para trazer tudo que for diferente do df data$raw do vetor c

# Configurando h2o
ml$data$h2o <- h2o::as.h2o(ml$data$raw)
# Dividindo em treino e teste (aqui estamos configurando 70% teste)
ml$data$splits <- h2o::h2o.splitFrame(ml$data$h2o, ratios = 0.7)
names(ml$data$splits) <- c("train", "test")

# Rodando o modelo
ml$model <- h2o::h2o.gbm(x = ml$vars$x, y = ml$vars$y, training_frame = ml$data$splits$train)
ml$predictions <- h2o::h2o.predict(ml$model, ml$data$splits$test)
h2o::h2o.performance(ml$model, ml$data$splits$test)

ml$data$predictions <- ml$data$splits$test |>
  tibble::as_tibble() |>
  dplyr::bind_cols( #unifica as colunas de dataframes diferentes
    dplyr::as_tibble(ml$predictions) |> 
      dplyr::select(Predict = predict, PredictProbability = Yes) |> # selecionando as colunas
      dplyr::mutate(PredictProbability = round(100*PredictProbability, 2)) # arrumando a porcentagem p 0 a 100
  ) |>
  # 11 is not a magic number, it is inverting the order of the deciles
  dplyr::mutate(RiskGroup = as.factor(11 - dplyr::ntile(PredictProbability, 10))) |> # criando um grupo de risco de acordo com a probabilidade
  dplyr::select(customerID, Churn, Predict, PredictProbability, RiskGroup, dplyr::everything()) |> 
  dplyr::arrange(dplyr::desc(PredictProbability)) #ordenando por probabilidade descendente

# # Overall Churn
# ml$data$overall_churn <- ml$data$raw |>
#   dplyr::group_by(Churn) |>
#   dplyr::tally() |>
#   dplyr::mutate(
#     `% Customers` = round(100 * n / sum(n), 2),
#     Customer = "Churn Yes / No"
#   ) |>
#   dplyr::rename(`Count Customers` = n)
# 
# ml$charts$overall_churn <- ml$data$overall_churn |>
#   highcharter::hchart(
#     hcaes(x = Customer, y = `% Customers`, group = Churn),
#     type = "bar",
#     stacking = "normal",
#     dataLabels = list(enabled = TRUE)
#   ) |>
#   highcharter::hc_title(text = "Overall company's Churn") |>
#   highcharter::hc_size(width = NULL, height = 200) |>
#   highcharter::hc_xAxis(title = list(text = "")) |>
#   highcharter::hc_yAxis(max = 100) |>
#   highcharter::hc_colors(colors)
# 
# # Variables importance
# ml$vars$importance <- h2o::h2o.varimp(ml$model) |>
#   tibble::as_tibble()
# 
# ml$charts$vars_importance <- highcharter::highchart() |>
#   highcharter::hc_add_series(ml$vars$importance$percentage*100, name = "") |>
#   highcharter::hc_chart(type = "bar") |>
#   highcharter::hc_xAxis(categories = ml$vars$importance$variable) |>
#   highcharter::hc_yAxis(
#     title = list(text = "Importance Percentage"), labels = list(format = "{value}%")
#   ) |>
#   highcharter::hc_chart(zoomType = "xy") |>
#   highcharter:: hc_colors(colors[4]) |>
#   highcharter:: hc_legend(enabled = FALSE) |>
#   highcharter::hc_tooltip(
#     formatter = JS(
#       "function(){return  'Importance (%): <b>' + Highcharts.numberFormat(this.y) + '%</b>';}"
#     ),
#     useHTML = FALSE
#   ) |>
#   highcharter::hc_title(text = "Variables Importance") |>
#   highcharter::hc_size(width = NULL, height = 500)
# 
# # Calculates active and canceled customers and cumulative canceled for each decile
# ml$data$chrun_by_risk_groups <- ml$data$predictions |>
#   dplyr::group_by(RiskGroup, Churn) |>
#   dplyr::tally() |>
#   dplyr::mutate(prop = 100 * n / sum(n)) |>
#   dplyr::ungroup() |>
#   dplyr::group_by(Churn) |>
#   dplyr::mutate(
#     prop_bad_good = 100 * n / sum(n),
#     cum_prop = cumsum(prop_bad_good),
#     n_cum_sum = cumsum(n)) |>
#   dplyr::ungroup() |>
#   dplyr::group_by(RiskGroup) |> 
#   dplyr::mutate(precisao = 100 * n_cum_sum / sum(n_cum_sum)) |>
#   dplyr::ungroup() |>
#   dplyr::mutate(across(.cols = c("prop", "prop_bad_good", "cum_prop", "precisao"), .fns = round, 2 ))
# 
# # Chart with risk groups by deciles
# ml$charts$risk_groups_churn <- ml$data$chrun_by_risk_groups |>
#   highcharter::hchart(hcaes(x = RiskGroup, y = prop, group = Churn), type = "column") |>
#   highcharter::hc_add_series(
#     name="Cumulative % of canceled customers (recall)",
#     data = (ml$data$chrun_by_risk_groups %>% dplyr::filter(Churn == "Yes"))$cum_prop,
#     type = "line",
#     dashStyle = "DashDot",
#     opposite = FALSE,
#     dataLabels = list(
#       enabled = TRUE,
#       color = "#666",
#       style = list(fontSize = "16px"),
#       formatter = JS("function () {if(this.y===0){return null;} return Math.round( this.y ) + '%'; }")
#     )
#   ) |>
#   highcharter::hc_yAxis(title = list(text = "Proportion (%)"), max = 100) |>
#   highcharter::hc_xAxis(title = list(text = "Risk Group")) |>
#   highcharter::hc_colors(colors) |>
#   highcharter::hc_plotOptions(column = list(
#     stacking = "normal",
#     dataLabels = list(
#       enabled = TRUE,
#       color = "black",
#       style = list(fontSize = "17px"),
#       formatter = JS("function () {
#           if(this.y===0){return null;} return Math.round(100 * this.y / this.total) + '%';
#         }")
#     )
#   )) |>
#   highcharter::hc_tooltip(
#     table = FALSE,
#     shared = TRUE,
#     split = FALSE,
#     headerFormat = "<span style='font-size: 14px'>{point.key}</span><br/>",
#     pointFormat = "<span style='font-size: 22px; color:{point.color}'>\u25CF</span>
#         <span style='font-size: 16px;'>{series.name}:</span>
#         <span style='font-size: 16px; font-weight: bold;'>{point.y}</span> <br/>"
#   ) |>
#   highcharter::hc_title(text = "Churn by Group Risk and cumulative canceled customers")
# 
# # Chart with financial values for each decile
# ml$data$charge_for_risk_groups <- ml$data$predictions |>
#   dplyr::group_by(Churn, RiskGroup) |>
#   dplyr::summarise(SumMonthlyCharges = sum(MonthlyCharges, na.rm = TRUE), .groups = "drop")
# 
# ml$charts$charge_for_risk_groups <- ml$data$charge_for_risk_groups |>
#   highcharter::hchart(hcaes(x = RiskGroup, y = SumMonthlyCharges, group = Churn), type = "column") |>
#   highcharter::hc_plotOptions(column = list(
#     stacking = "normal",
#     dataLabels = list(
#       enabled = TRUE,
#       color = "black",
#       style = list(fontSize = "17px"),
#       formatter = JS("function () {
#           if(this.y===0){return null;} return Math.round(this.y / 1000) + 'k';
#         }")
#     )
#   )) |>
#   highcharter::hc_yAxis(title = list(text = "Monthly Charges ($)")) |>
#   highcharter::hc_xAxis(title = list(text = "Risk Group")) |>
#   highcharter::hc_colors(colors) |>
#   highcharter::hc_title(text = "Total Monthly Charges by Risk Group (probability deciles)") 
