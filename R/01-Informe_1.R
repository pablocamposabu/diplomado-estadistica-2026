# ========================================================================
# TÍTULO     : 01-Informe_1
# PROYECTO   : Diplomado en Estadística, mención Métodos Estadísticos
#
# DESCRIPCIÓN:
# Cálculos realizados para el Informe 1 del Módulo 2 del Diplomado en Estadística UC 2026.
#
# INPUT      : input/01-bbdd_contaminación.xlsx
# OUTPUT     : output/01-Informe_1.pdf
#
# AUTOR/A    : Emilia Barrientos Carrasco y Pablo Campos Abutridy
# FECHA      : 2026-07-29
# NOTAS      :
# ========================================================================

# 0) Ajustes -------------------------------------------------------------

rm(list = ls())


# 1) Librerías -----------------------------------------------------------

library(tidyverse)
library(corrplot)
library(GGally)
library(car)
library(lmtest)
library(nortest)
library(readxl)
library(performance)
library(sjPlot)


# 2) Datos ---------------------------------------------------------------

df <- read_xlsx(
  "../DE-Modulo_2/data/01-bbdd_contaminacion.xlsx"
)


# 3) Pregunta 1 ----------------------------------------------------------

## 3.1) Mejor MRLS basado en variables metereológicas ----

# Definir sub-set con variables metereológicas e Y.
df_met <- df |>
  select(
    PM2.5,
    Viento,
    TProm,
    TMin,
    TMax,
    Humed
  )

# Análisis de correlación
correlacion_met <- cor(df_met)

# Gráfico de correlación
gg_corr_met <- corrplot(
  correlacion_met,
  type = "lower",
  method = "number"
)

# ggpairs(df_met)

### 3.1.1) Modelo con predictor = Viento ----

mod_met_viento <- lm(
  PM2.5 ~ Viento,
  data = df_met
)
summary(mod_met_viento)

### 3.1.2) Modelo con predictor = TProm ----

mod_met_tprom <- lm(
  PM2.5 ~ TProm,
  data = df_met
)
summary(mod_met_tprom)

### 3.1.3) Modelo con predictor = TMin ----

mod_met_tmin <- lm(
  PM2.5 ~ TMin,
  data = df_met
)
summary(mod_met_tmin)

### 3.1.4) Modelo con predictor = TMax ----

mod_met_tmax <- lm(
  PM2.5 ~ TMax,
  data = df_met
)
summary(mod_met_tmax)

### 3.1.5) Modelo con predictor = Humed ----

mod_met_humed <- lm(
  PM2.5 ~ Humed,
  data = df_met
)
summary(mod_met_humed)


## 3.2) Chequeo de supuestos ----

### 3.2.1) Linealidad ----

plot(mod_met_tmin, 1)
# Deberia estar distribuidos aleatoriamente alrededor de la linea horizontal
# que representa un error residual de cero.

# Test RESET p-value = 0.0001268
resettest(
  mod_met_tmin,
  power = 2:3,
  type = "fitted"
)

plot(mod_met_tmin, which = 1)

### 3.2.2) Normalidad ----
plot(mod_met_tmin, 2)

# p-value = 0.0004369 <- Se cumple con normalidad de los residuos.
shapiro.test(mod_met_tmin$residuals)


#### 3.2.3) Homocedasticidad ----

# p-value = 9.285e-11 -> Se cumple con la homocedasticidad (varianza constante).
bptest(mod_met_tmin)


### 3.2.4) Independencia ----

# En este caso es independiente por construcción.
dwtest(mod_met_tmin)

check_model(mod_met_tmin)

### 3.2.5) Observaciones influyentes ----

# 4) Pregunta 2 ----------------------------------------------------------

# Definir sub-set de variables contaminantes de df.
df_cont <- df |>
  select(
    PM2.5,
    NO,
    NO2,
    CO,
    O3
  )

# Definir correlaciones entre variables
correlacion_cont <- cor(df_cont)

# Visualizar correlaciones entre variables
gg_corr_con <- corrplot(
  correlacion_cont,
  type = "lower",
  method = "number"
)

## 4.1) Mejor MRLS de contaminantes ----

### 4.1.1) Modelo con predictor = NO ----
mod_cont_no <- lm(
  PM2.5 ~ NO,
  data = df_cont
)
summary(mod_cont_no)

### 4.1.2) Modelo con predictor = NO2 ----
mod_cont_no2 <- lm(
  PM2.5 ~ NO2,
  data = df_cont
)
summary(mod_cont_no2)

### 4.1.3) Modelo con predictor = CO ----
mod_cont_co <- lm(
  PM2.5 ~ CO,
  data = df_cont
)
summary(mod_cont_co)

### 4.1.4) Modelo con predictor = O3 ----
mod_cont_o3 <- lm(
  PM2.5 ~ O3,
  data = df_cont
)
summary(mod_cont_o3)


### 4.1.5) Tabla comparativa MRLS ----

tbl_mod_cont <- tab_model(
  list(
    mod_cont_no,
    mod_cont_co,
    mod_cont_o3,
    mod_cont_no2
  ),
  title = "Comparación de MRLS con predictores contaminantes",
  dv.labels = paste0("M", 1:4),

  show.ci = FALSE,
  show.se = TRUE,
  show.r2 = TRUE,
  show.fstat = TRUE,
  #show.aic = TRUE,
  show.obs = TRUE,

  p.style = "stars",
  string.pred = "Predictor",
  string.est = "β",
  string.se = "EE",
  string.p = "p",

  digits = 2,
  digits.p = 3,
  CSS = list(
    css.table = paste(
      "width: auto;",
      "margin-left: auto;",
      "margin-right: auto;",
      "font-size: 8pt;"
    ),
    css.caption = "text-align: center;",
    css.tdata = "padding: 0.15em 0.25em;",
    css.colnames = "padding: 0.15em 0.25em;"
  )
)
tbl_mod_cont


## 4.2) Chequeo de supuestos ----
check_model(mod_cont_no2)

### 4.2.1) Linealidad ----
# Test Ramsey
resettest(
  mod_cont_no2,
  power = 2:3,
  type = "fitted"
)
# p-value = 0.01824 <- No se cumple la linealidad de los residuos.

### 4.2.2) Normalidad ----
# Test de Shapiro-Wilk
shapiro.test(mod_cont_no2$residuals)
# p-value = 2.286e-06 <- No se cumple con normalidad de los residuos.

#### 4.2.3) Homocedasticidad ----
# Test de Breusch-Pagan
bptest(mod_cont_no2)
# p-value = 0.0008067 <- No se cumple con la homocedasticidad (varianza constante)

### 4.2.4) Independencia ----
# Test de Durbin-Watson
dwtest(mod_met_tmin)

# En este caso es independiente por construcción.
check_model(mod_cont_no2)


# 5) Pregunta 3 ----------------------------------------------------------

# Con base a todas las variables (meteorológicas y contaminantes),
# mediante una técnica iterativa (forward o backward) seleccione el mejor modelo predictivo.
# Indique para cada paso qué variable entra/sale del modelo, indicando el aumento/disminución del R2-ajustado.

## 5.1) Correlación ----

# Calcular correlación entre todas las variables de df.
correlacion_total <- cor(df)
correlacion_total

# Gráfico de correlación total.
gg_corr_total <- corrplot(
  correlacion_total,
  type = "lower",
  method = "number"
)

tabla_corr <- cor(df, use = "pairwise.complete.obs") |>
  round(2) |>
  knitr::kable(
    caption = "Matriz de correlaciones",
    align = "c"
  )
tabla_corr

## 5.2) Selección automática de MRLM ----

# Definir un MRLM sin variables.
mod_vacio <- lm(PM2.5 ~ 1, data = df)
summary(mod_vacio)

# Definir un MRLM con todas las variables.
mod_total <- lm(PM2.5 ~ ., data = df)
summary(mod_total)


### 5.2.1) Método backward ----

# Definir MRLM a partir de selección automática (Backward).
mod_backward <- step(
  mod_total,
  direction = "backward"
)

# R^2 ajustado = 0,8196.
# F = p-value: < 2.2e-16.
summary(mod_backward)


### 5.2.2) Método forward ----

# Definir MRLM a partir de selección automática (Forward).
mod_forward <- step(
  mod_vacio,
  direction = "forward",
  scope = formula(mod_total)
)

# R^2 ajustado = 0.8196.
# F = p-value: < 2.2e-16.
summary(mod_forward)


### 5.2.3) Método both ----

# Definir MRLM a partir de selección automática (Both).
mod_both <- step(
  mod_vacio,
  direction = "both",
  scope = formula(mod_total)
)

# R^2 ajustado = 0.8196.
# F = p-value: < 2.2e-16
summary(mod_both)


## 5.3) Métricas para seleccionar el mejor modelo ----

### 5.3.1) Tabla AIC ----

# Definir objeto tbl_aic para generar tabla de valor AIC en MRLM automáticos
tbl_aic <- cbind(
  AIC(mod_backward),
  AIC(mod_forward),
  AIC(mod_both)
) |>
  as.data.frame()

# Definir columnas y filas de tbl_aic
colnames(tbl_aic) <- c(
  "Backward",
  "Forward",
  "Both"
)
rownames(tbl_aic) <- "AIC"

# Visualizar Tabla AIC
tbl_aic

### 5.3.2) Tabla BIC ----

# Definir objeto tbl_bic para generar tabla de valor BIC en MRLM automáticos
tbl_bic <- cbind(
  BIC(mod_backward),
  BIC(mod_forward),
  BIC(mod_both)
) |>
  as.data.frame()

# Definir nombres de columnas y filas de tbl_bic
colnames(tbl_bic) <- c(
  "Backward",
  "Forward",
  "Both"
)
rownames(tbl_bic) <- "BIC"

# Visualizar Tabla BIC
tbl_bic


## 5.4) Modelo forward paso a paso ----

# Paso 1
mod_forward_1 <- lm(PM2.5 ~ 1, data = df)
# Paso 2
mod_forward_2 <- lm(PM2.5 ~ NO2, data = df)
# Paso 3
mod_forward_3 <- lm(PM2.5 ~ NO2 + CO, data = df)
# Paso 4
mod_forward_4 <- lm(PM2.5 ~ NO2 + CO + Humed, data = df)
# Paso 5
mod_forward_5 <- lm(PM2.5 ~ NO2 + CO + Humed + O3, data = df)
# Paso 6
mod_forward_6 <- lm(PM2.5 ~ NO2 + CO + Humed + O3 + TMin, data = df)
# Paso 7
mod_forward_7 <- lm(PM2.5 ~ NO2 + CO + Humed + O3 + TMin + NO, data = df)
# Paso 8
mod_forward_8 <- lm(
  PM2.5 ~ NO2 + CO + Humed + O3 + TMin + NO + Viento,
  data = df
)


## 5.5) Tabla resumen modelo forward ----

tbl_mod_forward <- tab_model(
  list(
    mod_forward_1,
    mod_forward_2,
    mod_forward_3,
    mod_forward_4,
    mod_forward_5,
    mod_forward_6,
    mod_forward_7,
    mod_forward_8
  ),
  title = "Comparación de MRLM seleccionados por método forward",
  dv.labels = paste0("Modelo ", 1:8),

  show.ci = FALSE,
  #show.se = TRUE,
  show.r2 = TRUE,
  #show.fstat = TRUE,
  show.aic = TRUE,
  show.obs = TRUE,

  p.style = "stars",
  string.pred = "Predictor",
  string.est = "β",
  #string.se = "EE",
  string.p = "p",

  digits = 2,
  digits.p = 3,
  CSS = list(
    css.table = paste(
      "width: auto;",
      "margin-left: auto;",
      "margin-right: auto;",
      "font-size: 8pt;"
    ),
    css.caption = "text-align: center;",
    css.tdata = "padding: 0.15em 0.25em;",
    css.colnames = "padding: 0.15em 0.25em;"
  )
)
tbl_mod_forward


## 5.4) Respuesta Pregunta 3 ----

# Los procedimientos backward, forward y both seleccionaron el mismo modelo final,
# compuesto por las variables Viento, TMin, Humed, NO, NO2, CO y O3.
# Aunque las variables aparecen en distinto orden en las fórmulas,
# el conjunto de predictores es idéntico y, por consiguiente, los modelos son equivalentes.
# Por lo tanto, no existe un método superior según las métricas finales; los tres procedimientos convergen a la misma especificación.
# La diferencia entre ellos se encuentra únicamente en la trayectoria seguida durante la selección.

# 6) Pregunta 4 ----------------------------------------------------------

# Basado en los resultados previos, y si lo desea con fundamento en información genérica
# respecto a los efectos de las variables atmosférica y de contaminantes atmosféricos sobre el PM2.5,
# proponga un MODELO CON TRES PREDICTORES (debe incluir UNA variable meteorológica y DOS contaminantes).
# Indique si se cumplen o no los supuestos y evalúe con especial énfasis el problema de multicolinealidad.
# Apóyese de tablas de correlación, gráficos y métricas respectivas.

## 6.1) MRLM con tres predictores (1 metereológica y 2 contaminantes) ----

# Definir modelo PM2.5 ~ TMin + NO2 + CO.
mod_tres_pred <- lm(
  PM2.5 ~ TMin + NO2 + CO,
  data = df
)
summary(mod_tres_pred)


## 6.2) Chequeo de supuestos ----
gg_supuestos_mtp <- check_model(mod_tres_pred)
gg_supuestos_mtp

### 6.2.1) Linealidad ----

# Test de linealidad
residualPlots(mod_tres_pred)

# Test Ramsey
resettest(
  mod_tres_pred,
  power = 2:3,
  type = "fitted"
)

# p-value = 4.871e-05 <- NO se cumple linealidad de los residuos.

### 6.2.2) Normalidad ----

# Test de Shapiro-Wilk
shapiro.test(mod_tres_pred$residuals)

# W = 0.97541, p-value = 0.005909 <- NO se cumple normalidad de los residuos.

### 6.2.3) Homocedasticidad ----

# Test de Breusch-Pagan
bptest(mod_tres_pred)

# BP = 53.646, df = 3, p-value = 1.335e-11 <- NO se cumple homocedasticidad de los residuos.

### 6.2.4) Independencia ----

# Test de Durbin-Watson
dwtest(mod_tres_pred)
# DW = 2.1726, p-value = 0.8634 alternative hypothesis: true autocorrelation is greater than 0

### 6.2.5) Colinealidad ----

# Cálculo de Tabla VIF para mtp
tbl_vif_mtp <- vif(mod_tres_pred) |>
  as.data.frame()
tbl_vif_mtp

# Agregar nombre de columnas
colnames(tbl_vif_mtp) <- "VIF (mod_tres_predictores)"

# Visualizar tabla VIF_mtp
tbl_vif_mtp
