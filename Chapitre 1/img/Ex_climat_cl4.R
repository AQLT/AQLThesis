# id <- c(manuf ="001585934",
#         C1 = "001786503",
#         C3 = "001786504",
#         C4 = "001786505",
#         C5 = "001786506"
#         )
library(insee)
library(rjd3filters)
library(rjd3x11plus)
library(ggplot2)
library(patchwork)
library(zoo)
library(forecast)
if (basename(normalizePath(".")) == "DT") {
    dir <- "img/ex" # inside the sub-project DT
} else {
    dir <- "DT/img/ex" # inside the project will all the programs
}
if (!dir.exists(dir))
  dir.create(dir)

# Import des données
y <- data.frame(insee::get_insee_idbank("001786505"))
y <- y[order(y[,"TIME_PERIOD"]), ]
first_date <- as.numeric(strsplit(y$TIME_PERIOD[1], "-")[[1]])
y <- ts(y$OBS_VALUE, start = first_date, frequency = 12)
y <- window(y, end = c(2023, 5))
saveRDS(y, file.path(dir, "climat_c4.RDS"))
y <- readRDS(file.path(dir, "climat_c4.RDS"))

y <- window(y, start = 2010)
last_dates <- c(tail(time(y), 7))
names(last_dates) <- as.character(zoo::as.yearmon(last_dates))
der_est <- lapply(last_dates, function(x) window(y, end = x))

# MM de longueur 13 adaptées : 
select_trend_filter(y)
# Estimation finale
tc_f <- henderson(y, length = 13, musgrave = FALSE) # Estimation finale
plot(y)
lines(tc_f, col = "red")

p <- forecast::autoplot(ts.union(y, tc_f)) +
  theme_bw() +
  theme(legend.background = element_rect(fill = alpha('gray99', 0.4),
                                         colour = "gray80", linetype = "solid"),
        legend.justification = c(0,0),
        legend.position = c(0,0),
        legend.key = element_blank(),
        legend.title = element_blank()) +
  labs(x = NULL, y = NULL)  +
  scale_color_discrete(labels =c("Série initiale","Série lissée"))
p
ggMultisave <- function (filename, out = c("pdf", "jpg", "svg"), ...) 
{
  invisible(lapply(sprintf("%s.%s", gsub("\\.$", "", filename), 
                           out), ggplot2::ggsave, ...))
}
ggMultisave(file.path(dir, "tc_finale"), 
            plot = p,
            width = 8, height = 3)

## Méthode polynomiale

# Calcul des IC-ratios : 
icr <- sapply(der_est, function(x) {
  ic_ratio(x, henderson(x, length = 13, musgrave = FALSE))
})
lp_est <- lapply(c("LC", "QL", "CQ", "DAF"), function(method) {
  res <- lapply(seq_along(icr), function(i) {
    lp_coef <- lp_filter(horizon = 6,
                         kernel = "Henderson",
                         endpoints = method,
                         ic = icr[i])
    rjd3filters::filter(der_est[[i]], lp_coef)
  })
  names(res) <- names(der_est)
  res
})
lp_if <- lapply(c("LC", "QL", "CQ", "DAF"), function(method) {
  res <- lapply(seq_along(icr), function(i) {
    lp_coef <- lp_filter(horizon = 6,
                         kernel = "Henderson",
                         endpoints = method,
                         ic = icr[i])
    implicit_forecast(der_est[[i]], lp_coef)
  })
  names(res) <- names(der_est)
  res
})
names(lp_est) <- names(lp_if) <-  c("LC", "QL", "CQ", "DAF")

# Estimation locale des IC-ratios 
# On réplique l'estimation directe pour avoir 
# des estimateurs de la pente et de la concavité 

gen_MM <- function(p=6, q=p, d=2){
  X_gen <- function(d = 1, p = 6, q = p){
    sapply(0:d, function(exp) seq(-p, q)^exp)
  }
  k = rjd3filters::get_kernel("Henderson", h = p)
  k = c(rev(k$coef[-1]), k$coef[seq(0,q)+1])
  K = diag(k)
  X = X_gen(d=d, p = p, q = q)
  e1 = e2 = e3 = matrix(0, ncol = 1, nrow = d+1)
  e1[1] = 1
  e2[2] = 1
  e3[3] = 1
  # Estimateur de la constante
  M1 = K %*% X %*% solve(t(X) %*% K %*% X, e1)
  # estimateur de la pente
  M2 = K %*% X %*% solve(t(X) %*% K %*% X, e2)
  # estimateur de la concavité
  M3 = K %*% X %*% solve(t(X) %*% K %*% X, e3)
  mm <- list(const = M1, pente = M2, concav = M3)
  lapply(mm, moving_average, lags = -p)
}
all_mm <- lapply(6:0, gen_MM, p = 6, d = 2)
est_pente <- finite_filters(all_mm[[1]]$pente,
                            lapply(all_mm[-1], `[[`, "pente"))
est_concav <- finite_filters(all_mm[[1]]$concav,
                             lapply(all_mm[-1], `[[`, "concav"))

henderson_f <- lp_filter(h=6)@sfilter
lp_filter2 <- function(icr, method = "LC", h = 6, kernel = "Henderson"){
  all_coef = lapply(icr, function(ic){
    lp_filter(horizon = h,
              kernel = kernel,
              endpoints = method,
              ic = ic)
  })
  rfilters = lapply(1:h, function(i){
    q=h -i
    all_coef[[i]][,sprintf("q=%i", q)]
  })
  finite_filters(henderson_f, rfilters = rfilters)
}
loc_lc_est <- 
  lapply(der_est, function(x) {
    est_loc_pente <- c(tail(est_pente * x, 6))
    sigma2 <- var_estimator(x, henderson_f)
    icr = 2/(sqrt(pi) * (est_loc_pente / sqrt(sigma2)))
    lp_coef = lp_filter2(ic = icr, method = "LC", h = 6, kernel = "Henderson")
    rjd3filters::filter(x, lp_coef)
  })
sapply(der_est, function(x) {
  est_loc_pente <- c(tail(est_pente * x, 6))
  sigma2 <- var_estimator(x, henderson_f)
  icr = 2/(sqrt(pi) * (est_loc_pente / sqrt(sigma2)))
  abs(icr)
})
est_pente * der_est[[1]]

loc_lc_if <- 
  lapply(der_est, function(x) {
    est_loc_pente <- c(tail(est_pente * x, 6))
    sigma2 <- var_estimator(x, henderson_f)
    icr = 2/(sqrt(pi) * (est_loc_pente / sqrt(sigma2)))
    lp_coef = lp_filter2(ic = icr, method = "LC", h = 6, kernel = "Henderson")
    implicit_forecast(x, lp_coef)
  })
loc_ql_est <- 
  lapply(der_est, function(x) {
    est_loc_concav <- c(tail(est_concav * x, 6))
    sigma2 <- var_estimator(x, henderson_f)
    icr = 2/(sqrt(pi) * (est_loc_concav / sqrt(sigma2)))
    lp_coef = lp_filter2(ic = icr, method = "QL", h = 6, kernel = "Henderson")
    rjd3filters::filter(x, lp_coef)
  })
loc_ql_if <- 
  lapply(der_est, function(x) {
    est_loc_concav <- c(tail(est_concav * x, 6))
    sigma2 <- var_estimator(x, henderson_f)
    icr = 2/(sqrt(pi) * (est_loc_concav / sqrt(sigma2)))
    lp_coef = lp_filter2(ic = icr, method = "QL", h = 6, kernel = "Henderson")
    implicit_forecast(x, lp_coef)
  })
# Pour la méthode RKHS, pour le filtre b_q,phi on utilise 
# le paramètre bw de l'article Dagum et Bianconcini (2015) 
bw_phase <-c(`q=6` = 7, `q=5` = 6.95, `q=4` = 6.84, `q=3` = 6.85, `q=2` = 7.34, 
             `q=1` = 9.24, `q=0` = 11.78)
rkhs_filter <- list(
  "$b_{q,\\Gamma}$" = rkhs_filter(horizon = 6, degree = 3,
                                  asymmetricCriterion = "FrequencyResponse",
                                  kernel = "Biweight", passband = pi),
  "$b_{q,G}$" = rkhs_filter(horizon = 6, degree = 3,
                            asymmetricCriterion = "Accuracy",
                            kernel = "Biweight", passband = pi),
  "$b_{q,\\phi}$" = finite_filters(
    do.call(cbind, lapply(seq_along(bw_phase), function(i){
    rkhs_filter(horizon = 6, degree = 3,
                kernel = "Biweight",
                optimalbw = FALSE,
                bandwidth = bw_phase[i])[,names(bw_phase)[i]]
  })))
)
rkhs_est  <- lapply(rkhs_filter, function(method) {
  lapply(der_est, function(x) {
    x * method
  })
})

rkhs_if <- lapply(rkhs_filter, function(method) {
  lapply(der_est, function(x) {
    implicit_forecast(x, method)
  })
})

fst_f <- finite_filters(
  sfilter = fst_filter(lags = 6, leads = 6, pdegree=2, 
                       smoothness.weight=0.05, smoothness.degree=3,
                       timeliness.weight=0.95,
                       timeliness.passband=2*pi/12,
                       timeliness.antiphase=TRUE),
  rfilters = lapply(5:0, fst_filter, lags = 6, pdegree=2, 
                    smoothness.weight=0.05, smoothness.degree=3,
                    timeliness.weight=0.95,
                    timeliness.passband=2*pi/12,
                    timeliness.antiphase=TRUE))
fst_est <- lapply(der_est, function(x) {
  x * fst_f
})
fst_if <- lapply(der_est, function(x) {
  implicit_forecast(x, fst_f)
})

## Graphiques

# Pour tracer toutes les estimations
plot_est <- function(data, nperiod = 6) {
  joint_data <- do.call(ts.union, data)
  joint_data <- 
    window(joint_data, 
           start = last_dates[1] - nperiod * deltat(joint_data))
  
  data_legend <- data.frame(x = last_dates,
                            y = sapply(data, tail, 1),
                            label = colnames(joint_data))
  
  forecast::autoplot(joint_data) + theme_bw() +
    scale_x_continuous(labels = zoo::as.yearmon) +
    geom_text(aes(x = x, y = y, label = label, colour = label),
              data = data_legend,
              check_overlap = TRUE, hjust = 0, nudge_x = 0.01,
              size = 2, inherit.aes = FALSE) +
    theme(legend.position = "none")  +
    labs(x = NULL, y = NULL)
}

plot_prevs <- function (data, nperiod = 6) {
  joint_data <- do.call(ts.union, lapply(data, function(x) {
    first_date <- time(x)[1] - deltat(x)
    # On rajoute la dernière date observée par lisibilité
    ts(c(window(y, start = first_date, end = first_date), x), 
       start = first_date, frequency = frequency(x))
  }))
  
  data_legend <- data.frame(x = sapply(data, function(x) tail(time(x), 1)),
                            y = sapply(data, tail, 1),
                            label = colnames(joint_data))
  forecast::autoplot(joint_data, linetype = "dashed") + 
    forecast::autolayer(window(y,
                               start = last_dates[1] - nperiod * deltat(y)),
                        colour = FALSE) +
    theme_bw() +
    scale_x_continuous(labels = zoo::as.yearmon) +
    geom_text(aes(x = x, y = y, label = label, colour = label),
              data = data_legend,
              check_overlap = TRUE, hjust = 0, nudge_x = 0.01,
              size = 2, inherit.aes = FALSE) +
    theme(legend.position = "none") +
    labs(x = NULL, y = NULL)
}

all_est <- c(lp_est, list("LC param. locale" = loc_lc_est),
              list("QL param. locale" = loc_ql_est), 
              rkhs_est,
              list(FST = fst_est))
all_if <- c(lp_if, list("LC param. locale" = loc_lc_if),
            list("QL param. locale" = loc_ql_if), 
            rkhs_if,
            list(FST = fst_if))
y_lim <- c(102, 105)
all_plots_est <- lapply(
  names(all_est), 
  function(x) plot_est(all_est[[x]]) + 
    ggtitle(latex2exp::TeX(sprintf("Tendance-cycle avec %s", x))) +
    coord_cartesian(ylim = y_lim)
)
all_plots_prev <- lapply(
  names(all_if), 
  function(x) plot_prevs(all_if[[x]]) + 
    ggtitle(latex2exp::TeX(sprintf("Prévisions implicites avec %s", x)))
)

wrap_plots(all_plots_est[1:4], ncol = 2)
wrap_plots(all_plots_prev[1:4], ncol = 2)

wrap_plots(all_plots_est[c(1,5,2,6)], ncol = 2)
wrap_plots(all_plots_prev[c(1,5,2,6)], ncol = 2)

wrap_plots(c(all_plots_est[7:9],
             all_plots_prev[7:9]), nrow = 2)

wrap_plots(c(all_plots_est[10], all_plots_prev[10]), nrow = 1)



ggMultisave(file.path(dir, "lp_es"), 
            plot = wrap_plots(all_plots_est[1:4], ncol = 2),
            width = 8, height = 5)

ggMultisave(file.path(dir, "lp_if"), 
            plot = wrap_plots(all_plots_prev[1:4], ncol = 2),
            width = 8, height = 5)

ggMultisave(file.path(dir, "lp_local_es"), 
            plot = wrap_plots(all_plots_est[c(1,5,2,6)], ncol = 2),
            width = 8, height = 5)

ggMultisave(file.path(dir, "lp_local_if"), 
            plot = wrap_plots(all_plots_prev[c(1,5,2,6)], ncol = 2),
            width = 8, height = 5)

ggMultisave(file.path(dir, "rkhs"), 
            plot = wrap_plots(c(all_plots_est[7:9],
                                all_plots_prev[7:9]), nrow = 2),
            width = 8, height = 5)

ggMultisave(file.path(dir, "fst"), 
            plot = wrap_plots(c(all_plots_est[10], all_plots_prev[10]), nrow = 1),
            width = 8, height = 3)
