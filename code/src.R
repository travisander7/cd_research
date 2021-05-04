library(tidyverse)

# impact of beta blockers on occurrence of strokes and mortality
df <- read_csv(
 'Author,Ee,Ne,Ec,Nc
  Barber,10,52,12,47
  Norris,5,226,15,228
  Kahler,0,38,6,31
  Ledwich,2,20,3,20'
) %>%
  mutate(correction = Ee*Ne*Ec*Nc == 0) %>%
  mutate_if(is.numeric, function(x) ifelse(.$correction, x + 0.5, x)) %>%
  mutate(
    OR = (Ee*(Nc-Ec))/(Ec*(Ne-Ee)),
    var = 1/Ee + 1/(Nc-Ec) + 1/Ec + 1/(Ne-Ee),
    p_value1 = pnorm(-abs(log(OR)), 0, sqrt(var)),
    p_value2 = 2*p_value1
  )

add_cis <- function(df, alpha){
  df %>%
    mutate(
      lower = exp(qnorm(alpha/2, log(OR), sqrt(var))),
      upper = exp(qnorm(1 - alpha/2, log(OR), sqrt(var)))
    )
}

dfs <- c(0.01, 0.05, 0.1) %>%
  set_names %>%
  map(add_cis, df = df)

#################### First study CIs ####################
pdf('presentations/cis.pdf')
dfs %>%
  map_df(slice, 1, .id = 'alpha') %>%
  ggplot() +
  geom_point(aes(OR, alpha), col = 'blue', size = 3) +
  geom_errorbarh(aes(xmin = lower, xmax = upper, y = alpha)) +
  xlab('Odds ratio') +
  ylab('Level of significance') +
  ggtitle('Confidence intervals for different levels of significance')
dev.off()

#################### First study CD ####################
df1 <- slice(df, 1)

pdf('presentations/cd1.pdf')
cd_plot <- pvaluefunctions::conf_dist(
  estimate = log(df1$OR)
  , stderr = sqrt(df1$var)
  , type = "logreg"
  , plot_type = "p_val"
  , est_names = df1$Author
  , conf_level = 0.95
  , null_values = 0 # null value on the log-odds scale
  , trans = "exp"
  , alternative = "two_sided"
  , xlab = "Odds Ratio"
  , xlim = log(c(0.01, 2)) # axis limits on the log-odds scale
  , plot_counternull = TRUE
  , x_scale = "linear"
)
dev.off()

#################### All studies CD ####################
pdf('presentations/cd3.pdf')
cd_plot <- pvaluefunctions::conf_dist(
  estimate = log(df$OR)
  , stderr = sqrt(df$var)
  , type = "logreg"
  , plot_type = "p_val"
  , est_names = df$Author
  , conf_level = 0.95
  , null_values = 0 # null value on the log-odds scale
  , trans = "exp"
  , alternative = "two_sided"
  , xlab = "Odds Ratio"
  , xlim = log(c(0.01, 2)) # axis limits on the log-odds scale
  , plot_counternull = TRUE
  , x_scale = "linear"
  , together = TRUE
)
dev.off()

#################### Meta-analysis CD ####################
source("code/FullOriginalGmetaCode.R")
meta <- df %>%
  mutate_if(is.numeric, function(x) ifelse(.$correction, x - 0.5, x)) %>%
  select(Ee, Ne, Ec, Nc) %>%
  gmeta(
    gmi.type = "2x2", 
    method = "exact1", 
    gmo.xgrid = seq(-6, 6, by = 0.001), 
    report.error = TRUE
  )

meta_mean <- meta$combined.mean
meta_sd <- meta$combined.sd

pdf('presentations/cd_meta.pdf')
cd_plot <- pvaluefunctions::conf_dist(
  estimate = c(log(df$OR), meta_mean)
  , stderr = c(sqrt(df$var), meta_sd)
  , type = "logreg"
  , plot_type = "p_val"
  , est_names = c(df$Author, 'Meta')
  , conf_level = 0.95
  , null_values = 0 # null value on the log-odds scale
  , trans = "exp"
  , alternative = "two_sided"
  , xlab = "Odds Ratio"
  , xlim = log(c(0.01, 2)) # axis limits on the log-odds scale
  , plot_counternull = TRUE
  , x_scale = "linear"
  , together = TRUE
)
dev.off()