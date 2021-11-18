library(tidyverse)
source('code/get_cd.R')

filepath <- 'data/Datasets.xlsx'
col_names <- c('trt.events', 'trt.total', 'ctrl.events', 'ctrl.total')

read_data <- function(range, cols){
  readxl::read_excel(filepath, range = range) %>%
    select(unlist(cols)) %>%
    `names<-`(col_names)
}

meta <- tibble(
  sheet = c(
    'Streptokinase', 'MI and Death for Rosiglitazone', 'PCI vs MED', 
    'Calcium and Pregnancy', 'Promotion', 'Heparin', 'Dopamine', 
    'Anitibiotics for Rheumatic Feve', 'Hormone Replacement'
  ),
  cells = c('D1:G23', 'B1:E49', 'B2:E19', 'C1:F14', 'B1:E11', 'B1:E7', 'B2:E18', 'B1:E17', 'B1:E24'),
  range = str_c(sheet, '!', cells),
  cols = list(
    c('Strepto Death', 'Strepto Total', 'Control Death', 'Control Total'),
    c('Rosiglitazone MI', 'Rosiglitazone Total', 'Control MI', 'Control Total'),
    c('PCI_Death', 'PCI_Total', 'MED_Death', 'MED_Total'),
    c('Calcium_Event', 'Calcium_Total', 'Placebo_Event', 'Placebo_Total'),
    c('White_Promoted', 'White_Eligible', 'Black_Promoted', 'Black_Eligible'),
    c('Heparin_Event', 'Heparin_Total', 'Control_Event', 'Control_Total'),
    c('Dopamine_Event', 'Dopamine_Total', 'Control_Event', 'Control_Total'),
    c('Antibiotics_Event', 'Antibiotics_Total', 'Placebo_Event', 'Placebo_Total'),
    c('Trt_Event', 'Trt_Total', 'Ctrl_Event', 'Ctrl_Total')
  )
)

sheets <- meta %>%
  select(range, cols) %>%
  pmap(read_data) %>%
  set_names(meta$sheet) %>%
  map_df(summarize, total_events = sum(trt.events + ctrl.events), .id = 'sheet') %>%
  right_join(meta, 'sheet') %>%
  arrange(total_events)

# Breaks on 4 and above
df <- sheets %>%
  slice(6) %>%
  select(range, cols) %>%
  pmap(read_data) %>%
  pluck(1)

df %>%
  summarize_all(sum) %>%
  summarize(trt.events/trt.total, ctrl.events/ctrl.total)
# 23 studies, about 0.4% prob, theta is 0.59 or 0.76

perm <- with(df, rema::rema(trt.events, trt.total, ctrl.events, ctrl.total, alpha = 0.005))
only_one <- nrow(perm$dist) == 1
extreme <- perm$tstat %in% range(perm$dist$test.stat)

perm_ci <- log(perm$CI)
thetas <- seq(max(perm_ci[1], -15), min(perm_ci[2], 15), by = 0.005) 
thetas <- seq(-2, 3, 0.005)
perm_cd <- get_cd(thetas, perm$dist$norm.probs, perm$dist$test.stat, perm$tstat)
liu <- gmeta::gmeta(
  with(df, cbind(trt.events, trt.total, ctrl.events, ctrl.total)),
  gmi.type = '2x2',
  method = 'exact1',
  gmo.xgrid = thetas
)

# dyn.load(TMB::dynlib('code/meta2x2_re_full_new'))
# m_ii_cc_ff <- with(sim, ii_cc_ff(TRT_event, TRT_n, CTRL_event, CTRL_n, thetas))

tibble(thetas, perm = perm_cd, liu = liu$combined.cd) %>%
  pivot_longer(-thetas, names_to = 'method', values_to = 'distribution') %>%
  mutate(curve = 1-2*abs(0.5-distribution)) %>%
  ggplot(aes(thetas, curve, col = method)) +
  geom_line()
