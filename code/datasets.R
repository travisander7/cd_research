library(tidyverse)

filepath <- 'data/Datasets.xlsx'
col_names <- c('trt.events', 'trt.total', 'ctrl.events', 'ctrl.total')

meta <- tibble(
  sheet = readxl::excel_sheets('data/Datasets.xlsx'),
  cells = c('D1:G23', 'B1:E49', 'B2:E19', 'C1:F14', 'B1:E42'),
  range = str_c(sheet, '!', cells),
  cols = list(
    c('Strepto Death', 'Strepto Total', 'Control Death', 'Control Total'),
    c('Rosiglitazone MI', 'Rosiglitazone Total', 'Control MI', 'Control Total'),
    c('PCI_Death', 'PCI_Total', 'MED_Death', 'MED_Total'),
    c('Calcium_Event', 'Calcium_Total', 'Placebo_Event', 'Placebo_Total'),


  )
)

a <- meta %>%
  slice(1)

df <- readxl::read_excel(filepath, range = a$range) %>%
  select(unlist(a$cols)) %>%
  `names<-`(col_names)
  
m <- with(df, rema::rema(trt.events, trt.total, ctrl.events, ctrl.total))

df <- readxl::read_excel(filepath, 'MI and Death for Rosiglitazone', 'B1:E49')
m <- with(df, rema::rema(`Rosiglitazone MI`, `Rosiglitazone Total`, `Control MI`, `Control Total`))
