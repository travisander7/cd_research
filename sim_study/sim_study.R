library(tidyverse)
source('code/model_data.R')
source('code/get_cd.R')
source('code/ii_cc_ff.R')
source('code/0_icf_code.R')
source('code/pRandom_flexible.R')

future::plan('multisession', workers = 32)

dir('data/datasets', full.names = TRUE) %>%
	furrr::future_map(model_data, .options = furrr::furrr_options(seed = 1)) %>%
	bind_rows %>%
	write_csv('data/sim.csv')
	
