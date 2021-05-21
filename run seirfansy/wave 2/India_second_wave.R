library(dplyr)
library(arm)

flag = Sys.getenv("SLURM_ARRAY_TASK_ID") 

date_start= "2021-02-01"
date_end = "2021-05-15"
phases = c(as.Date("2021-02-01"), as.Date("2021-02-16"), as.Date("2021-03-01"), as.Date("2021-03-16"), 
           as.Date("2021-04-01"), as.Date("2021-04-16"), as.Date("2021-05-01"), as.Date("2021-05-15"))

source("par_initializeR.R")
source("model_estimateR.R")
source("model_initializeR.R")
source("mcmc_performR.R")
source("model_deterministic_simulateR.R")
source("R0_calculateR.R")
source("model_predictR.R")
source("model_stochastic_simulateR.R")
source("model_plotR.R")

## Load data on Covid cases in India

data = read.csv(url("https://api.covid19india.org/csv/latest/case_time_series.csv"))
data = data %>%
  mutate(Current.Confirmed = Total.Confirmed - Total.Recovered - Total.Deceased)
date_initial= which(data$Date_YMD == date_start)
date_final = which(data$Date_YMD == date_end)
data_initial = data[date_initial, ]
data_initial = unname(as.numeric(data_initial[1, c(4,6,8,3,5,7)]))
data_train = data[date_initial : date_final, ]
daily_confirmed <- data_train[,"Daily.Confirmed"] ## Daily Positive
daily_recovered <- data_train[,"Daily.Recovered"] ## Daily Recovered
daily_deaths <- data_train[,"Daily.Deceased"] ##  Daily Deaths

data_country_confirmed <- data_train[ ,"Total.Confirmed"]
data_country_recovered <- data_train[ ,"Total.Recovered"]
data_country_deaths <- data_train[ ,"Total.Deceased"]

N = 1341e6 #population of India
country_name = "India"

data_multinomial = data.frame("Confirmed" = daily_confirmed, "Recovered" = daily_recovered, "Deceased" = daily_deaths)
rownames(data_multinomial) = 1:nrow(data_multinomial)

mcfr = tail(data_country_deaths,1) / (tail(data_country_deaths,1) + tail(data_country_recovered,1))

dbirth_rate_current_country = 1/(365 * 69.4)  #average lifespan - 72.6 years

dates = seq(as.Date(date_start), as.Date(date_end), by = 1)
phases = sapply(phases[-length(phases)], function(x)  which(dates == as.Date(x)))

Result = model_predictR(data = data_multinomial,init_pars=NULL,data_init = data_initial, T_predict = 150,
                        niter = 3e5, BurnIn = 3e5, model = "Multinomial", N = N, lambda = 1/(69.416 * 365),
                        mu = 1/(69.416 * 365), period_start = phases, opt_num = 1000, auto.initialize=T,alpha_u=0.5,f=0.15,
                        plot = TRUE, save_plots = TRUE)

saveRDS(Result$plots, paste0("plots_", country_name, "_", flag, ".rds"))
saveRDS(Result$prediction,paste0("Prediction_", country_name, "_", flag, ".rds"))
saveRDS(Result$mcmc_pars,paste0("Prediction_pars_", country_name, "_", flag, ".rds"))