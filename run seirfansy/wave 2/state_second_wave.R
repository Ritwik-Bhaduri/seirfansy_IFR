library(dplyr)
library(arm)

flag = Sys.getenv("SLURM_ARRAY_TASK_ID") # use values 1 - 36

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


data_state = read.csv(url("https://api.covid19india.org/csv/latest/state_wise_daily.csv"))
colnames(data_state)[3:ncol(data_state)]

state_codes = c("AN", "AP", "AR", "AS", "BR", "CH", "CT", "DD", "DL", "DN", "GA", "GJ", "HP", "HR", "JH", "JK", "KA", "KL", "LA", 
                "MH", "ML", "MN", "MP", "MZ", "NL", "OR", "PB", "PY", "RJ" ,"SK" ,"TG" ,"TN", "TR", "UP", "UT", "WB", "India")
state = state_codes[flag]

## Hyperparamters
pop=c(434e3,497e5,126e4,309e5,99e6,106e4,322e5,52e3,19e6,344e3,182e4,627e5,686e4,254e5,319e5,125e5,641e5,348e5,274e3,
      1142e5,265e4,272e4,733e5,112e4,228e4,437e5,280e5,242e3,6.89e7,619e3,35.2e6,679e5,36.6e5,204.2e6,10.1e6,99e6)
N = pop[flag]

data = data_state[, c("Date_YMD", "Status", state)]
daily_confirmed = filter(data, Status %in% "Confirmed")[,3]
daily_recovered = filter(data, Status %in% "Recovered")[,3]
daily_deceased = filter(data, Status %in% "Deceased")[,3]
date = as.character(filter(data, Status %in% "Confirmed")[,1])
data = data.frame(date = date, "Daily.Confirmed" = daily_confirmed, 
                  "Daily.Recovered" = daily_recovered, "Daily.Deceased" = daily_deceased)

date_initial = which(data$date == date_start)
date_final = which(data$date == date_end)

## data initial
temp  = data
temp = temp[1:date_initial,]
data_initial_cum = colSums(temp[,-1])
data_initial_daily = tail(temp[,-1],1)
if(data_initial_cum[1] == 0) data_initial_cum[1] = 1
if(data_initial_daily[1] == 0) data_initial_daily[1] = 1
data_initial = unname(unlist(c(data_initial_cum, data_initial_daily)))

data_train = data[date_initial:date_final, ]
obsP_tr <- data_train[,"Daily.Confirmed"] ## Daily Positive
obsR_tr <- data_train[,"Daily.Recovered"] ## Daily Recovered
obsD_tr <- data_train[,"Daily.Deceased"] ##  Daily Deaths


mCFR = tail(cumsum(obsD_tr) / cumsum(obsD_tr+obsR_tr),1)
## Run predictR

data_multinomial = abs(data.frame("Confirmed" = obsP_tr, "Recovered" = obsR_tr, "Deceased" = obsD_tr))

dates = seq(as.Date(date_start), as.Date(date_end), by = 1)
phases = sapply(phases[-length(phases)], function(x)  which(dates == as.Date(x)))

Result = model_predictR(data = data_multinomial,init_pars=NULL,data_init = data_initial, T_predict =150,
                        #niter = 3e5, BurnIn = 3e5, model = "Multinomial", N = N, lambda = 1/(69.416 * 365),
                        #mu = 1/(69.416 * 365), period_start = phases, opt_num = 1000,auto.initialize=T,alpha_u=0.5,f=0.15,
                        niter = 3e5, BurnIn = 3e5, model = "Multinomial", N = N, lambda = 1/(69.416 * 365),
                        mu = 1/(69.416 * 365), period_start = phases, opt_num = 1000,auto.initialize=T,alpha_u=0.5,f=0.15,
                        plot = FALSE, save_plots = FALSE)

saveRDS(Result$prediction,paste0("Prediction_", state, ".rds"))
saveRDS(Result$mcmc_pars,paste0("Prediction_pars_", state, ".rds"))