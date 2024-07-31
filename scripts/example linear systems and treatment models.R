# https://www.jobnmadu.com/r-blog/2023-07-31-r-markdown/linearsystems/
# https://jobnmadu.github.io/Dyn4cast/

install.packages(c("forecast", "lubridate", "Metrics", "tidyr", "ggplot2", 
                   "magrittr", "formattable", "xlsx", "readxl"))
devtools::install_github("JobNmadu/Dyn4cast")
library(Dyn4cast)

y <- linearsystems$MKTcost
x <- select(linearsystems, -MKTcost)
Model1 <- Linearsystems(y, x, 6, 15)

# https://www.jobnmadu.com/r-blog/2024-07-29-r-markdown/treatmentmodels/

# Estimation of treatment effects model
Treatment <- treatments$treatment
data <- treatments[, c(2:3)]
treat <- treatment_model(Treatment, data)

# Estimated treatment effects model
summary(treat)

# Estimated various treatment effects
treat$Effect

# Estimated propensity scores from the model
head(treat$P_score)

# fitted values from the model
head(treat$Fitted_estimate)

# Residuals of the estimated model
tail(treat$Residuals)

##### Plots of the propensity scores from the model

# Treatment effects (ATE)
treat$`ATE plot`

# Treatment effects on the treated (ATT)
treat$`ATT plot`

# Treatment effects on the evenly matched (ATM)
treat$`ATM plot`
