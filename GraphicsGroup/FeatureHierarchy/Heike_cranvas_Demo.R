library(classdata)
library(cranvas)

qbudget <- qdata(budget)
qscatter(`Release Date`, y = `Production Budget`, data=qbudget)
qscatter(x = `Domestic Gross`, y = `Worldwide Gross`, data=qbudget)
