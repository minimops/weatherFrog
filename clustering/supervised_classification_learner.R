#supervised classification

library(mlr3)

# get all classification learners from mlr_learners:
lrns = mlr_learners$mget(mlr_learners$keys("^classif"))
names(lrns)

# get a specific learner from mlr_learners:
lrn = lrn("classif.rpart")
print(lrn)


source("clustering/ClusterAssesmentHelper.R")
origdat <- attachGwl(readRDS("Data/cli_data_05_avgDay_wide.rds"))
origdat[, ":=" (date = NULL, gwl = as.factor(gwl))]
origdat <- origdat[gwl != "U"]

source("clustering/Var_Extraction_Method/f_extr_funs.R")
newdat <- attachGwl(scaleNweight(extrapolate(seq(2006, 2010))))
newdat[, ":=" (date = NULL, gwl = as.factor(gwl))]
newdat <- newdat[gwl != "U"]


task1 = TaskClassif$new("measuredat", origdat, target = "gwl")
task2 = TaskClassif$new("extrdat", newdat, target = "gwl")


#randomly pick 75percent of rows
set.seed(1234)
train <- sample(seq_len(nrow(origdat)), round(nrow(origdat) * 0.75))
#or pick 4 of 5 years
train <- seq_len(round(4 / 5 * nrow(origdat)))


#measure data
lrn$train(task1, train)
predorg <- lrn$predict(task1, seq_len(nrow(origdat))[-train])
predorg$confusion
measure <- msr("classif.acc")
predorg$score(measure) 
#0.333 random
#0.219 4years
#0.335 random and without gwl "U"


#extraction data
lrn$train(task2, train)
predext <- lrn$predict(task2, seq_len(nrow(origdat))[-train])
predext$confusion
measure <- msr("classif.acc")
predext$score(measure) 
#0.254 random
#0.183 4years
#0.306 random and without gwl "U"
#seems to be doing worse...

