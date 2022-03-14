ptcasur <- read.csv(file.choose(), header = TRUE)
View(ptcasur)
nptca <- ptcasur[ptcasur$Days !=0,]
nptca <- ptcasur[ptcasur$Days !=-220,]
View(nptca)
km <- with(nptca, Surv(Days, OccurrenceofDeath))
km_fit <- survfit(Surv(Days, OccurrenceofDeath) ~ 1, data=nptca)
summary(km_fit, times = c(1,5,10*(1:5)))
#plot(km_fit, xlab="Days", main = 'Kaplan Meyer Plot') #base graphics is always ready
autoplot(km_fit)
km_trt_fit <- survfit(Surv(Days, OccurrenceofDeath) ~ Gender, data=nptca)
autoplot(km_trt_fit)
km_trt_fit1 <- survfit(Surv(Days, OccurrenceofDeath) ~ nptca$State_A, data=nptca)
autoplot(km_trt_fit1)
list(nptca$Days)
write.csv(nptca,"E:\\Prof.Bhanu Duggal\\Data Analysis\\sur.csv", row.names = FALSE)
km_trt_fit2 <- survfit(Surv(Days, OccurrenceofDeath) ~ nptca$Age_Class2, data=nptca)
autoplot(km_trt_fit2)
str(nptca)
# Fit Cox Model
nptca$State_A <- factor(nptca$State_A)
nptca$Gender <- factor(nptca$Gender)
nptca$Age_Class2 <- factor(nptca$Age_Class2)
str(nptca)
cox <- coxph(Surv(Days, OccurrenceofDeath) ~ AgeYears+State_A1+Gender, data = nptca)
summary(cox)
cox_fit <- survfit(cox)
autoplot(cox_fit)
newfit <- survfit(Surv(Days, OccurrenceofDeath) ~ Age_Class2+State_A1+Gender, data = nptca)
print(newfit)
summary(newfit)
summary(newfit)$table
accessvalue <- data.frame(time = newfit$time,
                n.risk = newfit$n.risk,
                n.event = newfit$n.event,
                n.censor = newfit$n.censor,
                surv = newfit$surv,
                upper = newfit$upper,
                lower = newfit$lower
)
head(accessvalue)
#Visualize survival curves
ggsurv <- ggsurvplot(newfit, fun = "event", conf.int = TRUE,
                     ggtheme = theme_bw())
ggsurv