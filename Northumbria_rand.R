
library(dplyr)
data <- read_excel("C:/Users/d_val/Downloads/dummy data.xlsx") #change URL

#Create strata
data<-data[order(data$Rand_code),] #order data by ID
data$strata<-ifelse(data$LA_OCT>=median(data$LA_OCT) & data$WHO5>=median(data$WHO5),1,
                   ifelse(data$WHO5>=median(data$WHO5),2,
                          ifelse(data$LA_OCT>=median(data$LA_OCT),3,4)))

set.seed(123)
prop1<-0.333 #set proportion assigned to arm1
prop2<-0.333 #set proportion assigned to arm2
prop3<-1-(prop1+prop2) #implied proportion assigned to control
data<-data %>% group_by(strata) %>% mutate(treat={
    n <- n()
    allocation1 <- floor(n*prop1) #proportion of stratum allocated to arm1
    allocation2 <- floor(n*prop2) #proportion of stratum allocated to arm2
    assign <- c(rep(1, allocation1), rep(2, allocation2), rep(0,n- (allocation1+allocation2))) #vector of 1s (treat) and 0s (control)
    sample(assign) #randomise order of assignment within each stratum
  }) %>% ungroup()

table(data$strata,data$treat) #check allocation
