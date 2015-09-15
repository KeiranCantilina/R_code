#data <- read.csv ("/Users/atruhlar/Dropbox/Cornell/Research/New_data/Ecoli_normalized.csv")
data <- read.csv ("C://Users//Ross//Desktop//R_code//Trial2_CRmorphotypeID.csv")
#data$Farm <- factor(data$Farm)
data$Farm <- factor(data$Farm)
shapiro.test(data$Morph3_prop)

bartlett.test(Morph3_prop~Location, data=data)


## First attempt

model <- aov(Morph3_prop~Location*Farm, data=data)
summary(model)
TukeyHSD(model)


## Subset by farm

Farm1 <- subset(data, Farm==1)
t.test(Morph3_prop~Location, data = Farm1)
	# p-value = 0.05945, slightly higher rdar proportion in drain pipe than manure storage

Farm2 <- subset(data, Farm==2)
t.test(Morph3_prop~Location, data = Farm2)
	# p-value = 0.2193, slightly higher rdar proportion in drain pipe than manure storage

Farm3 <- subset(data, Farm==3)
t.test(Morph3_prop~Location, data = Farm3)
	# p-value = 0.03884, lower rdar proporation in drain pipe than manure storage


## Subset by location

manure_storage <- subset(data, Location=="Manure_Storage")
model_manure_storage <- aov(Morph3_prop~Farm, data = manure_storage)
summary(model_manure_storage)
	# p-value = 0.0565.  Manure applied at each farm was not significantly different
	
drain_pipe <- subset(data, Location=="Drain_Pipe")
model_drain_pipe <- aov(Morph3_prop~Farm, data = drain_pipe)
summary(model_drain_pipe)
TukeyHSD(model_drain_pipe)
	# p-value = 0.0001 for 3-1, 0.002 for 3-2.  Not signficant for 2-1