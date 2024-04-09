getwd()
cars_df=read.csv("mtcars.csv")
View(cars_df)
str(cars_df)
dim(cars_df)
names(cars_df)
row.names(cars_df)
row.names(cars_df)=cars_df$model
cars_df=cars_df[,-1]
View(cars_df)

library(dplyr)

# select function - for extracting specific columns 
# df1 = select(cars_df,mpg:hp)
df1=cars_df %>% select(mpg:hp) #pipe of dplyr it will take out content of one column to the output of other column
View(df1)
df1 = cars_df %>% select(c(mpg,disp,wt,gear))
View(df1)

#Filter function - for extracting specific rows or observation
#extract record where gears=4 and columns to be displayed are mpg, disp, wt and gears.
df1 = cars_df %>% filter(gear==4) %>% select(c(mpg,disp,wt,gear))
View(df1)

# extract record where cyl=4 or mpg>20 and columns are required are mpg,cl
df1 = cars_df %>% filter(cyl==4 | mpg > 20) %>% select(c(mpg,cyl))
View(df1)

#extract records where mpg<20 and carb = 3 and coumns needed are mpg and carb
df1 = cars_df %>% filter(mpg < 20 & carb == 3) %>% select(c(mpg,carb))
View(df1)

# Arrange function -Sort as per specific columns
df1 =cars_df %>% arrange(cyl,desc(mpg))
View(df1)

#Rename function - change names of one or more column
df1 = cars_df %>% rename(MilesPerGallon=mpg,Cylinders=cyl,Displacement=disp)
View(df1)

#Mutate function - creating new columns on the basis of existing column
df1 = cars_df %>% mutate(Power=hp*wt)
View(df1)

#Group_by and summaries - segregating data as per categorical variable and summarizing
df1$gear = as.factor(df1$gear)
str(df1)

summary_df = df1%>% group_by(df1$gear) %>% summarise(no=n(), mean_mpg=mean(mpg), mean_wt=mean(wt))
summary_df

summary_df = df1%>% group_by(df1$Cylinders) %>% summarise(no=n(), mean_mpg=mean(mpg), mean_wt=mean(wt))
summary_df


#Data Visualization
#histogram - for single column frequency
hist(df1$mpg, main="Histogeam of MilePergallon(mtcars)",col="lightgreen",xlab="Miles Per Gallon")
#box plot - diagrammatic representation of summary
summary(df1$mpg)
boxplot(df1$mpg)

#bar plot - categorical variable representation'
table(df1$gear)
barplot(table(df1$gear))

#scatter plot - plot() - plots relationship between two variable
plot(df1$mpg~df1$disp)
plot(df1$mpg~df1$cyl)
plot(df1$mpg~df1$wt)
