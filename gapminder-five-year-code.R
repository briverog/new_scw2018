#Reading in the data
gapminder = read.csv("gapminder-FiveYearData.csv")

head(gapminder)
str(gapminder)
summary(gapminder)

# shift + command (or control) +m --> para escribir %>% 
gapminder %>% group_by(country) %>% tally()
gapminder %>%  group_by(country) %>% summarise(avg = mean(pop),
                                               std = sd(pop), total = n())
#Use arrange function:
gapminder %>% group_by(country) %>% summarise(avg=mean(pop),std = sd(pop), total = n()) %>% 
  arrange(avg)
#to sort by ave desc
gapminder %>% group_by(country) %>% summarise(avg=mean(pop),std = sd(pop), total = n()) %>% 
  arrange(desc(avg))

#Mutate:
gapminder_mod = gapminder
gapminder_mod = gapminder_mod %>% mutate(gdp = pop * gdpPercap) %>% head()
gapminder_mod = gapminder_mod %>% mutate(gdp = pop * gdpPercap)

# Calculate the average life expectancy per country. Which nation has the longest average life 
# expectancy and which has the shortest average life expectancy? 
gapminder %>% group_by(country) %>% summarise(avg=mean(lifeExp),std = sd(lifeExp), total = n()) %>% 
  arrange(avg) %>% head(1)
gapminder %>% group_by(country) %>% summarise(avg=mean(lifeExp),std = sd(lifeExp), total = n()) %>% 
  arrange(desc(avg)) %>% head(1)
# to print head and tail at the same time: filter:
gapminder %>% group_by(country) %>% summarise(avg=mean(lifeExp)) %>% 
  filter(avg == max(avg) | avg == min(avg))


#### Plotting
#in base r:
plot(x = gapminder_mod$gdpPercap, y = gapminder_mod$lifeExp)

##GGPLOT2
#data
#grammar of graphics (AES)
#geom

library(ggplot2)
ggplot(gapminder_mod, aes(x = gdpPercap, y= lifeExp)) + geom_point()
#log10 conversion
ggplot(gapminder_mod, aes(x = log10(gdpPercap), y= lifeExp)) + geom_point()

#make points transparents with alpha
ggplot(gapminder_mod, aes(x = log10(gdpPercap), y= lifeExp)) +
       geom_point(alpha = 1/3, size = 3)

# color 
summary(gapminder_mod) ## ahora vemos que hay 5 continentes

p <- ggplot(gapminder_mod, aes(x = log10(gdpPercap), y= lifeExp, color = continent))+
        geom_point()

#split the graph by continents
p + facet_wrap(~continent)
# to keep working on the same graph, we can assign again to a variable, and add something else
p = p + facet_wrap(~continent)
p2 = p + geom_smooth(color =  "orange")
p2

#Combine dplyr with ggplot2

gapminder_mod %>% ggplot(aes(gdpPercap,lifeExp)) + geom_point()

gapminder %>%  mutate (gdp = pop * gdpPercap) %>% 
  ggplot(aes(gdp, lifeExp)) + geom_point()

# Histrogram
#Try plotting a histogram of GDP per capita, gdpPercap.
p3 <- ggplot(gapminder_mod, aes(lifeExp, fill = continent)) + geom_histogram(binwidth = 1)+
      ggtitle("Histogram_gapminder")
p3

#saving plots

ggsave(p3, file="~/scw_2018/advanced.R/histogram_lifeExp.png")

#line plot
gapminder_mod %>% filter(country == "Afghanistan") %>% 
  ggplot(aes(x=year, y = lifeExp)) + geom_line(color = "purple")
#2. Plot lifeExp against year and facet by continent and fit a smooth 
#and/or linear regression, w/ or w/o facetting

p5 <- ggplot(gapminder_mod, aes(x=lifeExp, y=year, color = continent)) + geom_point() + 
  facet_wrap(~continent) + geom_smooth(color =  "black", lwd = 1, se = F)
  
#lwd es el grosor de la linea. se = F, quita la sombra automatica alrededor de la linea de dispersion

p6 = p5 + geom_smooth(color = "orange", method = lm)
p6

ggsave(p6, file= "geom_smooth_type.png")


#density plot
p7 = ggplot(gapminder_mod, aes(gdpPercap, lifeExp)) +
  geom_point(size=0.25)+
  geom_density_2d() + scale_x_log10()

ggsave(p7, file= "densityPlot.png")


#combine plots:
library(gridExtra)
gridExtra::grid.arrange(
  p5 <- ggplot(gapminder_mod, aes(x=lifeExp, y=year, color = continent)) + geom_point() + 
    facet_wrap(~continent) + geom_smooth(color =  "black", lwd = 1, se = F),
  p7 = ggplot(gapminder_mod, aes(gdpPercap, lifeExp)) +
    geom_point(size=0.25)+
    geom_density_2d() + scale_x_log10()
)



### LOOPS
#synthax:
#for (variable in list){
#  do something
# }

gapminder_mod %>% filter(continent == "Asia") %>% 
  summarise(avg = mean(lifeExp))

contin <- unique(gapminder_mod$continent)
contin

for (c in contin){
  for (y in unique(gapminder_mod$year)){
    #print(c)
    res <- gapminder_mod %>% filter(continent == c) %>% 
      summarise(avg = mean(lifeExp))
    print(paste0("The avg life expectancy of ", c,"for the year", y, "is: ", res))
  }
}

##with dplyr
gapminder_mod %>% filter(continent == "Asia") %>% 
  summarise(avg = mean(lifeExp))

# Functions:
mean(2,3)

adder <- function(x,y){
  print(paste0("The sum of", x, "and", y, "is: ", x+y))
  #return(x + y)
}

adder(2,3)










gapminder_mod %>% group_by(continent, year) %>% 
  summarise(avg = mean(lifeExp))

gapminder_mod %>% filter(continent == "Asia") %>% 
  summarise(avg = mean(lifeExp))





