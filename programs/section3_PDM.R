# Install wbstats package
install.packages("wbstats")

# Load libraries
library(tidverse)
library(stargazer)
library(wbstats)

# Load world bank data
wb_indicators <- wb_indicators()

# Load libraries
library(tidyverse)
library(stargazer)
library(wbstats)
library(ggplot2)
library(plyr)

# Define paths 
dir <- "C:/Users/eline/Documents/BAM/blok1/ASP/BAM_ASP_A2/"
dir <- "C:/Users/ppria/Documents/BAM_ASP_A2/"
dirProg <- paste0(dir, "programs/")
dirData <- paste0(dir, "data/")
dirRslt <- paste0(dir, "results/")

# Load world bank data
dfExport <- wb_data(indicator=c("IC.EXP.TMBC",       # Time to export
                                "NY.GDP.PCAP.CD",    # GDP per capita
                                "TG.VAL.TOTL.GD.ZS", # Merchandise trade % GDP
                                "NE.EXP.GNFS.ZS",    # Exports of goods and services (% of GDP)
                                "IC.EXP.CSDC.CD"),   # Cost to export 
                    country = "countries_only", 
                    start_date = 2014, 
                    end_date = 2019)

# Load and save file with variables of interest
save(dfExport, file=paste0(dirData, "SelectedWorldData.sav"))
load(file=paste0(dirData, "SelectedWorldData.sav"))

# Rename column names 
colnames(dfExport)[colnames(dfExport) == "date"]              <- "Year"
colnames(dfExport)[colnames(dfExport) == "country"]           <- "Country"
colnames(dfExport)[colnames(dfExport) == "date"]              <- "Year"
colnames(dfExport)[colnames(dfExport) == "IC.EXP.TMBC"]       <- "TimeExport"
colnames(dfExport)[colnames(dfExport) == "NY.GDP.PCAP.CD"]    <- "GDPPerCap"
colnames(dfExport)[colnames(dfExport) == "TG.VAL.TOTL.GD.ZS"] <- "MerchandiseGDP"
colnames(dfExport)[colnames(dfExport) == "NE.EXP.GNFS.ZS"]    <- "ExportGoodsServices"
colnames(dfExport)[colnames(dfExport) == "IC.EXP.CSDC.CD"]    <- "CostExport"

# Subset complete observations, and implement an admittedly arbitrary 
# observation period
dfExport.sub <- dfExport[complete.cases(dfExport),]

# Generate list with all countries with complete observations
complete <- dfExport.sub %>%
  count(Country) %>%
  filter(n == 6)
completeCountry <- as.vector(complete$Country)
library(dplyr)
library(plyr)
# Generate data frame only containing countries with complete observations
dfExport.sub.cmplt <- dfExport.sub %>%
  filter(Country %in% completeCountry)

# Convert to data frame
dfExport.sub.cmplt <- as.data.frame(dfExport.sub.cmplt)

# Generate table with summary statistics
stargazer(dfExport.sub.cmplt, type="text")

```

## 1 

```{r}
# Plot Cost Export
plot_CostExport <- ggplot(dfExport.sub.cmplt, aes(x=CostExport, y=TimeExport, color=Country)) +
  geom_point() + geom_smooth()

plot_CostExport

subCountries <- c("Australia", "Bolivia", "Brazil", "Portugal", "Thailand", "Zimbabwe", "Bangladesh", "Bulgaria", "China", "Denmark", "France", "Finland", "India")
dfExport.sub.cmplt <- dfExport.sub.cmplt[dfExport.sub.cmplt$Country %in% subCountries,]
ggplot(dfExport.sub.cmplt, aes(x=CostExport, y=TimeExport))+
  #add the annual outcomes coloured by Country
  geom_point(aes(color=Country), size=1.5)+
  #add regression lines for the countries
  geom_smooth(method="lm", se=FALSE, colour="gray")+
  #label the axis
  xlim(0, 300) + ylim(0, 70)+
  xlab("Cost of Export")+ 
  ylab("Time of Export")+
  theme(axis.title= element_text(size=rel(1)),
        axis.text= element_text(size=rel(1)))+
  guides(colour = guide_legend(override.aes = list(size=1.5)))


#Intento de nueva grafica
####Panel Data: Plotting Total Variation



###Illustrate within and between variation for a subset of the data

#Select observation with non missing TimeExport and CostExport
dfExport.sub.cmplt<- subset(dfExport.sub, !is.na(TimeExport)& !is.na(CostExport),
                     select= c(Country, Year, TimeExport, CostExport))
#Determine country averages of LifeExpect and GDPcap to the data frame
dfExport.sub.cmplt.avg<- ddply(dfExport.sub.cmplt, .(Country), summarise,
                    avgTimeExport = mean(TimeExport, na.rm = TRUE),
                    avgCostExport = mean(CostExport, na.rm = TRUE),
                    runValid = length(Country))

#Merge averages in dfWorld.avg with dfWorld.sub (this can be done with
#'mutate', but the the nice data frome with averages goes lost)
dfExport.sub.cmplt <- merge(dfExport.sub.cmplt, dfExport.sub.cmplt.avg, by= "Country")

#Calculate deamed value of the dependent
attach(dfExport.sub.cmplt)
dfExport.sub.cmplt$diff.TimeExport <- TimeExport - avgTimeExport
dfExport.sub.cmplt$diff.CostExport <- CostExport - avgCostExport
detach(dfExport.sub.cmplt)

#Select countries with at least 50 observations
dfExport.sub.cmplt <- dfExport.sub.cmplt[dfExport.sub.cmplt$numValid = 6,]

#Select countries
subCountries <- c("Australia", "Bolivia", "Brazil","Portugal", "Thailand", "Bulgaria", "China", "Finland", "India")
dfExport.sub.cmplt <- dfExport.sub.cmplt[dfExport.sub.cmplt$Country %in% subCountries,]
dfExport.sub.cmplt.avg <- dfExport.sub.cmplt.avg[dfExport.sub.cmplt.avg$Country %in% subCountries,]

#Total variation: the straight line is the best fitting line
#through the scatter, which is obtained with pooled estimation
#of the relation between life expectancy and GDP
ggplot(dfExport.sub.cmplt, aes(x=CostExport, y=TimeExport))+
  #add the annual outcomes coloured by Country
  geom_point(aes(color=Country), size=2)+
  #add regression lines for the countries
  geom_smooth(method="lm", se=FALSE, colour="gray")+
  #label the axis
  xlim(0, 300) + ylim(0, 110)+
  xlab("Cost of Export")+ 
  ylab("Time of Export")+
  theme(axis.title= element_text(size=rel(1)),
        axis.text= element_text(size=rel(1)))+
  guides(colour = guide_legend(override.aes = list(size=1.5)))


###Total variation including regression lines per country and averages life expectancy and gdp per country
ggplot(dfExport.sub.cmplt, aes(x=CostExport, y=TimeExport))+
  #add the annual outcomes coloured by Country
  geom_point(aes(color=Country), size=1.5)+
  #add regression lines for the countries
  geom_smooth(aes(group=Country), method="lm", se=FALSE, colour="black", linetype=2)+
  geom_point(aes(x=avgCostExport, y=avgTimeExport, color=Country), data= dfExport.sub.cmplt.avg, size=2, show.legend=FALSE)+
  #label the axis
  xlim(0, 300) + ylim(0, 70)+
  xlab("Cost of Export")+ 
  ylab("Time of Export")+
  theme(axis.title= element_text(size=rel(1)),
        axis.text= element_text(size=rel(1)))+
  guides(colour = guide_legend(override.aes = list(size=1.5)))


View(CostExport_graph)
ggsave(
  CE,
  plot = last_plot(),
  device = "png",
  path = dirRslt,
  scale = 1,
  width = NA,
  height = NA,
  units = c("in", "cm", "mm"),
  dpi = 300,
  limitsize = TRUE,
)
library(png)
```