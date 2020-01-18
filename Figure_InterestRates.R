# Producing figure of nominal and real interest rates


#**********************************************************************
#                          Notes                                   ####
#**********************************************************************


# SPF 10-year inflation forecasts
# https://www.philadelphiafed.org/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/cpi10



#**********************************************************************
#                           Packages                               ####
#**********************************************************************

library(quantmod)
library(Quandl)
library(tidyverse)
library(readxl)
library(lubridate)
library(xts)
library(zoo)
library(magrittr)
library(stringr)
library(ggplot2)
library(extrafont)
library(scales)

# font_import()
loadfonts()
windowsFonts()

# C:\Windows\Fonts\calibrib.ttf => C:/R/user_libs/extrafontdb/metrics/calibrib
# C:\Windows\Fonts\calibrii.ttf => C:/R/user_libs/extrafontdb/metrics/calibrii
# C:\Windows\Fonts\calibril.ttf => C:/R/user_libs/extrafontdb/metrics/calibril
# C:\Windows\Fonts\calibrili.ttf => C:/R/user_libs/extrafontdb/metrics/calibrili

# Intro to quantmod
#    1. quantmod http://statmath.wu.ac.at/~hornik/QFS1/quantmod-vignette.pdf
#    2. https://www.quantinsti.com/blog/a-guide-on-r-quantmod-package-how-to-get-started/

# Quandl for R
# https://www.quandl.com/tools/r

# Intro to zoo  cran.r-project.org/web/packages/zoo/vignettes/zoo-quickref.pdf


save_figure <- function(fig_ob, fig_folder = Outputs_folder,  fig_type = "png", ...){
	figure_name <- deparse(substitute(fig_ob))
	ggsave(paste0(fig_folder, figure_name, ".", fig_type ), fig_ob, ... )
	
}

theme_bw2 <- function(){
	theme_bw() + theme(
		text = element_text(family = "Calibri Light", color = "grey20"),
		#plot.title=element_text(hjust=0.5),
		plot.subtitle=element_text(hjust=0.5),
		plot.caption=element_text(hjust=0, size = 9))
	
}


# SOA colors

SOA_darkblue  <- "#024d7c"
SOA_midblue   <- "#2377b9"
SOA_lightblue <- "#77c4d5"
SOA_red       <- "#d23138"



#**********************************************************************
#                     Global settings                              ####
#**********************************************************************
#dir_data_raw <- "data_raw/"
#dir_data_out <- "data_out/"
Quandl.api_key("rsakY2-RD8pa1JNBk9sd") 

# Note for Don: The api key is associated with my Quandl account. 
# You may want to register to obtain you own api key (Account settings -> API Key)




#**********************************************************************
#                     Loading data from FRED                       ####
#**********************************************************************

# http://rstudio-pubs-static.s3.amazonaws.com/24858_1f006c3965614b0099c963913100e9f0.html


# Major economic variables

FRED_vars <- c(

	"GS10",            # 10-Year Treasury constant maturity rate
	"CPIAUCSL",        # CPI-U, seasonally adjusted
	"CPILFESL",        # core CPI: CPI-U less food and energy, seasonally adjusted 
	"FII10"            # 10-year TIPS rate

)

# loading data through Quandl (2000 calls per 10 mins). 
df_FRED <- Quandl(paste0("FRED/", FRED_vars), order = "asc")
names(df_FRED) <- str_replace_all(names(df_FRED), c("FRED." = "", " - Value" = ""))

df_FRED %<>% 
	mutate(year    = year(Date),
				 month   = month(Date),
				 yearMon = as.yearmon(Date)) %>% 
	select(year, month, yearMon, everything(), -Date) %>% 
	rename(
				 T10    = GS10,
				 CPIU   = CPIAUCSL,
				 CPIc   = CPILFESL,
				 TIPS10 = FII10
	) %>% 
	as_tibble()
df_FRED

names(df_FRED)	

df_FRED



#**********************************************************************
#                     Constructing figures                       ####
#**********************************************************************

df_FRED2 <- 
  df_FRED %>% 
	mutate(T10 = T10 / 100,
				 TIPS10   = TIPS10/100,
		     infl     = CPIU / lag(CPIU, 12) - 1,
				 infl     = ifelse(is.na(infl), 0, infl),
				 infl_Avg5= rollmean(infl, 60, align = "right", fill = NA),
				 T10_real = T10 - infl_Avg5,
				 # infl    = CPIU / lag(CPIc, 12) - 1,
				 # T10_real = T10 - infl
  ) %>% 
	select(-CPIc, -CPIU, -infl, -infl_Avg5) %>% 
	gather(variable, value, -year, -month, -yearMon) %>% 
	mutate(variable = factor(variable, 
													 levels = c("T10","T10_real", "TIPS10"),
													 labels = c("10-year nominal rate",
													 					  "10-year real rate",
													 					  "10-year TIPS rate")
																	 )
	)


fig_title = "10-year nominal and real U.S. Treasury rates "

fig_caption1 = "Note: 10-year real rates are proxied by 10-year nominal treasury rates minus average annual CPI inflation rates of the past five years.\n"
fig_caption2 = "Source: Federal Reserve Bank of St. Louis, Federal Reserve Economic Data (FRED)"

fig <- 
df_FRED2 %>% 
	filter(year >=1960, yearMon <= as.yearmon(date("2019-06-01"))) %>% 
	ggplot(aes(x = yearMon, y = value, color =  variable )) + theme_bw2() + 
	geom_line(size = 0.75) + 
	geom_hline(yintercept = 0, linetype = 2) +
	scale_x_continuous(breaks = seq(1960, 2020, 5))+ 
	scale_y_continuous(breaks = seq(0, 0.2, 0.025),  labels = percent) + 
	scale_color_manual(values = c(SOA_red, SOA_darkblue, SOA_lightblue)) + 
	labs(
		   title = fig_title,
		   caption = paste0(fig_caption1, fig_caption2),
		   x = NULL,
			 y = NULL,
			 color = NULL) + 
	theme(# legend.position = c(c(0.85, 0.85)),
	    
		    legend.position = "bottom",
				legend.background = element_rect(linetype = "solid", color = NULL ),
				plot.title = element_text(colour = SOA_darkblue )
				# text = element_text(family = "Calibri Light", color = "grey20"),
				)

fig
save_figure(fig, "Figures/", width = 10*0.8, height = 6*0.8)



#**********************************************************************
#                     Discount rate figures                       ####
#**********************************************************************

# Figure 4 code snippet
rates.all <- readRDS("./Data/rates.all.rds")
levs <- c("DGS10", "private", "public")
labs <- c("10-year Treasury yield", "Private average assumed return", "State-local average assumed return")

lastyear <- 2018

pdata <- rates.all %>% filter(fyear>=1990, fyear<=lastyear) %>%
	select(-DGS30) %>%
	mutate(public=na.approx(public, na.rm=FALSE)) %>%
	gather(series, value, -fyear) %>%
	mutate(seriesf=factor(series, levels=levs, labels=labs),
				 fyear=as.integer(fyear))

gtitle1 <- "Assumed investment returns and risk-free returns"
gtitle2 <- "Public and private retirement systems"
xlab <- "Pension fund fiscal year"
src <- "\nNotes:
- Public plan assumptions for 2001+ from Public Plans Database, Center for Retirement Research. Earlier years from multiple sources.
- Private plan assumptions provided via correspondence with authors of:
   Andonov, Aleksandar and Bauer, Rob and Cremers, Martijn, Pension Fund Asset Allocation and Liability Discount Rates (March 3, 2016). http://ssrn.com/abstract=2070054
- 10-Year Treasury yield from Federal Reserve Bank of St. Louis (FRED)"
p <- ggplot(data=pdata, aes(x=fyear, y=value, colour=seriesf)) +
	scale_colour_manual(values = c(SOA_red, SOA_lightblue, SOA_darkblue)) +
	scale_y_continuous(name="Percent (%)", breaks=seq(0, 30, 2), limits=c(0, 15)) +
	# scale_x_continuous(name=xlab, breaks=c(seq(1975, 2015, 5), 2017)) +
	scale_x_continuous(name=xlab, breaks=c(seq(1975, 2015, 5), 2018)) +
	# legend +
	labs(caption=src,
			 colour = NULL) +
	ggtitle(label=gtitle1, subtitle=gtitle2) +
	theme_bw() +
	theme(plot.title = element_text(size=rel(1.3), face="bold")) +
	theme(plot.subtitle = element_text(size=rel(1), face="bold")) +
	theme(axis.title = element_text(face="bold", size=rel(1))) +
	theme(axis.text = element_text(face="bold", size=rel(1))) +
	theme(plot.caption = element_text(hjust=0, size=rel(0.7))) +
	theme(legend.justification=c(1, 1),
				legend.position=c(0.999, 0.998),
				legend.background = element_rect(color = "grey",  size=rel(0.5), linetype=1)) +
	theme(text = element_text(family = "Calibri Light", color = "grey20"),
				plot.title = element_text(colour = SOA_darkblue),
				plot.subtitle = element_text(colour = SOA_midblue)) + 
	geom_line(size=rel(1.2)) +
	geom_point(size=rel(1.5))
p

ggsave("./Figures/invest_eror_pubpriv_vsriskfree_soa.png",
			 p, width=8, height=6.5, units="in")

# Figure 5 code snippet

### Public and private plans risk assets - short term, annual

#fof.a <- readRDS("./Data/fof.a.rds")

fof.a <- read_csv("./Data/invest_equitysharesdb_short_data.csv")
fof.a$slgppf

pdata <- fof.a %>% filter(year(date) >= 1990) %>%
	mutate(slgppf=factor(slgppf, levels= c("State & local", "Private")))
pdata


gtitle1 <- "Equity-like investments as percentage of invested assets"
gtitle2 <- "State and local government and private sector defined benefit pension plans"
xlab <- "Calendar year"
xlab <- NULL
srcnote <- "\nSource: Authors' analysis of Z.1 Financial Accounts of the United States,\nFederal Reserve Board, Tables L.118.b, L.120.b, and L.122"
p <- pdata %>%
	ggplot(aes(x=date, y=equityshare, colour=slgppf)) +
	geom_line(size=rel(1)) +
	geom_point(size=rel(1)) +
	scale_y_continuous(name="Percent (%)", breaks=seq(0, 100, 5), limits=c(0, 75)) +
	scale_x_date(breaks=seq.Date(as.Date("1900-01-01"), as.Date("2020-01-01"), "5 years"), date_labels = "%Y") +
	scale_colour_manual(values=c(SOA_darkblue, SOA_lightblue)) +
	# egend +
	labs(x=xlab, caption=srcnote,
			 colour = NULL) +
	ggtitle(label=gtitle1, subtitle=gtitle2) +
	theme_bw() +
	theme(plot.title = element_text(size=rel(1.3), face="bold")) +
	theme(plot.subtitle = element_text(size=rel(1), face="bold")) +
	theme(axis.title = element_text(face="bold", size=rel(1))) +
	theme(axis.text = element_text(face="bold", size=rel(1))) +
	theme(legend.justification=c(0, 1),
				legend.position=c(0.001, 0.998),
				legend.background = element_rect(color = "grey",  size=rel(0.5), linetype=1)) +
	theme(plot.caption = element_text(hjust=0, size=rel(.9))) +
	theme(text = element_text(family = "Calibri Light", color = "grey20"),
				plot.title = element_text(colour = SOA_darkblue),
				plot.subtitle = element_text(colour = SOA_midblue)) + 
	geom_line(size=rel(1.2)) +
	geom_point(size=rel(1.5))
p

ggsave("./Figures/invest_equitysharesdb_short.png", p, width=8, height=6.5, units="in")







