# create and save rates data
# only run when data are updated

# get public plan historical earnings assumptions ####
# First, get old history from scattered sources
src94 <- "7.83 mean Survey of State and Local Gov ERS June 1994.pdf Zorn p.40"
src89 <- "http://www.uh.edu/~bsorense/Mitchell%26SmithPensions.pdf"
# https://www.osc.state.ny.us/retire/word_and_pdf_documents/reports/actuarial_assumption/aa_2015.pdf

hist.ir.s <- "fyear, value, source
1975, 5, 1978 Pension Task Force Report On Public Employee Retirement Systems House Committee on Education and Labor p. 161 -- 5% was the median across all plans
1989, 7.6, Pension Funding in the Public Sector Olivia S. Mitchell and Robert S. Smith The Review of Economics and Statistics p.281
1990, 7.76, https://www.questia.com/magazine/1G1-14379961/surveys-of-state-and-local-government-employee-retirement
1991, 7.81, https://www.questia.com/magazine/1G1-14379961/surveys-of-state-and-local-government-employee-retirement
1992, 7.83, Zorn
1994, 7.84, Zorn
1996, 7.84, Zorn p.12
1998, 7.88, Zorn p.12
2000, 7.91, Zorn p.12"
# 1990-1992, 1994, 1996, 1998, and 2000 are generally available from Zorn
irhist <- read_csv(hist.ir.s) %>% mutate(series="survey")

# now get more recent history from the PPD
ppdir.fy <- ppd %>% 
  group_by(fyear=fy) %>%
  summarise(value=mean(InvestmentReturnAssumption_GASB, na.rm=TRUE)*100,
            wvalue=sum(InvestmentReturnAssumption_GASB * MktAssets_net, na.rm=TRUE) /
              sum(MktAssets_net * !is.na(InvestmentReturnAssumption_GASB), na.rm=TRUE) * 100) %>%
  mutate(series="survey") %>%
  filter(fyear<=2018)


# private rates from Andonov, Bauer, Cremers 2016
private <- read_csv(
  "fyear, value
  1993, 8.214
  1994, 8.145
  1995, 7.987
  1996, 7.972
  1997, 7.71
  1998, 7.495
  1999, 7.961
  2000, 8.052
  2001, 7.761
  2002, 7.469
  2003, 7.267
  2004, 6.816
  2005, 6.643
  2006, 6.472
  2007, 6.307
  2008, 6.512
  2009, 6.181
  2010, 5.721
  2011, 5.112
  2012, 4.36")
private <- private %>% mutate(series="privmean")
private

# get Treasury rates ####
# FRED
# DGS10 10-Year Treasury Constant Maturity Rate
# DGS30 30-Year Treasury Constant Maturity Rate
# http://www.federalreserve.gov/releases/h15/current/h15.pdf
# http://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/yieldmethod.aspx.
# 30 is missing 1960s to ~1975, and also mid 2000s

t10 <- FRED("DGS10")
t30 <- FRED("DGS30")
# t30 %>% group_by(year) %>% summarise(value=mean(value, na.rm=TRUE))
# glimpse(t10)
# ht(t10)

Trates <- bind_rows(t10, t30)

Trates.fy <- Trates %>% 
  mutate(fyear=ifelse(month(date) >= 7, year + 1, year)) %>%
  group_by(series, fyear) %>%
  summarise(value=mean(value, na.rm=TRUE))
Trates.fy %>% ht

# glimpse(ppd)
# str_subset(names(ppd), "sset")
rates.all <- irhist %>% select(fyear, pub.hist=value) %>%
  full_join(ppdir.fy %>% select(fyear, pub.recent=value)) %>%
  full_join(private %>% select(fyear, private=value)) %>%
  full_join(Trates.fy %>% spread(series, value)) %>%
  mutate(public=ifelse(!is.na(pub.recent), pub.recent, pub.hist)) %>%
  select(fyear, public, private, DGS10, DGS30) %>%
  arrange(fyear)
saveRDS(rates.all, "./data/rates.all.rds")

