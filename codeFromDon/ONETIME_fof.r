# get and save flow of funds data
# fofu %>% filter(variable=="LM653064100") %>% count(freq)
# fof %>% filter(variable=="FL225035043") %>% count(freq)

vnames <- read_csv("snamex, shortname
                   # general variables
                   FA206210001, other.slgperscurtax
                   FA206240001, other.slgprodimptax
                   FA206231001, other.slgcorptax
                   FA086902005, other.gdp
                   FL213162005, other.munisec
                   FL213162400, other.stdebt
                   FL214090005, other.slgfinass
                   FL214190005, other.slgfinliab
                   LM653064100, other.mfcorpequity
                   LM654090000, other.mfassets
                   
                   # private DB pension funds
                   FL574090045, ppfdb.finassets
                   FL573065043, ppfdb.mortgages
                   LM573064143, ppfdb.corpequity
                   LM573064243, ppfdb.mfshares
                   FL573073005, ppfdb.claims
                   FL573093043, ppfdb.otherassets
                   FL574190043, ppfdb.entitlement
                   FL575035005, ppfdb.redirect
                   
                   # SLG DB funds
                   FL224090045, slgdb.finassets
                   FL223065043, slgdb.mortgages
                   LM223064145, slgdb.corpequity
                   LM223064243, slgdb.mfshares
                   FL223073045, slgdb.claims
                   FL223093043, slgdb.otherassets
                   FL224190043, slgdb.entitlement
                   FL225035043, slgdb.redirect")
vnames

# get the tables that the variables of interest appear in, to make documentation easier
glimpse(fof)
tabs <- fof %>% filter(variable %in% vnames$snamex) %>%
  group_by(freq, table, lineno) %>%
  filter(date==max(date)) %>%
  ungroup %>%
  select(freq, date, variable, table, lineno, description, units, value) %>%
  arrange(freq, variable, table, lineno)



# get quarterly FOF data from unique-variables file (fofu) ####
df <- fofu %>% filter(variable %in% vnames$snamex, freq=="Q") %>%
  mutate(vname=vnames$shortname[match(variable, vnames$snamex)]) %>%
  select(vname, date, value) %>%
  separate(vname, c("slgppf", "vname"), sep="\\.", 
           extra="merge", remove=TRUE) %>% # so we can track public plans, private plans, other data
  select(slgppf, date, vname, value)
ht(df)
count(df, vname)
# which vars don't we have?
vnames[!vnames$shortname %in% unique(paste(df$slgppf, df$vname, sep=".")), ]
# the data no longer include redirect and we must calculate it


# quarterly data calculations ####
df.q <- df
dfother.q <- filter(df.q, slgppf=="other") %>%
  spread(vname, value) %>%
  mutate(mfstockshare=mfcorpequity / mfassets, # economywide share of mutual fund assets in corp equities
         slgtax=slgperscurtax + slgprodimptax + slgcorptax)

# redirect has disappeared from the data, so compute
fof.q <- df.q %>% filter(slgppf!="other") %>%
  spread(vname, value) %>%
  left_join(select(dfother.q, date, gdp, mfstockshare, slgtax)) %>%
  mutate(invassets=entitlement - claims,
         redirect=invassets - (finassets - claims), # no longer a reported variable
         fr.mv=invassets / entitlement * 100,
         equity = corpequity + (mfshares + otherassets) * mfstockshare + redirect, 
         equityshare = equity / invassets * 100,
         equityslgtax = equity / slgtax * 100,
         equitygdpshare = equity / gdp *100)
saveRDS(fof.q, "./data/fof.q.rds")


# annual calculations ####
df.a <- df %>% mutate(date=floor_date(date, "year")) %>%
  group_by(slgppf, vname, date) %>%
  summarise(value=sum(value)) %>%
  ungroup

dfother.a <- filter(df.a, slgppf=="other") %>%
  spread(vname, value) %>%
  mutate(mfstockshare=mfcorpequity / mfassets, # economywide share of mutual fund assets in corp equities
         slgtax=slgperscurtax + slgprodimptax + slgcorptax)

# redirect has disappeared from the data, so compute
fof.a <- df.a %>% filter(slgppf!="other") %>%
  spread(vname, value) %>%
  left_join(select(dfother.a, date, gdp, mfstockshare, slgtax)) %>%
  mutate(invassets=entitlement - claims,
         redirect=invassets - (finassets - claims), # no longer a reported variable
         fr.mv=invassets / entitlement * 100,
         equity = corpequity + (mfshares + otherassets) * mfstockshare + redirect, 
         equityshare = equity / invassets * 100,
         equityslgtax = equity / slgtax * 100,
         equitygdpshare = equity / gdp *100)
saveRDS(fof.a, "./data/fof.a.rds")
