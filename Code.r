###################################################
#The Real Effects of Capital Requirements on the Brazilian Healthcare Industry
#Lucas S. Macoris (INSPER) and Bernardo Ricca (INSPER)
###################################################

###################################################
# Loading Libraries ####
library(dplyr)
library(tidyr)
library(lubridate)
library(DescTools)
library(kableExtra)
library(broom)
library(lfe)
library(ggplot2)
library(stargazer)
library(ggpubr)
library(grid)
library(zoo)
library(gridExtra)

#Setup memory requirements
memory.limit(memory.limit()*10)

#Which day-month pair does this version refers to?
log_version=today()
diretorio="G:\\My Drive\\Working Papers\\Ongoing Work\\ANS\\ANS"
setwd(diretorio)

# Create Separate Paths for Tables and Figures along with a Date Folder and User Type
if(!dir.exists(paste0(diretorio,"\\Results\\"))){dir.create(paste0(diretorio,"\\Results\\"))}
if(!dir.exists(paste0(diretorio,"\\Results\\",log_version))){dir.create(paste0(diretorio,"\\Results\\",log_version))}
if(!dir.exists(paste0(diretorio,"\\Results\\",log_version,"\\Charts"))){dir.create(paste0(diretorio,"\\Results\\",log_version,"\\Charts"))}
if(!dir.exists(paste0(diretorio,"\\Results\\",log_version,"\\Tables"))){dir.create(paste0(diretorio,"\\Results\\",log_version,"\\Tables"))}

#Set Paths
fig_path <- paste0(diretorio,"\\Results\\",log_version,"\\Charts\\")
table_path <- paste0(diretorio,"\\Results\\",log_version,"\\Tables\\")

###################################################

###################################################
#Part 1: Collecting financial data from .csv files####

#Since ANS changes accounting rules over time, we must
#break the loop and insert information in chunks.

###################################################
# 1.1 Financials 2010-2012 ####
# 1.1.1 Variable Names ####
#Read files 
N2010 <- read.csv("Raw Datasets\\Financials\\4T2010.csv", sep=";", stringsAsFactors = FALSE)%>%
  filter(nchar(CD_CONTA_CONTABIL)<=3|CD_CONTA_CONTABIL %in% c('1221','1222','4637','4638'))%>%
  select(CD_CONTA_CONTABIL,DESCRICAO)%>%distinct()

N2011 <- read.csv("Raw Datasets\\Financials\\4T2011.csv", sep=";", stringsAsFactors = FALSE)%>%
  filter(nchar(CD_CONTA_CONTABIL)<=3|CD_CONTA_CONTABIL %in% c('1221','1222','4637','4638'))%>%
  select(CD_CONTA_CONTABIL,DESCRICAO)%>%distinct()

N2012 <- read.csv("Raw Datasets\\Financials\\4T2012.csv", sep=";", stringsAsFactors = FALSE)%>%
  filter(nchar(CD_CONTA_CONTABIL)<=3|CD_CONTA_CONTABIL %in% c('1221','1222','4637','4638'))%>%
  select(CD_CONTA_CONTABIL,DESCRICAO)%>%distinct()

NamesPre<-full_join(N2010,N2011)%>%
  full_join(N2012)%>%
  mutate_all(trimws,'both')%>%
  distinct()%>%
  arrange(CD_CONTA_CONTABIL)

rm(N2010,N2011,N2012)

# 1.1.2 Financials - Do for the first year (2010) #### 
#1.1 Read the file 
R2010 <- read.csv("Raw Datasets\\Financials\\4T2010.csv", sep=";", stringsAsFactors = FALSE)

#1.3 Adjusting Data
R2010$VL_SALDO_FINAL <- gsub(",", ".", R2010$VL_SALDO_FINAL)
R2010$VL_SALDO_FINAL <- as.numeric(R2010$VL_SALDO_FINAL)

#1.4 Keeping only selected variables
R2010 <- R2010[nchar(R2010$CD_CONTA_CONTABIL) <=3|R2010$CD_CONTA_CONTABIL %in% c('1221','1222','4637','4638'), ]%>%select(-4)

#1.5 Converting to Long
R2010 <- spread(R2010, CD_CONTA_CONTABIL, VL_SALDO_FINAL)

#1.6 Cleaning up
is.na(R2010[,3:length(R2010)]) <- !R2010[,3:length(R2010)]
R2010$DATA <- as.Date(R2010$DATA, format = "%d/%m/%Y")

Financials_2010_2012<-R2010

# 1.1.3 Financials - Now, repeat through a loop ####

for(i in 1:2){
  
  #2.1 Read the file
  Data <- read.csv(paste0("Raw Datasets\\Financials\\4T201",i,".csv"), sep=";", stringsAsFactors = FALSE)
  
  #2.2 Adjusting Data
  Data$VL_SALDO_FINAL <- gsub(",", ".", Data$VL_SALDO_FINAL)
  Data$VL_SALDO_FINAL <- as.numeric(Data$VL_SALDO_FINAL)
  
  #2.3 Keeping only selected variables
  Data <- Data[nchar(Data$CD_CONTA_CONTABIL) <=3 |Data$CD_CONTA_CONTABIL %in% c('1221','1222','4637','4638'), ]%>%select(-4)
  
  #2.4 Converting to Long
  Data <- spread(Data, CD_CONTA_CONTABIL, VL_SALDO_FINAL)
  
  #2.5 Cleaning up
  is.na(Data[,3:length(Data)]) <- !Data[,3:length(Data)]
  Data$DATA <- as.Date(Data$DATA, format = "%d/%m/%Y")
  
  
  Financials_2010_2012<-full_join(Financials_2010_2012,Data)
  
}

#2013-2019


#1.2 2013 onwards


# 1.2 Financials 2013-2019 ####

# 1.2.1 Do for the first year (2013) ####

#1.1 Read the file
Data <- read.csv("Raw Datasets\\Financials\\4T2013.csv", sep=";", stringsAsFactors = FALSE)

#1.2 Read the labels
PC <- read.csv("Raw Datasets\\Financials\\PC.csv")
Names2013<-PC

#1.3 Adjusting Data
Data$VL_SALDO_FINAL <- gsub(",", ".", Data$VL_SALDO_FINAL)
Data$VL_SALDO_FINAL <- as.numeric(Data$VL_SALDO_FINAL)

#1.4 Keeping only selected variables
Data <- Data[Data$CD_CONTA_CONTABIL %in% PC$CD_CONTA_CONTABIL, ]%>%select(-4)

#1.5 Converting to Long
Data <- spread(Data, CD_CONTA_CONTABIL, VL_SALDO_FINAL)

#1.6 Cleaning up
is.na(Data[,3:103]) <- !Data[,3:103]
Data$DATA <- as.Date(Data$DATA, format = "%d/%m/%Y")

Financials_2013_2019<-Data


# 1.2.2 Now, repeat through a loop ####
for(i in 4:9){
  
  #2.1 Read the file
  Data <- read.csv(paste0("Raw Datasets\\Financials\\4T201",i,".csv"), sep=";", stringsAsFactors = FALSE)
  
  #2.2 Adjusting Data
  Data$VL_SALDO_FINAL <- gsub(",", ".", Data$VL_SALDO_FINAL)
  Data$VL_SALDO_FINAL <- as.numeric(Data$VL_SALDO_FINAL)
  
  #2.3 Keeping only selected variables
  Data <- Data[Data$CD_CONTA_CONTABIL %in% PC$CD_CONTA_CONTABIL, ]%>%select(-4)
  
  #2.4 Converting to Long
  Data <- spread(Data, CD_CONTA_CONTABIL, VL_SALDO_FINAL)
  
  #2.5 Cleaning up
  is.na(Data[,3:103]) <- !Data[,3:103]
  Data$DATA <- as.Date(Data$DATA, format = "%d/%m/%Y")
  
  
  Financials_2013_2019<-rbind(Financials_2013_2019,Data)
  
}

###################################################

###################################################
#Part 2 Adjusting and Joining Data####

#Now, we get the Conversion.csv data, which maps all variables of the two periods into an unique identifier
#Reason: accounting rules have changed

Labels<-read.csv("Raw Datasets\\Financials\\Conversion.csv",sep=';',encoding = 'UTF-8')
names(Labels)<-c('Desc','Y2013_2019','Y2010_2012')

#2.1.1 For the first dataset, we keep only columns in which we can map the names:
Financials_2010_2012<-Financials_2010_2012[names(Financials_2010_2012)%in% c('DATA','REG_ANS',Labels[,3])]

#2.1.2 Change variable names for the first dataset (2010-2012)
N2010_2012<-Labels[,c(2,3)]
Names=data.frame(Index=NA,Desc=NA)

#2.1.3 Do a loop to search for the correspondent names
for (i in 3:96){
  
  Corresp=data.frame(Index=NA,
                     Desc=NA)
  
  Corresp$Index=names(Financials_2010_2012)[i]
  Corresp$Desc=N2010_2012[N2010_2012$Y2010_2012==Corresp$Index,1]
  
  Names=rbind(Names,Corresp)
  
}

#2.1.4 Finally, change the names to match those from the 2013-2019 dataset
names(Financials_2010_2012)[-c(1,2)]<-Names[-1,2]

#2.1.5 Now, we add null values for the columns that we won't have info
Financials_2010_2012$`231`<-NA
Financials_2010_2012$`235`<-NA
Financials_2010_2012$`236`<-NA
Financials_2010_2012$`237`<-NA
Financials_2010_2012$`238`<-NA
Financials_2010_2012$`321`<-NA
Financials_2010_2012$`341`<-NA

#2.1.6 Finally, let's merge the two datasets

Financials<-full_join(Financials_2010_2012,Financials_2013_2019)

#Save this datasets
saveRDS(Financials,'Final Datasets\\Financials.RDS')

###################################################

###################################################
#Part 3: Filtering Our Sample: Categorical and Financial Variables####

#In this part, we apply categorical and quantitative filter to our data.

#3.1 Categorical Filters
Cat=readRDS("Final Datasets/Categorical.RDS")

#Firm type must be in Cooperativa Médica, Medicina de Grupo, or Filantropia
Mod=Cat%>%distinct(Registro_ANS,Modalidade)%>%filter(Modalidade %in% c('Cooperativa Médica', 'Medicina de Grupo','Filantropia'))

#Firm is not created within our sample (Incorporation Date >= 2010)
Inc=Cat%>%distinct(Registro_ANS,Data_Registro_ANS)%>%filter(Data_Registro_ANS<=2010)

#Firm has at least 1MM of assets in place in 2010:
Assets<-Financials%>%filter(year(DATA)==2010)%>%filter(`1`>1000000)%>%distinct(REG_ANS)

Financials<-Financials%>%
  filter(REG_ANS %in% Mod$Registro_ANS,
         REG_ANS %in% Inc$Registro_ANS,
         REG_ANS %in% Assets$REG_ANS)

#3.2 Numerical Filters

Financials<-Financials%>%filter(
  #Assets>0
  `1`>0,
  #Liabilities>0
  `2`>0)

###################################################

###################################################
#Part 4: Calculating Financial Indexes
###################################################
# 4.1 Let's put NA on top of zero values for avoiding calculations to be affected####
OPS<-Financials%>%
  ungroup()%>%
  mutate_at(c(3:103),list(~replace(., 0,NA)))

# 4.2. Calculating Financial indicators ####

OPS<-OPS%>%
  group_by(DATA,REG_ANS)%>%
  summarize(
    
    ## Balance-Sheet Variables
    Ativo_Total=mean(`1`,na.rm=TRUE),
    Ativo_Operacional=mean(sum(`121`,`123`,`124`,`125`,`126`,`127`,`128`,`129`,`131`,`133`,`134`,`191`,na.rm=TRUE),na.rm=TRUE),
    Ativo_Financeiro=mean(sum(`122`,`132`,na.rm=TRUE)),
    PL=mean(`25`,na.rm=TRUE),
    
    ## Profit/Loss Variables
    Receita_Assistencial=sum(`31`,`32`,na.rm=TRUE),
    Custo_Assistencial=mean(`41`,na.rm=TRUE),
    Outras_Receitas_Op=sum(`33`,`34`,na.rm=TRUE),
    Outras_Desp_Op=sum(`44`,na.rm=TRUE),
    Despesas_Gerais=sum(`43`,`46`,na.rm=TRUE),
    IR_CSLL=sum(`611`,`612`,na.rm=TRUE),
    Rec_Op=sum(Receita_Assistencial,Outras_Receitas_Op,na.rm=TRUE),
    Rec_FinPatr=sum(`35`,`36`,na.rm=TRUE),
    Rec_Global=sum(Rec_Op,Rec_FinPatr,na.rm = TRUE),
    Result_Op=sum(Rec_Op,-Custo_Assistencial,-Outras_Desp_Op,-Despesas_Gerais,-IR_CSLL,na.rm=TRUE),
    Result_FinPatr=sum(`35`,`36`,-`45`,-`47`,na.rm=TRUE),
    DA=mean(sum(`4637`,`4638`,na.rm=TRUE)),
    
    #Main Financial Indicators#
    
    ## Indicadores de Resultado
    Sinistralidade=ifelse(!Receita_Assistencial, NA,Custo_Assistencial/Receita_Assistencial),
    EBITDA=mean(sum(Result_Op,DA,-IR_CSLL,na.rm=TRUE),na.rm=TRUE),
    PPE=mean(`133`,na.rm = TRUE),
    PPE_Ratio=mean(`133`/Ativo_Total,na.rm = TRUE),
    AF_Ratio=mean(Ativo_Financeiro/Ativo_Total,na.rm = TRUE),
    Leverage=mean(sum(`217`,`237`,na.rm=TRUE)/Ativo_Total,na.rm=TRUE),
    Current_Ratio=mean(`12`/`21`,na.rm=TRUE),
    EBITDA_Mg=ifelse(!Rec_Op,NA,mean(EBITDA/sum(Rec_Op),na.rm = TRUE)),
    Op_Mg=mean(1-(sum(Custo_Assistencial,Outras_Desp_Op,Despesas_Gerais,IR_CSLL,na.rm=TRUE)/Rec_Op),na.rm=TRUE),
    FinPatr_Mg=mean(Result_FinPatr/Rec_FinPatr,na.rm=TRUE),
    Net_Mg=mean(sum(Result_Op,Result_FinPatr,na.rm=TRUE)/Rec_Op,na.rm=TRUE),
    Contr_Op_=sum(Rec_Op,na.rm=TRUE)/sum(Rec_Global,na.rm=TRUE),
    Contr_Fin=sum(`35`,na.rm=TRUE)/sum(Rec_Global,na.rm=TRUE),
    Contr_Patr=sum(`36`,na.rm=TRUE)/sum(Rec_Global,na.rm=TRUE),
    ROI=mean(sum(Result_Op,Result_FinPatr,na.rm=TRUE)/Ativo_Total,na.rm=TRUE),
    ROE=mean(sum(Result_Op,Result_FinPatr,na.rm=TRUE)/PL,na.rm=TRUE))%>%
  ungroup()%>%
  #Double Checking Filter to avoid any accounting discrepancy that is too high
  filter(abs(Ativo_Total-Ativo_Financeiro-Ativo_Operacional)<1000)


#Let's remove any Infs that might affect results for average values and remove unnecessary datasets
OPS[mapply(is.infinite, OPS)] <- NA
OPS[mapply(is.nan, OPS)] <- NA
rm(Inc)


###################################################

###################################################
#Part 5: Joining Solvency Data and calculating Solvency Margin
#5.1 Load the data and do adjustments####

Solvency<-readRDS('Final Datasets\\Solvency_2007_2019.RDS')
Solvency$DATA<-as.Date(Solvency$DATA,format='%d/%m/%Y')
Solvency$Year<-year(Solvency$DATA)
Solvency<-Solvency%>%filter(Year<2010)

#5.2 Now, let's calculate both indicators for the solvency. We must keep at least one indicator without missing values.####

Solvency<-Solvency%>%
  group_by(REG_ANS,Year)%>%
  #Lest sum all revenues and cost to check whether values are in line with the overall costs and revenues
  mutate(Rev_Check=sum(Rev_Pre,Rev_Pos,na.rm=TRUE)/Rev,
         Cost_Check=sum(Cost_Pre,Cost_Pos,na.rm=TRUE)/Costs)%>%
  #Applying some filters
  filter(!is.na(Cost_Check)|!is.na(Rev_Check),
         !is.na(Rev),
         !is.na(Costs),
         Rev>=10,Costs>=10)

#5.3 There are some firms in which we cannot recover 100% of the data. For these firms, let's calculate ratios of Pre/Post Costs and Revenues####

AvgFirmsCharact<-Solvency%>%group_by(REG_ANS,Year)%>%
  mutate(DenRev=sum(Rev_Pre,Rev_Pos,na.rm=TRUE),
         DenCost=sum(Cost_Pre,Cost_Pos,na.rm=TRUE))%>%
  mutate(DenRev=ifelse(DenRev==0,NA,DenRev),
         DenCost=ifelse(DenCost==0,NA,DenCost))%>%
  group_by(REG_ANS)%>%
  summarize(Ratio_Rev=mean(Rev_Pre/DenRev,na.rm=TRUE),
            Ratio_Cost=mean(Cost_Pre/DenCost,na.rm=TRUE))%>%
  filter(is.finite(Ratio_Rev),is.finite(Ratio_Cost),
         Ratio_Rev<=1,Ratio_Cost<=1)

#5.4 Now, we adjust the data and input average characteristics for pre/post for those in which we weren't able to do so.####

Solvency<-Solvency%>%
  left_join(AvgFirmsCharact)%>%ungroup()%>%
  #First, use only information in which we can reliably recompose overall revenues and costs
  filter(!is.na(Ratio_Rev),
         !is.na(Ratio_Cost),
         #Revenues and Costs must have an accuracy of 95%
         Rev_Check>=0.95,
         Cost_Check>=0.95)%>%
  #Create a marker to highlight observations where we need to input values
  mutate(MarkRev=ifelse(is.na(Rev_Pre),1,0),
         MarkCost=ifelse(is.na(Cost_Pre),1,0),
         Rev_Pre=ifelse(is.na(Rev_Pre),Ratio_Rev*Rev,Rev_Pre),
         Rev_Pos=ifelse(MarkRev==1,(1-Ratio_Rev)*Rev,Rev_Pos),
         Cost_Pre=ifelse(is.na(Cost_Pre),Ratio_Cost*Costs,Cost_Pre),
         Cost_Pos=ifelse(MarkCost==1,(1-Ratio_Cost)*Costs,Cost_Pos))%>%
  #Filter unnecessary variables
  dplyr::select(-c(MarkRev,MarkCost,Ratio_Rev,Ratio_Cost))

#5.5 Calculating Solvency Margin####

Solvency[Solvency==0]<-NA

Solvency<-Solvency%>%
  rowwise()%>%
  mutate(
    #S1: 20% x (100% x RevPre + 50% x RevPos)
    S1=0.2*sum(Rev_Pre,0.5*Rev_Pos,na.rm=TRUE))%>%
  #S2: To calculate S2, we must calculate rolling averages for pre and post costs 
  arrange(REG_ANS,Year)%>%
  group_by(REG_ANS)%>%
  mutate(Costs_Pre_Mean=rollapply(Cost_Pre,width=3,FUN='mean',partial=1,align = 'right'),
         Costs_Post_Mean=rollapply(Cost_Pos,width=3,FUN='mean',partial=1,align = 'right'))%>%
  #S2: 33% x Mean_3_years(100% x CostPre + 50% x CostPos):
  rowwise()%>%
  mutate(S2=0.33*(sum(Cost_Pre,0.5*Cost_Pos,na.rm = TRUE)))%>%
  select(1:4,S1,S2)%>%
  #Remove outliers from the sample
  filter(S1/S2>0.5,S1/S2<=2)

###################################################

###################################################
#Part 6: Joining Data Altogether and Basic Plots ####

#6.1 Lets load the State Market Share File
State_Shares<-readRDS('Final Datasets\\State_Shares.RData')

#6.2 Now, we'll merge the solvency information with the financials
Baseline_Data<-OPS%>%mutate(REG_ANS=as.character(REG_ANS),
                            Year=as.character(year(DATA)))

#6.3 Aggregate all customers at the national level for each firm
NationalBenefs<-State_Shares%>%
  group_by(REG_ANS,Year)%>%
  summarize(Benef=sum(Benef,na.rm=TRUE))

#6.4 Now, join with the baseline data to get a sense of the number of customers per year, as well as growth ratios

Baseline_Data<-Baseline_Data%>%
  left_join(NationalBenefs)%>%
  #Lets consider only firms in which we have all year between min and max years 
  group_by(REG_ANS)%>%
  mutate(Length=max(as.numeric(Year)-min(as.numeric(Year))+1),
         Min_Year=min(as.numeric(Year)),
         Max_Year=max(as.numeric(Year)),
         Nobs=n())%>%
  #We'll keep only information in which we have available information at least between 2011 and 2013 (1 year before, 1 year after) and 100 Customers
  filter(Length==Nobs,
         Min_Year<=2011,
         Max_Year>=2013)%>%
  arrange(REG_ANS,Year)%>%
  group_by(REG_ANS)%>%
  mutate(Growth_Cust=(Benef/dplyr::lag(Benef,1))-1)%>%
  select(-c(Length,Min_Year,Max_Year,Nobs))

#6.5 First, let's create our measure of adjusted equity for 2009:
Baseline_Data$REG_ANS <- as.factor(Baseline_Data$REG_ANS)

#Get Adjusted equity as of the beginning of 2009. 
Adj_Equity <-read.csv("Raw Datasets\\Financials\\1T2009.csv", sep=";", stringsAsFactors = FALSE)%>%
  dplyr::filter(CD_CONTA_CONTABIL %in% c('25','1323','1321','128'))%>%
  distinct()%>%
  mutate_all(trimws,'both')%>%
  dplyr::select(-c(DATA,DESCRICAO))%>%
  mutate(VL_SALDO_FINAL=gsub(',','.',VL_SALDO_FINAL))%>%
  spread(key=CD_CONTA_CONTABIL,value=VL_SALDO_FINAL)%>%
  mutate_at(c(2:5),as.numeric)%>%
  rowwise()%>%
  dplyr::mutate(Adj_Equity=sum(`25`,-`128`,-`1321`,-`1323`,na.rm=TRUE))

Adj_Equity$REG_ANS<-as.character(Adj_Equity$REG_ANS)

Adj_Equity<-Adj_Equity%>%
  left_join(dplyr::filter(Solvency,year(DATA)=='2009')%>%
              mutate(REG_ANS=as.character(REG_ANS)))%>%
  filter(!is.na(S1),!is.na(S2))%>%
  rowwise()%>%
  mutate(SM=max(S1,S2,na.rm = TRUE))%>%
  dplyr::select(REG_ANS,S1,S2,SM,Adj_Equity)

#6.6.2 Winsorization

Baseline_Data<-Baseline_Data%>%
  mutate(Benef=as.character(Benef))%>%
  group_by(Year)%>%
  mutate(across(where(is.numeric),Winsorize,probs=c(0.01,0.99),na.rm=TRUE))%>%
  ungroup()%>%
  mutate(Benef=as.numeric(Benef))

#6.7 Number of firm-year observations, on average: 10 years
Baseline_Data%>%
  group_by(REG_ANS)%>%
  tally()%>%
  summarize(
    `Mean Obs`=mean(n,na.rm=TRUE),
    `Median Obs`=median(n,na.rm=TRUE))

#6.8 Number of firms per year: 446
Baseline_Data%>%
  group_by(Year)%>%
  tally()%>%
  summarize(
    `Mean Obs`=mean(n,na.rm=TRUE),
    `Median Obs`=median(n,na.rm=TRUE))

#6.9 Representativeness of the data:

#Get the population of firms
Mod=Cat%>%distinct(Registro_ANS,Modalidade)%>%
  filter(!(Modalidade %in% c('Cooperativa odontológica','Odontologia de Grupo',
                             'Seguradora','Administradora','Seguradora Especializada em Saúde',
                             'Autogestão','Administradora de Benefícios')))

#Get the number of benefs within this population (health assistance)
TotalBenefs<-NationalBenefs%>%
  distinct(REG_ANS,Year,.keep_all=TRUE)%>%
  filter(REG_ANS %in% Mod$Registro_ANS)%>%
  group_by(Year)%>%
  summarize(Total=sum(Benef,na.rm=TRUE))

#Result: 83.2% of the sample
Baseline_Data%>%
  group_by(Year)%>%
  summarize(Benefs=sum(Benef,na.rm=TRUE))%>%
  left_join(TotalBenefs)%>%
  mutate(Perc=Benefs/Total)%>%
  summarize_at(4,mean)

#Get the number of active/delisted firms in each period
Survival<-Cat%>%
  dplyr::filter(Registro_ANS %in% Mod$Registro_ANS)%>%
  mutate(Year=ifelse(Data_Descredenciamento=='NA',NA,
                     year(as.Date(Data_Descredenciamento,format='%Y'))))%>%
  dplyr::filter(Ativa==0,Year!='1999')%>%
  group_by(Year)%>%
  summarize(Delisted=n())

Active=data.frame(Year=seq(2000,2019,1),
                  Active=c(1970,2037,2000,1749,1654,1581,1527,1497,1408,1272,1218,1184,1180,1123,1086,1049,975,958,932,920))

SurvPlot<-left_join(Active,Survival)%>%
  arrange(Year)%>%
  mutate(Perc_Delisted=Delisted/dplyr::lag(Active,1))

#Plot number of firms
F1<-ggplot(SurvPlot,aes(x=Year,y=Active))+
  geom_col(alpha=0.5)+
  scale_colour_grey()+
  theme_minimal()+
  geom_label(aes(label=Active))+
  scale_x_continuous(breaks=seq(2000,2019,1))+
  scale_y_continuous(breaks=seq(0,2000,250))+
  theme(axis.text = element_text(size=15))+
  theme(axis.text.x = element_text(angle = 90))+
  theme(plot.title = element_text(face='bold',size=20))+
  labs(x='',
       y='Number of active Healthcare Firms',
       title='Panel A: Evolution of Active Healthcare Firms over time')

#Plot number of delisted
F2<-ggplot(SurvPlot,aes(x=Year,y=Delisted))+
  geom_col(alpha=0.5)+
  geom_smooth(method = 'lm',se=FALSE,linetype='dashed')+
  scale_colour_grey()+
  theme_minimal()+
  geom_label(aes(label=ifelse(!is.na(Perc_Delisted),paste0(round(Perc_Delisted*100,1),"%"),"-")))+
  scale_x_continuous(breaks=seq(2000,2019,1))+
  scale_y_continuous(breaks=seq(0,150,25))+
  theme(axis.text = element_text(size=15))+
  theme(axis.text.x = element_text(angle = 90))+
  theme(plot.title = element_text(face='bold',size=20))+
  labs(x='',
       y='Number of delisted Healthcare Firms',
       title='Panel B: Evolution of delistings and mortality rates over time',
       subtitle='Mortality rates, presented in boxes, are conditional on being active in the last year.')

out.path=paste0(fig_path,"F1.pdf")
pdf(out.path, height = 20, width =30, paper = "USr")
print(grid.arrange(F1,F2,nrow=2, bottom = textGrob("Source: Agência Nacional da Saúde Suplementar (ANS)", x = 0.8)))
dev.off()

#Plot number of Healthcare Customers + Average Age

Benefs=data.frame(Year=seq(2000,2019,1),
                  Benefs=c(30.5,31,31.4,31.5,32.1,33.8,35.4,37.2,39.3,41.5,42.6,44.9,46,
                           47.8,49.5,50.4,49.2,47.6,47.2,47.4))  

F3=ggplot(Benefs,aes(x=Year,y=Benefs))+
  geom_col(alpha=0.5)+
  geom_smooth(method = 'lm',se=FALSE,linetype='dashed')+
  scale_colour_grey()+
  theme_minimal()+
  scale_x_continuous(breaks=seq(2000,2019,1))+
  scale_y_continuous(breaks=seq(0,50,10))+
  theme(axis.text = element_text(size=15))+
  theme(axis.text.x = element_text(angle = 90))+
  theme(plot.title = element_text(face='bold',size=20))+
  labs(x='',
       y='Number of healthcare customers (in Millions)',
       title='Panel A: Evolution of active healthcare customers over time')

Age=read.csv('Raw Datasets\\MarketShare\\Age.csv',sep=';')
names(Age)<-c('Year','Age')

F4=ggplot(Age,aes(x=Year,y=Age))+
  geom_point(size=4,alpha=0.5)+
  geom_smooth(method = 'lm',se=FALSE,linetype='dashed')+
  scale_colour_grey()+
  theme_minimal()+
  scale_x_continuous(breaks=seq(2000,2019,1))+
  scale_y_continuous(breaks=seq(30,35,1))+
  theme(axis.text = element_text(size=15))+
  theme(axis.text.x = element_text(angle = 90))+
  theme(plot.title = element_text(face='bold',size=20))+
  labs(x='',
       y='Age',
       title='Panel B: Average Customer Age over time',
       subtitle='Weighted average of 18 bins of ~5 years, capped at 80+. This is a lower bound for the average customer age.')

out.path=paste0(fig_path,"F2.pdf")
pdf(out.path, height = 20, width =30, paper = "USr")
print(grid.arrange(F3,F4,nrow=2, bottom = textGrob("Source: Agência Nacional da Saúde Suplementar (ANS)", x = 0.8)))
dev.off()


#Hazard Ratio Levels

Hazard=data.frame(Year=seq(2001,2019,1),
                  Hazard=c(0.781595384,0.780030433,0.796122166,0.791688795,0.787015463,
                           0.78413625,0.770749713,0.780910491,0.808572868,0.797223168,
                           0.806445556,0.819879595,0.836725125,0.83184289,0.818940017,
                           0.83115969,0.819871916,0.807941351,0.820052067))

F6=Hazard%>%
  ggplot(aes(x=Year,y=Hazard))+
  geom_col(size=1,alpha=0.5,group=1)+
  geom_smooth(method = 'lm',se=FALSE,linetype='dashed')+
  scale_colour_grey()+
  theme_minimal()+
  scale_x_continuous(breaks=seq(2001,2019,1))+
  scale_y_continuous(breaks=seq(0.7,0.85,0.025),labels=scales::percent)+
  coord_cartesian(ylim=c(0.7,0.85))+
  theme(axis.text = element_text(size=15))+
  theme(axis.text.x = element_text(angle = 90))+
  theme(plot.title = element_text(face='bold',size=20))+
  labs(x='',
       y='Average Hazard Ratio (Health Assistance Costs/Health Assistance Revenues)',
       title='Average Hazard Ratio (1 - Gross Margin) over time',
       subtitle='Based on the average values for the Hazard Ratio of Health Cooperatives, Filantrophy, Medical-Assistance Firms.')

out.path=paste0(fig_path,"F3.pdf")
pdf(out.path, height = 20, width =30, paper = "USr")
print(grid.arrange(F6,nrow=1, bottom = textGrob("Source: Agência Nacional da Saúde Suplementar (ANS)", x = 0.8)))
dev.off()


#Leverage Over time: use treatment levels here.
Leverage<-Baseline_Data%>%
  group_by(Year)%>%
  summarize(Leverage=median(Ativo_Total/PL,na.rm=TRUE),
            Liquidity=median(Current_Ratio,na.rm=TRUE),
            AV=sum(Ativo_Financeiro,na.rm=TRUE),
            AV_Perc=median(AF_Ratio,na.rm=TRUE))

F7=Leverage%>%ggplot(aes(x=Year,y=Leverage))+
  geom_line(size=1,alpha=0.5,group=1)+
  geom_point(size=3,group=1)+
  scale_colour_grey()+
  theme_minimal()+
  scale_y_continuous(breaks=seq(2,3,0.25))+
  annotate("text", x = 3.1, y = 2.25, label = "Last year without the \n Solvency Margin Rule")+
  coord_cartesian(ylim=c(2,3))+
  geom_vline(xintercept=2,linetype='dashed')+
  theme(axis.text = element_text(size=15))+
  theme(axis.text.x = element_text(angle = 90))+
  theme(plot.title = element_text(face='bold',size=20))+
  labs(x='',
       y='Median Leverage (Total Assets/Total Equity)',
       title='Panel A: Evolution of Leverage before/after the rule',
       subtitle='Based on the median values of our subsample')

F8=Leverage%>%ggplot(aes(x=Year,y=AV_Perc))+
  geom_line(size=1,alpha=0.5,group=1)+
  geom_point(size=3,group=1)+
  scale_colour_grey()+
  theme_minimal()+
  scale_y_continuous(breaks=seq(0.3,0.6,0.05),labels=scales::percent)+
  coord_cartesian(ylim=c(0.3,0.6))+
  geom_vline(xintercept=2,linetype='dashed')+
  theme(axis.text = element_text(size=15))+
  theme(axis.text.x = element_text(angle = 90))+
  theme(plot.title = element_text(face='bold',size=20))+
  labs(x='',
       y='Median Asset Coverage (Mandatory Financial Buffer/Total Assets)',
       title='Panel B: Evolution of Financial Buffers before/after the rule',
       subtitle='Based on the median values of our subsample')

out.path=paste0(fig_path,"F4.pdf")
pdf(out.path, height = 20, width =30, paper = "USr")
print(grid.arrange(F7,F8,nrow=2, bottom = textGrob("Source: Agência Nacional da Saúde Suplementar (ANS)", x = 0.8)))
dev.off()


F9<-Leverage%>%ggplot(aes(x=Year,y=AV/1000000000))+
  geom_col(alpha=0.5)+
  scale_colour_grey()+
  theme_minimal()+
  geom_label(label=paste0("R$ ",round(Leverage$AV/1000000000,1),"bi"))+
  scale_y_continuous(breaks=seq(0,30,2.5))+
  geom_vline(xintercept=2,linetype='dashed')+
  theme(axis.text = element_text(size=15))+
  theme(axis.text.x = element_text(angle = 90))+
  theme(plot.title = element_text(face='bold',size=20))+
  labs(x='',
       y='Financial Assets (in billions of BRL)',
       title='Evolution of Financial Assets before/after the rule',
       subtitle='Total value per year based on our subsample.')


out.path=paste0(fig_path,"F5.pdf")
pdf(out.path, height = 20, width =30, paper = "USr")
print(grid.arrange(F9,nrow=1, bottom = textGrob("Source: Agência Nacional da Saúde Suplementar (ANS)", x = 0.8)))
dev.off()


#Herfindahl-Hirschman Index
HHI=read.csv("Raw Datasets\\MarketShare\\HHI.csv")

F10=HHI%>%arrange(desc(index(HHI)))%>%mutate(across(c(2),~.x*10000))%>%
  ggplot(aes(x=Period,y=HHI,col=Industry))+
  geom_point(size=5)+
  theme_minimal()+
  scale_colour_grey()+
  theme(axis.title = element_text(size=15))+
  theme(axis.text = element_text(size=12))+
  theme(axis.text.x = element_text(angle = 90))+
  theme(plot.title = element_text(face='bold',size=15))+
  theme(legend.position = 'bottom')+
  labs(x='',y='Herfindahl-Hirschman-Index (HHI)',
       title='Healthcare Industry Concentration over time',
       subtitle='Total Market as all categories, Selected Market including only studied categories.')

out.path=paste0(fig_path,"F10.pdf")
pdf(out.path, height = 20, width =30, paper = "USr")
print(grid.arrange(F10,nrow=1, bottom = textGrob("Source: Agência Nacional da Saúde Suplementar (ANS)", x = 0.8)))
dev.off()
###################################################

###################################################
#Part 7: Preparing for Regression and Estimation
###################################################
#7.1 Joining relevant data: here, our observation will be the State-Firm-Year. So, let's construct a panel to match this info####

#Rename the variable so that we have both state customers and total customers
names(State_Shares)[3]<-'State_Benef'

#7.1.1: Joining Customers Data
Panel=State_Shares%>%
  #Get only rows in which we have financials
  filter(REG_ANS %in% Baseline_Data$REG_ANS)%>%
  #Join Baseline Financial Data
  left_join(Baseline_Data)%>%
  ungroup()%>%
  #Now, lets calculate a State Share for Each REG_ANS/Year: how many customers do they have in each state (in % of total customers?)
  ungroup()%>%
  mutate(State_Perc=State_Benef/Benef)%>%
  #Lets consider only triples in which the share is greater than 5%
  filter(State_Perc>=0.05)


#7.1.2 Joining CAGED Data
Municipality<-readRDS("Final Datasets\\CAGED.RDS")

State_CAGED<-Municipality%>%select(1:13)%>%
  dplyr::select(-c(Cd_Mun,Mun))%>%
  mutate(across(starts_with('Y'),as.character))%>%
  mutate(across(starts_with('Y'),as.numeric))%>%
  group_by(UF)%>%
  summarize(across(starts_with('Y'),sum,na.rm=TRUE))%>%
  gather(key='Year',value='Flow',-UF)%>%
  mutate(Year=gsub('Y','',Year))

#Joining CAGED
Panel<-Panel%>%left_join(State_CAGED)%>%ungroup()%>%distinct()

#7.1.3 Joining Categorical Data
Panel<-Panel%>%
  left_join(dplyr::select(Cat,Registro_ANS,UF,Modalidade,Ativa,Data_Descredenciamento,Motivo_do_Descredenciamento)%>%
              mutate(UF_Home=UF,REG_ANS=as.character(Registro_ANS))%>%
              dplyr::select(-UF))

#7.1.4 Joining Complaints Data
Complaints<-readRDS('Final Datasets\\Complaints.RDS')
Complaints[,1]<-as.character(Complaints[,1])
Complaints[,2]<-as.character(Complaints[,2])
Panel<-Panel%>%left_join(Complaints)
Panel<-Panel%>%
  mutate(Complaints=Winsorize(Complaints,probs=c(0.01,0.99),na.rm = TRUE))%>%
  mutate(Complaints=ifelse(is.na(Complaints),0,Complaints))

#7.1.5 Preparing Data for the regressions

Regression<-Panel%>%ungroup()%>%
  dplyr::select(DATA,Year,REG_ANS,UF,State_Benef,Benef,everything())%>%
  #Create the Shock Variable: this will be the flow of employment in the year that we are analyzing the outcome
  mutate(Flow=(Flow-mean(Flow,na.rm=TRUE))/sd(Flow,na.rm=TRUE))%>%
  #Our outcome Variables will be a one-year lead, generally in logs, as well as the flow, which is state-specific
  arrange(UF,Year)%>%
  group_by(UF)%>%
  mutate(Flow=dplyr::lead(Flow,1))%>%
  arrange(REG_ANS,UF,Year)%>%
  group_by(REG_ANS,UF)%>%
  mutate(O_State_Benef=dplyr::lead(log(State_Benef),1),
         O_Share=dplyr::lead(Share,1),
         O_OpAssets=dplyr::lead(log(Ativo_Operacional),1),
         O_FinAssets=dplyr::lead(log(Ativo_Financeiro),1),
         O_Assets=dplyr::lead(log(Ativo_Total),1),
         O_OpRevenue=dplyr::lead(log(Rec_Op),1),
         O_EBITDA=dplyr::lead(log(EBITDA),1),
         O_Hazard=dplyr::lead(Sinistralidade,1))%>%
  ungroup()%>%
  #Create categorical variables
  mutate(
    REG_ANS=as.character(REG_ANS),
    Year=as.character(Year),
    Home=ifelse(UF==UF_Home,1,0),
    Coop=ifelse(Modalidade=='Cooperativa Médica',1,0),
    Post=ifelse(as.numeric(Year)>=2012,1,0),
    Size=log(Ativo_Total))%>%
  #Create Year Fixed Dummys for Dynamic Effects: Baseline is 2012
  mutate(Y_2010=ifelse(Year=='2010',1,0),
         Y_2011=ifelse(Year=='2011',1,0),
         Y_2012=ifelse(Year=='2012',1,0),
         Y_2013=ifelse(Year=='2013',1,0),
         Y_2014=ifelse(Year=='2014',1,0),
         Y_2015=ifelse(Year=='2015',1,0),
         Y_2016=ifelse(Year=='2016',1,0),
         Y_2017=ifelse(Year=='2017',1,0),
         Y_2018=ifelse(Year=='2018',1,0),
         Y_2019=ifelse(Year=='2019',1,0))

#Remove Federações, which may cause some discrepancy
Fed<-as.character(c(322547,323993,363286,363774,393321,416576,311961,366811,344150,383520,
                    334511,316741,383520,363944,300870,313971,319996,348406,334511,414573,
                    347361,386596,321958,328031,328294,324213,358282,312720,300896,355691,
                    367087,347361))

Treatment<-Adj_Equity%>%
  rowwise()%>%
  filter(!(REG_ANS %in% Fed))%>%
  mutate(SSM=sum(Adj_Equity,-SM),
         SSM_Perc=SSM/SM)%>%
  ungroup()%>%
  mutate(Q_SSM=ntile(SSM_Perc,10))%>%
  filter(!(Q_SSM %in% c(5,6)))%>%
  mutate(Q_SSM=ifelse(Q_SSM<5,1,0))%>%
  filter(SSM_Perc<quantile(SSM_Perc,0.99,na.rm=TRUE),
         SSM_Perc>quantile(SSM_Perc,0.01,na.rm=TRUE))%>%
  dplyr::select(REG_ANS,Q_SSM,SSM_Perc)

#6.6.1 First, Lets Create the Solvency Sufficiency Measure
Regression<-Regression%>%
  left_join(Treatment,by=c('REG_ANS'))

#Provide new summary stats
SummaryStats=Regression%>%dplyr::filter(Year==2010,Home==1)%>%
  ungroup()%>%
  select(Q_SSM,Benef,SSM_Perc,ROE,Share,Sinistralidade,EBITDA_Mg,Size,Net_Mg,PPE_Ratio)%>%
  ungroup()%>%
  gather(key = 'Measure',value='Measure_Value',-c(Q_SSM))%>%
  group_by(Measure)%>%
  do(tidy(t.test(Measure_Value ~ Q_SSM, data = .)))%>%
  summarize(across(where(is.numeric),round,3))%>%
  dplyr::select(Measure,estimate2,estimate1,estimate,p.value)

names(SummaryStats)<-c('Measure','Treated','Control','Difference','p-val')
SummaryStats[,1]<-c('Customers','EBITDA (%)','YoY Growth in Customers',
                    'Net Margin','PPE','Return on Equity - ROE',
                    'Hazard Ratio','Size (ln[Assets])','SSM (%)')

kable(SummaryStats,format='latex')%>%save_kable(paste0(table_path,'T1.tex'))

#Save
#saveRDS(Regression,'Final Datasets\\Regression Sample.RDS')

#7.2 Final Regression data [CLICK HERE TO JUMP] ####
Regression<-readRDS('Final Datasets\\Regression Sample.RDS')
#7.3 Plot: % of Breaches over time ####

K=data.frame(Year=as.character(seq(2013,2022,1)),K=seq(0.35,1,0.65/9))
F10=Baseline_Data%>%
  select(REG_ANS,Year,PL)%>%
  left_join(Adj_Equity%>%select(SM,REG_ANS))%>%
  left_join(K)%>%
  rowwise()%>%
  mutate(Solvency=PL-SM*K)%>%
  filter(!is.na(Solvency))%>%
  group_by(Year)%>%
  summarize(Result=mean(Solvency<0))%>%
  ggplot(aes(x=Year,y=Result))+
  geom_col()+
  geom_text(aes(y=Result*1.05,label=scales::percent(Result)),col='black')+
  scale_y_continuous(labels = scales::percent)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90,size=15))+
  theme(axis.text.y = element_blank())+
  theme(plot.title = element_text(face='bold',size=20))+
  labs(x='',
       y='% of Firms',
       title='Solvency Margin breaches over time',
       subtitle='% of Firms having Equity levels lower than estimated solvency margin levels needed for each year.')

out.path=paste0(fig_path,"F10.pdf")
pdf(out.path, height = 20, width =30, paper = "USr")
print(grid.arrange(F10,nrow=1, bottom = textGrob("Source: Agência Nacional da Saúde Suplementar (ANS)", x = 0.8)))
dev.off()

#7.4 Plot: Market-share over time ####

F11=Regression%>%
  group_by(Year,Q_SSM)%>%
  summarize(Benef=sum(Benef,na.rm=TRUE))%>%
  group_by(Year)%>%
  mutate(Total_Benef=sum(Benef,na.rm=TRUE),
         Q_SSM=ifelse(is.na(Q_SSM),'Out of Reg. Sample',
                      ifelse(Q_SSM==1,'Treatment','Control')))%>%
  rowwise()%>%
  mutate(Share=Benef/Total_Benef)%>%
  ggplot(aes(x=Year,y=Share,fill=as.factor(Q_SSM)))+
  geom_col()+
  geom_text(aes(y=Share,label=scales::percent(Share,accuracy = 0.1),
                group=as.factor(Q_SSM)),col='white',position = position_stack(vjust = .5))+
  scale_y_continuous(labels = scales::percent)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90,size=15))+
  theme(axis.text.y = element_blank())+
  theme(plot.title = element_text(face='bold',size=20))+
  scale_fill_grey()+
  labs(x='',
       y='',
       fill='Exposure',
       y='% of Firms',
       title='Evolution of Market-Share across groups',
       subtitle='Market-Share (in % of total Customers) relative to the total market.')

out.path=paste0(fig_path,"F11.pdf")
pdf(out.path, height = 20, width =30, paper = "USr")
print(grid.arrange(F11,nrow=1, bottom = textGrob("Source: Agência Nacional da Saúde Suplementar (ANS)", x = 0.8)))
dev.off()












###################################################

###################################################
#Part 7: Firm-Level Results
###################################################
# 7.1 Summary of Results ####
#Summary of results: this is nice! Here, having higher ex ante buffers seems to drive results on the long run.
#Claim: SSM is somewhat out of the control of managers? We could try to look at that.
#Result: firms that were constrained in 2010 grew less over time. 
#So, the SAME firm, suffering from the same shock, the only difference is that treated was lacking resources.
#This effect is concentrated within the subset of non-cooperative firms, but results hold in the overall sample
#Robust from getting SSM PERC OUT
#Robus to the inclusion of Ativa==1. Effect vanishes when looking only at situations where Flow<0. Interesting!

# 7.2 Basic D-D ####
#Key Takeaway: effect is driven by Employment Flow>0
#Robust to the inclusion of +Sinistralidade+PPE_Ratio+ROE+ROI+Net_Mg+Leverage+Current_Ratio

#Set the formulas

formulas <- c(
  O_State_Benef~Q_SSM*Post+controls|0|0|REG_ANS,
  O_State_Benef~Q_SSM*Post+controls|Year|0|REG_ANS,
  O_State_Benef~Q_SSM*Post+controls|UF+Year+REG_ANS|0|REG_ANS,
  O_State_Benef~Q_SSM*Post+controls|as.factor(UF):as.factor(Year)+REG_ANS|0|REG_ANS)

#Add Covariates:

#controls<-c('Post*(State_Perc+Share+Size)','Sinistralidade','PPE_Ratio','ROI','Net_Mg','Leverage','Current_Ratio')
controls<-c('Post*(State_Perc+Share+Size)')
covariates <- paste(controls, collapse=" + ")
formulas <- gsub("+ controls",paste0("+ ", covariates),formulas, fixed= T)

#Apply Formulas
formulas <- lapply(formulas, as.formula)

#Labels
label.FE <- c(rep("$\\checkmark$",4))
label.cluster <- c(rep("Firm",4))
label.dep.var <- c("Dependent Variable: log(1 +$Customers_{i,s,t}$)")
label.covariates<-c('Post','Treated','Treated $\\times$ Post')


# I: Full Sample:

#LaTeX
lm.list <- lapply(formulas,felm, data=Regression)
out.path <- paste0(table_path,"R1.tex")

stargazer(lm.list,
          dep.var.caption=label.dep.var,
          #Columns
          add.lines = list(c("Firm Controls",label.FE),
                           c("Firm Controls $\\times$ Post",label.FE),
                           c("Year FE","No",rep("$\\checkmark$",3)),
                           c("State FE","No","No",rep("$\\checkmark$",1),"No"),
                           c("Firm FE","No","No",rep("$\\checkmark$",1),"No"),
                           c("State-Time FE","No","No","No","$\\checkmark$"),
                           c("Cluster",label.cluster)),
          #Omit
          omit = c("Constant", "State_Perc","Size","Post:State_Perc","Share"),
          omit.stat = c("f","ser","adj.rsq"),
          omit.table.layout =  "d",
          #Labels
          title="OLS - Growth in Customer Base and Solvency Margin Sufficiency",
          covariate.labels=label.covariates,
          type='latex', out=out.path)

# II: Only cooperatives:

#LaTeX
lm.list <- lapply(formulas,felm, data=filter(Regression,Coop==1))
out.path <- paste0(table_path,"R2.tex")

stargazer(lm.list,
          dep.var.caption=label.dep.var,
          #Columns
          add.lines = list(c("Firm Controls",label.FE),
                           c("Firm Controls $\\times$ Post",label.FE),
                           c("Year FE","No",rep("$\\checkmark$",3)),
                           c("State FE","No","No",rep("$\\checkmark$",1),"No"),
                           c("Firm FE","No","No",rep("$\\checkmark$",1),"No"),
                           c("State-Time FE","No","No","No","$\\checkmark$"),
                           c("Cluster",label.cluster)),
          #Omit
          omit = c("Constant", "State_Perc","Size","Post:State_Perc","Share"),
          omit.stat = c("f","ser","adj.rsq"),
          omit.table.layout =  "d",
          #Labels
          title="OLS - Growth in Customer Base and Solvency Margin Sufficiency - Only Health Cooperatives",
          covariate.labels=label.covariates,
          type='latex', out=out.path)


# III: Without cooperatives:

#LaTeX
lm.list <- lapply(formulas,felm, data=filter(Regression,Coop==0))
out.path <- paste0(table_path,"R3.tex")

stargazer(lm.list,
          dep.var.caption=label.dep.var,
          #Columns
          add.lines = list(c("Firm Controls",label.FE),
                           c("Firm Controls $\\times$ Post",label.FE),
                           c("Year FE","No",rep("$\\checkmark$",3)),
                           c("State FE","No","No",rep("$\\checkmark$",1),"No"),
                           c("Firm FE","No","No",rep("$\\checkmark$",1),"No"),
                           c("State-Time FE","No","No","No","$\\checkmark$"),
                           c("Cluster",label.cluster)),
          #Omit
          omit = c("Constant", "State_Perc","Size","Post:State_Perc","Share"),
          omit.stat = c("f","ser","adj.rsq"),
          omit.table.layout =  "d",
          #Labels
          title="OLS - Growth in Customer Base and Solvency Margin Sufficiency - Excluding Health Cooperatives",
          covariate.labels=label.covariates,
          type='latex', out=out.path)


# 7.3 Dynamic Effects Regression: Coop x Non Coop####

#All Sample
R4<-felm(O_State_Benef~Q_SSM*(Y_2010+Y_2012+Y_2013+Y_2014+Y_2015+Y_2016+Y_2017+Y_2018)+
           Post*(State_Perc+Share+Size)-(Post+Q_SSM+Y_2010+Y_2012+Y_2013+Y_2014+Y_2015+Y_2016+Y_2017+Y_2018)|
           as.factor(UF):as.factor(Year)+REG_ANS|0|REG_ANS,exactDOF=TRUE,data=filter(Regression))

Graph_R4<-t(confint(R4,level=0.9))%>%rbind(coefficients(R4))%>%as.data.frame()%>%t()
Graph_R4<-Graph_R4[grep("Q_SSM:Y",rownames(Graph_R4)),]%>%as.data.frame()
Graph_R4<-rbind(Graph_R4[1,],c(0,0,0),Graph_R4[2:8,])
Graph_R4$Year<-as.character(seq(2010,2018,1))
names(Graph_R4)<-c('LI','HI','Estimate','Year')

# Plot it
Graph_R4<-ggplot(Graph_R4,aes(x=Year,y=Estimate))+
  geom_pointrange(aes(ymin=LI,ymax=HI))+
  scale_colour_grey()+
  theme_minimal()+
  geom_point(size=3)+
  scale_y_continuous(labels = scales::percent,limits=c(-0.75,0.75))+
  geom_vline(xintercept = 2,linetype='dashed')+
  geom_hline(yintercept = 0,linetype='dashed')+
  theme(axis.text = element_text(size=12))+
  theme(axis.text.x = element_text(angle = 90))+
  theme(plot.title = element_text(face='bold',size=15))+
  labs(col='Type',
       y='Differential effect for Treated Firms',
       x='',
       title='Panel A: All firms',
       subtitle='Based on the estimates of the Year x Treated')+
  theme(legend.position = 'bottom')

#Without Cooperatives
R5<-felm(O_State_Benef~Q_SSM*(Y_2010+Y_2012+Y_2013+Y_2014+Y_2015+Y_2016+Y_2017+Y_2018)+
           Post*(State_Perc+Share+Size)-(Post+Q_SSM+Y_2010+Y_2012+Y_2013+Y_2014+Y_2015+Y_2016+Y_2017+Y_2018)|
           as.factor(UF):as.factor(Year)+REG_ANS|0|REG_ANS,exactDOF=TRUE,data=filter(Regression,Coop==0))

Graph_R5<-t(confint(R5,level=0.9))%>%rbind(coefficients(R5))%>%as.data.frame()%>%t()
Graph_R5<-Graph_R5[grep("Q_SSM:Y",rownames(Graph_R5)),]%>%as.data.frame()
Graph_R5<-rbind(Graph_R5[1,],c(0,0,0),Graph_R5[2:8,])
Graph_R5$Year<-as.character(seq(2010,2018,1))
names(Graph_R5)<-c('LI','HI','Estimate','Year')

# Plot it
Graph_R5<-ggplot(Graph_R5,aes(x=Year,y=Estimate))+
  geom_pointrange(aes(ymin=LI,ymax=HI))+
  scale_colour_grey()+
  theme_minimal()+
  geom_point(size=3)+
  scale_y_continuous(labels = scales::percent,limits=c(-0.75,0.75))+
  geom_vline(xintercept = 2,linetype='dashed')+
  geom_hline(yintercept = 0,linetype='dashed')+
  theme(axis.text = element_text(size=12))+
  theme(axis.text.x = element_text(angle = 90))+
  theme(plot.title = element_text(face='bold',size=15))+
  labs(col='Type',
       y='Differential effect for Treated Firms',
       x='',
       title='Panel B: Without Health cooperatives',
       subtitle='Based on the estimates of the Year x Treated')+
  theme(legend.position = 'bottom')

R6<-felm(O_State_Benef~Q_SSM*(Y_2010+Y_2012+Y_2013+Y_2014+Y_2015+Y_2016+Y_2017+Y_2018)+
           Post*(State_Perc+Share+Size)-(Post+Q_SSM+Y_2010+Y_2012+Y_2013+Y_2014+Y_2015+Y_2016+Y_2017+Y_2018)|
           as.factor(UF):as.factor(Year)+REG_ANS|0|REG_ANS,exactDOF=TRUE,data=filter(Regression,Coop==1))


Graph_R6<-t(confint(R6,level=0.9))%>%rbind(coefficients(R6))%>%as.data.frame()%>%t()
Graph_R6<-Graph_R6[grep("Q_SSM:Y",rownames(Graph_R6)),]%>%as.data.frame()
Graph_R6<-rbind(Graph_R6[1,],c(0,0,0),Graph_R6[2:8,])
Graph_R6$Year<-as.character(seq(2010,2018,1))
names(Graph_R6)<-c('LI','HI','Estimate','Year')

# Plot it
Graph_R6<-ggplot(Graph_R6,aes(x=Year,y=Estimate))+
  geom_pointrange(aes(ymin=LI,ymax=HI))+
  scale_colour_grey()+
  theme_minimal()+
  geom_point(size=3)+
  scale_y_continuous(labels = scales::percent,limits=c(-0.75,0.75))+
  geom_vline(xintercept = 2,linetype='dashed')+
  geom_hline(yintercept = 0,linetype='dashed')+
  theme(axis.text = element_text(size=12))+
  theme(axis.text.x = element_text(angle = 90))+
  theme(plot.title = element_text(face='bold',size=15))+
  labs(col='Type',
       y='Differential effect for Treated Firms',
       x='',
       title='Panel C: Only Health Cooperatives',
       subtitle='Based on the estimates of the Year x Treated')+
  theme(legend.position = 'bottom')


# PDF graphics device
out.path=paste0(fig_path,"Dynamic_1.pdf")
pdf(out.path, height = 100, width =30, paper = "letter")
grid.arrange(Graph_R4,Graph_R5,Graph_R6,nrow=3)
dev.off()


# 7.4 Robustness 1: Employment Flows####

#Set the formulas
formulas <- c(
  O_State_Benef~Q_SSM*Post+controls|0|0|REG_ANS,
  O_State_Benef~Q_SSM*Post+controls|Year|0|REG_ANS,
  O_State_Benef~Q_SSM*Post+controls|UF+Year+REG_ANS|0|REG_ANS,
  O_State_Benef~Q_SSM*Post+controls|as.factor(UF):as.factor(Year)+REG_ANS|0|REG_ANS)

#Add Covariates:

#controls<-c('Post*(State_Perc+Share+Size)','Sinistralidade','PPE_Ratio','ROI','Net_Mg','Leverage','Current_Ratio')
controls<-c('Post*(State_Perc+Share+Size)')
covariates <- paste(controls, collapse=" + ")
formulas <- gsub("+ controls",paste0("+ ", covariates),formulas, fixed= T)

#Apply Formulas
formulas <- lapply(formulas, as.formula)

# I: Employment Flow >0:

#LaTeX
lm.list <- lapply(formulas,felm, data=filter(Regression,Flow>0,Coop==0))
out.path <- paste0(table_path,"R4.tex")

stargazer(lm.list,
          dep.var.caption=label.dep.var,
          #Columns
          add.lines = list(c("Firm Controls",label.FE),
                           c("Firm Controls $\\times$ Post",label.FE),
                           c("Year FE","No",rep("$\\checkmark$",3)),
                           c("State FE","No","No",rep("$\\checkmark$",1),"No"),
                           c("Firm FE","No","No",rep("$\\checkmark$",1),"No"),
                           c("State-Time FE","No","No","No","$\\checkmark$"),
                           c("Cluster",label.cluster)),
          #Omit
          omit = c("Constant", "State_Perc","Size","Post:State_Perc","Share"),
          omit.stat = c("f","ser","adj.rsq"),
          omit.table.layout =  "d",
          #Labels
          title="OLS - Growth in Customer Base and Solvency Margin Sufficiency",
          covariate.labels=label.covariates,
          type='latex', out=out.path)

# II: Employment Flow <0:

#LaTeX
lm.list <- lapply(formulas,felm, data=filter(Regression,Flow<=0,Coop==0))
out.path <- paste0(table_path,"R5.tex")

stargazer(lm.list,
          dep.var.caption=label.dep.var,
          #Columns
          add.lines = list(c("Firm Controls",label.FE),
                           c("Firm Controls $\\times$ Post",label.FE),
                           c("Year FE","No",rep("$\\checkmark$",3)),
                           c("State FE","No","No",rep("$\\checkmark$",1),"No"),
                           c("Firm FE","No","No",rep("$\\checkmark$",1),"No"),
                           c("State-Time FE","No","No","No","$\\checkmark$"),
                           c("Cluster",label.cluster)),
          #Omit
          omit = c("Constant", "State_Perc","Size","Post:State_Perc","Share"),
          omit.stat = c("f","ser","adj.rsq"),
          omit.table.layout =  "d",
          #Labels
          title="OLS - Growth in Customer Base and Solvency Margin Sufficiency",
          covariate.labels=label.covariates,
          type='latex', out=out.path)


# 7.5 Robustness 2: Dynamic Effects Regression: Home x Away ####

#Coop==0, Home==1
R4<-felm(O_State_Benef~Q_SSM*(Y_2010+Y_2012+Y_2013+Y_2014+Y_2015+Y_2016+Y_2017+Y_2018)+
           Post*(State_Perc+Share+Size)-(Post+Q_SSM+Y_2010+Y_2012+Y_2013+Y_2014+Y_2015+Y_2016+Y_2017+Y_2018)|
           as.factor(UF):as.factor(Year)+REG_ANS|0|REG_ANS,exactDOF=TRUE,data=filter(Regression,Home==1,Coop==0))

Graph_R4<-t(confint(R4,level=0.9))%>%rbind(coefficients(R4))%>%as.data.frame()%>%t()
Graph_R4<-Graph_R4[grep("Q_SSM:Y",rownames(Graph_R4)),]%>%as.data.frame()
Graph_R4<-rbind(Graph_R4[1,],c(0,0,0),Graph_R4[2:8,])
Graph_R4$Year<-as.character(seq(2010,2018,1))
names(Graph_R4)<-c('LI','HI','Estimate','Year')

# Plot it
Graph_R4<-ggplot(Graph_R4,aes(x=Year,y=Estimate))+
  geom_pointrange(aes(ymin=LI,ymax=HI))+
  scale_colour_grey()+
  theme_minimal()+
  geom_point(size=3)+
  scale_y_continuous(labels = scales::percent,limits=c(-0.75,0.75))+
  geom_vline(xintercept = 2,linetype='dashed')+
  geom_hline(yintercept = 0,linetype='dashed')+
  theme(axis.text = element_text(size=12))+
  theme(axis.text.x = element_text(angle = 90))+
  theme(plot.title = element_text(face='bold',size=15))+
  labs(col='Type',
       y='Differential effect for Treated Firms',
       x='',
       title='Panel A: Without Health-Cooperatives, only home State',
       subtitle='Based on the estimates of the Year x Treated')+
  theme(legend.position = 'bottom')

#Coop==0,Home==0
R5<-felm(O_State_Benef~Q_SSM*(Y_2010+Y_2012+Y_2013+Y_2014+Y_2015+Y_2016+Y_2017+Y_2018)+
           Post*(State_Perc+Share+Size)-(Post+Q_SSM+Y_2010+Y_2012+Y_2013+Y_2014+Y_2015+Y_2016+Y_2017+Y_2018)|
           as.factor(UF):as.factor(Year)+REG_ANS|0|REG_ANS,exactDOF=TRUE,data=filter(Regression,Coop==0,Home==0))

Graph_R5<-t(confint(R5,level=0.9))%>%rbind(coefficients(R5))%>%as.data.frame()%>%t()
Graph_R5<-Graph_R5[grep("Q_SSM:Y",rownames(Graph_R5)),]%>%as.data.frame()
Graph_R5<-rbind(Graph_R5[1,],c(0,0,0),Graph_R5[2:8,])
Graph_R5$Year<-as.character(seq(2010,2018,1))
names(Graph_R5)<-c('LI','HI','Estimate','Year')

# Plot it
Graph_R5<-ggplot(Graph_R5,aes(x=Year,y=Estimate))+
  geom_pointrange(aes(ymin=LI,ymax=HI))+
  scale_colour_grey()+
  theme_minimal()+
  geom_point(size=3)+
  scale_y_continuous(labels = scales::percent,limits=c(-1.2,1.2))+
  geom_vline(xintercept = 2,linetype='dashed')+
  geom_hline(yintercept = 0,linetype='dashed')+
  theme(axis.text = element_text(size=12))+
  theme(axis.text.x = element_text(angle = 90))+
  theme(plot.title = element_text(face='bold',size=15))+
  labs(col='Type',
       y='Differential effect for Treated Firms - Without Health-Cooperatives, only home State',
       x='',
       title='Panel A: Without Health-Cooperativas, only away States',
       subtitle='Based on the estimates of the Year x Treated')+
  theme(legend.position = 'bottom')

# PDF graphics device
out.path=paste0(fig_path,"Dynamic_2.pdf")
pdf(out.path, height = 100, width =30, paper = "letter")
grid.arrange(Graph_R4,Graph_R5,nrow=2)
dev.off()



# 7.6 Robustness 3: Matching Approach (by Year, UF, and Home/Away State)####
Matching<-Regression%>%filter(Coop==1)%>%
  dplyr::select(Growth_Cust,Q_SSM,Year,UF,Home,ROI,ROE,Sinistralidade,EBITDA_Mg,Size,Net_Mg,PPE_Ratio,State_Benef)%>%
  drop_na()%>%
  mutate(UF=as.numeric(as.factor(UF)))%>%
  mutate_all(as.numeric)

Y=Matching$Growth_Cust
Tr=Matching$Q_SSM
Cat=Matching[,3:5]
X=Matching[,6:13]

library(Matching)

#A drop of -1.2%. Given that the baseline customer growth is 2.05, this implies almost 50% lower growth.
MatchingEst=Matchby(Y=Y,by=Cat,Tr=Tr,X=X[,1:5],estimand = 'ATT',AI=TRUE,ties = TRUE)

#Covariate Balance
MatchBalance(Q_SSM~State_Benef+ROI+ROE+Sinistralidade+EBITDA_Mg+Size+Net_Mg+PPE_Ratio,data=Matching, match.out=MatchingEst, nboots=5000)


# 7.7 IV Regression: firms' financial outcomes ##### 

#Here, we'll use the fact that these firms are indeed losing market share.
#Since bad firms would lose anyway, we instrumentalize growth in terms of out previous regressions
#How the induced lesser growth in customer base transmits to firms decisions?
#Results show that Op Assets and Revenues are affected, which is what we would expect!

formulas<-c(
  O_Assets~Post*(State_Perc+Share+Size)-Post|as.factor(UF):as.factor(Year)+REG_ANS|(O_State_Benef~Q_SSM*Post+Post*(State_Perc+Share+Size))|REG_ANS,
  O_FinAssets~Post*(State_Perc+Share+Size)-Post|as.factor(UF):as.factor(Year)+REG_ANS|(O_State_Benef~Q_SSM*Post+Post*(State_Perc+Share+Size))|REG_ANS,
  O_OpAssets~Post*(State_Perc+Share+Size)-Post|as.factor(UF):as.factor(Year)+REG_ANS|(O_State_Benef~Q_SSM*Post+Post*(State_Perc+Share+Size))|REG_ANS,
  O_OpRevenue~Post*(State_Perc+Share+Size)-Post|as.factor(UF):as.factor(Year)+REG_ANS|(O_State_Benef~Q_SSM*Post+Post*(State_Perc+Share+Size))|REG_ANS,
  O_Hazard~Post*(State_Perc+Share+Size)-Post|as.factor(UF):as.factor(Year)+REG_ANS|(O_State_Benef~Q_SSM*Post+Post*(State_Perc+Share+Size))|REG_ANS,
  O_EBITDA~Post*(State_Perc+Share+Size)-Post|as.factor(UF):as.factor(Year)+REG_ANS|(O_State_Benef~Q_SSM*Post+Post*(State_Perc+Share+Size))|REG_ANS)

#Apply Formulas
formulas <- lapply(formulas, as.formula)

#Labels
label.FE <- c(rep("$\\checkmark$",6))
label.cluster <- c(rep("Firm",6))
label.dep.var <- c("Dependent Variable:")
label.columns<-c("$\\Delta$ Assets",
                 "$\\Delta$ Fin. Assets",
                 "$\\Delta$ Op. Assets",
                 "$\\Delta$ Op. Revenue",
                 "$\\Hazard Ratio",
                 "$\\Delta$ EBITDA")

label.covariates<-c('$\\widehat{Cust}$')

#LaTeX
lm.list <- lapply(formulas,felm, data=filter(Regression,Home==1))
out.path <- paste0(table_path,"R6.tex")

stargazer(lm.list,
          dep.var.caption=label.dep.var,
          column.labels = label.columns,
          #Columns
          add.lines = list(c("Full Firm Controls",label.FE),
                           c("Year FE",rep("$\\checkmark$",6)),
                           c("State FE",rep("$\\checkmark$",6)),
                           c("Firm FE",rep("$\\checkmark$",6)),
                           c("State-Sector FE",rep("$\\checkmark$",6)),
                           c("Cluster",label.cluster)),
          #Omit
          omit = c("Constant", "State_Perc","Size","Post:State_Perc","Share","Post:Size","Post:Share"),
          omit.stat = c("f","ser","adj.rsq"),
          omit.table.layout =  "d",
          #Labels
          title="IV - Second-order effects on firms' outcomes",
          covariate.labels=label.covariates,
          type='latex', out=out.path)


# 7.8 IV Regression: Probability of Survival####
#Adjust this: probability of each event and overall prob of delisting.
Survival<-Regression%>%dplyr::filter(Home==1)%>%
  mutate(Dead_Date=ifelse(Data_Descredenciamento=="NA",NA,as.numeric(Data_Descredenciamento)))%>%
  mutate(Dead=ifelse(Dead_Date>=Year,1,0))%>%
  mutate(Dead=ifelse(is.na(Dead),0,Dead))

# Average delisting rate is: 11%
mean(Surv_Analysis$Dead,na.rm=TRUE)

#Reduces the likelihood of delisting by -0.09 x 20 (avg effect)% = -0.018%. 0.018/0.11 =~ 16% more likely to delist
R7<-felm(Dead~Post*(State_Perc+Share+Size)|as.factor(UF):as.factor(Year)|(O_State_Benef~Q_SSM*Post+Post*(State_Perc+Share+Size))|REG_ANS,data=Surv_Analysis)

label.FE <- c(rep("$\\checkmark$",1))
label.cluster <- c(rep("Firm",1))
label.dep.var <- c("Dependent Variable:")
label.columns<-c("Survival")
label.covariates<-c('$\\widehat{Cust}$')

#LaTeX
out.path <- paste0(table_path,"R7.tex")

stargazer(R7,
          dep.var.caption=label.dep.var,
          column.labels = label.columns,
          #Columns
          add.lines = list(c("Full Firm Controls",label.FE),
                           c("Year FE",rep("$\\checkmark$",1)),
                           c("State FE",rep("$\\checkmark$",1)),
                           c("Firm FE",rep("$\\checkmark$",1)),
                           c("State-Sector FE",rep("$\\checkmark$",1)),
                           c("Cluster",label.cluster)),
          #Omit
          omit = c("Constant", "State_Perc","Size","Post:State_Perc","Share","Post:Size","Post:Share","Post"),
          omit.stat = c("f","ser","adj.rsq"),
          omit.table.layout =  "d",
          #Labels
          title="IV - Second-order effects on firms' outcomes - Likelihood of delisting",
          covariate.labels=label.covariates,
          type='latex', out=out.path)

#Tease out the specific motives
library(dummies)

Dummy=dummy(Survival$Motivo_do_Descredenciamento)%>%as.data.frame()
names(Dummy)[1:7]<-c('D_Alt_Deliberacao','D_Deliberacao','D_Liquidacao','NA','D_Pedido_Cancelamento','D_Canc_Pos_Cisao','D_Incorporacao')
Dummy$D_Deliberacao<-Dummy[,"D_Alt_Deliberacao"]+Dummy[,"D_Deliberacao"]
Dummy$D_Cancelamento<-Dummy[,"D_Pedido_Cancelamento"]+Dummy[,"D_Canc_Pos_Cisao"]
Dummy=Dummy%>%select(D_Deliberacao,D_Cancelamento,D_Incorporacao,D_Liquidacao)


Survival<-cbind(Surv_Analysis,Dummy)
#Survival<-cbind(Surv_Analysis,Dummy)%>%filter(REG_ANS %in% unique(filter(Surv_Analysis,Ativa!=1)$REG_ANS))

formulas<-c(
  D_Deliberacao~Post*(State_Perc+Share+Size)|as.factor(UF):as.factor(Year)|(O_State_Benef~Q_SSM*Post+Post*(State_Perc+Share+Size))|REG_ANS,
  D_Cancelamento~Post*(State_Perc+Share+Size)|as.factor(UF):as.factor(Year)|(O_State_Benef~Q_SSM*Post+Post*(State_Perc+Share+Size))|REG_ANS,
  D_Incorporacao~Post*(State_Perc+Share+Size)|as.factor(UF):as.factor(Year)|(O_State_Benef~Q_SSM*Post+Post*(State_Perc+Share+Size))|REG_ANS,
  D_Liquidacao~Post*(State_Perc+Share+Size)|as.factor(UF):as.factor(Year)|(O_State_Benef~Q_SSM*Post+Post*(State_Perc+Share+Size))|REG_ANS)

#Apply Formulas
formulas <- lapply(formulas, as.formula)

#Labels
label.FE <- c(rep("$\\checkmark$",4))
label.cluster <- c(rep("Firm",4))
label.dep.var <- c("Dependent Variable:")
label.columns<-c("$\\RAG",
                 "$\\Cancellation",
                 "$\\Incorporation",
                 "$\\Liquidation")

label.covariates<-c('$\\widehat{Cust}$')

#LaTeX
lm.list <- lapply(formulas,felm, data=Survival)
out.path <- paste0(table_path,"R8.tex")

stargazer(lm.list,
          dep.var.caption=label.dep.var,
          column.labels = label.columns,
          #Columns
          add.lines = list(c("Full Firm Controls",label.FE),
                           c("Year FE",rep("$\\checkmark$",4)),
                           c("State FE",rep("$\\checkmark$",4)),
                           c("Firm FE",rep("$\\checkmark$",4)),
                           c("State-Sector FE",rep("$\\checkmark$",4)),
                           c("Cluster",label.cluster)),
          #Omit
          omit = c("Constant", "State_Perc","Size","Post:State_Perc","Share","Post:Size","Post:Share","Post"),
          omit.stat = c("f","ser","adj.rsq"),
          omit.table.layout =  "d",
          #Labels
          title="IV - Second-order effects on firms' outcomes - Delisting Motivations",
          covariate.labels=label.covariates,
          type='latex', out=out.path)








###################################################

###################################################
#Part 8: State-Level Results (TO-DO)
###################################################
#Setup the data ####

#HHI
State_HHI=Regression%>%
  dplyr::select(Year,UF,State_Benef)%>%
  arrange(Year,UF)%>%
  group_by(Year,UF)%>%
  mutate(Share=State_Benef/sum(State_Benef,na.rm=TRUE))%>%
  summarize(HHI=sum(Share^2,na.rm=TRUE),
            State_Benef=sum(State_Benef))%>%
  left_join(dplyr::select(Regression,c(UF,Year,Post))%>%distinct())

# Internations
State_Internations=read.csv('C:/Users/Lucas/Desktop/Internations.csv')
names(State_Internations)[1]='Date'

State_Internations=State_Internations%>%
  mutate(Date=as.Date(Date,format='%m/%d/%Y'))%>%
  filter(month(Date)==12)%>%
  mutate(Year=as.character(year(Date)))%>%
  select(-Date)%>%
  gather(key='UF',value = 'Internations',-Year)%>%
  mutate(Internations=as.numeric(Internations))

# Outpatient Procedures
State_Outpatient=read.csv('C:/Users/Lucas/Desktop/Outpatients.csv')
names(State_Outpatient)[1]='Date'

State_Outpatient=State_Outpatient%>%
  mutate(Date=as.Date(Date,format='%m/%d/%Y'))%>%
  filter(month(Date)==12)%>%
  mutate(Year=as.character(year(Date)))%>%
  select(-Date)%>%
  gather(key='UF',value = 'Outpatient',-Year)%>%
  mutate(Outpatient=as.numeric(Outpatient))

State_Regression=Regression%>%
  dplyr::select(SSM_Perc,State_Benef,Year,UF,Size,Post,Flow,Complaints)%>%
  group_by(Year,UF)%>%
  summarize(Q_SSM=weighted.mean(SSM_Perc,Size,na.rm=TRUE),
            Flow=weighted.mean(Flow,Size,na.rm=TRUE),
            Complaints=sum(Complaints,na.rm=TRUE))%>%
  filter(!is.nan(Q_SSM))%>%
  left_join(State_HHI)%>%
  left_join(State_Internations)%>%
  left_join(State_Outpatient)%>%
  ungroup()%>%
  mutate(Q_SSM=ntile(Q_SSM,5),
         Q_SSM=Q_SSM<median(Q_SSM))


#Regressions
# HHI Regression ####

  formulas<-c(
    log(1+HHI)~Post+Q_SSM|UF+Year|0|UF,
    log(1+HHI)~Post*Q_SSM|UF+Year|0|UF,
    log(1+HHI)~Post*Flow*Q_SSM|UF+Year|0|UF)
  
  #Apply Formulas
  formulas <- lapply(formulas, as.formula)
  
  #LaTeX
  lm.list <- lapply(formulas,felm, data=State_Regression)
  out.path <- paste0(table_path,"R9.tex")

  #Render Results
  stargazer(lm.list,
            dep.var.caption="log(1+$HHI_{s,t}$)",
            #Columns
            add.lines = list(c("Year FE",rep("$\\checkmark$",3)),
                             c("State FE",rep("$\\checkmark$",3)),
                             c("Cluster",rep("State",3))),
            #Omit
            omit = c("Constant"),
            omit.stat = c("f","ser","adj.rsq"),
            omit.table.layout =  "d",
            #Labels
            title="OLS - Growth in Customer Base and Solvency Margin Sufficiency - Excluding Health Cooperatives",
            covariate.labels=c('$Rule$','$EmpFlow$','$Exposure$','$Rule \\times EmpFlow$','$Rule \\times Exposure$',
                               '$EmpFlow \\times Exposure$', '$Rule \\times EmpFlow \\times Exposure $'),
            type='latex', out=out.path)
  
  PreTest<-Regression%>%dplyr::filter(Home==1,Year==2010,Ativa==1)%>%
  group_by(Coop)%>%
  mutate(Q_SSM=ntile(Ativo_Total/PL,5))%>%dplyr::select(REG_ANS,Q_SSM,Coop)


  
  
  
  
  
  
  
  
  
  
  
  
  
Leverage<-Regression%>%dplyr::filter(REG_ANS %in% PreTest$REG_ANS)%>%
  select(-c(Q_SSM,Coop))%>%
  left_join(PreTest)%>%
  group_by(Q_SSM,Year,Coop)%>%
  summarize(SSM=median(Ativo_Total/PL,na.rm=TRUE))

ggplot(Leverage,aes(x=Year,y=SSM,group=Q_SSM,col=as.factor(Q_SSM)))+
  geom_line(size=1)+
  theme_minimal()+
  theme(axis.text = element_text(size=12))+
  theme(axis.text.x = element_text(angle = 90))+
  theme(plot.title = element_text(face='bold',size=15))+
  facet_wrap(Coop~.,nrow=2,labeller = labeller(Coop=c(`0`='All Sample, Excluding Health Cooperatives',`1`='Health Cooperatives')))+
  labs(y='SSM (%)',x='',col='Quintiles of SSM (%) as of 2010',
       title='Median Equity Buffers, by Quintiles',subtitle='Based on the distribution of SSM (%) as of 2010.')+
  theme(legend.position = 'bottom')


# Health Outcomes

# Health Outcomes Regression ####

formulas<-c(log(Internations)~Post*Flow*Q_SSM|UF+Year|0|UF,
            log(Outpatient)~Post*Flow*Q_SSM|UF+Year|0|UF)

#Apply Formulas
formulas <- lapply(formulas, as.formula)

#LaTeX
lm.list <- lapply(formulas,felm, data=State_Regression)
out.path <- paste0(table_path,"R12.tex")

label.dep.var <- c("Dependent Variable:")
label.columns<-c("log(Internations)","log(Outpatient)")

#Render Results
stargazer(lm.list,
          dep.var.caption = label.dep.var,
          column.labels=label.columns,
          #Columns
          add.lines = list(c("Year FE",rep("$\\checkmark$",2)),
                           c("State FE",rep("$\\checkmark$",2)),
                           c("Cluster",rep("State",2))),
          #Omit
          omit = c("Constant"),
          omit.stat = c("f","ser","adj.rsq"),
          omit.table.layout =  "d",
          #Labels
          title="State-level Health Outcomes according to Exposure to Solvency Margin",
          covariate.labels=c('$Rule$','$EmpFlow$','$Exposure$','$Rule \\times EmpFlow$','$Rule \\times Exposure$',
                             '$EmpFlow \\times Exposure$', '$Rule \\times EmpFlow \\times Exposure $'),
          type='latex', out=out.path)

###################################################


###################################################
#Part 9: Pricing
###################################################



