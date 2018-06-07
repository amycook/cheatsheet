# Ben questions
# - how to save part of the environment
# - how to remove a list of elements from the environemtn
# - how to ensemblee predictions - existing package?
# - something wrong with my nested for loops


###produce data frame of count of each unique number of rows
predict.df2<-(ddply(predict.df,.(CustomerID,X1,X2,X3),nrow))

#dplyr otuput dataframes for each group
iris %>% group_by(Species) %>% do(head(.,3))

# dplyr new variable, calculations by group - group_by within mutate
iris <- iris %>% group_by(Species) %>% mutate(meaan = mean(Sepal.Length))

#can replace ddply with 
my_fun <- function(x, y){
        res_x = mean(x) + 2
        res_y = mean(y) * 5 
        return(data.frame(res_x, res_y))
}
# If the argument is named the result is:
# Apply my_fun() function to ds by group
iris %>% group_by(Species) %>% do(my_fun(x=.$Sepal.Length, y=.$Sepal.Width))


#ddply alternative
Sept.IT %>% group_by(AnimalIndexSK) %>% do(tail(.,1))

# summarise function while keeping all rows
iris %>% group_by(Species) %>% mutate(min(.$Sepal.Length))

#### function - for loop to create multiple objects with different names
multi.forest<-function(train,nforest=1:10){
        sample=double(32)
        for (i in 1:length(nforest)){
                sample=factor(sample(unique(train$IPCClass),size=32,
                                     replace=FALSE))
                t20.sample=train[train$IPCClass %in% sample,];
                t20.sample$IPCClass=factor(t20.sample$IPCClass)
                assign(paste("Forest",i,sep=""),randomForest(IPCClass~
                                                                     VisitFrequency+NumberEmployees+Day+Month+
                                                                     AvMonthlyPurch+AvMonthlySpend+Av.Sale,
                                                             data=t20.sample,
                                                             ntree=400,mtry=4),envir=.GlobalEnv)
                cat(i," ")
        }
        
}


### function to delete duplicates of CustomerID rows by choosing majority vote


final.rec<-function(Input=predict.df2){
        idunique=sort(unique(Input$CustomerID))
        Answer=data.frame(matrix(0,nrow=length(idunique),ncol=ncol(Input)))
        colnames(Answer)=colnames(Input)
        for (i in 1:length(idunique)){
                Subset=Input[Input$CustomerID==idunique[i],]
                maxv1=max(Subset$V1)
                Chosen = Subset[Subset$V1==maxv1,]
                Answer[i,] = Chosen[1,]
        }
        return(Answer)
}

rec<-final.rec(Input=predict.df2)
head(rec,20)

head(rec,30)

# find duplicates, and print original and duplicate
# check for duplicates
CGC %>% select(Nomenclature, date, Enterococci1) %>% 
        filter(duplicated(.) | duplicated(., fromLast = TRUE)) %>% head

dup.index <- c(CGC %>% select(Nomenclature, date, Enterococci1) %>% duplicated() %>% which(arr.ind = TRUE), 
               CGC %>% select(Nomenclature, date, Enterococci1) %>% duplicated(fromLast = TRUE) %>%
                       which(arr.ind = TRUE))

#have successfully created a prediction data frame - lists each customer ID and the top 3
#recommendations

#reorder each row of matrix to output the highest three fractions in order from highest to lowest

list<-function(predict){
        vector<-NULL
        topthree<-NULL
        output<-NULL
        for (i in 1:nrow(predict)){
                vector<-sort(predict[i,],decreasing=TRUE)[1:3]
                topthree<-rownames(as.matrix(vector))
                output<-rbind(output,topthree)
        }
        output
        
}    
top3.predict<-list(t20.predict)



#now lets vary the number of trees.. 
```{r optimise_ntree,echo=FALSE}
oob.err<-double(4)
ntree<-c(100,200,300,400)
for(i in 1:4){
        fit<-randomForest(IPCClass~
                                  VisitFrequency+NumberEmployees+Day+Month+
                                  AvMonthlyPurch+AvMonthlySpend+Av.Sale,
                          data=t20.sample,mtry=4,ntree=ntree[i])
        oob.err[i]<-fit$err.rate[ntree[i]]
        cat(ntree," ")
}

qplot(1:6,oob.err,geom=c("point","line"),color="pink")

```

#optimise mtry value..

```{r optimise_mtry,echo=FALSE}
oob.err<-double(10)
for(mtry in 1:10){
        fit<-randomForest(IPCClass~
                                  VisitFrequency+NumberEmployees+Day+Month+
                                  AvMonthlyPurch+AvMonthlySpend+Av.Sale,
                          data=t20.sample,mtry=mtry,ntree=500)
        oob.err[mtry]<-fit$err.rate[500]
        cat(mtry," ")
}

qplot(1:mtry,oob.err,geom=c("point","line"),color="pink")

```
# nice ggplot changing angle of x axis labels
Nonevent.graph<-ggplot(Nonevent[1:20,],aes(x=reorder(x,-freq),y=freq))+
        geom_bar(stat="identity")+
        theme(legend.position="none", axis.text.x=element_text(angle=45,hjust=1),
               )+
        labs(title = "'Non-event' top 20")
#complicated plot to look good in Word. refer 'dataclean.R' in barrow island folder. tick marks
Loc.scat.u10<-ggplot(Genspec.Loc[Genspec.Loc$V1<10& Genspec.Loc$V1>1,],aes(x=GenSpec,y=V1))+
        geom_point(aes(colour=LocationBIW,size=LocationBIW))+
        coord_flip()+ylab("Count")+
        scale_y_continuous(breaks = round(seq(min(Genspec.Loc$V1)-1, max(Genspec.Loc$V1), by = 1),0))+
        labs(title = "Locations with species counts under 10 and more than 1")+
        theme(legend.position=c(.3,-.17),legend.title=element_blank(),legend.text=element_text(size=8),
              plot.margin=unit(c(0,0,4,0),"cm"))+
        guides(col=guide_legend(ncol=4))



#create a barplot of each variable in your data frame
bar.plots<-function(df,path){
        nm=colnames(df)
        for(i in seq_along(nm)){
                #remove NA's optional below
                plots<-ggplot(df[!(is.na(df[,i])),],aes_string(x=nm[i]))+
                        geom_bar()+
                        theme(axis.text.x=element_text(angle=45,hjust=1))
                ggsave(plots,filename=paste("barplot",nm[i],".png",sep=""),path=path)
                
                cat(i," ")
        }
}

#ggplot - categorical with two lines - need to melt two columns into one
NISloc.plot<-ggplot(NISloc.graph,aes(x=Var1, y=percent.NIS, group = Incident_category))+
        geom_line(aes(colour= Incident_category))+
        theme(legend.position="bottom",text=element_text(size=11),axis.text.x=element_text(angle=45,hjust=1))+
        labs(title = "% of NIS found at each location")+
        guides(fill=guide_legend(nrow=2))


#drop columns by name
drops<-c('Priority','Product','Innovation','Total.Costs','Job.Size','test','Order.Value')
BT13<-BT13[,!(names(BT13) %in% drops)]
# eliminate spaces in column names
names(LCC) <- gsub(" ", "_", names(LCC))


#row shift function- use values from other rows
#refer this site http://stackoverflow.com/questions/14689424/use-a-value-from-the-previous-row-in-an-r-data-table-calculation
rowShift <- function(x, shiftLen = 1L) {
        r <- (1L + shiftLen):(length(x) + shiftLen)
        r[r<1] <- NA
        return(x[r])
}

#skim the top x number of entried per level in a dataframe
skim<- function(df= PC, col=PC$Post.Code, n=1){
        ref= unique(col)
        uniq= df[rep(FALSE,times=nrow(df)),]
        for(i in 1:length(ref)){
                rows= df[col %in% ref[i],]
                comp= rows[complete.cases(rows),][n,]
                uniq= rbind(uniq,comp)
        }
        return(uniq)
}


#split up column into multiple strings - as done originally with titanic data.
combi$Name<-as.character(combi$Name)
combi$Name[1]
#use strsplit - string split - to break apart names using the two symbols in the names: comma and period
strsplit(combi$Name[1],split='[,.]')[[1]]
strsplit(combi$Name[1],split='[,.]')[[1]][2]
# tidyr::separate
df <- data.frame(x = c(NA, "a.b", "a.d", "b.c"))
df %>% separate(x, c("A", "B"))

#create a function to APPLY string split over each row

combi$Title<-sapply(combi$Name,FUN=function(x){strsplit(x,split='[,.]')[[1]][2]})
head(combi$Title)
#remove space in front of title
combi$Title<-sub(' ','',combi$Title)
table(combi$Title)
#combine french mademoiselle and madame
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
#delete leading and trailing white space
# returns string w/o leading whitespace
trim.leading <- function (x)  sub("^\\s+", "", x)
# returns string w/o trailing whitespace
trim.trailing <- function (x) sub("\\s+$", "", x)
# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#merging data sets
df1 = data.frame(CustomerId=c(1:6),Product=c(rep("Toaster",3),rep("Radio",3)))
df2 = data.frame(CustomerId=c(2,4,6),State=c(rep("Alabama",2),rep("Ohio",1)))
df1
df2
merge(df1, df2, by='CustomerId')
merge(df1, df2, by='CustomerId', all=TRUE)
merge(df1, df2, by='CustomerId', all.x=TRUE)
merge(df1, df2, by='CustomerId', all.y=TRUE)
merge(df1, df2, by=NULL)

#drop levels in a variable/factor

invoices$Account.Status<- droplevels(invoices$Account.Status)

#drop levels in each column that is a factor

all.vars<- colnames(all7c %>% select(-mlsto))

for (i in 1:length(all.vars)){
        if(all7c[,all.vars[i]] %>% class == 'factor'){
                all7c[,all.vars[i]]<- droplevels(all7c[,all.vars[i]])
        }
        
} 

#redo levels, reorder levels
LocBIW.ordered <- within(species, 
                         LocationBIW <- factor(LocationBIW, 
                                               levels=names(sort(table(LocationBIW), 
                                                                 decreasing=FALSE))))
#try forcats
forcats::fct_reorder


###RENAME COLUMNS###
colnames(invoices)[names(invoices) %in% 'Job']<-'Job.Name'
mtcars <- rename(mtcars, new.name = disp) #dplyr, name on left is new name
colnames(mins30)[grepl("?", colnames(mins30))] <- gsub("?", "", colnames(mins30)[grepl("?", colnames(mins30))]) 

iris$Petal.Length<- select(iris, petaltal =Petal.Length)
#try >>>>> setnames(dt, 'Created Date', 'CreatedDate')

#delete variable/column
invoices<-invoices[,!(names(invoices) %in% 'Created.Date')]

# select columns
mins30 %>% select_if(grepl('hatch_disperser|Matte.Temp|Timestamp|Slag.Temp', names(.)))

#convert data to long set for ggplot, melt
trap.graph<-melt(trap.graph,value.name='Var1')

#reorder a data frame
NIS.location<-NIS.location[order(-NIS.location$freq),]

#write.csv to a path
write.csv(invert.count, 'C:/Users/n9232371/Documents/barrow/FID data/surv_invcount.csv')

#rename a level
levels(x)[levels(x)=="beta"] <- "two"

#alternate - relabel a few levels at once
CobieRN<- c(grep('Cobie', client$client2),grep('COBie', client$client2))
client$client2[CobieRN]<- 'Cobie Group'

# apply vector of greps to strings
sapply(key.oCR$iteration, grep, colnames(CR))

#new column with labels based off other column - grep
all4b$JD.Primary<- ifelse(grepl(paste3(vec7,collapse="|"),all4b$Job.Name, ignore.case=TRUE) & !(all4b$Discipline %in% 'Structural'),
                          colnames(cat)[7], all4b$JD.Primary)

#table with percentages by column
Class.table<-round(prop.table(table(sur.invert$CE.MethodFull,sur.invert$Class),2)*100,2)
Class.table<-as.data.frame.matrix(Class.table) #optional - not req'd with kable in markdown

# delete rows with certain columns NA
data[complete.cases(data[,c(2,3,5)]),]  
# delete rows with a certain total number of NA's in a row
details<-details[rowSums(is.na(details))!=4,]

#divide one column by another etc. transform function
airquality<- transform(airquality, new = -Ozone, Temp = (Temp-32)/1.8)

#shorten date to month and year
pass$Date<- format(pass$Date,"%Y-%b")

#remove rows with n number of NA's
df <- df[rowSums(is.na(df)) < n, ]

#heaps of letters
LETTERS702 <- c(LETTERS, sapply(LETTERS, function(x) paste0(x, LETTERS)))

#take a random sample of a dataframe to check how it's shaping up
test2[sample(1:nrow(test2),20),]

# subset certain columns, rows, and row numbers
all4[all4$Discipline %in% 'Structural',names(all4) %in% c('Director','Job.Number','Job.Name')][1:50,]

#loop through grepping function
#singPC$Suburb is a vector of the list of words to loop through (and grep through)
#check if grepped words are in 'SUBS$SUBURB' vector.
match<- NULL
for(i in 1:nrow(singPC)){
        RN= grep(paste("\\b",singPC[i,]$Suburb,"\\b", sep="") , SUBS$SUBURB)
        mi= data.frame('row'=RN, 'Suburb'= rep(singPC[i,]$Suburb, length(RN)))
        match= rbind(match, mi)
}
return(match)

#function to generate lm or aov as you choose, slicing the dataset in data and adding a variable
BT.lin<- function(type= 'lm', data= all7, add.var = 'Job.Type.Primary') {
        formula = paste("inv.vs.cost~ inv.mlsto + Discipline + client.count + Business+
                        Biz.size + Biz.type + 
                        no.users + pc.contracttech +
                        pc.director + pc.midtech + pc.midpro+ pc.gradpro+ pc.seniortech+ pc.seniorpro+
                        pc.pro + mean.peeps + hours.perday + 
                        code.director + ProjEng.Pos", add.var, sep="+")
        if (type== 'aov') {
                model= aov(as.formula(formula)              
                           ,data= data)
                return(summary(model)) 
        }
        else{
                model=lm(as.formula(formula)                     
                         ,data= data)
                df<- as.data.frame(summary(model)$coefficients)
                df$var<-rownames(summary(model)$coefficients)
                colnames(df)[names(df) %in% 'Pr(>|t|)']<-'Pval'
                return(df %>% arrange(-abs(Estimate)))
        }
        
        
        
}

#print all rows with word contained in a certain column 
seed.gs %>% filter(grepl('citrullus', seed.gs$GenSpec, ignore.case=T))

#make all dots on boxplot match colours
update_geom_defaults("point", list(colour = NULL))

#create new columns with numeric variables in bins, for loop
for (i in 1:length(colnames)){
        
        #if variable is numeric or an integer, break into 8 bins
        if(df[,colnames[i]] %>% class %in% c('numeric','integer')) {
                df[, paste0('cut',i)] <- cut(df[,colnames[i]], 8)
        }}
        
#mosaic plot
mosaicplot(all7a$Discipline ~ all7a$Job.Source)

#subset out non-factor columns
df<- df[,!sapply(df, is.factor)]

# convert columns to factor. [] keeps object as dataframe
all8b[]<- lapply(all8b, factor)
map_at(CGC, c("Enterococci1", "Enterococci2", "Enterococci3"), as.character) %>%
        as_data_frame


#check number of characters in each row in a column
check<- sapply(all7c$mlsto %>% as.character, FUN=function(x) {nchar(x)})
check1<- as.data.frame(check)
check1$orig<- names(check)
check1 %>% arrange(check) %>% head
check1 %>% summary
table(check1$check)

#cube root function
math.cbrt <- function(x) {
        sign(x) * abs(x)^(1/3)
}

#output k nearest neighbours from dataframe
inv.knn<- function(df= all7d, predict= 'inv.mlsto', new.cases= c(1000), k=3){
        inv.knn = knn(df[,predict], df[new.cases,predict],
                      df$pc.pro, k=k)
        indices<- attr(inv.knn,  "nn.index")
        #checkout the nearest neigbours!
        df %>% slice(c(new.cases,indices[1,])) %>% print
        
}

inv.knn(df= all7d, predict= 'inv.mlsto', new.cases= 2000, k=3)

#write a function for testing transformations

trans<- function(var= 'timespan', df= all7d, transform = 'cbrt'){
        #initial q plot
        init<- qqPlot(df[,var], 
                      main = 'untouched variable')
        print(init)
        
        #make trial dataframe
        trial = sapply(df[,var], function(x){
                if(transform == 'log'){
                        log(x)
                } else
                        
                        if(transform == 'neglog')
                        {
                                sign(x)*log(abs(x) + 1)
                        } else
                                
                                if(transform == 'sqrt')
                                {
                                        sqrt(x)
                                } else
                                        if(transform == 'cbrt')
                                        {
                                                sign(x) * abs(x)^(1/3)
                                        } else
                                        { x^2 }
                
        }) %>% 
                as.data.frame
        
        colnames(trial)[names(trial) %in% '.']<-'trans.var'
        
        #qplot for transformation
        qqtrans<- qqPlot(trial$trans.var, 
                         main = 'transformed')
        print(qqtrans)
        
        ##move on to density plots
        
        #original variable
        dens<- ggplot(df, aes_string(x=var)) + geom_histogram(aes(y=..density..))
        print(dens)
        
        #transformed variable
        dens.trans<- ggplot(trial, aes_string(x='trans.var')) + geom_histogram(aes(y=..density..))
        print(dens.trans)
}

#radial chart
#create spiderplot

spider.data<- tot$pc.good
radial.plot(spider.data,
            labels = tot$variable,
            rp.type = "p",
            radial.lim = c(-0.4,100),
            poly.col = rgb(0, 0, 1, .8),
            line.col = "blue",
            lwd = 1)
#check length of all the levels in a dataframe
sapply(xTrain,function(x) length(levels(x)))

#subset to a single column data frame
p.1<- p.1[order(p[,1], decreasing=TRUE),,drop=FALSE]

#if else chain
if(client=='private'){
        tot.price <- net.price * 1.12      # 12% VAT
} else {
        if(client=='public'){
                tot.price <- net.price * 1.06    # 6% VAT
        } else {
                tot.price <- net.price * 1    # 0% VAT
        }
}


#attempt at caret package with gbm

# must turn outcome into two level factor
all9a<- read.csv('C:/Users/n9232371/Documents/Consultbusiness/data/all9a.csv')[,-1]
all9a$f.rpdol<- as.factor(all9a$b.rpdol)
set.seed(100)
sample<- sample(1:nrow(all9a), 2/3*nrow(all9a), replace=F)
train<- all9a[sample,]
test<- all9a[-sample,]

# set seeds
set.seed(123)
seeds <- vector(mode = "list", length = 51)
for(i in 1:50) seeds[[i]] <- sample.int(1000, 22)
## For the last model:
seeds[[51]] <- sample.int(1000, 1)

# first tune shrinkage and number of trees
train2<- train
train2$b.rpdol<- as.factor(train2$b.rpdol)
levels(train2$b.rpdol)[levels(train2$b.rpdol)=="0"] <- "profit"
levels(train2$b.rpdol)[levels(train2$b.rpdol)=="1"] <- "loss"

formula<- "b.rpdol ~ Discipline + pc.pro + b.timespan.cbrt + no.users + inv.mlsto.log +
client.totinv.log + pc.majpos.log + Billing.Type + majority.pos"

cv.Control <- trainControl(method = "cv",
                           number = 10,
                           seeds= seeds
                           , summaryFunction=twoClassSummary,
                           classProbs=TRUE
                           )
gbmGrid <- expand.grid(shrinkage = c(0.001),
                       n.trees = c(10000), interaction.depth = c(1,2,3,4,5),
                       n.minobsinnode = c(10,20,30))
set.seed(2)
gbmFit <- train(as.formula(formula), data= train2,
                 method = "gbm", trControl = cv.Control, verbose = FALSE,
                 bag.fraction = 0.5, tuneGrid = gbmGrid,
                metric = 'ROC',
                na.action = na.pass)

plot(gbmFit)
plot(gbmFit, plotType = 'level')

#save R objects, serializes an R object into a format that can be saved.

saveRDS()
readRDS()
ggsave(p, filename = 'forward_mvmts.png', path = 
               'C:/Users/n9232371/OneDrive/shared files/AACo/images/', 
       width = 5, height = 4)


#insert prefix character in front of string
Pre.move1824$Vendor<- gsub("^", "1 ", Pre.move1824$Vendor)

###polar coordinates example
df <- data.frame(
        x = sort(sample(1:40, 400, replace=TRUE)), 
        y = sample(0:9, 400, replace=TRUE)
)


ggplot(df, aes(x=x, y=y, fill=y)) + 
        geom_bar(stat='identity', position="fill") + 
        coord_polar(theta="y") + 
        scale_fill_continuous(low="blue", high="pink")

#regular expression print grepped item
m <- gregexpr("\\b([[:alpha:]])", properties[i], perl=TRUE)

#regular expression, split by a character and keep character
# put round brackets around each thing you want to be an 'item' that you can refer to with \\1, \\2 etc.
gsub("([:0-9:])", "\\1~", head(folds_plot$iteration), ignore.case = TRUE) %>% strsplit(., "~")
#back referenceing strsplit to gsub

first.letters = paste(regmatches(properties[i], m)[[1]][-1], 
                      sep = "", collapse = "")

#create empty list with names
props.vecs <- function(properties = c("Canobie", "Dalgonally")){
        list.props = vector("list", length(properties))
        names(list.props)[1:length(properties)]<- properties
        
        return(list.props)
}

#dplyr unlimited dataframe length display
options(dplyr.width = Inf)
options(dplyr.print_max = 1e9)

#apply multiple functions to a list
#find mean and sd for each location
lapply(weighted.1824, function(x) c(mean= mean(x), sd= sd(x)))

#remove full NA columns, all NA
FP_year <- FP_year[,!sapply(FP_year, function(x) all(is.na(x)))]

#make multiple columns character class at once
movements[,c('id','vendor')] <- as.matrix(movements[,c('id','vendor')])

#output column of dataframe as vector using dplyr
df %>% filter(x > 5) %>% .$y

ggplot(e(), aes_string(x = 'no.users', y = 'return.pdol')) + 
        geom_point(aes_string(size = 'inv.mlsto', colour = 'code.client'), 
                   alpha = 0.6, shape = 16) +
        labs(title = "Similar Jobs", y = "Return per Dollar", x = "Number of Users",
             colour = "Client", size = "Invoiced Amount ($)") +
        geom_text(aes(label = mlsto), check_overlap = TRUE, hjust = 0, nudge_x = 0.1,
                  nudge_y = -0.04) +
        scale_colour_brewer(palette = "Set1") +
        scale_size(range = c(5,13))


# split string into first word, then rest of the string
# () brackets create named groups
rexp <- "^(\\w+)\\s?(.*)$"
Stock.R$Type <- sub(rexp,"\\1", Stock.R$Type.And.Group)
Stock.R$Group <- sub(rexp,"\\2", Stock.R$Type.And.Group)

# Or remove vectors programmatically.  Delete objects with underscore in name
rm(list = ls()[grepl("pro\\.", ls())])

#create nested dataframe using dplyr
IT.Sept.merge <- IT.Sept %>% group_by(Property, YearBrand, SexCode, Group, Category, MasterBreed) %>% 
        nest(Weight, Age)

#rbind a list of dataframes
test <- rbind.fill(list.name)
#dplyr way: makes names of each list element a new column
folds_plot <- bind_rows(ind_folds, .id = "id")


#creating summary columns of nested dataframes

#what to do for NA's !! 

IT.Sept.merge <- IT.Sept.merge %>% mutate(
        # min.IT = map_dbl(data, function(x) min(x$Weight, na.rm= TRUE)),
        mean.IT = map_dbl(data, function(x) mean(x$Weight, na.rm= TRUE) %>% round(0)),
        # max.IT = map_dbl(data, function(x) max(x$Weight, na.rm= TRUE)),
        sd.IT = map_dbl(data, function(x) sd(x$Weight, na.rm= TRUE) %>% round(0)),
        IT.head = map_dbl(data, function(x) nrow(x)),
        mean.Age = map_dbl(data, function(x) mean(x$Age, na.rm= TRUE) %>% round(0)),
        sd.Age = map_dbl(data, function(x) sd(x$Age, na.rm= TRUE) %>% round(0)))


#find indices of a logical vector that are true, in this case duplicated rows
duplic <- mvmts.latest %>% select(-Outward, -Location, -Movement.Id) %>% duplicated() %>% which(arr.ind = TRUE)

#remove white space when reading csv
read.csv(file = 'Data.csv', sep = ',', strip.white = TRUE)

# read the names of all files in this directory into a character vector:
file.names <- list.files(path = '~/directory_with_many_csv_files/') 

# read in each of these files as a R object with the same name as the corresponding .csv file minus the .csv at the end of the filename
for(i in 1:length(file.names)){
        assign( x = strsplit(x = file.names[i], split = '.csv')[[1]], value = read.csv(file.names[i], na.strings = c('        NA')))
}

#figure out library path
FWDMODEL$LIB_PATH <- file.path(Sys.getenv("R_HOME"),"library")

# for ggplot legend, wrap factor labels by forcing a 'new line' in levels 
combo.finmvmts$mvmts<-gsub(" ", "\n", combo.finmvmts$mvmts)

# rounding
x$pc.in %>% round_any(5, floor)

#rbind two lists!!! thankyou hadley
combine.1$data= map2(combine.1$data, part.2$data, function(x,y) rbind(x,y))

#dplyr join functions
superheroes <-"
    name, alignment, gender,         publisher
Magneto,       bad,   male,            Marvel
Storm,      good, female,            Marvel
Mystique,       bad, female,            Marvel
Batman,      good,   male,                DC
Joker,       bad,   male,                DC
Catwoman,       bad, female,                DC
Hellboy,      good,   male, Dark Horse Comics
"
superheroes <- read_csv(superheroes, trim_ws = TRUE, skip = 1)

publishers <- "
publisher, yr_founded
DC,       1934
Marvel,       1939
Image,       1992
"
publishers <- read_csv(publishers, trim_ws = TRUE, skip = 1)

# only merge items present in both dfs - delete other rows
inner_join(superheroes, publishers)

#semi joins won't repeat rows. also, keeps all columns from x only
semi_join(superheroes, publishers)

#returns all rows from x and all columns from x and y. if two matches, duplicates the rows
left_join(superheroes, publishers)

#returns all rows in x that do not have a match in y. keep all x columns
anti_join(superheroes, publishers)

#returns all rows and columns from x and y.
full_join(superheroes, publishers)

# how to cite packages
citation('mice')
# copy and paste bibtex stuff into .bib file


data_prac %>%  group_by(ID) %>%  
        mutate(smoke_hist = ifelse(cumsum(smoke== "smoker") >= 1, yes = "has_smoked",
                                   no = "non_smoker")) 

#colour generation
cow = data.frame(cow = c('pow', 'wow', 'now', 'plow', 'cow'))
ggplot(cow, aes(x = cow)) + geom_bar(stat = "count", fill = rainbow(5)) 
ggplot(cow, aes(x = cow)) + geom_bar(stat = "count", fill = terrain.colors(5)) 
ggplot(cow, aes(x = cow)) + geom_bar(stat = "count", fill = topo.colors(5)) 
ggplot(cow, aes(x = cow)) + geom_bar(stat = "count", fill = heat.colors(5)) 
ggplot(cow, aes(x = cow)) + geom_bar(stat = "count", fill = cm.colors(5)) 

# use dplyr to create train/test set in two lines
set.seed = 100
train = sample_frac(all10mice, 0.75, replace = FALSE)
test = setdiff(all10mice, train)

# line break after a table in rmarkdown:

# | Job Number | Date Issued | Amount Invoiced | Amount Paid | Invoice Status |
# |------------|-------------|-----------------|-------------|----------------|
# | 1.2.300    | 2014/09/01  | \$5,000         | \$5,000     | Paid           |
# | 1.2.300    | 2014/10/02  | \$2,000         | \$2,000     | Paid           |
# | 1.2.300    | 2014/11/04  | \$3,000         | \$3,000     | Paid           |
# | 1.2.300    | 2014/12/02  | \$4,000         | \$4,000     | Paid           |
# | 1.2.300    | 2015/02/02  | \$5,000         | \$0         | Outstanding    |
# | 1.2.400    | 2014/08/05  | \$20,000        | \$20,000    | Paid           |
# | 1.2.400    | 2014/09/01  | \$2,100         | \$0         | Outstanding    |
# | ...        | ...         | ...             | ...         | ...            |\
# \\
# \

# Testing


# Create double space after a list in rmarkdown
# * total cost of external subcontractors or disbursements per project
# * total number of users that entered hours on each project
# * mean number of hours per day entered on a project
# * number of disciplines active in a project 
# \ 
# \
# 
# **Invoicing data**
#         
# * total amount invoiced and renumerated per project

# gather example
mini_iris <- iris[c(1, 51, 101), ]
# gather Sepal.Length, Sepal.Width, Petal.Length, Petal.Width
gather(mini_iris, key = flower_att, value = measurement,
       Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)


#change level in a factor, rename levels
mutate(cfor.varimp, var = revalue(var, c("norm.log" = "log.pred",
                                         "norm.rf" = "rf.pred",
                                         "norm.boost" = "boost.pred")))

# arrange multiple plots
p1 <-
        ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
        geom_point()

p2 <-
        ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
        geom_jitter()

p3 <-
        ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
        geom_count(alpha = 0.5)

gridExtra::grid.arrange(p1, p2, p3, ncol = 3)


# force markdown pdf to have interactive html widgets
output <- opts_knit$get("rmarkdown.pandoc.to")

if (output=="html") {
        
        opts_chunk$set(fig.width=8,
                       fig.asp = 0.618)
        
} # # end html `if` statement

## setting up the figure parameters for docx
if (output=="pdf") {
        
        opts_chunk$set(dev = 'pdf',
                       fig.width = 6,
                       fig.height = 0.618,
                       screenshot.force = TRUE,
                       echo = FALSE)
} # end docx `if` statement


# use map within mutate

traffic3 <- traffic2 %>% mutate(
        mode.colour = map_chr(data, function(x) table(x$colour, exclude = NULL) %>% sort(decreasing=TRUE) %>% 
                                      names %>% .[1])
)


# change multiple columns to different class, factor, character, numeric etc, purrr
new_BCC <- map_at(new_BCC, c("Site", "BCC Site Number"), as.factor) %>% as_data_frame

# spread function  dplyr
stocks <- data.frame(
        time = as.Date('2009-01-01') + 0:9,
        X = rnorm(10, 0, 1),
        Y = rnorm(10, 0, 2),
        Z = rnorm(10, 0, 4)
)

stocksm <- stocks %>% gather(stock, price, -time)
stocksm %>% spread(stock, price)
# use unite to create a new column that combines several columns in the title (unique col combos).
# you must name the new column as the first item in unite() - in the example below 'Label_all'
test %>% unite(Label_all, TestReportLabel, Units) %>% spread(Label_all, QualifiedResult)


# dcast is the reshape2 version fo spread
names(airquality) <- tolower(names(airquality))
aqm <- melt(airquality, id=c("month", "day"), na.rm=TRUE) %>% head(20)

acast(aqm, day ~ month ~ variable)
acast(aqm, month ~ variable, mean)
acast(aqm, month ~ variable, mean, margins = TRUE)
dcast(aqm, month ~ variable)

# change legend labels in ggplot
ggplot(mice.totAUC, aes(x= factor(L1), y= value)) + 
        geom_violin(aes(fill = sigdiff, colour = sigdiff), 
                    alpha = 0.8) + 
        stat_summary(fun.y="mean", geom="point", colour = "gray30", shape =3) +
        stat_summary(aes(label = round(..y.., 3)), fun.y="mean",
                     geom="text", colour = "gray20", size =3, 
                     hjust = -.2, angle = 40, vjust = -.2
        ) +
        scale_fill_manual(values = c('sienna1','navajowhite'),
                          name = "Significantly different from boosted trees") +
        scale_colour_manual(values = c('sienna1','navajowhite'),
                            name = "Significantly different from boosted trees") +
        labs(x = "Method", y = "Area Under Curve (AUC)",
             title = "AUC Distribution from 100 Models of Each Method - Imputed Data") +
        scale_x_discrete(labels = c("boost" = "Boosted Trees", 
                                    "log" = "Logistic Regression",
                                    "naiive" = "Naive Bayes", "rf" = "Random Forest")) +
        theme(legend.position = "bottom")

# rolling cumulative sum of previous two entries
rainf$rf.48hr <- stats::filter(rainf$rf.24hr, rep(1, 2), sides = 1)


# dplyr select columns with logical statement
CGC %>% filter(is.na(rf.24hr), !is.na(station.code)) %>% select_if(grepl('rf|code', names(.))) %>% head

# print vector/df in same way you would input it
names(CGC) <- dput(names(super)[1:17])

# change all afctor columns to character
df = df %>% mutate_if(is.factor, as.character, envir = .GlobalEnv)


# create sparse binary matrix to eliminate categorical variables. Include NA values
train <- model.matrix(as.formula(formula),
                      model.frame(as.formula(formula), super3, na.action = NULL))

# cool palettes
piratepal('pony') # yarrr package
piratepal() # view palettes

# must delete names on palette first ... 
pirated <- structure(list(pony = c("#EB5291FF", "#FBBB68FF", "#F5BACFFF", 
                        "#9DDAF5FF", "#6351A0FF", "#ECF1F4FF", "#FEF79EFF", "#1794CEFF", 
                        "#972C8DFF"), 
               eternal = c("#170C2EFF", "#751029FF", "#52194CFF",
                           "#473B75FF", "#4D709CFF", "#6F766BFF", "#92ADC4FF"), 
               evildead = c("#19180DFF", "#212510FF", "#2E100BFF", "#392E12FF", "#57512BFF", "#968E4CFF"), 
               usualsuspects = c("#323337FF", "#534C53FF", "#3F516AFF", "#9B6659FF", "#E83B41FF",
                                 "#9F9CA2FF", "#EAAE9DFF"), 
               ohbrother = c("#1A0F0AFF", "#3D291AFF", "#715639FF", "#747D6DFF", "#AD9D0BFF", "#94C4DFFF", 
                             "#E6DDA8FF"), 
               brave = c("#A8643BFF", "#B65B23FF", "#94220EFF", 
                         "#272D17FF", "#202126FF"), 
               bugs = c("#667840FF", "#BAD6A8FF", "#85C7C1FF", "#A59A6BFF", "#67553FFF"), 
               cars = c("#E7B08FFF", "#884C49FF", "#E0363AFF", "#6A1D1AFF", "#9DDAE6FF"), 
               nemo = c("#FBCF35FF", "#ED4C1CFF", "#9C7E70FF", "#5AC2F1FF", "#11776CFF"), 
               up = c("#5F8CF4FF", "#DCD6FCFF", "#E27A48FF", "#605646FF", "#434159FF"), 
               ipod = c("#D7D7D7FF", "#F3AEAFFF", "#AEADB0FF", "#9ED9BFFF", "#5CCBEBFF", "#DEEB61FF", "#F2F2F2FF"),
               info = c("#E7695DFF", "#6B8993FF", "#F6F0D4FF", "#95CE8AFF", "#D2D2D2FF", "#94D4D4FF",
                        "#969696FF", "#F1F3E8FF", "#88775FFF")), 
          .Names = c("pony", "eternal", "evildead", "usualsuspects", "ohbrother", "brave", 
                     "bugs", "cars", "nemo", "up", "ipod", "info"))

saveRDS(pirated, 'C:/Users/n9232371/OneDrive/shared files/Statslearningcourse/colours/pirated.rds')
pirated <- readRDS('C:/Users/n9232371/OneDrive/shared files/Statslearningcourse/colours/pirated.rds')

q <- ggplot(diamonds, aes(x = clarity, fill = clarity)) + geom_bar(stat = 'count')
q + scale_fill_manual(values = rep(pirated$pony,2))
q + scale_fill_manual(values = rep(pirated$eternal,2))
q + scale_fill_manual(values = rep(pirated$info,2))

# remove package from environment
detach("package:yarrr", unload=TRUE)

# log transform of ggplot axis axes

explore_sitetype <- ggplot(super3, aes(x = sitetype, y = EnteroCFU)) + geom_violin() + 
        stat_summary(fun.y = "mean",
                     geom = "point",
                     aes(colour = "mean"),
                     show.legend = T) +
        labs(x = "", y = "Enterococci CFU",
             title = "Violin Plots of Enterococci Distributions for each Site Type") +
        scale_y_continuous(trans = "log",
                           breaks = c(1,5,15,50,250,1000,5000,25000)) +
        scale_colour_manual(values = "navyblue") +
        theme(legend.title=element_blank(),
              legend.position = "bottom")

# function with cowplotting 

site_rates <- function(arear = "BCC", levels.ind = 1:30,
                       number = 1, titler = "Brisbane City Council",
                       plot.ind = "1 of 1", filenamer = "bc_site_rates.png",
                       heightr = 5){
        
        site.ras = site_stats %>% filter(area == arear) %>% arrange(FNR) %>% 
                ggplot(., aes(x = site_id, y = FNR)) +
                geom_text(data = hlines, aes(y = y.int, label = key),
                          vjust = -.8, hjust = 1, size = 3, 
                          x = site_stats %>% filter(area == arear) %>% nrow,
                          colour = 'grey30') +
                geom_point(aes(colour = "FNR")) +
                geom_point(aes(y = FPR, colour = "FPR")) +
                geom_hline(data = hlines,
                           aes(yintercept = y.int), colour = 'grey60') + 
                theme_bw() + 
                theme(axis.text.x=element_text(angle=45,hjust=1),
                      legend.title=element_blank(),
                      legend.position='top',
                      legend.direction = 'horizontal',
                      axis.title.x = element_blank()) +
                scale_y_continuous(labels = percent) +
                # or scale_y_continuous(labels = function(x) paste(x,"%",sep=""))
                scale_colour_manual(values = pirated$pony[c(1,5)]) + 
                labs(y = 'Rate', x = "", 
                     title = paste("Mean TNR and FPR for Individual Sites \nin",
                                   titler, "at a Threshold of 0.3", sep = " "))
        
        
        site_bar <- super.temp %>% filter(area == arear) %>% 
                ggplot(., aes(x = site_id, fill = binary.CFU.fac)) +
                geom_bar() +
                scale_fill_manual(values = pirated$pony[c(8,2)]) +
                theme_bw() + 
                theme(legend.position = 'bottom',
                      axis.text.x=element_text(angle=45,hjust=1),
                      legend.title=element_blank(),
                      plot.title = element_blank(),
                      plot.margin= unit(c(0,0,0,15.5), 'pt')) +
                labs(y = 'Water Sample Count', x = "Site ID")
        
        #cowplot
        cow_siteid <- plot_grid(site.ras, site_bar, ncol = 1,
                                rel_heights = c(5,4))
        ggsave(cow_siteid, filename = filenamer, path = 
                       '\\\\rstore.qut.edu.au/projects/sef/healthywater/reports/HW_reports/ggplots', 
               width = 7, height = heightr)
        
}


# output or print the way you would program an object
dput()

# pipe into operation such as multiply

accepted
You need to put * in quotes - "*"(), also use 1 as your argument in prop.table to match the example.

mtcars %>%
        xtabs(~ gear + cyl, data = .) %>%
        prop.table(., 1) %>%
        "*"(100 ) %>% round(.,2)


#caret tuning
cv.Control <- trainControl(method = "cv",
                           number = 5,
                           summaryFunction= defaultSummary)

gbmGrid <- expand.grid(shrinkage = c(0.0005, 0.001, 0.005, 0.01),
                       n.trees = c(4000, 6000, 8000, 10000), 
                       interaction.depth = c(3),
                       n.minobsinnode = c(20))

gbmFit <- train(log.CFU ~ log.r24 + log.r48 + log.r72 + log.rwk + log.r24_48 +
                        log.r48_72 + area + site_id,
                data = super3 %>% filter(sitetype == cat),
                method = "gbm", trControl = cv.Control, verbose = FALSE,
                bag.fraction = 0.5, tuneGrid = gbmGrid,
                metric = 'RMSE',
                na.action = na.pass) 

plot(gbmFit)
plot(gbmFit, plotType = 'level')


#vignette
vignette('glmnet_beta', package = 'glmnet')

# ROC curve - pROC package
pred.p <- pROC::roc(holdout$hatch_disperser, preds)
plot.roc(pred.p, col = 'blue', print.thres = T)

auc(pred.p)
coords(pred.p, 'best')

# mutate_if
ticks[, c(1,6,8,2,4,10, 9, 3, 5, 11)] %>% mutate_if(is.numeric, function(x) round(x,1))

# ggplot legend for individual geom_line
ggplot(temp.plot, aes(y = Dust.downpipe.North, x = Timestamp)) +
        geom_line(aes(colour = "Dust.downpipe.North")) +
        geom_line(aes(y = Dust.downpipe.South, colour = 'Dust.downpipe.South'))

# ifelse statement to colour ggplot points
ggplot(PD, aes(x = TimeStamp, y = AS02_DATA_FROM_AS01.MtPV.V)) +
        geom_point(colour = ifelse(PD$AS02_DATA_FROM_AS01.MtPV.V<1, 'red', 'black'))

# convert all factor variables to numeric matrix
model.matrix( ~ Species + newp + 0, data = newish,
              contrasts.arg = lapply(newish %>% select_if(is.factor), contrasts, contrasts=FALSE)) %>% head

# more complicated example of model.matrix
factor_names <- PD %>% select_if(is.factor) %>% names
factor_names <- factor_names[!factor_names %in% 'TimeStamp']
formula <- paste(factor_names, collapse = " + ")
formula <- paste("~ ", formula, sep = "")

train <- model.matrix(as.formula(formula),
                      data = train,
                      contrasts.arg = lapply(train %>% select_if(is.factor), contrasts, contrasts=FALSE))


# replace outliers
outlier_replace <- function(var = PD$X322.PIT.112.PV.V){
        
        IQR <- quantile(var, probs = c(0.75), na.rm = TRUE) - quantile(var, probs = c(0.25), na.rm = TRUE)
        upper_lim <- quantile(var, probs = c(0.75), na.rm = TRUE) + 1.5*IQR
        lower_lim <- quantile(var, probs = c(0.25), na.rm = TRUE) - 1.5*IQR
        
        var <- ifelse(var > upper_lim|var < lower_lim, NA, var)
        
        return(var)
        
}

# correlation matrix
ggcorr(summ, palette = "RdBu")

# vectorize a function
vec_switch <- Vectorize(function(a){ switch(as.character(a),
                                            "Haul Trucks 220" = "220 t",
                                            "Haul Trucks 180" = "180 t",
                                            as.character(a))
}, "a")

# nested gradient with purrr
# calc gradient of each var btwn maintenance events

lm_func <- function(df = x, col = 'LP1'){
        # remove rows with NA for col
        df <- df[!is.na(df[,col]),]
        
        #formula for lm
        formula = paste(col, '~ date')
        # extract coef from lm, or return NA if less than 2 rows
        ifelse(nrow(df) < 2, NA, lm(as.formula(formula), data = df) %>% tidy %>% filter(term == 'date') %>% .$estimate)
}

# nest summ by ID and last maint event
# mistake ! should group_by date of last_maint_event as well :(
summ <- summ %>% group_by(ID, last_maint_event) %>% nest

grad_btwn_maint <- summ %>% mutate(
        
        npoints = map_dbl(data, function(x) nrow(x)),
        LP1_med_grad = map_dbl(data, function(x) lm_func(df = x, col = 'LP1_med')),
        LP2_med_grad = map_dbl(data, function(x) lm_func(df = x, col = 'LP2_med')),
        LP3_med_grad = map_dbl(data, function(x) lm_func(df = x, col = 'LP3_med')),
        LP4_med_grad = map_dbl(data, function(x) lm_func(df = x, col = 'LP4_med')),
        LP1_var_grad = map_dbl(data, function(x) lm_func(df = x, col = 'LP1_var')),
        LP2_var_grad = map_dbl(data, function(x) lm_func(df = x, col = 'LP2_var')),
        LP3_var_grad = map_dbl(data, function(x) lm_func(df = x, col = 'LP3_var')),
        LP4_var_grad = map_dbl(data, function(x) lm_func(df = x, col = 'LP4_var'))
        
)

## plotly
# move legend to bottom
p <- ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) + 
    geom_bar(stat = "identity") +
    theme(legend.position = "bottom")
ggplotly(p) %>% 
    layout(legend = list(orientation = "h",
                         x= 0, y = -0.1))


## REGULAR EXPRESSION REGEX INDEX
# () makes a named item
# ^ front of string
# ? preceding character is optional
# $ end of string
# . matches any character except line breaks
# * match the preceding token 0 or more times
# [^ ] matches any character except a white space
# [-+]?[0-9]*\.?[0-9]+ matches numbers with decimal places

# mosaic plot geom_mosaic ggmosaic
data(Titanic)
titanic <- as.data.frame(Titanic)
titanic$Survived <- factor(titanic$Survived, levels=c("Yes", "No"))
ggplot(data=titanic) +
        geom_mosaic(aes(weight=Freq, x=product(Survived, Class), fill=Survived))

# alternative to paste like % in python - glue!
greet <- function(name) {
        glue::glue("How do you do, {name}?")
}
greet("Hadley")
#> How do you do, Hadley?

#python code chunk
```{python, eval = F}

import pandas as pd

links = pd.read_pickle('C:/Users/cook79166/SharePoint/T/General/PRTs/PRT_documents_info_tag.p')
print links.head()

import 

def this(asd):
        sum(asd)

df.apply(lambda asd: sum(asd))    

```

# ggplotly tool tip and size example
a <- ggplot(starwars %>% filter(grepl("Human|Droid|Gungan|Wookiee", species)), aes(x = height, y = mass, colour = species, text = name)) +
        geom_point(size = 3) + 
        theme(legend.position = "bottom")

ggplotly(a, tooltip = "text", width = 800, height = 550) %>% 
        layout(legend = list(orientation = "h",
                             x= 0, y = -0.1))


# labels above bar graph dodged bars
ggplot(active_ladles, 
       aes(x = measure, y = time_pc, fill = sim_case)) +
        geom_histogram(position = "dodge", stat = 'identity', binwidth = 1) +
        geom_text(aes(label = time_pc, colour = sim_case), 
                  position = position_dodge(width = 1), vjust = -.5, size = 3) +
        labs(title = "Active Ladles",
             x = "Active Ladles",
             y = "% of time") +
        scale_x_continuous(breaks = seq(0, 15, by = 1)) +
        theme(legend.position = "bottom")

# working with excel

# convert excel number to date
as.POSIXct(c1$Hora * (60*60*24), origin="1899-12-30", tz = "GMT")

# decision tree code

form = as.formula("Cu.in.slag ~ Fe + SiO2 + Al2O3 + MgO + CaO + Slag.Temp")
tree_s <- rpart(form,
                control = rpart.control(minsplit = 10, minbucket = 10,
                                        maxdepth = 25, xval = 10),
                data=c1,
                cp=.001)


# plotcp(tree_s)
# printcp(tree_s)
# summary(tree_s)

# pretty plot
prp(tree_s, box.palette = "GnBu",
    varlen = 0, digits = 3)

# gbm typical code

gbm.fit <- gbm(formula = form, 
               distribution = "gaussian",
               n.trees = 500, interaction.depth = 4, 
               shrinkage = 0.01,
               data = c1)

# gbm.perf(gbm.fit)
print("Boosted trees R2 =")
round(R2(c1$Cu.in.slag, predict(gbm.fit, c1, n.trees = 240)), 2)

imp <- summary(gbm.fit, plotit = F)

ggplot(imp, aes(x = fct_reorder(var, rel.inf), y = rel.inf, fill = rel.inf)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        theme_bw() +
        labs(y = NULL, x = "Variable Importance Score",
             title = "Variable Importances predicting Cu (%) - Boosted Tree") +
        theme(legend.position = "none")

# summary(gbm.fit, method = permutation.test.gbm)

# CV R2
R2(c1$Cu.in.slag, gbm.fit$cv.fitted)


# R squared R2 function

R2 <- function(actual, predict){
        1 - (sum((actual-predict )^2, na.rm = T)/sum((actual-mean(actual))^2, na.rm = T))
}

