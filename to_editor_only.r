#required library
if(TRUE){
  library(rms)
  library(Hmisc)
  library(survival)
  library(Formula)
  library(ggplot2)
  library(glmnet)
}
  

###some global setting
if(TRUE){
  
  # all data have been deposited at Sun Yat-sen University Cancer Center
  # ( www.researchdata.org.cn), with the Research Data Deposit (RDD) number as RDDA XXXXXXXXXX 
  
  #trainData+testData=totalData 
  globalSeed=2019 #lasso require a seed to repeat the same result,2019 was when this study started
  thisstatus="moveornot"  #event colum
  thistime="movetime"     #time colum
  thisCor=0.75   #findCorrelation over 0.75
  thissetP=0.05  #trusted p value
  
}


###clean data
if(TRUE){
  
  #Prepare variable name
  if(TRUE){
    
    T1range=which( is.na(str_match(names(totalData), "T1_" ))==FALSE)
    T2range=which( is.na(str_match(names(totalData), "T2_" ))==FALSE)
    T1Crange=which( is.na(str_match(names(totalData), "T1C_" ))==FALSE)
    
    radioFeatures=names(totalData)[c(T1range,T2range,T1Crange)]
    
  }
  
  #Delete highly related/too much empty/Constant or rare distribution
  if(TRUE){
   
    killmedian=c()
    for(cname in radioFeatures){
      teidsk=summary(totalData[,cname])
      
      if( as.numeric(teidsk[3])==1 |  as.numeric(teidsk[3])==0 | 
          as.numeric(teidsk[3])==as.numeric(teidsk[2]) |  
          as.numeric(teidsk[3])==as.numeric(teidsk[5]) |  
          as.numeric(teidsk[1])==as.numeric(teidsk[2]) |  
          as.numeric(teidsk[2])==as.numeric(teidsk[5]) |  
          as.numeric(teidsk[6])==as.numeric(teidsk[5])   
      ){
        killmedian=c(killmedian,cname)
      }
      
    }
    
    radioFeatures_sig=setdiff(radioFeatures,killmedian)
    
    correlationMatrix = cor( totalData[,radioFeatures_sig],use = "complete.obs")
    highlyCorrelated = findCorrelation(correlationMatrix, names=TRUE,cutoff=thisCor)
    
    radioFeatures_killrelated=setdiff(radioFeatures_sig,highlyCorrelated)
    radioFeatures_sig=radioFeatures_killrelated
    
  }
  
  #univariable in training set,who P<0.05
  if(TRUE){
    
    #require Private function for batch univariable analyze
    if(TRUE){
      #单因素分析
      singleAnalyze=function(Cnames,setStatus="dead",statusTime="deadtime",prepareData2=totalData,p=0.05){
        prepareData2$msurv=Surv( time=as.double(prepareData2[,statusTime]),event=as.double(prepareData2[,setStatus]) )
        tempLogRank=chageType(lapply(X=Cnames,FUN=singleAna,data=prepareData2))
        importantNames=as.character(tempLogRank$V1[which(tempLogRank$V2<p)]) 
        return(importantNames)
      }
      
      
      chageType=function(data=tempTrainAUC){
        tempTrainAUC2=as.data.frame(t(as.data.frame(data)))
        leng=ncol(tempTrainAUC2)
        for(i in 2:leng){
          
          tempTrainAUC2[,i]=as.numeric(as.character( tempTrainAUC2[,i]))
        }
        return(tempTrainAUC2)
      }
      
      singleAna=function(cname,data=trainGroup,ystr="msurv~"){
        
        
        logstr=paste0(ystr,cname)
        m1=survival::coxph(as.formula(logstr),data=data)
        t1=summary(m1)
        
        return(c(cname,min(as.numeric(as.character(t1$coefficients[,5])))))
        
      }
    }
    
    
    radioFeatures_univariable=singleAnalyze(Cnames=radioFeatures_sig,setStatus=thisstatus,statusTime=thistime,prepareData2=trainData,p=thissetP)
    radioFeatures_sig=radioFeatures_univariable
   
  }
  

}


#manunal setting T1\T2\T1C to run
if(TRUE){
 
  radioFeatures_before=radioFeatures_sig
  T1_col=radioFeatures_before[which( is.na(str_match(radioFeatures_before, "T1_" ))==FALSE)] 
  T1C_col=radioFeatures_before[which( is.na(str_match(radioFeatures_before, "T1C_" ))==FALSE)]
  T2_col=radioFeatures_before[which( is.na(str_match(radioFeatures_before, "T2_" ))==FALSE)]
  
  #manunal setting here,sampple , current running T1C related 
  runFeatures=c(T1C_col);thistag="T1C"
  
}


#LASSO
if(TRUE){
  
  
  #lasso
  thisstr=as.formula(paste0("msurv~",paste0(runFeatures,collapse = "+")))
  xfactors = model.matrix(thisstr,trainData)
  x=as.matrix(data.frame(xfactors))                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
  set.seed(globalSeed)
  glmmod <- cv.glmnet(x=x, y=trainData$msurv, alpha=1, family="cox",nfolds=10)
  plot(glmmod)
  temp=coef(glmmod,s = "lambda.min")
  thisbian=temp@Dimnames[[1]][temp@i+1]
  
  
  #stepwise
  final_formula=as.formula(paste0("msurv~",paste0(thisbian,collapse = "+")))
  m1 <- coxph(final_formula,data=trainData)
  m1step=stats::step(m1,trace=0,direction = "both",steps=10000)
  
  
  #to predict
  trainY=predict(m1step)
  trainData$train=trainY
  testY=predict(m1step,newdata = testData)
  testData$train=testY
  
 
}


