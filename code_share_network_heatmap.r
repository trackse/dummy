#code share :: you could repeat our result

#network analyze
if(TRUE){
  library(network) #新版变得更简单，只需要变为as.matrix(数字)就OK
  library(sna)
  # library(ggplot2)
  library(GGally)
  
  # allkongdao_ori :: foramen
  varnames2=c(allkongdao_ori,"Dead","Survival")
  
 
  thiscopydb=prepareDb
  thiscopydb$Dead=thiscopydb$dead
  thiscopydb$Survival=as.factor(ifelse(thiscopydb$dead==1,0,1))
 
  
  
  bip=thiscopydb[,varnames2]
  copy_bip=bip #方便配色、配大小
  
  
  bip=apply(bip, 2, FUN = function(x){return(as.integer(as.character(x)))})
  bip=as.matrix(bip)
  
  bip_net <- network(bip, directed=FALSE)
  
  # summary(bip_net)
  
  #to test only
  ggnet2(bip_net, label = TRUE)
  
  #一步步设置style
  if(TRUE){
    bip_net
    #注意了，大小、配色什么的，全是 vertices 
    #vertices 其实就是（行数+列数），先是行，后是列::直观点，就是你想显示多少个圈
    #edges,就是得有多少条连接线了，基本不用改
    
    
    #设置大小
    bip_net %v% "lbsize" =  c( rep(1,nrow(copy_bip)),rep(10,ncol(copy_bip) ) )
    # ggnet2(bip_net, label = TRUE, size = "lbsize")
    
    #设置color
    bip_net %v% "color" =  c( rep("grey",nrow(copy_bip)),rep("gold",ncol(copy_bip) ) )
    # ggnet2(bip_net, label = TRUE, size = "lbsize",color = "color")
    
    #设置标签，因为默认的标签显示，太碍眼;;edge.color类似改法，我这里全灰色好看些
    bip_net %v% "lb" =  c( rep("",nrow(copy_bip)),changeNames(varnames2) )
    # tail(c( rep("",nrow(copy_bip)),changeNames(varnames2)))
    # ggnet2(bip_net, label = TRUE, size = "lbsize",color = "color", node.label =  "lb")
    
    #再将线什么颜色搞得漂亮些
    # ggnet2(bip_net, label = TRUE, size = "lbsize",color = "color", node.label =  "lb",
    #        edge.label.fill = "white",edge.color = "grey85"
    #          # , layout.par = list(cell.jitter = 0.25)
    #        )
    
    #核心点，用什么函数，为了可重复，设定seed;;哪个图更支持你用哪个图
    
    set.seed(2023);ggnet2(bip_net,
                          # mode="fruchtermanreingold",
                          # mode="circrand",
                          mode="geodist",
                          
                          label = TRUE, size = "lbsize",color = "color", node.label =  "lb",
                          edge.label.fill = "white",edge.color = "grey85"
                            # , layout.par = list(cell.jitter = 0.25)
    )
   
    
    
  }
  
  
  
}


#热图-------不太好理解，回头再看看---做了network analyze后比较好理解了
#try heatmap---success
if(TRUE){
  
  prepareDb$survival=as.factor(ifelse( prepareDb$dead==0,1,0))
  
  anaObj=c("dead",allkongdao_ori,"survival")
  
  library("d3heatmap")
  # library("pheatmap")
  library("RColorBrewer")
  
  
  #just foramen data
  headdata=prepareDb[which(prepareDb$all孔道==1),anaObj]
  
  summary(headdata)
  
  
  
  headdata_numeric=apply(headdata,1,FUN=function(x){return (as.numeric(as.character(x)) )})
  
  headdata_matrix=as.matrix(t(headdata_numeric))
 
  set.seed(2023)
  d3heatmap(headdata_matrix, 
            # scale = "column", col = "Blues",
            colors = "RdYlBu",#best
            # colors = colorRamp(c("green","blue")),
            #colors = "YlOrRd",
            # col=col,
            
            k_row = 2, # Number of groups in rows
            k_col = 2, # Number of groups in columns
            labCol =changeNames(anaObj)
            
  )
  # dev.off()
}
