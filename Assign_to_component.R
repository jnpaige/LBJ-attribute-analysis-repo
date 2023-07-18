Feature.levels<-list(c("41GL317 1",5,6,7),
                     c("41GL317 2", 7,8,9,10),
                     c("41GL317 3", 10,11,12),
                     c("41GL317 4", 8,9,10,11),
                     c("41GL317 5", 8,9,10),
                     c("41GL317 6", 3,4,5),
                     c("41GL317 9", 8),
                     c("41GL317 13", 4),
                     c("41GL317 16", 7),
		     c("41GL135 3", 9,10))


data<-d

#Pick out the Test Units. Select 

assign.component<-function(data){
list<-list()
for(i in 1:length(Feature.levels)){
  
  target<-data[which(data$context %in% Feature.levels[[i]][1]),]
  target.tape<-Feature.levels[[i]][2:length(Feature.levels[[i]][])]
  target.tape<-as.numeric(target.tape)
  target$component<-NA
  target[which(as.numeric(target$lvl) > max(target.tape)),]$component<-"Component 1"
  target[which(as.numeric(target$lvl) %in% target.tape),]$component<-"Component 2"
  target[which(as.numeric(target$lvl) < min(target.tape)),]$component<-"Component 3"
  
  
  list[[i]]<-target
  
}

output<-do.call("rbind", list)
return(output)
}

