library(stringr)
library(gtools)
functions<-read.csv("data/r_functions.csv", header = TRUE, row.names = NULL)
probs<-read.csv("data/prior_probs.csv", sep = ";")

set.seed(1)

for(dd in 1:4){
for(i in 1:10){
  print(paste("A: ", i))
  NCols=floor(runif(1, min=2, max=10))
  NRows=floor(runif(1, min=2, max=10))
  
  A<-matrix(round(runif(NCols*NRows, min=0, max=100),2), ncol=NCols)
  
  no_errors=1
  
  while(no_errors<=10){
    fs_list=c()
    d_max=dd
    print(paste("trying S: ", no_errors))
    tryCatch({
      
      S<-A
      for(d in 1:d_max){
        fs=sample(as.character(functions[order(functions$f),"f"]), size=1, replace=TRUE, prob=probs[order(probs$f),"probs"])
        fs_list=c(fs_list,fs)
          S<-eval(parse(text = str_replace_all(fs, "A", "S")))
      }
      
      B=S
      
      if(!is.null(nrow(S))){
        n_elements_null=floor(runif(1, min=dim(S)[1]*dim(S)[2]*0.6, max=dim(S)[1]*dim(S)[2]*0.8))
        
        e=sum(is.na(B))
        while(e<=n_elements_null){
          print(paste("trying element ", e, "/", n_elements_null,sep = ""))
          element_r<-ceiling(runif(1, min=0, max=dim(S)[1]))
          element_c<-ceiling(runif(1, min=0, max=dim(S)[2]))
          print(paste("[", element_r, ",", element_c,"]",sep = ""))
          if(!is.na(B[element_r,element_c])){
            B[element_r,element_c]=NA
            e=e+1
          }
        }
      }
      
      
      write.csv(A, file = paste("data/syntetic/",dd,"/A_",i,".csv", sep = ""), row.names = FALSE)
      write.csv(S, file = paste("data/syntetic/",dd,"/A_",i,"-S_",no_errors,".csv", sep = ""), row.names = FALSE)
      write.csv(fs_list, file = paste("data/syntetic/",dd,"/A_",i,"-functions_",no_errors,".csv", sep = ""), row.names = FALSE)
      write.csv(B, file = paste("data/syntetic/",dd,"/A_",i,"-B_",no_errors,".csv", sep = ""), row.names = FALSE)
      
      no_errors=no_errors+1
      
    }, error = function(e) {
    })
  }
  
}
}
