setwd("E:/Dropbox/Matrix_SMTsolver_2018-2019")
library(stringr)
library(gtools)
functions<-read.csv("r_functions.csv", header = TRUE, row.names = NULL)

for(i in 1:10){
  print(paste("A: ", i))
  NCols=floor(runif(1, min=2, max=10))
  NRows=floor(runif(1, min=2, max=10))
  
  A<-matrix(round(runif(NCols*NRows, min=0, max=100),2), ncol=NCols)
  
  no_errors=1
  
  while(no_errors<=10){
    fs=c()
    d_max=ceiling(runif(1, min=0, max=5))
    print(paste("trying S: ", no_errors))
    tryCatch({
      
      for(d in 1:d_max){
        fs=c(fs, as.character(functions[floor(runif(1, min=1, max=nrow(functions))),"f"]))
        
      }
      
      S<-A
      for(j in 1:length(fs)){
        if(!is.null(nrow(S))){
          S<-do.call(str_replace(fs[j], "\\(M\\)", ""), args = list(S))
        }
      }
      
      if(is.null(nrow(S))){
        last_function<-fs[j]
        ##Función que devuelve una sola fila
        if(as.character(functions[which(functions$f==last_function),"m_o"])=="1"){
          S=as.matrix(rbind(S))
        }else{ ##Función que devuelve una sola columna
          S=t(as.matrix(rbind(S)))
        }
        
      }
      
      n_elements_null=floor(runif(1, min=dim(S)[1]*dim(S)[2]*0.6, max=dim(S)[1]*dim(S)[2]*0.8))
      
      B=S
      e=1
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
      
      write.csv(A, file = paste("data/A_",i,".csv", sep = ""), row.names = FALSE)
      write.csv(S, file = paste("data/A_",i,"-S_",no_errors,".csv", sep = ""), row.names = FALSE)
      write.csv(fs, file = paste("data/A_",i,"-functions_",no_errors,".csv", sep = ""), row.names = FALSE)
      write.csv(B, file = paste("data/A_",i,"-B_",no_errors,".csv", sep = ""), row.names = FALSE)
      
      no_errors=no_errors+1
      
    }, error = function(e) {
    })
  }
  
}
