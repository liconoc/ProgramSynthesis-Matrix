setwd("E:/Dropbox/Matrix_SMTsolver_2018-2019")
library(stringr)
library(gtools)


##Selects the lists of functions (or combination of functions) in such a way that
##applied to A we can obtain the full matrix partially filled in B.
##d_max is a limit on the number of functions to combine.
selecting_functions <- function (A, B, functions, d_max){
  d<-1
  result<-NA
  
  m<-nrow(A)
  n<-ncol(A)
  
  m_<-nrow(B)
  n_<-ncol(B)
  
  found=FALSE
  
  ##Loop from d=1 to d=d_max
  while (!found && d<=d_max) {
    #if(found){break}
    print(paste("d=",d, sep = ""))
    ##All the possible combinations of the functions
    ##Allow repeats because the order matters
    c=t(permutations(nrow(functions), d, as.character(functions$f), set=TRUE, repeats.allowed=TRUE))
    
    ##Each column of c is a combination
    for(combination in 1:ncol(c)){
      #if(found){break}
      suppressWarnings(remove(m_output))
      suppressWarnings(remove(n_output))
      ##check if the output of the previous function matches with the next one
      for(i in 1:d){
        #set inputs
        if(i==1){
          m_input<-m
          n_input<-n
        }else{
          m_input<-m_output
          n_input<-n_output
        }
        
        
        
        f<-c[i,combination]
        
        ##If the function has a min m or n
        if(!is.na(as.character(functions[which(functions$f==f),"m_min"]))){
          if(m_input<as.numeric(functions[which(functions$f==f),"m_min"])){
            m_output<--1
            n_output<--1
          }
        }
        if(!is.na(as.character(functions[which(functions$f==f),"n_min"]))){
          if(n_input<as.numeric(functions[which(functions$f==f),"n_min"])){
            m_output<--1
            n_output<--1
          }
        }
        if(functions[which(functions$f==f),"f"]=="det(M)"){
          if(n_input!=m_input){
            m_output<--1
            n_output<--1
          }
        }
        
          
        if((!exists("m_output") || m_output!=-1) && (!exists("n_output") || n_output!=-1)){
          #set output
          if(as.character(functions[which(functions$f==f),"m_o"])=="m"){
            m_output<-m_input
          }else if(as.character(functions[which(functions$f==f),"m_o"])=="n"){
            m_output<-n_input
          }else{
            m_output<-1
          }
          
          if(as.character(functions[which(functions$f==f),"n_o"])=="m"){
            n_output<-m_input
          }else if(as.character(functions[which(functions$f==f),"n_o"])=="n"){
            n_output<-n_input
          }else{
            n_output<-1
          }
        }
          
      }#for i
      
      ##If the size of the result match the size of B, return the function
      if(m_output==m_ && n_output==n_){
        
       A_<-A
       for(j in 1:length(c[,combination])){
         if(is.null(nrow(A_))){
           A_=as.matrix(rbind(A_))
           A_<-do.call(str_replace(c[,combination][j], "\\(M\\)", ""), args = list(A_))
           c[,combination][j]<-str_replace(c[,combination][j], "\\(M\\)", paste("\\(as.matrix\\(rbind\\(M\\)\\)\\)",sep = ""))
         }else{
           A_<-do.call(str_replace(c[,combination][j], "\\(M\\)", ""), args = list(A_))
         }
         
         
         
       }
       
       #Escalar
       if(is.null(nrow(A_))){
         #una sola fila
         if(m_=="1"){
           A_=as.matrix(rbind(A_))
         }else{ ##Función que devuelve una sola columna
           A_=t(as.matrix(rbind(A_)))
         }
         
       }
       
       tryCatch({
         if(sum(which((round(A_,2)==round(B,2))==TRUE))>0){
           #result<-list (result, as.vector(c[,combination]))
           solution<-as.vector(c[,combination])
           if(length(solution)==1){
             #result=c(result,solution)
             result=solution
             found=TRUE
           }else{
             for(i in length(solution):1){
               if(i==length(solution)){
                 solution_=solution[i]
               }else{
                 solution_=str_replace(solution_,"\\(M\\)",paste("\\(",solution[i],"\\)",sep = ""))
               }
             }
             #result=c(result,solution_)
             result=solution_
             found=TRUE
           }
           
         }
       }, error = function(e) {
         
       })
       
       #if(found){break}
        
      }
      if(found){break}

    }#for combination
    #if(found){break}
    d<-d+1
  }
  
  return(result)
}


######Real example
functions<-read.csv("r_functions.csv", header = TRUE, row.names = NULL)
d_max=5

A=as.matrix(cbind(c(5,6,6,5,5,9),c(0.4,0.3,0.3,0.1,0.2,0.3),c(30,55,53,41,28,37),c(25,33,34,34,42,40),c(91,142,140,115,81,103)))

B1=as.matrix(cbind(c(1,0.25,NA,NA,NA),c(NA,NA,NA,NA,NA),c(NA,NA,NA,NA,NA),c(NA,NA,NA,NA,NA),c(NA,NA,NA,NA,NA)))
selecting_functions(A,B1,functions,d_max)

B2=t(as.matrix(c(0.38,NA,NA,NA,NA)))
selecting_functions(A,B2,functions,d_max)


###Test
functions<-read.csv("r_functions.csv", header = TRUE, row.names = NULL)
d_max=5
results=data.frame(stringsAsFactors = FALSE)
for(a in 1:10){
  for(s in 1:10){
    print(paste("Trying: A",a," with S",s,sep = ""))
    A=as.matrix(read.csv(paste("data/A_",a,".csv", sep = "")))
    B=as.matrix(read.csv(paste("data/A_",a,"-B_",s,".csv", sep = "")))
    print(selecting_functions(A,B,functions,d_max))
    results=rbind(results, cbind(as.character(a),as.character(s),selecting_functions(A,B,functions,d_max)))
  }
}
write.csv(results,"results.csv")
