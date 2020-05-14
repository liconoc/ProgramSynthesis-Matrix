library(stringr)
library(gtools)
library(tidyverse) 
library(SnowballC)
library(tidytext)
library("dplyr")
library("stringr")
library("tm")
library(text2vec)
library(data.table)
library(Matrix)
library("R.utils")
library("tictoc")

`%notin%` <- Negate(`%in%`)

##Remove common terms from a string
removeCommonTerms <- function(string){
  
  stopwords_regex = paste(stopwords('en')[stopwords('en') %notin% c("not","which", "only", "any","same","between","most","until","on","before","after","all")], collapse = '\\b|\\b')
  stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
  docs = stringr::str_replace_all(string, stopwords_regex, '')
  
  docs<-gsub("[.]"," ", as.character(docs))
  docs<-gsub("/"," ", docs)
  docs<-gsub("@"," ", docs)
  docs<-gsub("\\("," ", docs)
  docs<-gsub(")"," ", docs)
  docs<-gsub(":"," ", docs)
  docs<-gsub(","," ", docs)
  docs<-gsub("-"," ", docs)
  docs<-gsub("\\?"," ", docs)
  docs<-gsub("[0-9]"," ", docs)
  docs<-stripWhitespace(tolower(docs))
  docs<-strsplit(docs, " ")
  
  v<-c()
  for(split in docs){
    v<-c(v, split)
  }
  
  return(v)
}

##Build a sparse matrix between the tf-idf of the functions and the text hint
buildSparseMatrix_probs <- function(function_terms) {
  fs<-unique(as.character(function_terms$functions))
  ts<-unique(as.character(function_terms$term))
  sparseMatrix <- matrix(0L, nrow = length(fs), ncol = length(ts), dimnames = list(fs,ts))
  for(row in 1:nrow(function_terms)){
    sparseMatrix[as.character(function_terms[row,"functions"]),as.character(function_terms[row,"term"])]<-function_terms[row,"prob"]
  }
  return(sparseMatrix)
}

##calculate the output size wihout applying the functions
calc_out <- function(f_algorithm,f_list,m,n, functions){
  for(row in 1:nrow(f_algorithm)){
    if(is.na(f_algorithm[row,"m_out"])&is.na(f_algorithm[row,"n_out"])){
      id_f<-f_algorithm[row,"id"]
      list_f<-as.character(f_list[which(f_list$id==id_f),"f"])
      
      for(i in 1:length(list_f)){
        #set inputs
        if(i==1){
          m_input<-m
          n_input<-n
        }else{
          m_input<-m_output
          n_input<-n_output
        }
        
        
        f<-list_f[i]
        f_=f
        
        f=gsub("\\(\\(as.matrix\\(rbind\\(A\\)\\)\\)\\)", "\\(A\\)", f)
        
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
        
        
        if((!exists("m_output") || m_output!=-1) && (!exists("n_output") || n_output!=-1)){
          #set output
          if(as.character(functions[which(functions$f==f),"m_o"])=="m"){
            m_output<-m_input
          }else if(as.character(functions[which(functions$f==f),"m_o"])=="n"){
            m_output<-n_input
          }else if(as.character(functions[which(functions$f==f),"m_o"])=="m*2"){
            m_output<-m_input*2
          }else{
            m_output<-1
          }
          
          if(as.character(functions[which(functions$f==f),"n_o"])=="m"){
            n_output<-m_input
          }else if(as.character(functions[which(functions$f==f),"n_o"])=="n"){
            n_output<-n_input
          }else if(as.character(functions[which(functions$f==f),"n_o"])=="n*2"){
            n_output<-n*2
          }else{
            n_output<-1
          }
        }
        
      }#for i
      
      f_algorithm[row,"m_out"]<-m_output
      f_algorithm[row,"n_out"]<-n_output
    }
    
  }#for row
  
  return(f_algorithm)
}

##Probabilities, alpha, beta
calc_probs <- function (f_algorithm,f_list, functions, probs, m_, n_, a, b){
  for(row in 1:nrow(f_algorithm)){
    if(is.na(f_algorithm[row,"probs"])){
      id_f<-f_algorithm[row,"id"]
      list_f<-as.character(f_list[which(f_list$id==id_f),"f"])
      prob=1
      for(i in 1:length(list_f)){
        prob_f<-probs[which(probs$f==list_f[i]),"prob"]
        prob=prob*prob_f
      }
      if(f_algorithm[row,"m_out"]==m_ & f_algorithm[row,"n_out"]==n_){
        prob=prob*(1+a)
      }
      if(sum(duplicated(list_f))>0){
        prob=prob*(1-b)
      }
      f_algorithm[row,"probs"]<-prob
    }
    
  }#for row
  
  return(f_algorithm)
}

#Probabilities for a text hint
calc_probs_nlp <- function (f){
  frequent_terms<-read.csv("data/frequent_terms.csv", stringsAsFactors = FALSE, sep = ";")
  colnames(frequent_terms)<-c("functions","term","Freq")
  
  frequent_terms$Freq<-as.numeric(frequent_terms$Freq)
  
  f<-as.character(f)
  f<-gsub("arr.ind=T"," ", f)
  f<-gsub("\\(.*\\)"," ", f)
  f<-gsub(","," ", f)
  f<-gsub("[0-9]"," ", f)
  f<-gsub("\\["," ", f)
  f<-gsub("\\]"," ", f)
  f<-gsub("\\("," ", f)
  f<-gsub("\\)"," ", f)
  f<-gsub("-"," ", f)
  f<-gsub("!"," ", f)
  f<-gsub("&"," ", f)
  f<-gsub("#"," ", f)
  f<-gsub(";"," ", f)
  f<-gsub(":"," ", f)
  f<-gsub("\\?"," ", f)
  f<-gsub("A"," ", f)
  f<-strsplit(f, " ")
  
  lista_terminos<-c()
  for(fu in 1:length(f[[1]])){
    
    if(f[[1]][fu]!=""){
      words<-as.character(c(removeCommonTerms(f[[1]][fu])))
      lista_terminos<-c(lista_terminos,words)
      lista_terminos<-lista_terminos[!lista_terminos==""]
    }
  }
  
  frequent_terms_example<-as.data.frame(table(lista_terminos))
  
  frequent_terms_example<-cbind("example", frequent_terms_example)
  
  colnames(frequent_terms_example)<-c("functions","term","Freq")
  
  mystopwords <- tibble(term = c("x", "y", "if", "it", "var", "the", ""))
  
  frequent_terms_example <- anti_join(frequent_terms_example, mystopwords, by = "term")
  
  lemma_unique<-frequent_terms_example %>%
    mutate(word_stem = wordStem(as.character(term), language="english"))
  
  example_stem<-lemma_unique[,c(1,4)]
  colnames(example_stem)<-c("functions", "term")
  example_stem<-rbind(lemma_unique[,1:2], example_stem)
  
  frequent_terms_example<-as.data.frame(table(example_stem))
  frequent_terms_example<-frequent_terms_example[which(frequent_terms_example$Freq>0),]
  
  function_terms_example<-rbind(frequent_terms, frequent_terms_example)
  
  frequent_terms <- function_terms_example %>%
    bind_tf_idf(term, functions, Freq)

  functions<-unique(frequent_terms$functions)
  terms<-unique(frequent_terms$term)
  for(f in 1:length(functions)){
    for(t in 1:length(terms)){
      f_=as.character(functions[f])
      t_=as.character(terms[t])
      
      if(nrow(frequent_terms[which(frequent_terms$functions==f_&frequent_terms$term==t_),])==0){
        frequent_terms<-rbind(frequent_terms, c(f_,t_,0, 0.0, 0.0, 0.0))
      }
    }
  }
  
  
  frequent_terms$p_g<-1/nrow(frequent_terms[which(frequent_terms$functions==(unique(frequent_terms$functions)[1])),])
  
  frequent_terms$tf_idf<-as.numeric(frequent_terms$tf_idf)
  
  frequent_terms$sum_pg_tfidf<-frequent_terms$tf_idf+frequent_terms$p_g
  
  frequent_terms$prob<-NA
  for(row in 1:nrow(frequent_terms)){
    suma_function<-sum(frequent_terms[which(frequent_terms$functions==frequent_terms[row,"functions"]),"sum_pg_tfidf"])
    frequent_terms[row,"prob"]<-frequent_terms[row,"sum_pg_tfidf"]/suma_function
  }
  
  dtm <- buildSparseMatrix_probs(frequent_terms)
  
  cos_sim_functions_example = sim2(x = dtm, method = "cosine", norm = "l2")
  
  probabilities_example<-cos_sim_functions_example["example",1:ncol(cos_sim_functions_example)-1]
  probabilities_example<-probabilities_example/sum(probabilities_example)
  probabilities_example<-as.data.frame(sort(probabilities_example, decreasing = TRUE))
  setDT(probabilities_example, keep.rownames = TRUE)[]
  colnames(probabilities_example)<-c("function", "prob")
  
  return(probabilities_example)
}


##Selects the lists of functions (or combination of functions) in such a way that
##applied to A we can obtain the full matrix partially filled in B.
automatrix_probs <- function (A, B, functions, probs, d_max, s_max, a, b, e, strategy, timeout, text){
  
  result<-data.frame()
  
  m<-nrow(A)
  n<-ncol(A)
  
  m_<-nrow(B)
  n_<-ncol(B)
  
  #vector
  if(is.null(m_)){
    m_=1
  }
  
  if(is.null(n_)){
    n_=length(B)
  }

  s=0
  found=FALSE
  explorado=FALSE
  
  sols<-c()
  ts<-c()
  
  colnames(probs)<-c("f", "prob")
  
  functions$id<-NULL
  
  f_algorithm<-as.data.frame(cbind(1:nrow(probs),probs$prob,1,NA, NA), stringsAsFactors = FALSE)
  colnames(f_algorithm)<-c("id","probs","nf","m_out", "n_out")
  
  f_list<-as.data.frame(cbind(1:nrow(probs),as.character(probs$f),1), stringsAsFactors = FALSE)
  colnames(f_list)<-c("id","f","pos")
  
  f_algorithm<-calc_out(f_algorithm,f_list,m,n, functions)
  for(row in 1:nrow(f_algorithm)){
      id_f<-f_algorithm[row,"id"]
      list_f<-as.character(f_list[which(f_list$id==id_f),"f"])
      prob=f_algorithm[row,"probs"]
      if(f_algorithm[row,"m_out"]==m_ & f_algorithm[row,"n_out"]==n_){
        prob=prob*(1+a)
      }
      if(sum(duplicated(list_f))>0){
        prob=prob*(1-b)
      }
      f_algorithm[row,"probs"]<-prob
   
    
  }#for row
  
  explored_nodes=0
  pruned_nodes=0
  created_nodes=nrow(probs)
  
  withTimeout(

  while (!found && !explorado) {
    start_time <- Sys.time()
  
    f_algorithm<-calc_out(f_algorithm,f_list,m,n, functions)  
    f_algorithm<-calc_probs(f_algorithm,f_list, functions, probs, m_, n_, a, b)
    
    id_g<-head(f_algorithm[which(f_algorithm$probs==max(f_algorithm$probs)),"id"],1)
    g<-as.character(f_list[which(f_list$id==id_g),"f"])
   
    explored_nodes=explored_nodes+1
  
   if((!is.na(functions[which(functions$f==g[1]),"m_min"]) & m<functions[which(functions$f==g[1]),"m_min"]) | (!is.na(functions[which(functions$f==g[1]),"n_min"]) & n<functions[which(functions$f==g[1]),"n_min"])){
    
       pruned_nodes=pruned_nodes+1
       
       f_algorithm<-f_algorithm[which(f_algorithm$id %notin% id_g),]
       f_list<-f_list[which(f_list$id %notin% id_g),]
     
   }else{ 
     
     if(f_algorithm[which(f_algorithm$id==id_g),"m_out"]==m_ & f_algorithm[which(f_algorithm$id==id_g),"n_out"]==n_){
       
       A_<-A
       tryCatch({
         
         for(j in 1:length(g)){
             A_<-eval(parse(text = str_replace_all(g[j], "A", "A_")))
         }
         
         if((length(which((round(A_,2)==round(B,2)))))==length(which(!is.na(B))) ){
           solution<-as.vector(g)
           if(length(solution)==1){
             result=solution
             sols<-c(sols,result)
             end_time <- Sys.time()
             ts<-c(ts, end_time - start_time)
             s=s+1
             f_algorithm<-f_algorithm[which(f_algorithm$id %notin% id_g),]
             f_list<-f_list[which(f_list$id %notin% id_g),]
             if(s==s_max){
               found=TRUE
             }else{
                 start_time <- Sys.time()
             }
           }else{
             for(i in length(solution):1){
               if(i==length(solution)){
                 solution_=solution[i]
               }else{
                 solution_=str_replace_all(solution_,"A",solution[i])
               }
             }
             
             result=solution_
             sols<-c(sols,result)
             
             end_time <- Sys.time()
             ts<-c(ts, end_time - start_time)
             
             s=s+1
            
             f_algorithm<-f_algorithm[which(f_algorithm$id %notin% id_g),]
             f_list<-f_list[which(f_list$id %notin% id_g),]
             if(s==s_max){
               found=TRUE
               
             }else{
               start_time <- Sys.time()
             }
           }
         }
       }, error = function(e) {
         
       })
     } ##if m_out n_out
       
   }
   
   if(nrow(f_algorithm[which(f_algorithm$id==id_g),])>0){
     if(f_algorithm[which(f_algorithm$id==id_g),"nf"]<d_max){
       
       for(r in 1:nrow(probs)){
         g_<-c(g,as.character(probs[r,"f"]))
         f_algorithm<-rbind(f_algorithm,c(max(f_algorithm$id)+1,NA,length(g_),NA,NA))
         new_list<-cbind(max(f_algorithm$id),g_,1:length(g_))
         colnames(new_list)<-c("id","f","pos")
         f_list<-rbind(f_list,as.data.frame(new_list))
        
         created_nodes=created_nodes+1
       }
     }
     
       f_algorithm<-f_algorithm[which(f_algorithm$id %notin% id_g),]
       f_list<-f_list[which(f_list$id %notin% id_g),]
   }
   
    if(nrow(f_algorithm)==0){
      explorado=TRUE
    }
  }, timeout=timeout, onTimeout="silent")

  results <- list("sols" = sols, "explored_nodes" = explored_nodes, "created_nodes" = created_nodes, "ts" = ts)
  
  result<-cbind(example=e, strategy=strategy, text=text, d_max=d_max, s_max=s_max, created_nodes=results$created_nodes,explored_nodes=results$explored_nodes, sol1=results$sol[1], sol2=results$sol[2], sol3=results$sol[3], sol4=results$sol[4], ts1=results$ts[1], ts2=results$ts[2], ts3=results$ts[3], ts4=results$ts[4])

  return(result)
  
}




###(Correctos)

#1.How to reverse a matrix

text<-"How to reverse a matrix"
e=1
#A= matrix(c(c(1:10),c(10:1)), ncol=2)
#S <- apply(A, 2, rev)
#B=matrix(rep(c(NA,NA),10), byrow=TRUE, nrow=10)
A=as.matrix(read.csv(paste("tesis/results/datasets/A_",e,".csv", sep="")))
B=as.matrix(read.csv(paste("tesis/results/datasets/B_",e,".csv", sep="")))
S=read.csv(paste("tesis/results/datasets/A_",e,".csv", sep=""))
#B[1,]=c(10,1)
res<-calcule_results(e,text,A,B, "apply(A, 2, rev)")
print(res)
# write.csv(A, file = paste("tesis/results/datasets/A_",e,".csv", sep=""), row.names = FALSE)
# write.csv(B, file = paste("tesis/results/datasets/B_",e,".csv", sep=""), row.names = FALSE)
# write.csv(S, file = paste("tesis/results/datasets/S_",e,".csv", sep=""), row.names = FALSE)


#2. replace 0's with 1's 

text<-"replace 0's with 1's"
e=2
# A=diag(5)
# S=1-A
# B=as.matrix(rbind((1-A)[1,],NA,NA,NA,NA))
A=as.matrix(read.csv(paste("tesis/results/datasets/A_",e,".csv", sep="")))
B=as.matrix(read.csv(paste("tesis/results/datasets/B_",e,".csv", sep="")))
S=read.csv(paste("tesis/results/datasets/A_",e,".csv", sep=""))
res<-calcule_results(e,text,A,B,"1-A")
print(res)
# write.csv(A, file = paste("tesis/results/datasets/A_",e,".csv", sep=""), row.names = FALSE)
# write.csv(B, file = paste("tesis/results/datasets/B_",e,".csv", sep=""), row.names = FALSE)
# write.csv(S, file = paste("tesis/results/datasets/S_",e,".csv", sep=""), row.names = FALSE)


#3.Get positions for NA (antiguo 5)

text<-"Get positions for NA"
e=3
# A <- matrix(c(NA,1,NA,1,NA,2,NA,NA,NA,1,2,3),ncol=4)
# S=is.na(A)
# B=matrix(rep(c(NA,NA,NA,NA),3), byrow=TRUE, nrow=3)
# B[1,1]=TRUE
# B[1,2]=FALSE
A=as.matrix(read.csv(paste("tesis/results/datasets/A_",e,".csv", sep="")))
B=as.matrix(read.csv(paste("tesis/results/datasets/B_",e,".csv", sep="")))
S=read.csv(paste("tesis/results/datasets/A_",e,".csv", sep=""))
res<-calcule_results(e,text,A,B, "is.na(A)")
print(res)
# write.csv(A, file = paste("tesis/results/datasets/A_",e,".csv", sep=""), row.names = FALSE)
# write.csv(B, file = paste("tesis/results/datasets/B_",e,".csv", sep=""), row.names = FALSE)
# write.csv(S, file = paste("tesis/results/datasets/S_",e,".csv", sep=""), row.names = FALSE)

#4. rowsums accross specific row in a matrix (antiguo 6)

text<-"rowsums accross specific row in a matrix"
e=4
# A <- matrix(c(45,47,92,43,45,88,44,48,92,49,47,96),ncol=4)
# S=rowSums(A)
# B=S
# B[3]<-NA
# B[2]<-NA
A=as.matrix(read.csv(paste("tesis/results/datasets/A_",e,".csv", sep="")))
B=as.matrix(read.csv(paste("tesis/results/datasets/B_",e,".csv", sep="")))
S=read.csv(paste("tesis/results/datasets/A_",e,".csv", sep=""))
res<-calcule_results(e,text,A,B, "rowSums(A)")
print(res)
# write.csv(A, file = paste("tesis/results/datasets/A_",e,".csv", sep=""), row.names = FALSE)
# write.csv(B, file = paste("tesis/results/datasets/B_",e,".csv", sep=""), row.names = FALSE)
# write.csv(S, file = paste("tesis/results/datasets/S_",e,".csv", sep=""), row.names = FALSE)


#5. How to get the sum of each four rows of a matrix in R (antiguo 7)

text<-"How to get the sum of each four rows of a matrix in R"
e=5
# A <- matrix(c(45,47,92,43,45,88,44,48,92,49,47,96, 12, 14, 15),ncol=4)
# S=rowSums(A)
# B=S
# B[2]<-NA
# B[3]<-NA
A=as.matrix(read.csv(paste("tesis/results/datasets/A_",e,".csv", sep="")))
B=as.matrix(read.csv(paste("tesis/results/datasets/B_",e,".csv", sep="")))
S=read.csv(paste("tesis/results/datasets/A_",e,".csv", sep=""))
res<-calcule_results(e,text,A,B,"rowSums(A)")
print(res)
# write.csv(A, file = paste("tesis/results/datasets/A_",e,".csv", sep=""), row.names = FALSE)
# write.csv(B, file = paste("tesis/results/datasets/B_",e,".csv", sep=""), row.names = FALSE)
# write.csv(S, file = paste("tesis/results/datasets/S_",e,".csv", sep=""), row.names = FALSE)


#6. Extract diagonal of a matrix in R (antiguo 9)

text<-"Extract diagonal of a matrix in R"
e=6
# A <- matrix(1:9,nrow=3)
# S=diag(A)
# B=S
# B[2]<-NA
A=as.matrix(read.csv(paste("tesis/results/datasets/A_",e,".csv", sep="")))
B=as.matrix(read.csv(paste("tesis/results/datasets/B_",e,".csv", sep="")))
S=read.csv(paste("tesis/results/datasets/A_",e,".csv", sep=""))
res<-calcule_results(e,text,A,B,"diag(A)")
print(res)
# write.csv(A, file = paste("tesis/results/datasets/A_",e,".csv", sep=""), row.names = FALSE)
# write.csv(B, file = paste("tesis/results/datasets/B_",e,".csv", sep=""), row.names = FALSE)
# write.csv(S, file = paste("tesis/results/datasets/S_",e,".csv", sep=""), row.names = FALSE)


#7. Rotate a Matrix in R (antiguo 10)

text<-"Rotate a Matrix in R"
e=7
# A <- matrix(1:9, 3)
# S=t(A)
# B=S
# B[2]<-NA
# B[5]<-NA
# B[9]<-NA
A=as.matrix(read.csv(paste("tesis/results/datasets/A_",e,".csv", sep="")))
B=as.matrix(read.csv(paste("tesis/results/datasets/B_",e,".csv", sep="")))
S=read.csv(paste("tesis/results/datasets/A_",e,".csv", sep=""))
res<-calcule_results(e,text,A,B,"t(A)")
print(res)
# write.csv(A, file = paste("tesis/results/datasets/A_",e,".csv", sep=""), row.names = FALSE)
# write.csv(B, file = paste("tesis/results/datasets/B_",e,".csv", sep=""), row.names = FALSE)
# write.csv(S, file = paste("tesis/results/datasets/S_",e,".csv", sep=""), row.names = FALSE)


#8. Concatenating matrices by row in r (antiguo 12)

text<-"Concatenating matrices by row in r "
e=8
# A <- rbind(c(1, -1/4), c(-1/4, 1)) 
# S=rbind(A,A)
# B=S
A=as.matrix(read.csv(paste("tesis/results/datasets/A_",e,".csv", sep="")))
B=as.matrix(read.csv(paste("tesis/results/datasets/B_",e,".csv", sep="")))
S=read.csv(paste("tesis/results/datasets/A_",e,".csv", sep=""))
res<-calcule_results(e,text,A,B,"rbind(A,A)")
print(res)
# write.csv(A, file = paste("tesis/results/datasets/A_",e,".csv", sep=""), row.names = FALSE)
# write.csv(B, file = paste("tesis/results/datasets/B_",e,".csv", sep=""), row.names = FALSE)
# write.csv(S, file = paste("tesis/results/datasets/S_",e,".csv", sep=""), row.names = FALSE)


#9. Concatenating matrices by column in r (antiguo 13)
text<-"Concatenating matrices by column in r"
e=9
# A <- rbind(c(1, -1/4), c(-1/4, 1)) 
# S=cbind(A,A)
# B=S
# B[2]=NA
# B[4]=NA
# B[6]=NA
A=as.matrix(read.csv(paste("tesis/results/datasets/A_",e,".csv", sep="")))
B=as.matrix(read.csv(paste("tesis/results/datasets/B_",e,".csv", sep="")))
S=read.csv(paste("tesis/results/datasets/A_",e,".csv", sep=""))
res<-calcule_results(e,text,A,B,"cbind(A,A)")
print(res)
# write.csv(A, file = paste("tesis/results/datasets/A_",e,".csv", sep=""), row.names = FALSE)
# write.csv(B, file = paste("tesis/results/datasets/B_",e,".csv", sep=""), row.names = FALSE)
# write.csv(S, file = paste("tesis/results/datasets/S_",e,".csv", sep=""), row.names = FALSE)


#10. Convert a matrix in r into a upper triangular matrix (antiguo 14)

text<-"Convert a matrix in r into a upper triangular matrix"
e=10
# A <- rbind(c(1, -1/4), c(-1/4, 1)) 
# S=A
# S=S[lower.tri(S)]
# B=S
A=as.matrix(read.csv(paste("tesis/results/datasets/A_",e,".csv", sep="")))
B=as.matrix(read.csv(paste("tesis/results/datasets/B_",e,".csv", sep="")))
S=read.csv(paste("tesis/results/datasets/A_",e,".csv", sep=""))
res<-calcule_results(e,text,A,B,"S[lower.tri(S)]")
print(res)
# write.csv(A, file = paste("tesis/results/datasets/A_",e,".csv", sep=""), row.names = FALSE)
# write.csv(B, file = paste("tesis/results/datasets/B_",e,".csv", sep=""), row.names = FALSE)
# write.csv(S, file = paste("tesis/results/datasets/S_",e,".csv", sep=""), row.names = FALSE)


###Pruebas nuevas

###11. Multiply a matrix by another matrix 
text<-"Multiply a matrix by another matrix "
e=11
# A = matrix(c(1,0,3,4,0,5), byrow=TRUE, nrow=2)
# S=A*A
# B = matrix(c(1,NA,NA,16,NA,NA), byrow=TRUE, nrow=2)
A=as.matrix(read.csv(paste("tesis/results/datasets/A_",e,".csv", sep="")))
B=as.matrix(read.csv(paste("tesis/results/datasets/B_",e,".csv", sep="")))
S=read.csv(paste("tesis/results/datasets/A_",e,".csv", sep=""))
res<-calcule_results(e,text,A,B,"A*A")
print(res)
# write.csv(A, file = paste("tesis/results/datasets/A_",e,".csv", sep=""), row.names = FALSE)
# write.csv(B, file = paste("tesis/results/datasets/B_",e,".csv", sep=""), row.names = FALSE)
# write.csv(S, file = paste("tesis/results/datasets/S_",e,".csv", sep=""), row.names = FALSE)

#12. How can I create a correlation matrix in R?
text<-"How can I create a correlation matrix in R?"
e=12
# A <- data.frame(x1=rnorm(5),
#                 x2=rnorm(5),
#                 x3=rnorm(5),
#                 x4=rnorm(5),
#                 x5=rnorm(5))
# S=cor(A)
# B=S
# B[2]<-NA
# B[3]<-NA
# B[5]<-NA
A=as.matrix(read.csv(paste("tesis/results/datasets/A_",e,".csv", sep="")))
B=as.matrix(read.csv(paste("tesis/results/datasets/B_",e,".csv", sep="")))
S=read.csv(paste("tesis/results/datasets/A_",e,".csv", sep=""))
res<-calcule_results(e,text,A,B,"cor(A)")
print(res)
# write.csv(A, file = paste("tesis/results/datasets/A_",e,".csv", sep=""), row.names = FALSE)
# write.csv(B, file = paste("tesis/results/datasets/B_",e,".csv", sep=""), row.names = FALSE)
# write.csv(S, file = paste("tesis/results/datasets/S_",e,".csv", sep=""), row.names = FALSE)


#13. which elements are greather than one
text<-"which elements are greather than one"
e=13
# A <- data.frame(x1=rnorm(5),
#                 x2=rnorm(5),
#                 x3=rnorm(5),
#                 x4=rnorm(5),
#                 x5=rnorm(5))
# S=A>1
# B=S
# B[1]=NA
# B[3]=NA
# B[5]=NA
# B[7]=NA
# B[9]=NA
# B[12]=NA
# B[16]=NA
A=as.matrix(read.csv(paste("tesis/results/datasets/A_",e,".csv", sep="")))
B=as.matrix(read.csv(paste("tesis/results/datasets/B_",e,".csv", sep="")))
S=read.csv(paste("tesis/results/datasets/A_",e,".csv", sep=""))
res<-calcule_results(e,text,A,B,"A>1")
print(res)
# write.csv(A, file = paste("tesis/results/datasets/A_",e,".csv", sep=""), row.names = FALSE)
# write.csv(B, file = paste("tesis/results/datasets/B_",e,".csv", sep=""), row.names = FALSE)
# write.csv(S, file = paste("tesis/results/datasets/S_",e,".csv", sep=""), row.names = FALSE)


#14. which elements are negative numbers
text<-"which elements are negative numbers"
e=14
# A <- data.frame(x1=rnorm(5),
#                 x2=rnorm(5),
#                 x3=rnorm(5),
#                 x4=rnorm(5),
#                 x5=rnorm(5))
# S=A<0
# B=S
# B[1]=NA
# B[3]=NA
# B[5]=NA
# B[7]=NA
# B[9]=NA
# B[12]=NA
# B[16]=NA
A=as.matrix(read.csv(paste("tesis/results/datasets/A_",e,".csv", sep="")))
B=as.matrix(read.csv(paste("tesis/results/datasets/B_",e,".csv", sep="")))
S=read.csv(paste("tesis/results/datasets/A_",e,".csv", sep=""))
res<-calcule_results(e,text,A,B,"A<0")
print(res)
# write.csv(A, file = paste("tesis/results/datasets/A_",e,".csv", sep=""), row.names = FALSE)
# write.csv(B, file = paste("tesis/results/datasets/B_",e,".csv", sep=""), row.names = FALSE)
# write.csv(S, file = paste("tesis/results/datasets/S_",e,".csv", sep=""), row.names = FALSE)


#15. What is the best way to transpose a matrix in R
text<-"What is the best way to transpose a matrix in R"
e=15
# A <- data.frame(x1=rnorm(5),
#                 x2=rnorm(5),
#                 x3=rnorm(5),
#                 x4=rnorm(5),
#                 x5=rnorm(5))
# S=t(A)
# B=S
# B[1]=NA
# B[3]=NA
# B[5]=NA
# B[7]=NA
# B[9]=NA
# B[12]=NA
# B[16]=NA
A=as.matrix(read.csv(paste("tesis/results/datasets/A_",e,".csv", sep="")))
B=as.matrix(read.csv(paste("tesis/results/datasets/B_",e,".csv", sep="")))
S=read.csv(paste("tesis/results/datasets/A_",e,".csv", sep=""))
res<-calcule_results(e,text,A,B,"t(A)")
print(res)
# write.csv(A, file = paste("tesis/results/datasets/A_",e,".csv", sep=""), row.names = FALSE)
# write.csv(B, file = paste("tesis/results/datasets/B_",e,".csv", sep=""), row.names = FALSE)
# write.csv(S, file = paste("tesis/results/datasets/S_",e,".csv", sep=""), row.names = FALSE)


#16. How to get the mean over a entire matrix
text<-"How to get the mean over a entire matrix"
e=16
# A <- matrix(1:9, 3)
# S=mean(A)
# B=S
A=as.matrix(read.csv(paste("tesis/results/datasets/A_",e,".csv", sep="")))
B=as.matrix(read.csv(paste("tesis/results/datasets/B_",e,".csv", sep="")))
S=read.csv(paste("tesis/results/datasets/A_",e,".csv", sep=""))
res<-calcule_results(e,text,A,B,"mean(A)")
print(res)
# write.csv(A, file = paste("tesis/results/datasets/A_",e,".csv", sep=""), row.names = FALSE)
# write.csv(B, file = paste("tesis/results/datasets/B_",e,".csv", sep=""), row.names = FALSE)
# write.csv(S, file = paste("tesis/results/datasets/S_",e,".csv", sep=""), row.names = FALSE)

#17. How to get the max over a entire matrix
text<-"How to get the mean over a entire matrix"
e=17
# A <- matrix(1:9, 3)
# S=max(A)
# B=S
A=as.matrix(read.csv(paste("tesis/results/datasets/A_",e,".csv", sep="")))
B=as.matrix(read.csv(paste("tesis/results/datasets/B_",e,".csv", sep="")))
S=read.csv(paste("tesis/results/datasets/A_",e,".csv", sep=""))
res<-calcule_results(e,text,A,B,"max(A)")
print(res)
# write.csv(A, file = paste("tesis/results/datasets/A_",e,".csv", sep=""), row.names = FALSE)
# write.csv(B, file = paste("tesis/results/datasets/B_",e,".csv", sep=""), row.names = FALSE)
# write.csv(S, file = paste("tesis/results/datasets/S_",e,".csv", sep=""), row.names = FALSE)


#18. How to get the min over a entire matrix
text<-"How to get the mean over a entire matrix"
e=18
# A <- matrix(1:9, 3)
# 
# S=min(A)
# B=S
A=as.matrix(read.csv(paste("tesis/results/datasets/A_",e,".csv", sep="")))
B=as.matrix(read.csv(paste("tesis/results/datasets/B_",e,".csv", sep="")))
S=read.csv(paste("tesis/results/datasets/A_",e,".csv", sep=""))
res<-calcule_results(e,text,A,B,"min(A)")
print(res)
# write.csv(A, file = paste("tesis/results/datasets/A_",e,".csv", sep=""), row.names = FALSE)
# write.csv(B, file = paste("tesis/results/datasets/B_",e,".csv", sep=""), row.names = FALSE)
# write.csv(S, file = paste("tesis/results/datasets/S_",e,".csv", sep=""), row.names = FALSE)


#19. Elegant way to report the number of missing values in a matrix 
text<-"Elegant way to report the number of missing values in a matrix"
e=19
# A <- rbind(c(1, -1/4,3,NA,NA), c(-1/4, 1,NA,NA,6))
# S= colSums(is.na(A))
# B=S
# B[1]=NA
# B[3]=NA
A=as.matrix(read.csv(paste("tesis/results/datasets/A_",e,".csv", sep="")))
B=as.matrix(read.csv(paste("tesis/results/datasets/B_",e,".csv", sep="")))
S=read.csv(paste("tesis/results/datasets/A_",e,".csv", sep=""))
res<-calcule_results(e,text,A,B,"colSums(is.na(A))")
print(res)
# write.csv(A, file = paste("tesis/results/datasets/A_",e,".csv", sep=""), row.names = FALSE)
# write.csv(B, file = paste("tesis/results/datasets/B_",e,".csv", sep=""), row.names = FALSE)
# write.csv(S, file = paste("tesis/results/datasets/S_",e,".csv", sep=""), row.names = FALSE)


#20. Na's in a diagonal matrix
text<-"Na's in a diagonal matrix"
e=20
# A <- rbind(c(NA, -1/4, NA, 3), c(-1/4, 1, 5, NA), c(NA, -1/4, NA, 3), c(-1/4, 1, 5, NA))
# S<-is.na(diag(A))
# B=S
# B[3]=NA
# B[4]=NA
A=as.matrix(read.csv(paste("tesis/results/datasets/A_",e,".csv", sep="")))
B=as.matrix(read.csv(paste("tesis/results/datasets/B_",e,".csv", sep="")))
S=read.csv(paste("tesis/results/datasets/A_",e,".csv", sep=""))
res<-calcule_results(e,text,A,B,"is.na(diag(A))")
print(res)
# write.csv(A, file = paste("tesis/results/datasets/A_",e,".csv", sep=""), row.names = FALSE)
# write.csv(B, file = paste("tesis/results/datasets/B_",e,".csv", sep=""), row.names = FALSE)
# write.csv(S, file = paste("tesis/results/datasets/S_",e,".csv", sep=""), row.names = FALSE)


###Pruebas 23/04/2020 más de una función, más de dos tb

#21. How to calculate the square of the mean for a correlation matrix
text<-"How to calculate the square of the mean for a correlation matrix"
e=21
# A <- matrix(rexp(100, rate=.1), ncol=10)
# S<-mean(cor(A))*mean(cor(A))
# B=S
A=as.matrix(read.csv(paste("tesis/results/datasets/A_",e,".csv", sep="")))
B=as.matrix(read.csv(paste("tesis/results/datasets/B_",e,".csv", sep="")))
S=read.csv(paste("tesis/results/datasets/A_",e,".csv", sep=""))
res<-calcule_results(e,text,A,B,"mean(cor(A))*mean(cor(A))")
print(res)
# write.csv(A, file = paste("tesis/results/datasets/A_",e,".csv", sep=""), row.names = FALSE)
# write.csv(B, file = paste("tesis/results/datasets/B_",e,".csv", sep=""), row.names = FALSE)
# write.csv(S, file = paste("tesis/results/datasets/S_",e,".csv", sep=""), row.names = FALSE)


#22. Multiply a matrix by another matrix and calculate the mean of its correlation matrix
text<-"Multiply a matrix by another matrix and calculate the mean of its correlation matrix"
e=22
# A <- matrix(rexp(100, rate=.1), ncol=10)
# S<-mean(cor(A*A))
# B=S
A=as.matrix(read.csv(paste("tesis/results/datasets/A_",e,".csv", sep="")))
B=as.matrix(read.csv(paste("tesis/results/datasets/B_",e,".csv", sep="")))
S=read.csv(paste("tesis/results/datasets/A_",e,".csv", sep=""))
res<-calcule_results(e,text,A,B,"mean(cor(A*A))")
print(res)
# write.csv(A, file = paste("tesis/results/datasets/A_",e,".csv", sep=""), row.names = FALSE)
# write.csv(B, file = paste("tesis/results/datasets/B_",e,".csv", sep=""), row.names = FALSE)
# write.csv(S, file = paste("tesis/results/datasets/S_",e,".csv", sep=""), row.names = FALSE)


#23. How to know if the maximum of the diagonal is greater than one?
text<-"How to know if the maximum of the diagonal is greater than one?"
e=23
# A <- matrix(rexp(100, rate=.1), ncol=10)
# S<-max(diag(A))>1
# B=S
A=as.matrix(read.csv(paste("tesis/results/datasets/A_",e,".csv", sep="")))
B=as.matrix(read.csv(paste("tesis/results/datasets/B_",e,".csv", sep="")))
S=read.csv(paste("tesis/results/datasets/A_",e,".csv", sep=""))
res<-calcule_results(e,text,A,B,"max(diag(A))>1")
print(res)
# write.csv(A, file = paste("tesis/results/datasets/A_",e,".csv", sep=""), row.names = FALSE)
# write.csv(B, file = paste("tesis/results/datasets/B_",e,".csv", sep=""), row.names = FALSE)
# write.csv(S, file = paste("tesis/results/datasets/S_",e,".csv", sep=""), row.names = FALSE)


#24. Can I multiply a matrix by itself conbining it by columns?
text<-"Can I make a binary multiplication a matrix by itself combining it by columns?"
e=24
# A <- matrix(1:9, 3)
# S<-cbind(A,A)*cbind(A,A)
# B=S
A=as.matrix(read.csv(paste("tesis/results/datasets/A_",e,".csv", sep="")))
B=as.matrix(read.csv(paste("tesis/results/datasets/B_",e,".csv", sep="")))
S=read.csv(paste("tesis/results/datasets/A_",e,".csv", sep=""))
res<-calcule_results(e,text,A,B,"cbind(A,A)*cbind(A,A)")
print(res)
# write.csv(A, file = paste("tesis/results/datasets/A_",e,".csv", sep=""), row.names = FALSE)
# write.csv(B, file = paste("tesis/results/datasets/B_",e,".csv", sep=""), row.names = FALSE)
# write.csv(S, file = paste("tesis/results/datasets/S_",e,".csv", sep=""), row.names = FALSE)


# #25. How can I make a coerced matrix with the diagonal matrix only for those numbers which are greater than one?
# text<-"How can I make a coerced matrix with the diagonal matrix only for those numbers which are greater than one?"
# A <- matrix(rexp(100, rate=.1), ncol=10)
# S<-cbind(diag(A>1), diag(A>1))
# B=S
# e=25
# res<-calcule_results(e,text,A,B,"cbind(diag(A>1), diag(A>1))")
# print(res)
# write.csv(A, file = paste("tesis/results/datasets/A_",e,".csv", sep=""), row.names = FALSE)
# write.csv(B, file = paste("tesis/results/datasets/B_",e,".csv", sep=""), row.names = FALSE)
# write.csv(S, file = paste("tesis/results/datasets/S_",e,".csv", sep=""), row.names = FALSE)

#26. Max of the coerced diagonal matrix 
text<-"Max of the coerced diagonal matrix"
e=26
# A <- matrix(1:9, 3)
# S<-max(diag(A*A))
# B=S
A=as.matrix(read.csv(paste("tesis/results/datasets/A_",e,".csv", sep="")))
B=as.matrix(read.csv(paste("tesis/results/datasets/B_",e,".csv", sep="")))
S=read.csv(paste("tesis/results/datasets/A_",e,".csv", sep=""))
res<-calcule_results(e,text,A,B,"max(diag(A*A))")
print(res)
# write.csv(A, file = paste("tesis/results/datasets/A_",e,".csv", sep=""), row.names = FALSE)
# write.csv(B, file = paste("tesis/results/datasets/B_",e,".csv", sep=""), row.names = FALSE)
# write.csv(S, file = paste("tesis/results/datasets/S_",e,".csv", sep=""), row.names = FALSE)


# #27. Finding the max number of the diagonal for the coerced matrix of its transpose
# text<-"Finding the max number of the diagonal for the coerced matrix of its transpose"
# A <- matrix(1:10, 5)
# S<-max(diag(t(A)*t(A)))
# B=S
# e=27
# res<-calcule_results(e,text,A,B,"max(diag(t(A)*t(A)))")
# print(res)
# write.csv(A, file = paste("tesis/results/datasets/A_",e,".csv", sep=""), row.names = FALSE)
# write.csv(B, file = paste("tesis/results/datasets/B_",e,".csv", sep=""), row.names = FALSE)
# write.csv(S, file = paste("tesis/results/datasets/S_",e,".csv", sep=""), row.names = FALSE)
# 
# 
# #28. Is the max of the transpose coerced diagonal greater than one?
# text<-"Is the max of the transpose coerced diagonal greater than one?"
# A <- matrix(1:10, 5)
# S<-max(diag(t(A)*t(A)))>1
# B=S
# e=28
# res<-calcule_results(e,text,A,B,"max(diag(t(A)*t(A)))")
# print(res)
# write.csv(A, file = paste("tesis/results/datasets/A_",e,".csv", sep=""), row.names = FALSE)
# write.csv(B, file = paste("tesis/results/datasets/B_",e,".csv", sep=""), row.names = FALSE)
# write.csv(S, file = paste("tesis/results/datasets/S_",e,".csv", sep=""), row.names = FALSE)








#############################################################9
####synthetic examples
Ff<-functions

for(Dd in 4){
  for(a in 3:10){
    print(a)
    for(b in 1:10){
      print(b)
      
      
      for(Ss in 1:4){
        print(Dd)
        
        Aa<-as.matrix(read.csv(paste("data/syntetic/",Dd,"/A_",a,".csv", sep = "")))
        Bb<-read.csv(paste("data/syntetic/",Dd,"/A_",a,"-B_",b,".csv", sep = ""))
        if(!is.null(nrow(Bb))){
          Bb<-as.matrix(Bb)
        }
        
        ###Uniform
        print("Uniform")
        probs<-read.csv("data/uniform_probs.csv", sep = ";")
        results<-data.frame()
        
        d_max=Dd
        s_max=Ss
        e=paste(a,b,Dd,Ss,sep = "_")
        text=""
        tryCatch({
          
          
          tic()
          result<-automatrix_probs(Aa, Bb, Ff, probs, d_max, s_max, 1, 1, e, "uniform", 120, "")
          toc(log = TRUE)
          log.txt <- tic.log(format = TRUE)
          t2 <- gsub( " sec elapsed", "", unlist(log.txt))
          tic.clearlog()
          results<-rbind(results,cbind(result, cbind(t2=t2)))
        })
        
        
        
        ###Prior
        print("Prior")
        probs<-read.csv("data/prior_probs.csv", sep = ";")
        result<-data.frame()
        
        d_max=Dd
        s_max=1
        e=paste(a,b,Dd,Ss,sep = "_")
        text=""
        
        tryCatch({
          
          tic()
          result<-automatrix_probs(Aa, Bb, Ff, probs, d_max, s_max, 1, 1, e, "prior", 120, "")
          toc(log = TRUE)
          log.txt <- tic.log(format = TRUE)
          t2 <- gsub( " sec elapsed", "", unlist(log.txt))
          tic.clearlog()
          results<-rbind(results,cbind(result, cbind(t2=t2)))
        })
        
        write.csv(results,file = paste("results/completos/syntetics/",e,".csv",sep = ""))      
        
      }
      
    }
  }
}  



# frequent_terms<-read.csv("function_terms_tfidf.csv")
# function_terms_subset<-frequent_terms[which(frequent_terms$functions %in% c("det(A)", "diag(A)")),]
# 
# function_terms_subset %>%
#   arrange(desc(tf_idf)) %>%
#   mutate(term = factor(term, levels = rev(unique(term)))) %>% 
#   group_by(functions) %>% 
#   top_n(8) %>% 
#   ungroup() %>%
#   ggplot(aes(term, tf_idf, fill = functions)) +
#   geom_col(show.legend = FALSE) +
#   labs(x = NULL, y = "tf_idf") +
#   facet_wrap(~functions, ncol = 2, scales = "free") +
#   coord_flip()




