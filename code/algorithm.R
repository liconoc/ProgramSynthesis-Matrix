if (!require("stringr")) {install.packages("stringr",dependencies=TRUE); library("stringr")}
if (!require("gtools")) {install.packages("gtools",dependencies=TRUE); library("gtools")}
if (!require("tidyverse")) {install.packages("tidyverse",dependencies=TRUE); library("tidyverse")}
if (!require("SnowballC")) {install.packages("SnowballC",dependencies=TRUE); library("SnowballC")}
if (!require("tidytext")) {install.packages("tidytext",dependencies=TRUE); library("tidytext")}
if (!require("dplyr")) {install.packages("dplyr",dependencies=TRUE); library("dplyr")}
if (!require("stringr")) {install.packages("stringr",dependencies=TRUE); library("stringr")}
if (!require("tm")) {install.packages("tm",dependencies=TRUE); library("tm")}
if (!require("text2vec")) {install.packages("text2vec",dependencies=TRUE); library("text2vec")}
if (!require("data.table")) {install.packages("data.table",dependencies=TRUE); library("data.table")}
if (!require("Matrix")) {install.packages("Matrix",dependencies=TRUE); library("Matrix")}
if (!require("R.utils")) {install.packages("R.utils",dependencies=TRUE); library("R.utils")}


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
        if(functions[which(functions$f==f),"f"]=="det(M)" || functions[which(functions$f==f),"f"]=="A[upper.tri(A)]<-0" || functions[which(functions$f==f),"f"]=="A[upper.tri(A)]" || functions[which(functions$f==f),"f"]=="A[lower.tri(A)]<-0" || functions[which(functions$f==f),"f"]=="A[lower.tri(A)]"){
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
  
  frequent_terms_example <- anti_join(frequent_terms_example, mystopwords, 
                                      by = "term")
  
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
  
  nodos_explorados=0
  nodos_podados=0
  nodos_creados=nrow(probs)
  
  withTimeout(

    while (!found && !explorado) {
      
      start_time <- Sys.time()
      
      f_algorithm<-calc_out(f_algorithm,f_list,m,n, functions)
      f_algorithm<-calc_probs(f_algorithm,f_list, functions, probs, m_, n_, a, b)
      
      id_g<-head(f_algorithm[which(f_algorithm$probs==max(f_algorithm$probs)),"id"],1)
      g<-as.character(f_list[which(f_list$id==id_g),"f"])
      
      nodos_explorados=nodos_explorados+1
      
      if((!is.na(functions[which(functions$f==g[1]),"m_min"]) & m<functions[which(functions$f==g[1]),"m_min"]) | (!is.na(functions[which(functions$f==g[1]),"n_min"]) & n<functions[which(functions$f==g[1]),"n_min"])){
        nodos_podados=nodos_podados+1
        
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
        } 
        
      }
      
      if(nrow(f_algorithm[which(f_algorithm$id==id_g),])>0){
        if(f_algorithm[which(f_algorithm$id==id_g),"nf"]<d_max){
          
          for(r in 1:nrow(probs)){
            g_<-c(g,as.character(probs[r,"f"]))
            f_algorithm<-rbind(f_algorithm,c(max(f_algorithm$id)+1,NA,length(g_),NA,NA))
            new_list<-cbind(max(f_algorithm$id),g_,1:length(g_))
            colnames(new_list)<-c("id","f","pos")
            f_list<-rbind(f_list,as.data.frame(new_list))
            
            nodos_creados=nodos_creados+1
          }
        }
        
        
        f_algorithm<-f_algorithm[which(f_algorithm$id %notin% id_g),]
        f_list<-f_list[which(f_list$id %notin% id_g),]
        
        
      }
      
      
      if(nrow(f_algorithm)==0){
        
        explorado=TRUE
      }
      
    }, timeout=timeout, onTimeout="silent")
  
  
  results <- list("sols" = sols, "nodos_explorados" = nodos_explorados, "nodos_creados" = nodos_creados, "ts" = ts)
  
  result<-cbind(ejemplo=e, estrategia=strategy, texto=text, d_max=d_max, s_max=s_max, nodos_creados=results$nodos_creados,nodos_explorados=results$nodos_explorados, sol1=results$sol[1], sol2=results$sol[2], sol3=results$sol[3], sol4=results$sol[4], ts1=results$ts[1], ts2=results$ts[2], ts3=results$ts[3], ts4=results$ts[4])

  print(results)
  return(result)
  
}

automatrix<-function(text, A, B, d_max, s_max, functions){
  results<-data.frame()
  
  probs_prior<-read.csv("data/prior_probs.csv", sep = ";")
  start_time <- Sys.time()
  probs_nlp<-as.data.frame(calc_probs_nlp(text))
  end_time <- Sys.time()
  t1<-end_time - start_time
  
  probs_nlp<-probs_nlp[order(probs_nlp),]
  probs_nlp<-probs_nlp[!is.na(probs_nlp$prob),]
  probs_prior<-probs_prior[order(probs_prior),]
  probs_prior<-probs_prior[!is.na(probs_prior$probs),]
  
  probs_gamma<-as.data.frame(cbind(as.character(probs_prior$f), (0.5*probs_prior$probs)+((1-0.5)*probs_nlp$prob)), stringsAsFactors = FALSE)
  probs_gamma$V2<-as.numeric(probs_gamma$V2)
  
  a=1
  b=0
  e=0
  
  result<-automatrix_probs(A, B, functions, probs_gamma, d_max, s_max, a, b, e, "prior+text", 120, "")
 
  tryCatch({
    s1<-as.character(result[1,"sol1"])
    results<-rbind(results,cbind(result, cbind(t2=t2, gamma=0, alpha=a, beta=b, solution=solution)))
  },
  error=function(cond) {
    results<-rbind(results,cbind(result, cbind(sol1=NA, sol2=NA, sol3=NA, sol4=NA, ts1=NA, ts2=NA, ts3=NA, ts4=NA, t2=t2, gamma=0, alpha=a, beta=b, solution=solution)))
  })
  

  
  return(results)
}


