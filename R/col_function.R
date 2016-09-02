col_function_type = list(spanning_header="spanning_header",spanning_data="spanning_data",aggregate="aggregate",metadata="metadata",empty="empty",empty_with_header="empty_with_header",data="data")

is_spanning_header = function(i,table_types){
  if(table_types[1,i,drop=F]==types$empty ||
     !all(which(table_types[,i,drop=F]!=types$empty) %in% which(table_types[,min(i+1,ncol(table_types)),drop=F]!=types$empty))){
    #|| any(diff(which(table_types[,i]!=types$empty))==1)
    return(F)
  }
  
  sum(table_types[,i,drop=F]==types$empty)/(nrow(table_types)) > mean(apply(table_types,2,function(x){sum(x==types$empty)/(nrow(table_types))}))
}

is_spanning_data = function(i,table_types,table){
  if(table_types[1,i,drop=F]==types$empty || !any(table_types[,i,drop=F]==types$empty)){
    return(F)
  }
  
  empty = sapply(2:nrow(table),function(j){table_types[j,i,drop=F]==types$empty})
  cell_equality = sapply(2:nrow(table),function(j){sum(table[j,,drop=F]==table[j-1,,drop=F],na.rm=T)})
  cell_distance = sapply(2:nrow(table),function(j){sum(RecordLinkage::levenshteinDist(unlist(table[j,,drop=F]),unlist(table[j-1,,drop=F])),na.rm=T)})

  # duplications = data.frame(matrix(ncol=3,nrow=nrow(table)-1))
  # duplications[[1]] = empty
  # duplications[[2]] = cell_equality
  # duplications[[3]] = cell_distance

  #all(cell_equality[empty]>(mean(cell_equality)+sd(cell_equality)) & cell_distance[empty]<(mean(cell_distance)-sd(cell_distance)))
  all(cell_equality[empty]>(mean(cell_equality)) & cell_distance[empty]<(mean(cell_distance)))
}

is_aggregate = function(i,table_types){
  if(colnames(table_types)[i]!=types$total){
    return(F)
  }
  
  #all data fields should be numeric punct or empty
  all(table_types[,i,drop=F]==types$numeric || table_types[,i,drop=F]==types$empty || table_types[,i,drop=F]==types$punctuation)
}

is_metadata = function(i,table_types){
  (colnames(table_types)[i]==types$empty | colnames(table_types)[i]==types$punctuation) & 
    sum(colnames(table_types)==types$empty | colnames(table_types)==types$punctuation)/ncol(table_types)<0.5 & 
    sum(table_types[,i,drop=F]==types$empty | table_types[,i,drop=F]==types$punctuation)/nrow(table_types)>0.8
}

is_empty = function(i,table_types){
  all(table_types[,i,drop=F]==types$empty | table_types[,i,drop=F]==types$punctuation) && colnames(table_types)[i,drop=F]==types$empty
}

is_empty_with_header = function(i,table_types){
  all(table_types[,i,drop=F]==types$empty | table_types[,i,drop=F]==types$punctuation) && colnames(table_types)[i,drop=F]!=types$empty
}

add_hypothesis.col_function_hypotheses = function(hypotheses,confidence,col_functions){
  for(i in seq_along(hypotheses)){
    if(all(hypotheses[[i]]$col_functions==col_functions)){
      hypotheses[[i]]$confidence = hypotheses[[i]]$confidence + confidence
      return(hypotheses)
    }
  }
  hypothesis = structure(list(col_functions=col_functions,confidence=confidence),class=c("col_function_hypothesis","hypothesis"))
  hypotheses[[length(hypotheses)+1]] = hypothesis
  return(hypotheses)
}

detect = function(table,configuration){
  if(class(table)[1]!="data.frame") return(stop(paste("Can not handle data type:",class(table)[1])))
  
  #create row function votes
  n = ncol(table)
  table_types = get_data_types(table)
  
  #list of row function hypotheses and votes
  hypotheses = create_hypothesis_list("col_function")
 
  #create first hypothesis
  col_functions = rep(col_function_type$data,ncol(table))
  for(i in seq_along(table_types)){
    if(is_empty(i,table_types)){
      col_functions[[i]] = col_function_type$empty
    }
    if(is_empty_with_header(i,table_types)){
      col_functions[[i]] = col_function_type$empty_with_header
    }
    if(is_aggregate(i,table_types)){
      col_functions[[i]] = col_function_type$aggregate
    }
  }
  hypotheses = add_hypothesis(hypotheses,confidence=1,col_functions=col_functions)
  
  #create second hypothesis (with spanning header, spanning_data and metadata)
  col_functions = rep(col_function_type$data,ncol(table))
  for(i in seq_along(table_types)){
    if(is_spanning_header(i,table_types) && ifelse(i>1,!any(col_functions[1:(i-1)]==col_function_type$data),T)){
      col_functions[[i]] = col_function_type$spanning_header
    }
    if(configuration$interpolate_spanning_column_data_cells && is_spanning_data(i,table_types,table)){
      col_functions[[i]] = col_function_type$spanning_data
    }
    if(is_metadata(i,table_types)){
      col_functions[[i]] = col_function_type$metadata
    }
    if(is_empty(i,table_types)){
      col_functions[[i]] = col_function_type$empty
    }
    if(is_empty_with_header(i,table_types)){
      col_functions[[i]] = col_function_type$empty_with_header
    }
  }
  hypotheses = add_hypothesis(hypotheses,confidence=1,col_functions=col_functions)

  hypotheses = normalize_confidence(hypotheses)
  
  return(hypotheses)
}

get_desc = function(hypothesis){
  return(paste("non-data columns:",paste(which(hypothesis$col_functions!=col_function_type$data),collapse=",")))
}

parse = function(table, hypothesis, errorHandler, configuration){
  col_functions = hypothesis$col_functions
  
  result = list()
  
  if(length(col_functions)!=ncol(table)){
    result$error= "Number of column functions is not equal not number of columns."
    return(list(result))
  }
  
  remove = c()
  result$edits = 0
  for(i in seq_along(hypothesis$col_functions)){
    if(hypothesis$col_functions[i]==col_function_type$spanning_header && configuration$interpolate_spanning_column_header_cells ||
       (hypothesis$col_functions[i]==col_function_type$spanning_data && configuration$interpolate_spanning_column_data_cells)){
      last = ""
      for(j in 1:nrow(table[,i,drop=F])){
        if(table[j,i,drop=F]!=""){
          last = table[j,i,drop=F]
        }else{
          table[j,i] = last
          result$edits = result$edits + 1
        }
      }
    }
    if((hypothesis$col_functions[i]==col_function_type$aggregate && configuration$remove_aggregates) ||
       hypothesis$col_functions[i]==col_function_type$metadata ||
       hypothesis$col_functions[i]==col_function_type$empty ||
       (hypothesis$col_functions[i]==col_function_type$empty_with_header && configuration$remove_named_empty_cols)){
      result$metadata = c(result$metadata,unlist(apply(table[,remove,drop=F],2,function(x){paste(x[x!=""])})))
      remove = c(remove,i)
    }
  }
  
  if(length(remove)==ncol(table)){
    table = NULL
  }else if(length(remove)>0){
    table = table[,-remove,drop=F]
  }
  
  result$intermediate = table
  
  return(list(result))
}

register_parsing_step(level="col_function",detect,parse,get_desc)
