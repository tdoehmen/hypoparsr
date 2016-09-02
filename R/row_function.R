row_function_type = list(header="header",spanning_header="spanning_header",data="data",aggregate="aggregate",metadata="metadata",empty="empty")

empty_vote = function(i,table_types){
  votes = 0
  if(all(table_types[i,,drop=F]==types$empty | table_types[i,,drop=F]==types$punctuation)){
    votes = votes + 1
  }
  if(all(table_types[i,,drop=F]==types$empty)){
    votes = votes + 1
  }
  return(votes/2)
}

aggregate_vote = function(i,table_types){
  votes = 0
  if(any(table_types[i,,drop=F]==types$total)){
    votes = votes + 1
  }
  if(table_types[i,1,drop=F]==types$total){
    votes = votes + 1
  }
  if(all(table_types[i,,drop=F]==types$total | table_types[i,,drop=F]==types$empty |  table_types[i,,drop=F]==types$numeric)){
    votes = votes + 1
  }
  return(votes/3)
}

header_vote = function(i,table_types){
  #TODO:check 30 first non-empty rows
  max_rows=min(30,(nrow(table_types)))
  if((i+1)<max_rows){
    sum(apply(table_types[(i+1):max_rows,,drop=F],1,function(x){sum(table_types[i,,drop=F]!=x & table_types[i,,drop=F]!=types$empty & x!=types$empty)}))/length((i+1):max_rows)/ncol(table_types)
  }else{
    0
  }
}

metadata_vote = function(i,table_types){
  if(any(table_types[i,,drop=F]!=types$empty)){
    sum(table_types[i,,drop=F]==types$empty)/ncol(table_types)
  }else{
    0
  }
}

data_vote = function(i,table_types){
  sum(table_types[i,,drop=F]==types$id |
        table_types[i,,drop=F]==types$numeric |
        table_types[i,,drop=F]==types$date |
        table_types[i,,drop=F]==types$time |
        table_types[i,,drop=F]==types$email |
        table_types[i,,drop=F]==types$url)/ncol(table_types)
}

count_function_votes = function(table_types){
  n = nrow(table_types)
  function_votes =  data.frame(header=numeric(n),aggregate=numeric(n),metadata=numeric(n),data=numeric(n),empty=numeric(n))
  for(i in 1:nrow(table_types)){
    if(empty_vote(i,table_types)){
      function_votes[i,] = list(0,0,0,0,1)
    }else{
      is_header = header_vote(i,table_types)
      is_aggregate = aggregate_vote(i,table_types)
      is_data = data_vote(i,table_types)
      is_metadata = metadata_vote(i,table_types)
      function_votes[i,] = list(is_header,is_aggregate,is_metadata,is_data,0)
    }
  }
  function_votes
}

normalize_function_votes = function(function_votes){
  n = nrow(function_votes)
  normalized_votes = data.frame(header=numeric(n),aggregate=numeric(n),metadata=numeric(n),data=numeric(n),empty=numeric(n))
  for(i in seq_along(function_votes)){
    normalized_votes[,i] = function_votes[,i]-max(function_votes[,i])
    #TODO: remove following line
    normalized_votes[,i] = normalized_votes[,i] + function_votes[,i]-max(function_votes[,i])
    normalized_votes[,i] = normalized_votes[,i] + function_votes[,i]-rowSums(function_votes[,-i])
  }
  normalized_votes
}

finalize_function_classification = function(row_functions,function_votes,data_start,data_end){
  #apply "empty" and "aggregate" function hypotheses
  row_functions[function_votes$empty==1] = row_function_type$empty
  row_functions[function_votes$aggregate>0 &
                  row_functions!=row_function_type$header &
                  row_functions!=row_function_type$spanning_header &
                  (1:length(row_functions)<data_start |
                     1:length(row_functions)>data_end)] = row_function_type$aggregate
  if(any(row_functions==row_function_type$spanning_header) &&
     !any(row_functions==row_function_type$header)){
    row_functions[tail(which(row_functions==row_function_type$spanning_header),1)]=row_function_type$header
  }
  row_functions
}

add_hypothesis.row_function_hypotheses = function(hypotheses,confidence,row_functions,function_votes,data_start,data_end){
  row_functions = finalize_function_classification(row_functions,function_votes,data_start,data_end)
  
  for(i in seq_along(hypotheses)){
    if(all(hypotheses[[i]]$row_functions==row_functions)){
      hypotheses[[i]]$confidence = hypotheses[[i]]$confidence + confidence
      return(hypotheses)
    }
  }
  
  hypothesis = structure(list(row_functions=row_functions, confidence=confidence),class=c("row_function_hypothesis","hypothesis"))
  hypotheses[[length(hypotheses)+1]] = hypothesis
  return(hypotheses)
}

detect = function(matrix,configuration){
  if(class(matrix)[1]!="data.frame") return(stop(paste("Can not handle data type:",class(matrix)[1])))
  
  #create row function votes
  n = nrow(matrix)
  table_types = get_data_types(matrix)
  function_votes = count_function_votes(table_types)
  normalized_function_votes = normalize_function_votes(function_votes)
  
  #data boundaries
  data_cells = which(normalized_function_votes$data==apply(normalized_function_votes,1,max))
  #list of row function hypotheses and votes
  hypotheses = create_hypothesis_list("row_function")
  
  #generate default hypothesis
  default_row_functions = rep(row_function_type$data,n)
  
  #add default hypothesis D
  row_functions = default_row_functions
  hypotheses = add_hypothesis(hypotheses,confidence=1,row_functions,function_votes,0,nrow(matrix))
  
  #add default hypothesis HD
  row_functions = default_row_functions
  row_functions[1] = row_function_type$header
  hypotheses = add_hypothesis(hypotheses,confidence=1,row_functions,function_votes,1,nrow(matrix))
  
  if(length(data_cells)<1){
    hypotheses = normalize_confidence(hypotheses)
    return(hypotheses)
  }
  
  data_start = min(data_cells)
  data_end = max(data_cells)

  #meta-data boundaries and metadata default hypothesis
  meta_lines = rle(normalized_function_votes$metadata==apply(normalized_function_votes,1,max) | normalized_function_votes$empty==apply(normalized_function_votes,1,max))
  meta_start_start = ifelse(head(meta_lines$values,1)==T, 1, NA)
  meta_start_end = ifelse(head(meta_lines$values,1)==T, head(meta_lines$length,1), NA)
  meta_end_start = ifelse(tail(meta_lines$values,1)==T, n-tail(meta_lines$length,1)+1, NA)
  meta_end_end = ifelse(tail(meta_lines$values,1)==T, n, NA)
  metadata_row_functions = default_row_functions
  if(!is.na(meta_start_start)){metadata_row_functions[meta_start_start:meta_start_end] = row_function_type$metadata}
  if(!is.na(meta_end_start)){metadata_row_functions[meta_end_start:meta_end_end] = row_function_type$metadata}
  
  #list of row function hypotheses and votes
  hypotheses = create_hypothesis_list("row_function")
  
  #add default hypothesis D
  row_functions = default_row_functions
  hypotheses = add_hypothesis(hypotheses,confidence=1,row_functions,function_votes,data_start,data_end)
  
  #add default hypothesis M*DM*
  row_functions = metadata_row_functions
  hypotheses = add_hypothesis(hypotheses,confidence=1,row_functions,function_votes,data_start,data_end)
  
  # #add default hypothesis HD
  row_functions = default_row_functions
  row_functions[1] = row_function_type$header
  hypotheses = add_hypothesis(hypotheses,confidence=1,row_functions,function_votes,data_start,data_end)
   
  #add default hypothesis M*HDM*
  row_functions = metadata_row_functions
  row_functions[ifelse(!is.na(meta_start_end),min(meta_start_end+1,n),1)] = row_function_type$header
  hypotheses = add_hypothesis(hypotheses,confidence=1,row_functions,function_votes,data_start,data_end)

  if(data_start>1){
    #add hypothesis H*D
    row_functions = default_row_functions
    row_functions[1:(data_start-1)] = row_function_type$header
    hypotheses = add_hypothesis(hypotheses,confidence=1,row_functions,function_votes,data_start,data_end)
    
    #add hypothesis M*H*DM*
    row_functions = metadata_row_functions
    row_functions[ifelse(!is.na(meta_start_end),min(meta_start_end+1,n),1):(data_start-1)] = row_function_type$header
    hypotheses = add_hypothesis(hypotheses,confidence=1,row_functions,function_votes,data_start,data_end)
  }
  
  if(data_start>2){
    #add hypothesis SH*HD
    row_functions = default_row_functions
    row_functions[1:(data_start-2)] = row_function_type$spanning_header
    row_functions[data_start-1] = row_function_type$header
    hypotheses = add_hypothesis(hypotheses,confidence=1,row_functions,function_votes,data_start,data_end)
    
    #add hypothesis M*SH*HDM*
    row_functions = metadata_row_functions
    row_functions[ifelse(!is.na(meta_start_end),min(meta_start_end+1,n),1):(data_start-2)] = row_function_type$spanning_header
    row_functions[data_start-1] = row_function_type$header
    hypotheses = add_hypothesis(hypotheses,confidence=1,row_functions,function_votes,data_start,data_end)
  }
  
  hypotheses = normalize_confidence(hypotheses)
  
  return(hypotheses)
}

get_desc = function(hypothesis){
  return(paste("header_lines:",paste(hypothesis$row_functions==row_function_type$header),collapse=","))
}

parse = function(matrix, hypothesis, errorHandler, configuration){
  row_functions = hypothesis$row_functions
  
  result = list()
  
  if(length(row_functions)!=nrow(matrix)){
    result$error= "Number of row functions is not equal not number of rows."
    return(list(result))
  }
  
  #produce header
  result$edits = 0
  header_index = which(row_functions==row_function_type$header)
  spanning_header_index = which(row_functions==row_function_type$spanning_header)
  for(i in spanning_header_index){
    last = ""
    for(j in seq_along(matrix[i,,drop=F])){
      if(matrix[i,j,drop=F]!=""){
        last = matrix[i,j,drop=F]
      }else{
        matrix[i,j] = last
        result$edits = result$edits + 1
      }
    }
  }
  colnames(matrix) = apply(matrix[c(spanning_header_index,header_index),,drop=F], 2, function(x){paste(x[x!=""], collapse=".")})
  result$edits = result$edits + sum(apply(matrix[c(spanning_header_index,header_index),,drop=F], 2, function(x){max(length(x[x!=""])-1,0)}))
  
  #remove rows
  table = matrix
  remove = which(row_functions==row_function_type$empty |
           row_functions==row_function_type$metadata | 
           row_functions==row_function_type$header |
           row_functions==row_function_type$spanning_header)
  if(configuration$remove_aggregates){
    remove = c(remove, which(row_functions==row_function_type$aggregate))
    result$metadata = unlist(apply(table[row_functions==row_function_type$aggregate,,drop=F],1,function(x){paste(x[x!=""])}))
  }
  result$metadata = c(result$metadata,unlist(apply(table[row_functions==row_function_type$metadata,,drop=F],1,function(x){paste(x[x!=""])})))
  if(length(remove)==nrow(table)){
    table = NULL
  }else if(length(remove)>0){
    table = table[-remove,,drop=F]
  }
  
  result$intermediate = table

  return(list(result))
}

register_parsing_step(level="row_function",detect,parse,get_desc)
