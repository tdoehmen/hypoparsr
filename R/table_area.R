get_data_density = function(matrix_types){
  density_ratings = matrix(nrow=nrow(matrix_types),ncol=ncol(matrix_types))
  for(i in 1:ncol(matrix_types)){
    for(j in 1:nrow(matrix_types)){
      if(matrix_types[j,i,drop=F]!=types$empty){
        square = matrix_types[max((j-1),1):min((j+1),nrow(matrix_types)),max((i-1),1):min((i+1),ncol(matrix_types)),drop=F]
        density_ratings[j,i] = sum(square!=types$empty)
      }else{
        density_ratings[j,i] = 0
      }
    }
  }
  density_ratings
}

get_dense_areas = function(matrix_types,density_ratings){
  dense_areas=list()
  while(max(density_ratings)>0){
    max_index = arrayInd(which.max(density_ratings), dim(density_ratings))
    max_row = max_index[[1]]
    max_col = max_index[[2]]
    
    empty_rows = which(apply(matrix_types,1,function(x){all(x==types$empty)}))
    empty_cols = which(apply(matrix_types,2,function(x){all(x==types$empty)}))
    
    row_start = max(1,empty_rows[empty_rows<=max_row]+1)
    row_end = min(nrow(matrix_types),empty_rows[empty_rows>=max_row]-1)
    col_start = max(1,empty_cols[empty_cols<=max_col]+1)
    col_end = min(ncol(matrix_types),empty_cols[empty_cols>=max_col]-1)
  
    if(row_start==row_end || col_start==col_end){
      break
    }
        
    density_ratings[row_start:row_end,col_start:col_end] = -1
    
    dense_areas[[length(dense_areas)+1]] = list(row_start=row_start,row_end=row_end,col_start=col_start,col_end=col_end)
  }
  
  dense_areas
}

clean_density_ratings = function(density_ratings,dense_areas){
  for(dense_area in dense_areas){
    density_ratings[dense_area$row_start:dense_area$row_end,dense_area$col_start:dense_area$col_end] = -1
  }
  
  density_ratings
}

expand_areas_upper_left = function(leftover_density_ratings, dense_areas){
  table_areas = list()
  
  for(i in seq_along(dense_areas))
  {
    dense_area = dense_areas[[i]]
    
    upper_neighbour = max(which(leftover_density_ratings[,dense_area$col_start]==-1 & 1:nrow(leftover_density_ratings)<dense_area$row_start),0)
    left_neighbour = max(which(leftover_density_ratings[dense_area$row_start,]==-1 & 1:ncol(leftover_density_ratings)<dense_area$col_start),0)
  
    if(i==length(dense_areas)){
      dense_area$row_end = nrow(leftover_density_ratings)
      dense_area$col_end = ncol(leftover_density_ratings)
    }
    
    table_areas[[i]] =  list(row_start=upper_neighbour+1,row_end=dense_area$row_end,col_start=left_neighbour+1,col_end=dense_area$col_end)
  }
  
  return(table_areas)
}

remove_empty_frame = function(table_area,matrix_types){
  matrix_types_subset = matrix_types[table_area$row_start:table_area$row_end,table_area$col_start:table_area$col_end,drop=F]
  
  empty_rows = rle(apply(matrix_types_subset,1,function(x) all(x=="empty")))
  first_nonempty_row = ifelse(head(empty_rows$values,1)==T, head(empty_rows$lengths,1)+1, 1)
  last_nonempty_row = ifelse(tail(empty_rows$values,1)==T, nrow(matrix_types_subset)-tail(empty_rows$lengths,1), nrow(matrix_types_subset))
  
  empty_columns = rle(apply(matrix_types_subset,2,function(x) all(x=="empty")))
  first_nonempty_col = ifelse(head(empty_columns$values,1)==T, head(empty_columns$lengths,1)+1, 1)
  last_nonempty_col = ifelse(tail(empty_columns$values,1)==T, ncol(matrix_types_subset)-tail(empty_columns$lengths,1), ncol(matrix_types_subset))
  
  table_area = list(row_start=(table_area$row_start-1)+first_nonempty_row,row_end=(table_area$row_start-1)+last_nonempty_row,col_start=(table_area$col_start-1)+first_nonempty_col,col_end=(table_area$col_start-1)+last_nonempty_col)
  return(table_area)
}

add_hypothesis.table_area_hypotheses = function(hypotheses,confidence,table_areas,matrix_types){
  #always remove empty leading/trailing columns/rows
  for(i in seq_along(table_areas)){
    table_areas[[i]] = remove_empty_frame(table_areas[[i]],matrix_types)
  }
  
  #simple check for duplicates (order sensitive)
  for(i in seq_along(hypotheses)){
    if(length(hypotheses[[i]]$table_areas) == length(table_areas)){
      if(all(unlist(hypotheses[[i]]$table_areas) == unlist(table_areas))){
        hypotheses[[i]]$confidence = hypotheses[[i]]$confidence + confidence
        return(hypotheses)
      }
    }
  }
  
  hypothesis = structure(list(table_areas=table_areas,confidence=confidence),class=c("table_area_hypothesis","hypothesis"))

  hypotheses[[length(hypotheses)+1]] = hypothesis
  return(hypotheses)
}

detect = function(matrix,configuration){
  if(class(matrix)[1]!="data.frame") return(stop(paste("Can not handle data type:",class(matrix)[1])))
  
  hypotheses = create_hypothesis_list("table_area")

  matrix_types = get_data_types(matrix)
  
  #add default hypothesis
  default_areas = list(list(row_start=1,row_end=nrow(matrix_types),col_start=1,col_end=ncol(matrix_types)))
  hypotheses = add_hypothesis(hypotheses,confidence=1,table_areas=default_areas,matrix_types=matrix_types)
  
  if(!configuration$only_one_table){
    #perform area detection
    density_ratings = get_data_density(matrix_types)
    dense_areas = get_dense_areas(matrix_types,density_ratings)
    leftover_density_ratings = clean_density_ratings(density_ratings,dense_areas)
    
    #expand detected areas (default: to the upper left side)
    table_areas_expanded_upper_left = expand_areas_upper_left(leftover_density_ratings, dense_areas)
    hypotheses = add_hypothesis(hypotheses,confidence=1,table_areas=table_areas_expanded_upper_left,matrix_types=matrix_types)
  }
  
  #normalize confidence values
  hypotheses = normalize_confidence(hypotheses)
  
  return(hypotheses)
}

get_desc = function(hypothesis){
  return(paste("number of tables:",length(hypothesis$table_areas)))
}

parse = function(matrix, hypothesis, errorHandler, configuration){
  results = list()
  
  for(table_area in hypothesis$table_areas){
    result = list()
    result$intermediate = matrix[table_area$row_start:table_area$row_end,table_area$col_start:table_area$col_end,drop=F]
    results[[length(results)+1]] = result
  }
  
  return(results)
}

register_parsing_step(level="table_area",detect,parse,get_desc)
