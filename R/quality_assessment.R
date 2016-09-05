get_result_features = function(result){
  features = list(warnings=NA,edits=NA,moves=NA,confidence=NA,total_cells=NA,typed_cells=NA,empty_header=NA,empty_cells=NA,non_latin_chars=NA,row_col_ratio=NA)
  
  table = result$table
  
  if(is.null(table)){
    return(features)
  } 
  
  if(nrow(table)==0){
    return(features)
  }
  types=sapply(table,class)
  sum(apply(table,1,function(x){sum(unlist(x)=="" | is.na(unlist(x)))}))
  
  #measure table shape
  features$warnings = length(result$warnings)
  features$edits = result$edits
  features$moves = result$moves
  features$confidence = sum(result$confidence,na.rm=T)
  features$total_cells = result$cells
  features$typed_cells = ncol(table[,types!="character",drop=F])*nrow(table) - sum(apply(table[,types!="character",drop=F],1,function(x){sum(unlist(x)=="" | is.na(unlist(x)))}))
  features$empty_header = sum(colnames(table)=="")
  features$empty_cells = sum(apply(table,1,function(x){sum(unlist(x)=="" | is.na(unlist(x)))}))
  features$non_latin_chars = sum(gregexpr("NONLATINCHARACTER",iconv(paste(table,collapse=" "), "utf8", "latin1", sub="NONLATINCHARACTER"), fixed=T)[[1]] > 0)
  features$row_col_ratio = as.integer(nrow(table)>ncol(table))

  features = lapply(features,function(x){max(x,0)})
  
  return(features)
}

rank_quality = function(group_results,rank_function=get_result_features,weights=c(1,1,1,-1,-1,-1,1,1,1,-1)){
  group_result_features = data.frame()
  for(result in group_results){
    result_features = rank_function(result)
    group_result_features = rbind(group_result_features,result_features)
  }
  colnames(group_result_features) = names(result_features)
  
  #1 1:0.061617706 2:0.14636832 3:0.17539515 4:-0.59766531 5:0.38812715 6:-0.069931857 7:-0.17779298 8:0.013805295 9:0.12391482 10:0.64217299 11:-0.21883665 #
  #1 1:0.0049145184 2:0.19123393 3:0.44251943 4:-0.98470479 5:0.85696524 6:0.22609122 7:0.031004677 8:0.020101082 9:0.19626725 10:1.3443941 11:-0.1704682
  #weights = list(cells=0.10168045,typed_cells=-0.39059928,empty_header=1.0380306,edits=0.98764759,confidence=-0.44670057)
  #weights=list(ncol=-0.0049145184,nrow=-0.19123393,cells=-0.44251943,typed_cells=0.98470479,empty_header=-0.85696524,numerics_header=-0.22609122,na_fields=-0.031004677,non_latin_chars=-0.020101082,metadata_fields=-0.19626725,edits=-1.3443941,confidence=0.1704682)
  #weights = list(cells=1,typed_cells=1,empty_header=1,edits=1,confidence=1)
  #weights = c(-1,-1,-1,-1,-1)
  
  quality_ratings = numeric(length(group_results))
  for(i in 1:length(group_result_features)){
    min = min(group_result_features[,i])
    max = max(group_result_features[,i])
    for(j in seq_along(group_results)){
      weight = weights[i]
      if((max-min) > 0){
        normalized_feature_value = (group_result_features[j,i]-min)/(max-min)
        quality_ratings[j] = quality_ratings[j] + weight * normalized_feature_value
      }else{
        quality_ratings[j] = quality_ratings[j] + weight * 0
      }
    }
  }
  
  return(order(quality_ratings))
}
