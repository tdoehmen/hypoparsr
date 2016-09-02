parsing_hierarchy = list()

types = list(text="text",total="total",empty="empty",punctuation="punctuation",id="id",numeric="numeric",date="date",time="time",email="email",url="url",logical="logical")#uk_postcode="uk_postcode",
#TODO: Add Quoting signs at beginning and end OR create version with leading/trailing quotes removed and without quotes removed in dialect hypothesis step when quoting sign is empty 
patterns=list()
patterns[types$text] = "^.*$"
patterns[types$total] = "^.*(total|Total|TOTAL).*$"
patterns[types$empty] = "^(\\s*)([\"']?)(\\s*)(NULL|null|Null|NA|na|N/A|n/a|NaN|nan|#N/A|#NA|1\\.#IND|1\\.#QNAN|-1\\.#IND|-1\\.#IND|-NaN|-nan|\\!)?(\\s*)([\"']?)(\\s*)$"
patterns[types$punctuation] = "^(\\s*)([\"']?)(\\s*)[[:punct:]]+(\\s*)([\"']?)(\\s*)$"
patterns[types$id] = "^(\\s*)([\"']?)(\\s*)(?=.*[[:upper:]].*)(?=.*[[:digit:]])[[:upper:][:digit:][:punct:]]+(\\s*)([\"']?)(\\s*)$"
#patterns[types$numeric] = "^(\\s*)([\"']?)(\\s*)(?=.*[[:digit:]])(?![[:alpha:]])[[:digit:][:punct:]\\p{Sc}\\s]+(\\s*)([\"']?)(\\s*)$"
patterns[types$numeric] = "^(\\s*)([\"']?)(\\s*)(?=.*[[:digit:]])[[:digit:]\\(\\+\\-\\.\\,\\)\\p{Sc}\\%\\s]+\\S*(\\s*)([\"']?)(\\s*)$"
patterns[types$date] = "^(\\s*)([\"']?)(\\s*)([[:digit:]]{2}|[[:digit:]]{4})[\\.\\/\\\\\\-\\s](([[:digit:]]{2}|[[:digit:]]{4})|[[:alpha:]]{3})[\\.\\/\\\\\\-\\s]([[:digit:]]{2}|[[:digit:]]{4})(\\s*)([\"']?)(\\s*)$"
patterns[types$time] = "^(\\s*)([\"']?)(\\s*)[[:digit:]]{2}(\\:)[[:digit:]]{2}((\\:)[[:digit:]]{2})?(\\s*)([\"']?)(\\s*)$"
patterns[types$email] = "^(\\s*)([\"']?)(\\s*)[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+(\\s*)([\"']?)(\\s*)$"
patterns[types$url] = "^(https?:\\/\\/)?([\\da-z\\.-]+)\\.([a-z\\.]{2,6})([\\/\\w \\.-]*)*\\/?$"
#patterns[types$uk_postcode] = "^(\\s*)([\"']?)(\\s*)(GIR 0AA)|((([A-Z-[QVX]][0-9][0-9]?)|(([A-Z-[QVX]][A-Z-[IJZ]][0-9][0-9]?)|(([A-Z-[QVX]][0-9][A-HJKPSTUW])|([A-Z-[QVX]][A-Z-[IJZ]][0-9][ABEHMNPRVWXY])))) [0-9][A-Z-[CIKMOV]]{2})(\\s*)([\"']?)(\\s*)$"
patterns[types$logical] = "^(\\s*)([\"']?)(\\s*)(True|False|TRUE|FALSE|true|false)(\\s*)([\"']?)(\\s*)$"

get_data_types = function(table){
  ncol=ncol(table)
  nrow=nrow(table)
  table_types = data.frame(matrix(ncol=ncol,nrow=nrow))
  for(i in 1:ncol){
    col = table[[i]]
    col_types = rep(types$empty,nrow)
    for(p in 1:length(patterns)){
      pattern = patterns[p]
      regex_res = grepl(pattern,col,perl=T)
      col_types = ifelse(!is.na(regex_res) & regex_res==T,names(patterns)[p],col_types)
    }
    table_types[[i]] = col_types
  }
  
  header = colnames(table)
  header_types = rep(types$empty,nrow)
  for(p in 1:length(patterns)){
    pattern = patterns[p]
    regex_res = grepl(pattern,header,perl=T)
    header_types = ifelse(!is.na(regex_res) & regex_res==T,names(patterns)[p],header_types)
  }
  colnames(table_types) = header_types
  
  return(table_types)
}

add_hypothesis = function(hypotheses,confidence,...) UseMethod("add_hypothesis")
add_hypothesis.default = function(hypotheses,confidence,...){
  parameter = list(...)
  parameter$confidence = confidence
  hypothesis = structure(parameter,class=c(sub("hypotheses","hypothesis",class(hypotheses)[[1]]),"hypothesis"))
  hypotheses[[length(hypotheses)+1]] = hypothesis
  return(hypotheses)
}
create_hypothesis_list = function(type){
  return(structure(list(),class=c(paste0(type,"_hypotheses"),"hypotheses")))
}
normalize_confidence = function(hypotheses){
  if(length(hypotheses)==0) return(list())
  
  total_confidence = sum(sapply(hypotheses,function(x){x$confidence}))
  for(i in seq_along(hypotheses)){
    hypotheses[[i]]$confidence = hypotheses[[i]]$confidence/total_confidence
  }
  return(hypotheses)
}
register_parsing_step = function(level,detector,parser,desc){
  parsing_step = list(level=level,detector=detector,parser=parser,desc=desc)
  parsing_hierarchy <<- c(parsing_hierarchy,list(parsing_step))
}
