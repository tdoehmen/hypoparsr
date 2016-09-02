# esacpe: [\^$.|?*+()
numeric_separators=list(list(decimal="\\.",thousand=""),list(decimal="\\.",thousand="\\,"),list(decimal="\\.",thousand=" "),list(decimal="\\,",thousand=""),list(decimal=",",thousand="\\."),list(decimal="\\,",thousand=" "))
numeric_negative_signs=list(list(start="\\-",end="$"),list(start="\\(",end="\\)"))
logical_true_qualifier=c("TRUE","True","true")
logical_false_qualifier=c("FALSE","False","false")
date_separators=c(".","-","/"," ")
date_orders=list(c("%d","%m","%Y"),c("%m","%d","%Y"),c("%Y","%m","%d"),c("%d","%b","%Y"))
time_patterns=list(c("%H:%M:%S"),c("%H:%M"))
na_value_qualifier=c("NULL","null","Null","NA","na","N/A","n/a","NaN","nan","N/A","#NA","1\\.#IND","1\\.#QNAN","-1\\.#IND","-1\\.#IND","-NaN","-nan","-","!","\\s*")

detect = function(table,configuration){
  if(class(table)[1]!="data.frame") return(stop(paste("Can not handle data type:",class(table)[1])))
  
  table_types = get_data_types(table)
  
  #generally determine possible data types
  data_types = list()
  for(i in seq_along(table_types)){
    #(the 100% method)
    if(any(table_types[,i,drop=F]==types$numeric) && all(table_types[,i,drop=F]==types$numeric | table_types[,i,drop=F]==types$empty | table_types[,i,drop=F]==types$punctuation)){
      data_types[[i]] = list(type=types$numeric,formats=list())
    }else if(any(table_types[,i,drop=F]==types$logical) && all(table_types[,i,drop=F]==types$logical | table_types[,i,drop=F]==types$empty | table_types[,i,drop=F]==types$punctuation)){
      data_types[[i]] = list(type=types$logical,formats=list())
    }else if(any(table_types[,i,drop=F]==types$date) && all(table_types[,i,drop=F]==types$date | table_types[,i,drop=F]==types$empty | table_types[,i,drop=F]==types$punctuation)){
      data_types[[i]] = list(type=types$date,formats=list())
    }else if(any(table_types[,i,drop=F]==types$time) && all(table_types[,i,drop=F]==types$time | table_types[,i,drop=F]==types$empty | table_types[,i,drop=F]==types$punctuation)){
      data_types[[i]] = list(type=types$time,formats=list())
    }else{
      data_types[[i]] = list(type=types$text,formats=list())
    }
    #(the majority method)
    # type_frequency = table(table_types[,i])
    # type_frequency_ordered = type_frequency[order(type_frequency,decreasing=T)]
    # type_frequency_filtered = type_frequency_ordered[names(type_frequency_ordered)!=types$empty & names(type_frequency_ordered)!=types$punctuation]
    # if(length(type_frequency_filtered)>0){
    #   data_types[[i]] = list(type=names(type_frequency_filtered[1]),formats=list(),confidence=length(type_frequency_filtered)/length(table[,i]))
    # }else{
    #   data_types[[i]] = list(type=names(names(type_frequency_ordered[1])),formats=list(),confidence=length(type_frequency_filtered)/length(table[,i]))
    # }
  }
  
  #determine data formatting for numeric, logical, date, time
  for(i in seq_along(data_types)){
    if(data_types[[i]]$type==types$numeric){
      idx = 1
      votes = c()
      formats = list()
      for(s in seq_along(numeric_separators)){
        for(t in seq_along(numeric_negative_signs)){
          sep = numeric_separators[[s]]
          neg = numeric_negative_signs[[t]]
          matches = grepl(paste0("^(?=.*[[:digit:]].*)\\s*[",neg$start,"\\+\\p{Sc}\\s]*([[:digit:]]{0,3}(",sep$thousand,"[[:digit:]]{3})*(",sep$decimal,"[[:digit:]]+){0,1})\\s*(?=.*",neg$end,".*)[^[:digit:]]*\\s*$"),unlist(table[,i]),perl=T)
          votes[idx] = sum(matches)
          formats[[idx]] = list(numeric_separator=sep)
          idx = idx+1
        }
      }
      candidate_formats = formats[votes>0]
      candidate_formats = candidate_formats[order(votes[votes>0],decreasing=T)]
      data_types[[i]]$formats = candidate_formats
    }
    else if(data_types[[i]]$type==types$logical){
      idx = 1
      votes = c()
      formats = list()
      for(s in seq_along(logical_true_qualifier)){
        for(t in seq_along(logical_false_qualifier)){
          matches = grepl(paste0("^\\s*(",logical_true_qualifier[s],"|",logical_false_qualifier[t],")\\s*$"),unlist(table[,i]),perl=T)
          votes[idx] = sum(matches)
          formats[[idx]] = list(true_qualifier=logical_true_qualifier[s],false_qualifier=logical_false_qualifier[t])
          idx = idx+1
        }
      }
      candidate_formats = formats[votes>0]
      candidate_formats = candidate_formats[order(votes[votes>0],decreasing=T)]
      data_types[[i]]$formats = candidate_formats
    }
    else if(data_types[[i]]$type==types$date){
      idx = 1
      votes = c()
      formats = list()
      for(s in seq_along(date_separators)){
        for(t in seq_along(date_orders)){
          date_pattern = paste(date_orders[[t]],collapse=date_separators[s])
          date_regex = gsub("%d","([0-9]|[0-3][0-9])",gsub("%m","([0-9]|[0][0-9]|[1][0-2])",gsub("%b","[[:alpha:]]{3}",gsub("%Y","[[:digit:]]{1,4}",gsub("\\.","\\\\.",date_pattern)))))
          matches = grepl(paste0("^\\s*(",date_regex,")\\s*$"),unlist(table[,i]),perl=T)
          votes[idx] = sum(matches)
          formats[[idx]] = list(date_pattern=date_pattern)
          idx = idx+1
        }
      }
      candidate_formats = formats[votes>0]
      candidate_formats = candidate_formats[order(votes[votes>0],decreasing=T)]
      data_types[[i]]$formats = candidate_formats
    }
    else if(data_types[[i]]$type==types$time){
      idx = 1
      votes = c()
      formats = list()
      for(s in seq_along(time_patterns)){
        time_pattern = paste0("^\\s*(",time_patterns[[s]],")\\s*$")
        time_regex = gsub("%H","([0-9]|[0-1][0-9]|[2][0-4])",gsub("%M","([0-5][0-9])",gsub("%S","([0-5][0-9])",time_pattern)))
        matches = grepl(time_regex,unlist(table[,i]),perl=T)
        votes[idx] = sum(matches)
        formats[[idx]] = list(time_pattern=time_patterns[s])
        idx = idx+1
      }
      candidate_formats = formats[votes>0]
      candidate_formats = candidate_formats[order(votes[votes>0],decreasing=T)]
      data_types[[i]]$formats = candidate_formats
    }
  }
  
  
  #determine NA values candidates in all numeric, logical, date, time fields
  votes = numeric(length(na_value_qualifier))
  for(i in seq_along(data_types)){
    if(data_types[[i]]$type==types$numeric ||
     data_types[[i]]$type==types$logical ||
     data_types[[i]]$type==types$date ||
     data_types[[i]]$type==types$time){
        for(s in seq_along(na_value_qualifier)){
          empty = which(table_types[,i,drop=F]==types$empty | table_types[,i,drop=F]==types$punctuation)
          matches = grepl(paste0("^\\s*",na_value_qualifier[s],"\\s*$"),unlist(table[empty,i,drop=F]),perl=T)
          votes[s] = votes[s] + sum(matches)
        }
     }
  }

  candidate_na_values = na_value_qualifier[votes>0]
  candidate_na_values = candidate_na_values[order(votes[votes>0],decreasing=T)]
  
  hypotheses = create_hypothesis_list("data_type")
  hypotheses = add_hypothesis(hypotheses, confidence=1, data_types=data_types, na_values=candidate_na_values)
  
  return(hypotheses)
}

get_desc = function(hypothesis){
  data_types = sapply(hypothesis$data_types,function(x) x$data_type)
  return(paste("character:",sum(data_types==types$character),"numeric:",sum(data_types==types$numeric),"numeric_unit:",sum(data_types==types$numeric_unit),"date:",sum(types==data_types$date),"logical:",sum(types==data_types$logical)))
}

parse = function(table, hypothesis, errorHandler, configuration){
  result = list()
  result$units = data.frame(matrix(nrow=nrow(table),ncol=ncol(table)))
  result$edits = 0
  
  if(length(hypothesis$data_types)!=ncol(table)){
    result$error = "Number of column data types is not equal not number of columns. Will assume type 'character' for missing values."
    return(list(result))
  }
  
  #trim whitespace, trim quotes, trim whitespace again from header
  colnames(table) = sapply(colnames(table),trimws)
  colnames(table) = gsub("(^[\"'])|([\"']$)","",colnames(table))
  colnames(table) = sapply(colnames(table),trimws)
  
  #table=result8$intermediate
  #parse column data types
  for(i in seq_along(table)){
    type = hypothesis$data_types[[i]]$type
    formats = hypothesis$data_types[[i]]$formats
    
    #trim whitespace, trim quotes, trim whitespace again
    text = sapply(table[,i,drop=F],trimws)
    text = gsub("(^[\"'])|([\"']$)","",text)
    text = sapply(text,trimws)
    valid = logical(length(text))
    
    #replace all NA values
    na_values = hypothesis$na_values
    for(na_value in na_values){
      na_match = grepl(paste0("^",na_value,"$"),text,perl=T)
      table[na_match,i] = NA
      text[na_match] = NA
      valid = valid | na_match
    }
    
    f = 1
    if(type==types$numeric){
       table[,i] = rep(NA_real_,nrow(table))
       while(!all(valid) && f<=length(formats)){
        relevant = !valid
        sep=formats[[f]]$numeric_separator

        for(j in which(relevant)){
          negative = F
          numeric_text = text[j]
          first_numeric = regexpr("[[:digit:]]",numeric_text)
          first_thousand = regexpr(sep$thousand,numeric_text)
          first_decimal = regexpr(sep$decimal,numeric_text)
          
          if(is.na(numeric_text)){
            valid[j] = T
            next
          }
          
          if(regexpr("\\(",numeric_text) != -1 && regexpr("\\(",numeric_text)<first_numeric && regexpr("\\)",numeric_text)>first_numeric){
            negative = T
            numeric_text = sub("\\(","",numeric_text)
            numeric_text = sub("\\)","",numeric_text)
          }else if(regexpr("-",numeric_text) != -1 && regexpr("-",numeric_text)<first_numeric){
            negative = T
            numeric_text = sub("-","",numeric_text)
          }
          if(first_decimal==-1 || first_thousand<=first_decimal){
            numeric_text = gsub(sep$thousand,"",numeric_text)
            numeric_text = gsub(sep$decimal,".",numeric_text)
            numeric_part = regexpr("[[:digit:]\\.\\,]+",numeric_text)
            numeric = regmatches(numeric_text, numeric_part)
            numeric = suppressWarnings(as.numeric(numeric))
            if(length(numeric)==0){
               numeric = NA
            }
            if(negative){
              numeric = -numeric
            }
            
            table[j,i] = numeric
            if(!is.na(numeric)){
              unit = regmatches(numeric_text, numeric_part, invert=T)
              unit = paste0(unlist(unit),collapse="")
              if(unit == "") unit = NA
              result$units[j,i] = unit
              
              valid[j] = T
            }else{
              valid[j] = F
            }
          }else{
            table[j,i] = NA
            valid[j] = T
          }
        }
        # #identify numerics
        # matches = gregexpr(paste0("([[:digit:]]{1,3}(",sep$thousand,"[[:digit:]]{3})*(",sep$decimal,"[[:digit:]]+){0,1})|(",sep$decimal,"[[:digit:]]+)"),text[relevant],perl=T)
        # #identify all exact matches with no other leading/trailing negative signs
        # matched = lapply(matches,length)==1
        # matches_with_other_negative = logical(sum(matched))
        # for(neg_alternative in numeric_negative_signs){
        #   if(neg$start != neg_alternative$start && neg$end != neg_alternative$end){
        #     matches_with_other_negative = matches_with_other_negative | grepl(paste0("^(?=.*[[:digit:]].*)\\s*[",neg_alternative$start,"\\+\\p{Sc}\\s]*([[:digit:]]{0,3}(",sep$thousand,"[[:digit:]]{3})*(",sep$decimal,"[[:digit:]]+){0,1})\\s*(?=.*",neg_alternative$end,".*)[^[:digit:]]*\\s*$"),text[relevant][matched],perl=T)
        #   }
        # }
        # raw_units = regmatches(text[relevant][matched], matches[matched], invert = TRUE)
        # matches_contains_leading_digits_or_separator = grepl(".*[[:digit:]]|\\.|\\,.*",lapply(raw_units,function(x){x[1]}))
        # 
        # matched[matched] = !matches_with_other_negative & !matches_contains_leading_digits_or_separator
        # if(!any(matched)){
        #   #check next format
        #   f = f + 1
        #   next
        # }

        # #extract numerics
        # extracted_numerics = regmatches(text[relevant][matched], matches[matched], invert = FALSE)
        # #identify negative numbers
        # matches_with_negative = grepl(paste0("^(?=.*[[:digit:]].*)\\s*[",neg$start,"\\+\\p{Sc}\\s]*([[:digit:]]{0,3}(",sep$thousand,"[[:digit:]]{3})*(",sep$decimal,"[[:digit:]]+){0,1})\\s*(?=.*",neg$end,".*)[^[:digit:]]*\\s*$"),text[relevant][matched],perl=T)
        # matches_without_negative = grepl(paste0("^(?=.*[[:digit:]].*)\\s*[\\+\\p{Sc}\\s]*([[:digit:]]{0,3}(",sep$thousand,"[[:digit:]]{3})*(",sep$decimal,"[[:digit:]]+){0,1})\\s*[^[:digit:]]*\\s*$"),text[relevant][matched],perl=T)
        # negative = matches_with_negative & !matches_without_negative
        # extracted_numerics[negative] = paste0("-",extracted_numerics[negative])
        # #units, remove negative signs #TODO:also replace leading/trailing quotes?!
        # raw_units = regmatches(text[relevant][matched], matches[matched], invert = TRUE)
        # result$units[relevant,i][matched] = unlist(lapply(raw_units,function(x){paste0(x,collapse="")}))
        # result$units[relevant,i][matched][negative] = sub(neg$start,"",sub(neg$end,"",result$units[relevant,i][matched][negative]))
        # result$units[relevant,i][matched][result$units[[i]][relevant][matched]==""] = NA
        # #parse
        # table[relevant,i][matched,] = suppressWarnings(as.numeric(extracted_numerics))
        # #check for nulls
        # valid[relevant] = valid[relevant] | (matched & !is.na(table[relevant,i]))
        # #save format
        
        #check next format
        f = f + 1
       }
       if(all(valid) && all(table[,i,drop=F]%%1==0,na.rm=T) && all(table[,i,drop=F] <= .Machine$integer.max,na.rm=T)){
         zero_start = grepl("^0.*[0-9]", text)
         # sapply(text,function(x){substr(x,1,1) == "0" & sum(grep("[1-9]",x))>0})
         if(any(zero_start)){
           valid[zero_start] = FALSE
         }else{
           table[,i] = as.integer(unlist(table[,i,drop=F]))
         }
       }
    }else if(type==types$logical){
      table[,i] = rep(NA,nrow(table))
      while(!all(valid) && f<=length(formats)){
        relevant = !valid
        true_qualifier=formats[[f]]$logical_true_qualifier
        false_qualifier=formats[[f]]$logical_false_qualifier
        #identify logicals
        matches = gregexpr(paste0("(",true_qualifier,"|",false_qualifier,")"),text[relevant],perl=T)
        #identify all exact matches
        matched = lapply(matches,length)==1
        if(!any(matched)){
          #increase counter
          f = f + 1
          next
        }
        #parse
        table[relevant,i,drop=F][matched,] = ifelse(text[relevant][matched]==true_qualifier,TRUE,FALSE)
        #check for nulls
        valid[relevant] = valid[relevant] | matched
        
        #check next format
        f = f + 1
      }
    }else if(type==types$date){
      table[,i] = as.Date(rep(NA,nrow(table)))
      while(!all(valid) && f<=length(formats)){
        relevant = !valid
        date_pattern=formats[[f]]$date_pattern
        
        #parse date 
        for(j in which(relevant)){
          parsed = tryCatch({
            table[j,i] = as.Date(text[j], date_pattern)
            if(as.numeric(format(table[j,i,drop=F],'%Y'))<100){
              table[j,i] = as.Date(text[j], sub("Y","y",date_pattern))
            }
          },error=function(e){
            table[j,i] = NA
          })
        }
        
        #check for nulls
        valid[relevant] = valid[relevant] | !is.na(table[relevant,i,drop=F])
        
        #check next format
        f = f + 1
      }
    }else if(type==types$time){
      table[,i] = as.Date(rep(NA,nrow(table)))
      while(!all(valid) && f<=length(formats)){
        relevant = !valid
        time_pattern=formats[[f]]$time_pattern
        
        parsed = tryCatch({
            strptime(text[relevant], time_pattern)
          },error=function(e){
            NA
          }
        )
        table[relevant,i] = parsed
        
        #check for nulls
        valid[relevant] = valid[relevant] | !is.na(table[,i,drop=F][relevant])
        
        #check next format
        f = f + 1
      }
    }else{
      table[,i] = rep(NA_character_,nrow(table))
      relevant = !valid
      table[relevant,i] = text[relevant]
      valid[relevant] = T
    }
    
    if(configuration$conservative_type_casting && any(!valid)){
      table[,i] = text
    }else{
      result$edits = result$edits + sum(!valid)
    }
    
    if(class(unlist(table[,i,drop=F])) == "character"){
      table[is.na(unlist(table[,i,drop=F])),i]=""
    }
    
    unique_units =  unique(result$units[,i])
    unique_units = unique_units[!is.na(unique_units) && unique_units!=""]
    if(length(unique_units)==1){
      colnames(table)[i] = paste0(colnames(table)[i],"(",unique_units,")")
    }else if(length(unique_units)>1 && !configuration$separate_multiple_units){
      table[,i] = text
      #result$units[,i] = character(length(nrow(table)))
    }

  }
  
  result$intermediate = table
  
  return(list(result))
}

register_parsing_step(level="data_type",detect,parse,get_desc)
