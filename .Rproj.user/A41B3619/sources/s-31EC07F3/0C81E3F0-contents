library('xlsx')
library('magrittr')
library('ggplot2')
library(plotly)
library(flexdashboard)
library(purrr)
library(dplyr)

plot(1:79, betta[,42])

dataset_path = "Хорошие/"

predata = list()
data = list("amp" = list(), "time" = c(), "direct" = c(), "dist" = c())

for (filename in list.files(dataset_path, pattern = "res",all.files = TRUE)){
  
  name = ((strsplit(filename, "_") %>% unlist())[5] %>% strsplit(".res"))[1] %>% unlist() 
  predata[[name]] = read.table(paste0(dataset_path,filename), sep = "\n", stringsAsFactors = FALSE)
  
  amount_of_records = length(grep("амплитуды",predata[[name]]$V1))
  
  amount_of_already_observed_records = length(data[['amp']])
  
  for (record in 1:amount_of_records){
    
    record_id = amount_of_already_observed_records + record
    
    data[['amp']][[record_id]] =
      (strsplit(predata[[name]]$V1[grep("амплитуды",predata[[name]]$V1)[record]]," ") %>%
         unlist())[-c(1,2)] %>% as.integer()
    
    hhmmss = (strsplit(predata[[name]]$V1[grep("время",predata[[name]]$V1)[record]]," ") %>%
                unlist())[-c(1,3,5,7:14)] %>% as.double() #hours, minutes, seconds
    
    data[['time']][record_id] = hhmmss[1]*3600+hhmmss[2]*60+hhmmss[3]
    
    data[['direct']][record_id] = 
      strsplit((strsplit(predata[[name]]$V1[grep("направление",predata[[name]]$V1)[record]]," ") %>% 
                  unlist())[-c(1:8,10:14)],";") %>% as.integer()
    
    data[['dist']][record_id] = (strsplit(predata[[name]]$V1[grep("начало импульса",predata[[name]]$V1)[record]]," ") %>%
                                   unlist())[-c(1:3)] %>% as.double()
    
    data[['width']][record_id] = (strsplit(predata[[name]]$V1[grep("ширина импульса",predata[[name]]$V1)[record]]," ") %>%
                                    unlist())[-c(1,2,4:6)] %>% strsplit(";") %>% as.integer()
    
    data[['length']][record_id] = (strsplit(predata[[name]]$V1[grep("длина",predata[[name]]$V1)[record]]," ") %>%
                                    unlist())[-c(1:8)]  %>% as.integer()
    
    data[['positive_frequency']][record_id] = (strsplit(predata[[name]]$V1[grep("частота допплера+",predata[[name]]$V1, fixed = TRUE)[record]]," ") %>%
                                     unlist())[-c(1:2)]  %>% as.double()
    
    data[['negative_frequency']][record_id] = (strsplit(predata[[name]]$V1[grep("частота допплера-",predata[[name]]$V1, fixed = TRUE)[record]]," ") %>%
                                                 unlist())[-c(1:2)]  %>% as.double()
    
    data[['file']][record_id] = name
    
    
  }
  
  
}


n = length(data[[1]])

amps = data[[1]] %>% as.data.frame(col.names = c(1:n)) 
data1 = data[-1] %>% as.data.frame() %>% mutate('index' = 1:length(file)) %>% group_by(file) %>% 
  mutate("index_in_predata" = dplyr::row_number(file))

bad_indexes = list()


bad_indexes[["check_dist"]] = which(data1$dist > 400 | data1$dist < 100)

width_check <- function(data, param1=40, param2=60){
  
  return(which(data < param1 | data > param2))
  
}

bad_indexes[["check_width"]] = width_check(data1$width, 40, 63)# Сделать значения параметрами

#bad_indexes[["check_length"]] = which(data1$length > 70 | data1$length < 20) можно убрать

for(i in 1:length(amps)){
  
if (length(which(is.na(amps[[i]]))) > 30){
  
  bad_indexes[["check_NA"]] = c(bad_indexes[["check_NA"]],i)
}
  
}

#Добавить сравнение последнего и предпоследнего ФАЙЛА: сравниваем последний элемент предпоследнего с первым последнего. 
#ПРоверка same meteors. Если не совпадает - последний выкидываем. Если совпадает - смотрим, сколько элементов из последнего добавить к предыдущему. 


#Расчет same meteors вставить перед всеми проверками

# while(record <= amount_of_records){
#   
#   #в сохранение плохих файлов добавить остальные параметры
#   
#   check_dist = (data[['dist']][record] > 400 || data[['dist']][record] < 100)
#   
#   if(check_dist){
#     
#     write.table(paste("Амплитуда:", data[['amp']][[record]]), "bad_andron.txt", append = TRUE)
#     write.table(paste("Время:", data[['time']][record]), "bad_andron.txt", apppend = TRUE)
#     write.table(paste("Направление:", data[['direct']][record]), "bad_andron.txt", append = TRUE)
#     write.table(paste("Расстояние:", data[['dist']][record]), "bad_andron.txt", append = TRUE)
#     
#     data[['amp']] = data[['amp']][-record]
#     data[['time']] = data[['time']][-record]
#     data[['direct']] = data[['direct']][-record]
#     data[['dist']] = data[['dist']][-record]
#     
#     amount_of_records = length(data[['amp']])
#     
#     
#   }else{
#     
#     record = record + 1
#     
#   }
#   
#   
# }




#добавить length_check, width_check ГОТОВО
#Проверка на пустые значения в амплитуде, такие просто убираем ГОТОВО
#изменить - удалять плохие измерения только в конце. до этого сохранять bad_ind, постоянно пополнять его 
#и вести дальнейшие расчеты за вычетом этих бэд индексов, например, i in с(1,5,7,9) ??



check_direct = function(record, time_threshold = 3600){
  
  plus_hour_index <- which(data1$time >= data1$time[record] + time_threshold)[1]
  
  if(!is.na(plus_hour_index)){
    
    indexes <- record:plus_hour_index
    check <- data1$direct[indexes] %>% unique() %>% length() == 1
    if(check == TRUE)return(indexes)else NA
    
  }
  else{
    return(NA)
  }
  
  
}


bad_indexes[["check_direct"]] = 1:n %>% sapply(check_direct)


# amount_of_records = length(data[['amp']])
# record = 1
# 
# while(record <= amount_of_records){
#   
#   one_hour_from_current_time_index = which(data[['time']] >= data[['time']][record] + 3600)[1]
#   
#   if(is.na(one_hour_from_current_time_index)){
#     
#     check_direct = FALSE
#     
#   }else{
#     
#     check_direct = (unique(data[['direct']][c(record:one_hour_from_current_time_index)]) %>% length()) == 1
#     
#   }
#   
#   
#   if(check_direct){
#     
#     bad_indexes = c(record:one_hour_from_current_time_index)
#     
#     write.table(paste("Амплитуда:", data[['amp']][bad_indexes]), "bad_andron.txt", append = TRUE)
#     write.table(paste("Время:", data[['time']][bad_indexes]), "bad_andron.txt", append = TRUE)
#     write.table(paste("Направление:", data[['direct']][bad_indexes]), "bad_andron.txt", append = TRUE)
#     write.table(paste("Расстояние:", data[['dist']][bad_indexes]), "bad_andron.txt", append = TRUE)
#     
#     data[['amp']] = data[['amp']][-bad_indexes]
#     data[['time']] = data[['time']][-bad_indexes]
#     data[['direct']] = data[['direct']][-bad_indexes]
#     data[['dist']] = data[['dist']][-bad_indexes]
#     
#     amount_of_records = length(data[['amp']])
#     
#   }else{
#     
#     record = record + 1
#     
#   }
#   
# }

##############



extract_observation <- function(index){
  
  observation <- data.frame(
  'dist' = data1$dist[index],
  'width' = data1$width[index],
  'length' = data1$length[index],
  'direct' = data1$direct[index],
  'time' = data1$time[index],
  'index' = data1$index[index]
  )
  
  return(observation)
}

same_meteor_check <- function(record, time_step = 0.5, dist_step = 2){
  
  current_ind <- record
  next_ind <- record + 1
  
  check <- data1$time[next_ind] - data1$time[current_ind] <= time_step && 
    data1$direct[next_ind] == data1$direct[current_ind] &&
    data1$dist[next_ind] - data1$dist[current_ind] <= dist_step
  
  if(check)return(c(current_ind, next_ind))
  
}

same_meteors_indexes <- 1:(n-1) %>% sapply(same_meteor_check) %>% unlist() %>% unique()


distinguish_meteors <- function(record, time_step = 0.5, dist_step = 2){

  current_ind <- same_meteors_indexes[record]
  next_ind <- same_meteors_indexes[record + 1]

  
  check <- data1$time[next_ind] - data1$time[current_ind] <= time_step && 
           data1$direct[next_ind] == data1$direct[current_ind] &&
           data1$dist[next_ind] - data1$dist[current_ind] <= dist_step

  if(!check)return(current_ind) 

}

breaks <- 1:(length(same_meteors_indexes) - 1) %>% map(distinguish_meteors) %>% unlist()

array_break <- function(array, splits){
  
  start_position = 1
  split_index = 1
  
  res = list()
  list_ind = 1
  
  for(i in 1:length(array)){
    
    if(array[i] == splits[split_index] && split_index <= length(splits)){
      
      res[[list_ind]] = array[start_position]:array[i]
      start_position = i + 1
      split_index = split_index + 1
      list_ind = list_ind + 1
      
    }
    
    if(split_index > length(splits)){
      
      res[[list_ind]] = array[start_position:length(array)]

    } 
    
    
  }
  
  return(res)
  
}



same_meteors <- array_break(same_meteors_indexes, breaks)


# same_meteors_indexes = c()
# amount_of_records = length(data[['amp']])
# 
# for(record in 1:amount_of_records){
#   
#   indexes =  which(
#     (data[['time']] >= data[['time']][record]) &
#       (data[['time']] <= data[['time']][record] + 2) &
#       (data[['dist']] <= data[['dist']][record] + 25) &
#       (data[['direct']] == data[['direct']][record])
#   )
#   
#   if(length(indexes) > 1){
#     
#     same_meteors_indexes = c(same_meteors_indexes, indexes)
#     
#   }
#   
# }
# 
# same_meteors_indexes = same_meteors_indexes %>% unique()
# 
# same_meteors = list()
# meteor_ind = 1
# same_meteors[[meteor_ind]] = same_meteors_indexes[1]
# 
# for(record in 2:length(same_meteors_indexes)){
#   
#   if(data[['time']][same_meteors_indexes[record]] < data[['time']][same_meteors_indexes[record] - 1] + 2 & 
#      data[['direct']][same_meteors_indexes[record]] == data[['direct']][same_meteors_indexes[record] - 1]){
#     
#     same_meteors[[meteor_ind]] = c(same_meteors[[meteor_ind]], same_meteors_indexes[record])
#     
#   }else{
#     
#     meteor_ind = meteor_ind + 1
#     same_meteors[[meteor_ind]] = same_meteors_indexes[record]
#     
#   }
#   
# }

#Находим регрессионную производную 

# r - количество соседних точек
# p - степень

delta=function(x, y, r, p){
  
  if(abs(y - x) <= r)return((1 - abs(x - y)/r)^p) else return(0)
}


#односторон
# delta=function(x, y, r, p){
#   
#   if(y - x <= r & y - x >= 0){
#     
#     d=(1 - abs(x - y)/r)^p
#     
#   }else{
#     
#     d=0
#     
#   } 
#   
#   return(d)
# }

dif=function(r,p, L, N){
  
  t=c(1:N)
  A0 = matrix(nrow = N,ncol = N)
  
  
  for (i in t){
    
    for (j in t){
      
      A0[i,j]=delta(i, j, r, p)
      
    }
    
  }
  
  K = diag(
    1/sapply(t, function(x)sum(A0[x,]))
  )
  
  mat.ojid=function(y,st){
    
    res = sapply(t, function(x, z)sum(z[x, ]*y^st), z = A)
    
    return(res)
    
  }
  
  A = K%*%A0
  
  M = diag(mat.ojid(t,1))
  
  Di = mat.ojid(t,2) - (mat.ojid(t,1))^2
  
  D=diag(Di)
  
  
  T=diag(t)
  betta=list()
  alpha =list()
  
  betta = sapply(c(1:L), function(x)(A - solve(D)%*%(M-T)%*%(A%*%T-M%*%A))%*%data[['amp']][[x]])  #регрессионное значение ряда
  
  alpha = sapply(c(1:L), function(x)solve(D)%*%(A%*%T-M%*%A)%*%data[['amp']][[x]]) # регрессионная производная
  
  
  return(list("alpha"=alpha,"betta"=betta))
}


R17 = dif(17,2,ncol(amps), nrow(amps))
betta=R17$betta
alpha=R17$alpha

# Максимум амплитуды находится после 20 элемента сделать параметр

betta_max_check = sapply(c(1:ncol(betta)), function(x)if(which.max(betta[,x]) >= 20)x) %>% unlist()
alpha_positive_check = sapply(c(1:ncol(alpha)), function(x)if(which(alpha[,x] > 0) %>% length() == 
                                                              nrow(alpha))x) %>% unlist()

#checks = c(betta_max_check, alpha_positive_check) %>% unique()

bad_indexes[["betta_max_check"]] = betta_max_check
  
bad_indexes[["alpha_positive_check"]] = alpha_positive_check

local_max_check = function(gamma=1,dif = 10){
  
  check_index = list()
  local_max = list()
  end_index = c()
  bad_index = c()
  
  # создаем последовательности индексвов, для которых alpha > gamma, для каждого измерения
  for(q in 1:ncol(alpha)){
    
    check_index = which(alpha[,q] >= gamma)
    
    if(length(check_index) == 0){
      local_max[[q]] = NA
    }else if(length(check_index) == 1){
      
      local_max[[q]] = check_index
      
    }else if(length(check_index) == 79){
      bad_index = c(bad_index,q)
      
    }else{
      break_index = c()
      for (i in 1:(length(check_index)-1)){
        
        if((check_index[i]+1) != check_index[i+1]){
          
          break_index = c(break_index,check_index[i])
          
          
        }
      }
      
      if(is.null(break_index)){
        local_max[[q]] = check_index[length(check_index)]
      }else{
        
        local_max[[q]] <- array_break(check_index, break_index)
      }
    }
  }
  
  # Оставляем последние индексы из последовательности
  max_in_end_index = c()
  for(i in 1:length(local_max)){
    
    if(length(local_max[[i]]) > 1){
      for(j in 1:length(local_max[[i]])){
        local_max[[i]][[j]] = local_max[[i]][[j]][length(local_max[[i]][[j]])]
        
      }
      
    }else{
      
      if(local_max[[i]] == 79 | is.na(local_max[[i]])){
        max_in_end_index = c(max_in_end_index,i)
      }
      
    }
  }
  
  
  
  t_max = list()
  
  for (i in 1:length(local_max)){
    
    if(length(local_max[[i]]) == 1 && is.na(local_max[[i]])){
      
      t_max[[i]] = NA
      
    }else if(length(local_max[[i]]) == 1 && local_max[[i]] == 79){
      
      t_max[[i]] = 79
      
    }else{
      a=c()
      
      for(j in 1:length(local_max[[i]])){
        
        tau_prev = c()
        Q = unlist(local_max[[i]][j])+1
        
        # if(is.na(Q)){
        # 
        #   t_max[[i]][j] = NA
        #   
        # }else
        if(Q-1 == 79){
          
          t_max[[i]][j] = 79
          
        }else if(alpha[Q,i] <= (- gamma)){
          
          a[j] = which(abs(alpha[,i]) == min(abs(alpha[Q,i]),abs(alpha[Q-1,i])))
          
        }else{
          
          while(Q != 80 && alpha[Q,i] >= (- gamma)){
            
            tau_prev = c(tau_prev,Q)
            
            Q = Q + 1
            
          }
          check = c()
          SUM_plus = c()
          SUM_minus= c()
          
          for (e in 1:length(tau_prev)){
            ind_plus = c()
            ind_minus = c()
            
            ind_plus = tau_prev[1:e][which(alpha[tau_prev[1:e],i]>=0)]
            ind_minus = tau_prev[e:length(tau_prev)][which(alpha[tau_prev[e:length(tau_prev)],i]<=0)]
            
            SUM_plus = sum(alpha[ind_plus,i])/sum(abs(alpha[tau_prev[1:e],i]))
            SUM_minus = sum(-alpha[ind_minus,i])/sum(abs(alpha[tau_prev[e:length(tau_prev)],i]))
            
            check = c(check, min(SUM_plus, SUM_minus))
            
          }
          
          
          
          a[j] = tau_prev[which.max(check)]
          
          
        }
        t_max[[i]] = a
        
        #tau[[i]][[j]] = tau_prev
        
      }
    }
    
  }
  
  
  for (i in 1: length(t_max)){
    
    for (j in 1:length(t_max[[i]])){
      
      if(!is.na(t_max[[i]][[j]])){
        if(which.max(betta[,i]) != t_max[[i]][j]){
          
          if(abs(max(betta[,i])-betta[[t_max[[i]][j]]])<=10){
            
            bad_index = c(bad_index,i)
          }
          
        }
        
        
        
      }
      
      
    }
  }
  bad_index = unique(bad_index)
  
  return(bad_index)
  
}

bad_indexes[["local_max_check"]] = local_max_check()

#Добавить проверку - модуль альфа меньше  параметра альфа. Если весь стобец такой - измерение хорошее.  

# Внесла изменения!

alpha_param_check <- function(alpha, threshold){

  comparison <- 1:ncol(alpha) %>% sapply(function(x) length(which(alpha[,x] > threshold)))
  
  indexes <- which(comparison == nrow(alpha) - 40)
  
  return(indexes)
  
}

bad_indexes[["alpha_param_check"]] = alpha_param_check(alpha, 0) 

indexes <- bad_indexes %>% unlist() %>% unname() 
indexes <- indexes[which(!is.na(indexes))] %>% unique()

bad_same_meteors = list()

for(i in 1:length(same_meteors)){
  
  if(intersect(indexes, same_meteors[[i]]) %>% length() > 0){
    
    bad_same_meteors[[i]] = intersect(indexes, same_meteors[[i]])
    
  }
  
}


#Потом добавить запись в файл. 
#checks = checks[-which(checks %in% unlist(indexes))]
same_meteors = lapply(c(1:length(same_meteors)), 
                      function(x)same_meteors[[x]][-which(same_meteors[[x]] %in% bad_same_meteors[[x]])])

#data = lapply(c(1:length(data)), function(x)data[[x]][-unlist(bad_same_meteors)])
#names(data) = c('amp','time','direct','dist',"width",'length','positive_frequency','negative_frequency','file')



#перед этой функцией из сэйм метоерс выкинуть плохие
meteors_choice = function(meteors){
  
  if(meteors %>% length() > 1){
    
    disp = sapply(meteors, function(x)data[['amp']][x] %>% unlist()) - betta[,meteors]  
    disp = sapply(c(1:ncol(disp)), function(x) sd(disp[,x]))
    min_disp_ind = which.min(disp)
    not_min_disp = meteors[-min_disp]
    
  }else{
    #заменить потом NA чтобы не ломался data
    not_min_disp = NA
      #meteors[1]
    
  }
  
  return(not_min_disp)
  
}

#indexes = sapply(same_meteors, meteors_choice)

bad_indexes[["min_disp_meteors_choice"]] = sapply(same_meteors, meteors_choice) %>% unlist() %>%
  as.data.frame() %>% drop_na() %>% c() %>% unname() %>% unlist()

betta = betta[,-unlist(bad_same_meteors)]
alpha = alpha[,-unlist(bad_same_meteors)]

#same_meteors = same_meteors[[i]][indexes[[i]]]

#data = lapply(c(1:length(data)), function(x)data[[x]][-bad_indexes[["min_disp_meteors_choice"]]])

saving_bad_results <- function(indexes){
  
  for(ind in indexes){
  
    
    start_index_in_predata <- 
      data1 %>% 
      ungroup() %>% 
      filter(index == ind) %>% 
      select(index_in_predata) %>% 
      unlist() %>% 
      unname()
    
    indexes_in_predata <- ((start_index_in_predata - 1)*11 + 1):(start_index_in_predata*11)
    
    file_name <- 
      data1 %>% 
      ungroup() %>% 
      filter(index == ind) %>% 
      select(file) %>% 
      unlist() %>% 
      unname() %>%
      as.character()
    
    data_to_write <- predata[[file_name]][[1]][indexes_in_predata]
    
    write.table(data_to_write, file = paste0('bad_observations_',substr(name,1,6),'.txt'), append = TRUE, row.names = FALSE)
  
  }
  
}

bad_indexes_for_saving <- bad_indexes %>% unlist() %>% unname() %>% unique()
bad_indexes_for_saving <- bad_indexes_for_saving[which(!is.na(bad_indexes_for_saving))]

saving_bad_results(bad_indexes_for_saving)

good_data_to_save <- 
  data1[-bad_indexes_for_saving,] %>% 
  ungroup() %>%
  select(-c(index, index_in_predata)) %>%
  mutate(file = substr(file,1,8))

write.table(good_data_to_save, file = paste0('good_observations_',substr(name,1,6),'.txt'), append = TRUE, row.names = FALSE)

#Проверка локальных максимумов

# local_max_check = function(gamma=1){
# 
# check_index = list()
# local_max = list()
# end_index = c()
# bad_index = c()
# 
# # создаем последовательности индексвов, для которых alpha > gamma, для каждого измерения
# for(q in 1:ncol(alpha)){
#   
# check_index = which(alpha[,q] >= gamma)
# 
# if(length(check_index) == 0){
#   local_max[[q]] = NA
# }else if(length(check_index) == 1){
#   
#   local_max[[q]] = check_index
#   
# }else if(length(check_index) == 79){
#   bad_index = c(bad_index,q)
#   
# }else{
#   break_index = c()
#   for (i in 1:(length(check_index)-1)){
#     
#     if((check_index[i]+1) != check_index[i+1]){
#       
#       break_index = c(break_index,check_index[i])
#       
#       
#     }
#   }
#   
#   if(is.null(break_index)){
#     local_max[[q]] = check_index[length(check_index)]
#   }else{
#     
#     local_max[[q]] <- array_break(check_index, break_index)
#   }
# }
# }
# 
# # Оставляем последние индексы из последовательности
# max_in_end_index = c()
# for(i in 1:length(local_max)){
#   
#   if(length(local_max[[i]]) > 1){
#     for(j in 1:length(local_max[[i]])){
#       local_max[[i]][[j]] = local_max[[i]][[j]][length(local_max[[i]][[j]])]
#       
#     }
#  
#   }else{
#   
#     if(local_max[[i]] == 79 | is.na(local_max[[i]])){
#       max_in_end_index = c(max_in_end_index,i)
#     }
#     
# }
# }
# 
# 
# 
# t_max = list()
# 
# tau = list()
# 
# 
# for (i in 1:length(local_max)){
#   
#   if(length(local_max[[i]]) == 1 && is.na(local_max[[i]])){
#   
#     t_max[[i]] = NA
#     
#   }else if(length(local_max[[i]]) == 1 && local_max[[i]] == 79){
#   
#     t_max[[i]] = 79
#     
#   }else{
#     a=c()
#   
#   for(j in 1:length(local_max[[i]])){
#    
# 
#     tau_prev = c()
#     Q = unlist(local_max[[i]][j])+1
#     
#     # if(is.na(Q)){
#     # 
#     #   t_max[[i]][j] = NA
#     #   
#     # }else
#     if(Q-1 == 79){
# 
#       t_max[[i]][j] = 79
# 
#     }else if(alpha[Q,i] <= (- gamma)){
#     
#       a[j] = which(abs(alpha[,i]) == min(abs(alpha[Q,i]),abs(alpha[Q-1,i])))
#     
#     }else{
#     
#       while(Q != 80 && alpha[Q,i] >= (- gamma)){
#       
#         tau_prev = c(tau_prev,Q)
#         
#       Q = Q + 1
#       
#       }
#       check = c()
#       SUM_plus = c()
#       SUM_minus= c()
#       
#       for (e in 1:length(tau_prev)){
#         ind_plus = c()
#         ind_minus = c()
#         
#         ind_plus = tau_prev[1:e][which(alpha[tau_prev[1:e],i]>=0)]
#         ind_minus = tau_prev[e:length(tau_prev)][which(alpha[tau_prev[e:length(tau_prev)],i]<=0)]
#         
#        SUM_plus = sum(alpha[ind_plus,i])/sum(abs(alpha[tau_prev[1:e],i]))
#        SUM_minus = sum(-alpha[ind_minus,i])/sum(abs(alpha[tau_prev[e:length(tau_prev)],i]))
#        
#        check = c(check, min(SUM_plus, SUM_minus))
#       
#       }
#       
#      
# 
#       a[j] = tau_prev[which.max(check)]
#       print(tau_prev)
#   
#     }
#     t_max[[i]] = a
# 
#     #tau[[i]][[j]] = tau_prev
#     
#     }
#   }
# 
# }
# 
# 
# for (i in 1: length(t_max)){
#   
#   
#     
#   for (j in 1:length(t_max[[i]])){
#     
#     if(!is.na(t_max[[i]][[j]])){
#     if(which.max(betta[,i]) != t_max[[i]][j]){
#       
#      if(abs(max(betta[,i])-betta[[t_max[[i]][j]]])<=20){
#        
#        bad_index = c(bad_index,i)
#      }
#       
#     }
#   
#   
#   
#   }
#     
#   
#   }
# }
# bad_index = unique(bad_index)
# 
# return(bad_index)
# 
# }

# plot(1:79,betta[,17], type = 'l')
# 
# }
# 
# d = list()
# 
# noise = list()
# for(i in 1:ncol(alpha)){
#   
#   d[[i]] = which(abs(alpha[,i]) <= 1)
#   
# 
# 
#  break_ = c()
#    for (j in 1:(length(d[[i]])-1)){
#      
#     if((d[[i]][j]+1) != d[[i]][j+1]){
#       break_ = c(break_,d[[i]][j],d[[i]][j+1])
#       
#           }
#    }
#  
# 
# 
# 
# if (is.null(break_)){
#   
#   noise[[i]] = d[[i]][length(d[[i]])]
#   
# }else if(length(break_) > 1){
#   
#   noise[[i]] = c(break_[length(break_)]:d[[i]][length(d[[i]])])
# }
# }
# 


# Удалить плохие индексы
# Записать в файл хорошие: 



data11 <- read.table(file = choose.files(), stringsAsFactors = FALSE, header = TRUE)

#удалить плохие измерения

n = nrow(data11)# пересчитанная без плохих

# Считаем скорость
angle = seq(from=0, to=315, by=45)

#data11 = ungroup(data1)

data11 = data11 %>% 
  mutate( "freq_plus" = NA, "freq_minus" = NA)

# Считаем частоты для дальнейших расчетов (если одна <5, то ее оставляем NA )
for (i in 1:nrow(data11)){

if (data11$positive_frequency[i] < 5 & data11$negative_frequency[i] < 5){
  
  data11$freq_minus[i]  = NA
  data11$freq_plus[i] = NA
  
 paste("Something wrong: quantity both frequency < 5:",length(data11$positive_frequency < 5 & data11$negative_frequency < 5) )
  
}else if(data11$positive_frequency[i] < 5 & data11$negative_frequency[i] > 5){
   
    data11$freq_minus[i] = - (data11$negative_frequency[i] - 50 )
    data11$freq_plus[i] = NA
}else if(data11$positive_frequency[i] > 5 & data11$negative_frequency[i] < 5){
  
    data11$freq_plus[i] = data11$positive_frequency[i] - 50
    data11$freq_minus[i] = NA
}else{
  
  if(data11$positive_frequency[i] > data11$negative_frequency[i]){
    
    if (data11$positive_frequency[i] > 100){
      data11$freq_plus[i] = data11$positive_frequency[i] - 50
      data11$freq_minus[i] = data11$negative_frequency[i] + 50 }else{
        data11$freq_plus[i] = data11$positive_frequency[i] - 50
        data11$freq_minus[i] = - (data11$negative_frequency[i] - 50)
      }
  }
    
  
  if(data11$positive_frequency[i] < data11$negative_frequency[i]){
    
    if (data11$negative_frequency[i] > 100){
      data11$freq_plus[i] = - (data11$positive_frequency[i] + 50)
      data11$freq_minus[i] = - (data11$negative_frequency[i] - 50 )}else{
        data11$freq_plus[i] = data11$positive_frequency[i] - 50
        data11$freq_minus[i] = - (data11$negative_frequency[i] - 50)
               }
    
  }
  
  
}
  
}
# Удаляем все строки, где и freq_minus, и freq_plus = NA
data11 = data11 %>% filter_at(vars(freq_minus,freq_plus), any_vars(!is.na(.)))
  
# Проверяем, если расчитанные частоты примерно равны (+-5) - изменяемый параметр, тогда расчитываем радиальную скорость (в направлении сканирования)

Calculate = data.frame()

Calculate = data11 %>% mutate('hour'= floor(data11$time/3600),
                              'x'= sin(2*pi*angle[data11$direct]/360),
                              'y'= cos(2*pi*angle[data11$direct]/360),
                              'V_rad'= NA
                              ) 

for(i in 1:nrow(Calculate)){

if(is.na(data11$freq_plus[i])){
  
  Calculate$V_rad[i] = data11$freq_minus[i] * 3.24
  
}else if(is.na(data11$freq_minus[i])){
  
  Calculate$V_rad[i] = data11$freq_plus[i] * 3.24
  
}else{
  
  if(abs(data11$freq_plus[i] - data11$freq_minus[i]) <= 5){
  
  Calculate$V_rad[i] = (data11$freq_plus[i] + data11$freq_minus[i])/2 * 3.24
  
  }else{
    
    Calculate[i,] = NA
    
  }
}

}

# Удалим все пустые строки

Calculate = na.omit(Calculate) 

# Считаем количество метеоров по часам
Quantity_in_hour = Calculate %>% group_by(hour) %>% summarise("Quantity_in_hour" = n())


# Проверяем: если количество метеоров за час < 5, тогда убираем данные из расчета
for(i in 1:nrow(Quantity_in_hour)){
  
  if(Quantity_in_hour$Quantity_in_hour[i] < 5){
    
    Calculate[which(Calculate$hour == Quantity_in_hour$hour[i] ), ] = NA
  }
  
}

#Выбрасиваем все пустые строки

Calculate = na.omit(Calculate) 


# Пересчитываем количество метеоров по часам
Quantity_in_hour = Calculate %>% group_by(hour) %>% summarise("Quantity_in_hour" = n())

Quantity_in_derection_by_hour = Calculate %>% group_by(hour,direct) %>% summarise("Quantity_in_derection" = n())


# Расчитываем скорости U  Y Уточнить: что за группировка по направлениям
 
  Speed = Calculate %>% group_by(hour) %>% 
   group_map((~solve(t(cbind(.x$x,.x$y))%*%cbind(.x$x,.x$y))%*%
               t(cbind(.x$x,.x$y))%*%.x$V_rad)) %>% unlist() %>% round(6)  # U Y 
  
  

  for(i in 1:length(Speed)){
    
  if (Speed[i] > 100)Speed[i] = NA
    
  }

  
  #Погрешность расчетов:
  for (i in 1:length(unique(Calculate$hour))){
  
 V = Calculate %>% group_by(hour) %>% 
    group_map((~.x$V_rad - cbind(.x$x,.x$y) %*% 
                   rbind(Speed[1+2*(i-1)],Speed[2+2*(i-1)])))
 
}
 
 sigma2 = c()
 
 Delta = list()

 
 # Вспомогательное выражение
 help = cbind(Calculate$x,Calculate$y)
 
 for (i in 1:length(V)){
   
 sigma2[i] = (t(V[[i]]) %*% V[[i]])/(length(V[[i]])-2) # U Y
 
 Delta [[i]] = round(sqrt(sigma2[i] * diag(solve(t(help)%*%help))),6)  # U+ Y+
  
 }
 
 q = nrow(Quantity_in_hour)
 WRITE = Quantity_in_hour %>% mutate("U" = Speed[seq(from = 1, to = length(Speed), by = 2)],
                             "Delta U" = unlist(Delta)[seq(from = 1, to = length(unlist(Delta)), by = 2)],
                             "Y" = Speed[seq(from = 2, to = length(Speed), by = 2)],
                             "Delta Y" = unlist(Delta)[seq(from = 2, to = length(unlist(Delta)), by = 2)],
                            "Date" = Calculate$file[1:q])
 
 # Функция, которая вставляет строку между строками дата фрейма
 
 insertRow <- function(existingDF, newrow, r) {
   existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
   existingDF[r,] <- newrow
   existingDF
 }
 
 # Заполнение пропусков                      
 if (nrow(WRITE)!=24){
   
   if (WRITE$hour[1] != 0){
     
     WRITE =  insertRow(WRITE, c(0, rep(NA,length(WRITE)-1)),1)  
   }
   
   # if (WRITE$hour[nrow(WRITE)] != 23){
   #   WRITE[24,] = c(23, rep(NA,length(WRITE)-1))
   # }
   # 
   for (i in 1:23){
     if(!((WRITE$hour[i] + 1) == WRITE$hour[i+1]) ||is.na(((WRITE$hour[i] + 1) == WRITE$hour[i+1]))){
       
       WRITE = insertRow(WRITE, c(WRITE$hour[i] + 1, rep(NA,length(WRITE)-1)),i+1)  
       
     }
     print(WRITE$hour[i+1])
   }
   
   if((nrow(WRITE) > 24) | (WRITE$hour[24] = 23)){

     WRITE = WRITE[1:24,]
   }
 }
 

# Записать в файл номер часа, кол-во метеоров, скорость  U, погрешность, Y, погрешность, 
 file_name = paste0(as.character(WRITE  %>% select(Date) %>% filter(!is.na(.)) %>% head(1)) %>% substr(1,6), ".txt")
 
 write.table(t(as.data.frame(names(WRITE))), file = file_name,col.names = FALSE, append = TRUE, row.names = FALSE, quote = FALSE, sep = "\t" )
 write.table(WRITE, file = file_name,col.names = FALSE, append = TRUE, row.names = FALSE, quote = FALSE, sep = "\t   " )

# Гармонический анализ 
 big_data <- read.table(file = choose.files(), stringsAsFactors = FALSE, header = TRUE)

garm_analiz = function(big_data, garmoniki = c(8,12,24)){ # big_data - значение скоростей за 48 часов.garmoniki - 12 и 24 часа по умолчанию
 
  
 big_data = c('201903.txt') #- Файл с расчетами за месяц
  # в файлы помимо номера часа нужно указывать дату из названия файлов с данными для обработки
  
    #DATA = read.table(file = big_data, header = TRUE,  sep = "\t",stringsAsFactors = FALSE, na.strings = "   NA")
    
    
    
    omega = 2*pi/garmoniki
    
    DATA = read.table(file = big_data, header = TRUE,  sep = "\t",stringsAsFactors = FALSE, na.strings = "   NA")
    names(DATA) = c("hour", "Quantity", "U", "Delta U","Y","Delta Y")
    #  DATA = data.frame("hour" = c(0:23,0:23,0:23),
    #                    "Quantity" = sample(0:100,72, replace = T),
    #                    "U" = sample(0:10,72, replace = T),
    #                    "Delta U" = sample(0:10,72, replace = T),
    #                    "Y" =  sample(0:10,72, replace = T),
    #                    "Delta Y" = sample(0:10,72, replace = T))
     
    if(length(DATA) < 48){
      
      message("Мало данных")
      
      Data = data.frame("Day" = c(1),"hour" = t[24],"Средняя скорость" = NA,
                        "А8" = NA,
                        "А12" = NA,
                        "А24" = NA,
                        "fi1" = NA,
                        "fi2" = NA,
                        "fi3" = NA,row.names = NULL)
      
      write.table( Data, file = 'ГА для U.txt',col.names = TRUE, append = TRUE, row.names = FALSE, quote = FALSE, sep = "\t   ")
      write.table( Data, file = 'ГА для Y.txt',col.names = TRUE, append = TRUE, row.names = FALSE, quote = FALSE, sep = "\t   ") 
      
    }
  

    for (i in 1:(nrow(DATA) - 48 + 1)){
      
     
      t = DATA$hour[c((i):(i+47))]
      
      data_to_calc = DATA[c(i:(i+47)),][-c(1,2,4,6)]
      
      if(nrow(na.omit(data_to_calc)) < 36){
        
        Data = data.frame("Day" = c(1),"hour" = t[24],"Средняя скорость" = NA,
                          "А8" = NA,
                          "А12" = NA,
                          "А24" = NA,
                          "fi1" = NA,
                          "fi2" = NA,
                          "fi3" = NA,row.names = NULL)
        
        write.table( Data, file = 'ГА для U.txt',col.names = TRUE, append = TRUE, row.names = FALSE, quote = FALSE, sep = "\t   ")
        write.table( Data, file = 'ГА для Y.txt',col.names = TRUE, append = TRUE, row.names = FALSE, quote = FALSE, sep = "\t   ") 
        
        
      }else{
        
        A = matrix(nrow = nrow(na.omit(data_to_calc)), ncol = length(garmoniki)*2 + 1)
        # Изменить.Нужно убрать только те строки, где полностью все NA.  Нельзя использовать na.omit, ведь только скорость может быть Na и в этом случае нужно проводить расчет для другой скорости
        N = nrow(na.omit(data_to_calc))
        
        A[,1] = 1
        for (j in 1:N){
            
          for (p in 1:length(garmoniki)){
        
          A[j,p*2] = cos(omega[p]*t[j])
          A[j,p*2+1] = sin(omega[p]*t[j])
          
          }
        }
         
        Y = as.matrix(na.omit(data_to_calc))
        
        Tetta = solve(t(A)%*%A)%*%t(A)%*%Y
        
        Amplituda = matrix(ncol = 2, nrow = length(garmoniki))
        fi= matrix(ncol = 2, nrow = length(garmoniki))
        
        #names(Amplituda) = c("U", "Y")
        
        Speed_avg = round(Tetta[1,],6)
        
        for (f in seq(from=2, to=nrow(Tetta), by=2)){
          
          Amplituda [(f-f/2),] = round(sqrt(Tetta[f,]^2 +Tetta[f+1,]^2),6)
          
          fi[(f-f/2),] = as.vector(round(atan(Tetta[f+1,]/Tetta[f,]),6))
          
        }
        
        
        # Ошибка опредеения параметров
        
        V_GA = Y - A%*%Tetta
        
        sigma2_GA = c()
        
        for(y in 1:2){
          
          sigma2_GA[y] = t(V_GA[,y])%*%V_GA[,y]/(nrow(na.omit(DATA))-2*length(garmoniki)-1)
          
        }
        
        
        #sigma2 <- as.data.frame(as.vector(sigma2))
        #colnames(sigma2) = colnames(Tetta)
        
        #Ковариационная матрица
        
        # S2 = data.frame(row.names = c("A0","A1*cosfi1","A1*sinfi1","A2*cosfi2","A1*sinfi2"))
        
        S2 <- matrix(nrow = length(garmoniki)*2 + 1, ncol = 2)
        
        for (k in 1: length(sigma2_GA)){
          
          S2[,k] = diag(sigma2_GA[k]*solve(t(A)%*%A))
          
        }
        #colnames(S2) = colnames(Tetta)
        
        # Ошибки амплитуд 
        
        delta_Speed_avg = round(sqrt(S2[1,]),6)
        delta_Amplituda = matrix(nrow = length(garmoniki), ncol = 2)
        
        for (m in seq(from=2, to=nrow(Tetta), by=2)){
          delta_Amplituda[m-m/2,] = round(sqrt((Tetta[m,]^2 * S2[m,] + Tetta[m+1,]^2 * S2[m+1,])/(Tetta[m,]^2 + Tetta[m+1,]^2)),6)
          
        }
        
        Data = data.frame("Средняя скорость" = Speed_avg,
                          "А8" = Amplituda[1,],
                          "А12" = Amplituda[2,],
                          "А24" = Amplituda[3,],
                          "fi1" = fi[1,],
                          "fi2" = fi[2,],
                          "fi3" = fi[3,],row.names = NULL) %>% mutate("Day" = c(1),"hour" = t[24])
        print(i)
        message(t[24])
        
        # Data = data.frame("Средняя скорость" = Speed_avg,
        #                   "А8" = Amplituda[1,],
        #                   "Погрешность_А8" = delta_Amplituda[1,],
        #                   "А12" = Amplituda[2,],
        #                   "Погрешность_А12" = delta_Amplituda[2,],
        #                   "А24" = Amplituda[3,],
        #                   "Погрешность_А24" = delta_Amplituda[3,],
        #                   "fi1" = fi[1,],
        #                   "fi2" = fi[2,],
        #                   "fi3" = fi[3,],row.names = NULL)
        
        # перевод в датафрейм для наглядности
        #  Amplituda <- as.data.frame(t(Amplituda))
        # colnames(Amplituda) = c("A1","A2")
        # 
        # fi <- as.data.frame(t(fi))
        # colnames(fi) = c("fi1","fi2")
        
        #Посмотреть, что не так с циклом!!!
       
write.table( Data[1,],file = 'ГА для U.txt',col.names = TRUE, append = TRUE, row.names = FALSE, quote = FALSE, sep = "\t   ")
write.table( Data[2,],file = 'ГА для Y.txt',col.names = TRUE, append = TRUE, row.names = FALSE, quote = FALSE, sep = "\t   ") 
       
      }
      
    }
     
 
                                
                                 
     }
     
garm_analiz(big_data)
    
 # t = read.table - считываем номера часов, которые непустые
  
 # read.table(#считываем данные из двух файлов, поступивших на вход программы и считываем все значения скорости)



  

# График: численность метеоров по часам
g1 <- ggplot(WRITE, aes(x = hour, y = Quantity_in_hour)) + 
  geom_bar(stat="identity",fill="steelblue") +
  ggtitle("Численность метеоров по часам")+
  geom_text(aes(label=Quantity_in_hour), vjust=-0.3, size=3.5)+
  theme_classic()+
  xlab("Час")+ylab("Количество")+
  scale_x_continuous(breaks = seq(from = 0, to = 23,by = 1))

# График: численность метеоров по направлениям
g2 <- ggplot(data = data11, aes(x = factor(direct))) + 
  geom_bar(stat="count",fill="steelblue", width=0.7) +
  ggtitle("Численность метеоров по направлениям")+
  theme_classic()+
  xlab("Направление")+ylab("Количество")

# График: численность метеоров по часам и направлениям. Нужно сделать так, чтобы напрвления можно было выбирать
# g3 <- ggplot(data = Calcucate %>% group_by(hour,direct) %>% summarise("Quantity_in_hour_and_derection" = n()), aes(x = hour, y = Quantity_in_hour_and_derection,  fill = as.factor(direct))) +
#   geom_bar(stat="identity", position=position_dodge2()) +
#   ggtitle("Численность метеоров по часам и направлениям")+
#   theme_classic()+
#   xlab("Час")+ylab("Количество")+
#   theme(legend.title = element_text( size=10, face="bold"))+
#   theme(legend.position ="top")+
#   scale_fill_discrete(name = "Направления")

g3 <- ggplot(data = Quantity_in_derection_by_hour , aes(x = hour, y = Quantity_in_derection, fill = as.factor(direct))) +
  geom_col(color = 1, size = 0.2, position = 'dodge2') +
  ggtitle("Численность метеоров по часам и направлениям")+
  theme_classic()+
  xlab("Час")+ylab("Количество")+
  theme(legend.title = element_text( size=10, face="bold"))+
  theme(legend.position ="top")+
  scale_fill_discrete(name = "Направления")+
  scale_x_continuous(breaks = c(0:23))

ggplot(df, aes(x = ER)) + geom_histogram(binwidth = 4,colour = "black", 
                                         fill = "greenyellow")

ggplot(data = Calcucate, aes(x = dist)) + stat_bin()+
  stat_bin(geom="text", aes(label=stat(count)), vjust=-1.5)

# График: гистограмма по дальности в течение дня
g4 <- ggplot(data = Calcucate, aes(x = dist)) +
  geom_histogram(binwidth = 10, colour = "black", 
                 fill = "greenyellow") +
  ggtitle("Численность метеоров по дальности")+
  theme_classic()+
  xlab("Дальность")+ylab("Количество")+
  theme(legend.title = element_text( size=10, face="bold"))+
  theme(legend.position ="top")+
  scale_fill_discrete(name = "Направления")+
  scale_x_continuous(breaks = seq(from = floor(min(Calcucate$dist)), 
                                  to = ceiling(max(Calcucate$dist)), by = 20))+
  scale_y_continuous(breaks = seq(from = 0, to = 100,by = 5))


