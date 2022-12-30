temp <- merge(data.frame(id=actual_ids$ids_k1, r1=ite_k1 - qnorm(.975) * ite_se_k1 > 0, a1=actual_ttts$a1),
              data.frame(id=actual_ids$ids_k2, r2=ite_k2 - qnorm(.975) * ite_se_k2 > 0), by="id", all.x = TRUE)

temp <- merge(temp, data.frame(id=actual_ids$ids_k2, a2=actual_ttts$a2), by="id", all.x = TRUE)

temp <- merge(temp, data.frame(id=actual_ids$ids_k3, r3=ite_k3 - qnorm(.975) * ite_se_k3 > 0, a3=actual_ttts$a3), by="id", all.x = TRUE)
head(temp)

temp3 <- as.data.frame(lapply(temp, function(x) as.numeric(x)))
temp3$r2[is.na(temp3$r2)] <- "NA"
temp3$a2[is.na(temp3$a2)] <- 1
temp3$r3[is.na(temp3$r3)] <- "NA"
temp3$a3[is.na(temp3$a3)] <- 1



temp$r2[is.na(temp$r2)] <- TRUE
temp$a2[is.na(temp$a2)] <- 1
temp$r3[is.na(temp$r3)] <- TRUE
temp$a3[is.na(temp$a3)] <- 1

library(dplyr)
patterns <- temp %>%
  group_by(r1, a1, r2, a2, r3, a3) %>%
  tally() %>%
  ungroup()

library("highcharter")

temp2 <- data.frame(r1=paste0("r1_", temp3$r1),
                    a1=paste0("a1_", temp3$a1),
                    r2=paste0("r2_", temp3$r2),
                    a2=paste0("a2_", temp3$a2),
                    r3=paste0("r3_", temp3$r3),
                    a3=paste0("a3_", temp3$a3)
)

library(stringr)
temp2 <- as.data.frame(lapply(temp2, function(x) str_replace(x, '_0', ': N') ))
temp2 <- as.data.frame(lapply(temp2, function(x) str_replace(x, '_1', ': Y') ))
temp2 <- as.data.frame(lapply(temp2, function(x) str_replace(x, '_NA', ': NA') ))

head(temp2)
library(tidyr)

n_r1 <- temp2%>%
  dplyr::group_by(r1)%>%
  dplyr::tally()%>%
  dplyr::mutate(perc = n/sum(n))%>%
  dplyr::mutate(New_r1 = paste0(r1, ' (', n , ', ', round(perc* 100,1) , '%)'))%>%
  dplyr::select(-n, - perc)

n_a1 <- temp2%>%
  dplyr::group_by(a1)%>%
  dplyr::tally()%>%
  dplyr::mutate(perc = n/sum(n))%>%
  dplyr::mutate(New_a1 = paste0(a1, ' (', n , ', ', round(perc* 100,1) , '%)'))%>%
  dplyr::select(-n, - perc)
n_r2 <- temp2%>%
  dplyr::group_by(r2)%>%
  dplyr::tally()%>%
  dplyr::mutate(perc = n/sum(n))%>%
  dplyr::mutate(New_r2 = paste0(r2, ' (', n , ', ', round(perc* 100,1) , '%)'))%>%
  dplyr::select(-n, - perc)

n_a2 <- temp2%>%
  dplyr::group_by(a2)%>%
  dplyr::tally()%>%
  dplyr::mutate(perc = n/sum(n))%>%
  dplyr::mutate(New_a2 = paste0(a2, ' (', n , ', ', round(perc* 100,1) , '%)'))%>%
  dplyr::select(-n, - perc)

n_r3 <- temp2%>%
  dplyr::group_by(r3)%>%
  dplyr::tally()%>%
  dplyr::mutate(perc = n/sum(n))%>%
  dplyr::mutate(New_r3 = paste0(r3, ' (', n , ', ', round(perc* 100,1) , '%)'))%>%
  dplyr::select(-n, - perc)

n_a3 <- temp2%>%
  dplyr::group_by(a3)%>%
  dplyr::tally()%>%
  dplyr::mutate(perc = n/sum(n))%>%
  dplyr::mutate(New_a3 = paste0(a3, ' (', n , ', ', round(perc* 100,1) , '%)'))%>%
  dplyr::select(-n, - perc)

temp <- merge(temp2, n_r1, by='r1', all.x = TRUE) 
temp <- merge(temp, n_a1, by='a1', all.x = TRUE) 
temp <- merge(temp, n_r2, by='r2', all.x = TRUE) 
temp <- merge(temp, n_a2, by='a2', all.x = TRUE) 
temp <- merge(temp, n_r3, by='r3', all.x = TRUE) 
temp <- merge(temp, n_a3, by='a3', all.x = TRUE) 

sank <- temp %>% select(-c(a3,   r3,   a2,   r2,  a1,   r1))


pl <- highchart() %>%
  hc_add_series(data = data_to_sankey(sank), type = "sankey"
                ,   hcaes(from = from, to = to, weight = weight)
                ,   nodes = list(list(id =  names(table(sank$New_r1))[1] , color = "gray" )
                                 ,list(id = names(table(sank$New_r1))[2] , color = "gray" )
                                 ,list(id = names(table(sank$New_a1))[1] , color = "gray" )
                                 ,list(id = names(table(sank$New_a1))[2] , color = "gray" )
                                 ,list(id = names(table(sank$New_r2))[1] , color = "gray" )
                                 ,list(id = names(table(sank$New_r2))[2] , color = "gray" )
                                 ,list(id = names(table(sank$New_r2))[3] , color = "gray" )
                                 ,list(id = names(table(sank$New_a2))[1] , color = "gray" )
                                 ,list(id = names(table(sank$New_a2))[2] , color = "gray" )
                                 ,list(id = names(table(sank$New_r3))[1] , color = "gray" )
                                 ,list(id = names(table(sank$New_r3))[2] , color = "gray" )
                                 ,list(id = names(table(sank$New_r3))[3] , color = "gray" )
                                 ,list(id = names(table(sank$New_a3))[1] , color = "gray" )
                                 ,list(id = names(table(sank$New_a3))[2] , color = "gray" )
                                 
                )) %>%
  hc_plotOptions(series = list(dataLabels = list( style = list(fontSize = "9px" , color = "black")
                                                  , backgroundColor = "white"
                                                  , borderRadius = 0
                                                  , borderWidth = 0
                                                  , borderColor = 'blue'
                                                  , padding = 5
                                                  , shadow = TRUE)))
pl

library(htmlwidgets)
#htmlwidgets::saveWidget(widget = pl, file="sankey_s.html")
