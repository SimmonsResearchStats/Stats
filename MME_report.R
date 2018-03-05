library(foreign)
library(data.table)
library(foreach)
library(weights)
library(XLConnect)

work_folder = "I:\\Public\\AmyWu\\MME"
setwd(work_folder)

Wave2 = 'Spirng 2017'
Wave1 = 'Fall 2016'

data1 = read.spss("I:\\StatisticsTeam\\MME Reports\\W50123Magazine4Reports.sav", to.data.frame = T)
data2 = read.spss("I:\\StatisticsTeam\\MME Reports\\W50127Magazine4Reports.sav", to.data.frame = T)

data1 = data.table(data1)
data2 = data.table(data2)

# change the column types
chara_col = c("BOOK_ID","WAVE_ID")
data1[, (chara_col) := lapply(.SD, as.character), .SDcols = chara_col ]
data2[, (chara_col) := lapply(.SD, as.character), .SDcols = chara_col ]


# summary(data1$BOOK_ID)
# summary(data2$BOOK_ID)
# 
# summary(data1$WAVE_ID)
# summary(data2$WAVE_ID)
# 
# length(unique(data1$BOOK_ID))
# #[1] 17944
# length(unique(data2$BOOK_ID))
# #[1] 17051
# 
# 
# length(unique(data1$WAVE_ID))
# #4
# length(unique(data2$WAVE_ID))
# #3

# clean out to be 2 dataset for 2 waves that we're comparing
# dt_wv1 = rbind(data1[WAVE_ID %in% c("1516", "1616", "1317", "1417")], 
#                data2[WAVE_ID %in% c("1516", "1616", "1317", "1417")])
dt_wv1 = copy(data1)

# dt_wv2 = rbind(data1[WAVE_ID %in% c("1316", "1416", "1516", "1616")], 
#                data2[WAVE_ID %in% c("1316", "1416", "1516", "1616")])
dt_wv2 = copy(data2)


# columns to keep 
dt_wv1 = dt_wv1[,c(1:67,107:110),with = F]
dt_wv2 = dt_wv2[,c(1:67,107:110),with = F]

# ATITUTE columns to be numeric
at_col = colnames(dt_wv1)[grep("AT",colnames(dt_wv1))]
dt_wv1[ , (at_col) := lapply(.SD,as.numeric), .SDcols = at_col]
dt_wv2[ , (at_col) := lapply(.SD,as.numeric), .SDcols = at_col]

# Magzine columns to be numeric
mag_col = c("INSP", "TRUST", "LIFE", "SOCI", "PTO",  "ADRC", "MOII", "OVER")
dt_wv1[ , (mag_col) := lapply(.SD,as.numeric), .SDcols = mag_col]
dt_wv2[ , (mag_col) := lapply(.SD,as.numeric), .SDcols = mag_col]

# # Change to TOP 2 box
# dt_wv1[ , (at_col) := lapply(.SD, function(x){ ifelse(x >= 4, 1, 0)}), .SDcols = at_col]
# 
# dt_wv2[ , (at_col) := lapply(.SD, function(x){ ifelse(x >= 4, 1, 0)}), .SDcols = at_col]


# Magzine levels
Mag = union(unique(dt_wv1$MAG), unique(dt_wv2$MAG))



# mean calculation
AllResults_Mean = foreach(mag = as.list(Mag), .combine = "rbind", .errorhandling = 'remove')%do%{
 
  # filter down to magzine
  dt_wv1_mag = dt_wv1[MAG == mag]
  dt_wv2_mag = dt_wv2[MAG == mag]
  
  Al_result = tryCatch(lapply(as.list(c(at_col, mag_col)), function(x){
    
      al_level = data.table(Name = mag)
      
      #change to TOP 2 box and count valid answers
      valid_count_wv1 = sum(dt_wv1_mag[[x]] != 0)
      valid_count_wv2 = sum(dt_wv2_mag[[x]] != 0)
      
      dt_wv1_mag_valid = dt_wv1_mag[get(x) != 0]
      dt_wv2_mag_valid = dt_wv2_mag[get(x) != 0]
      
      if(x %in% at_col){
        dt_wv1_mag_valid[, c(x) := ifelse(get(x) >= 4, 1, 0)]
        dt_wv2_mag_valid[, c(x) := ifelse(get(x) >= 4, 1, 0)]
      }
      

      # weight mean for survey 1
      wv1_mean = weighted.mean(dt_wv1_mag_valid[[x]],
                               dt_wv1_mag_valid[["WEIGHT_RES"]],
                               na.rm = T)
      
      # weight mean for survey 2
      wv2_mean = weighted.mean(dt_wv2_mag_valid[[x]],
                               dt_wv2_mag_valid[["WEIGHT_RES"]],
                               na.rm = T)
      
      if(is.na(wv1_mean)|is.na(wv2_mean)){
        cat(mag , "is missing")
        next
      }else{
        # weight t test 
        wgt_t_test = wtd.t.test(dt_wv1_mag_valid[[x]], dt_wv2_mag_valid[[x]], dt_wv1_mag_valid[["WEIGHT_RES"]], dt_wv2_mag_valid[["WEIGHT_RES"]])
        
      }
      
      
      #
      al_level = cbind(al_level, 
                       data.table(x,
                                  valid_count_wv1,
                                  valid_count_wv2,
                                  wv1_mean,
                                  wv2_mean,
                                  wv2_mean - wv1_mean,
                                  wgt_t_test$coefficients[[1]],
                                  wgt_t_test$coefficients[[3]],
                                  ifelse(wgt_t_test$coefficients[[3]] <= 0.05, 1, 0),
                                  ifelse(wgt_t_test$coefficients[[3]] <= 0.01, 1, 0)))
      
      
      
      # names(al_level) = c("Name", paste(eval(quote(Wave1)), eval(quote(x)), eval(quote(i)), 'mean'),
      #                     paste(eval(quote(Wave2)), eval(quote(x)), eval(quote(i)), 'mean'),
      #                     paste0(eval(quote(x)),'.', eval(quote(i)), 'dff'),
      #                     "TValue",
      #                     "TTest.Pvalue",
      #                     "TSig05",
      #                     "TSig01")
      
      return(al_level)
    
    
  }))
  
  Al_result = rbindlist(Al_result)
  
  return(Al_result)
 
 
}


names(AllResults_Mean) = c("Name", 
                           "Altitude",
                           "Fall 2016 Valid Answer Count",
                           "Spring 2017 Valid Answer Count",
                           "Fall 2016 Top 2 Box Mean",
                           "Spring 2017 Top 2 BOx Mean",
                           "Diff",
                           "TValue",
                           "TTest.Pvalue",
                           "TSig05",
                           "TSig01")

# mean calculation for demo

# a.	Male
# b.	Female
# c.	18-35 
# d.	35-54
# e.	African American
# f.	Hispanic

# Gender1	Male
# Gender2	Female
# Age1	18
# Age2	19
# Age3	20
# Age4	21
# Age5	22 - 24
# Age6	25 - 29
# Age7	30 - 34
# Age8	35 - 39
#
# Age9	40 - 44
# Age10	45 - 49
# Age11	50 - 54
# Hisp	Ethnicity
# NotHisp	Ethnicity
# race2	Black or African American

# demo cut we're interested
list_democut = list(c("Gender1"), c("Gender2"), c("Age1", "Age2", "Age3", "Age4", "Age5", "Age6", "Age7", "Age8"),
                    c("Age9", "Age10", "Age11"), c("Hisp"), c("NotHisp"), c("race2"))

# create a function to slice out demo for each cut
Demo_func = function(Data, Demo = list_democut){
  
  Demo_data = foreach(i = Demo)%do%{
    
    # filter out any columns has the certain demo
    Data_demo = Data[apply(Data[, eval(i), with = F], 1, function(x){any(x = 1)})]
    
  }
  
  return(Demo_data)
  
}

# slice out demo data first
Demo_Data_Cut_1 = Demo_func(Data = dt_wv1, Demo = list_democut)
Demo_Data_Cut_2 = Demo_func(Data = dt_wv2, Demo = list_democut)


AllResults_Mean_Demo = foreach(i = 1:length(Demo_Data_Cut_1) )%do%{
  
  # demo slice
  dt_wv1_dm = Demo_Data_Cut_1[[i]]
  dt_wv2_dm = Demo_Data_Cut_2[[i]]
  
  all_result_mag = foreach(mag = as.list(Mag), .combine = "rbind", .errorhandling = 'remove')%do%{
    
    # filter down to magzine
    dt_wv1_dm_mag = dt_wv1_dm[MAG == mag]
    dt_wv2_dm_mag = dt_wv2_dm[MAG == mag]
    
    Al_result = tryCatch(lapply(c(at_col, mag_col), function(x){
      
      al_level = data.table(Name = mag)
      
      #change to TOP 2 box and count valid answers
      valid_count_dm_wv1 = sum(dt_wv1_dm_mag[[x]] != 0)
      valid_count_dm_wv2 = sum(dt_wv2_dm_mag[[x]] != 0)
      
      dt_wv1_mag_dm_valid = dt_wv1_dm_mag[get(x) != 0]
      dt_wv2_mag_dm_valid = dt_wv2_dm_mag[get(x) != 0]
      
      if(x %in% at_col){
        dt_wv1_mag_dm_valid[, c(x) := ifelse(get(x) >= 4, 1, 0)]
        dt_wv2_mag_dm_valid[, c(x) := ifelse(get(x) >= 4, 1, 0)]
      }
      
      
      # weight mean for survey 1
      wv1_mean = weighted.mean(dt_wv1_mag_dm_valid[[x]],
                               dt_wv1_mag_dm_valid[["WEIGHT_RES"]],
                               na.rm = T)
      
      # weight mean for survey 2
      wv2_mean = weighted.mean(dt_wv2_mag_dm_valid[[x]],
                               dt_wv2_mag_dm_valid[["WEIGHT_RES"]],
                               na.rm = T)
      
      if(is.na(wv1_mean)|is.na(wv2_mean)){
        cat(mag , "is missing")
        next
      }else{
        # weight t test 
        wgt_t_test = wtd.t.test(dt_wv1_mag_dm_valid[[x]], dt_wv2_mag_dm_valid[[x]], dt_wv1_mag_dm_valid[["WEIGHT_RES"]], dt_wv2_mag_dm_valid[["WEIGHT_RES"]])
        
      }
      
      
      #
      al_level = cbind(al_level, 
                       data.table(x,
                                  valid_count_dm_wv1,
                                  valid_count_dm_wv2,
                                  wv1_mean,
                                  wv2_mean,
                                  wv2_mean - wv1_mean,
                                  wgt_t_test$coefficients[[1]],
                                  wgt_t_test$coefficients[[3]],
                                  ifelse(wgt_t_test$coefficients[[3]] <= 0.05, 1, 0),
                                  ifelse(wgt_t_test$coefficients[[3]] <= 0.01, 1, 0)))
      
      
      
      
      return(al_level)
      
      
    }))
    
    Al_result = rbindlist(Al_result)
    
    return(Al_result)
    
    
  }
  
  return(all_result_mag)
  
}
  
names(AllResults_Mean_Demo) = c("Male", "Female", "18-35", "35-54", "Hispanic", "Non-Hispanic", "African American")



# format the result
# format overall result first
col_decast = colnames(AllResults_Mean)[c(-1,-2)]

# method too complicated!
# for(col in col_decast){
#   assign(paste0("dcast_allresult", col), dcast.data.table(AllResults_Mean, Name ~ Altitude, value.var = col))
#   #dcast_allresult = dcast.data.table(AllResults_Mean, Name ~ Altitude, value.var = 'Spring 2017 Mean') 
# }
# 
# 
# setnames(`dcast_allresultSpring 2017 Mean`, paste0(colnames(`dcast_allresultSpring 2017 Mean`), '.x'))
# setnames(`dcast_allresultFall 2016 Mean`, paste0(colnames(`dcast_allresultFall 2016 Mean`), '.xx'))  
# setnames(dcast_allresultDiff, paste0(colnames(dcast_allresultDiff), '.xxx'))
# setnames(dcast_allresultTValue, paste0(colnames(dcast_allresultTValue), '.xxxx'))
# setnames(dcast_allresultTTest.Pvalue, paste0(colnames(dcast_allresultTTest.Pvalue), '.xxxxx'))
# setnames(dcast_allresultTSig05, paste0(colnames(dcast_allresultTSig05), '.xxxxxx'))
# setnames(dcast_allresultTSig01, paste0(colnames(dcast_allresultTSig01), '.xxxxxxx'))
# 
#                          
# Allresult_full = cbind(`dcast_allresultSpring 2017 Mean`, `dcast_allresultFall 2016 Mean`[, -1], 
#                        dcast_allresultDiff[, -1], dcast_allresultTValue[, -1],
#                        dcast_allresultTTest.Pvalue[, -1], dcast_allresultTSig05[, -1],
#                        dcast_allresultTSig01[, -1])
# 
# # write.csv(Allresult_full_1, "Fullresult_1.csv", row.names = F)
# # write.csv(Allresult_full_2, "Fullresult_2.csv", row.names = F)
# 
# write.csv(Allresult_full, "Fullresult.csv", row.names = F)


Allresult_full = dcast.data.table(AllResults_Mean, Name ~ Altitude, value.var = col_decast)

# reorder
full_colname = colnames(Allresult_full)

col = c()
for(x in 1:length(c(at_col, mag_col))){
  
  y = c(at_col, mag_col)[x]
  
  similar_col = grep(paste0('.*',c(y),'$'),full_colname, value = T)
  col = c(col, similar_col)
  
  print(y)
} 

Allresult_full = Allresult_full[, c("Name",col),with = F]
       
wbFilename = "Fullresult.xlsx"
wb = loadWorkbook(wbFilename, create = T)

sheet = "Full_Result"
createSheet(wb, name = sheet)

dataName = "overallresult"
createName(wb, name = dataName, formula = paste(sheet, "$A$1", sep = "!"))

csMean = createCellStyle(wb, name = "decimal")
setDataFormat(csMean, format = "0.0000")

allRows = seq(length = nrow(Allresult_full)) + 1

search.string = paste0(c("Mean","Diff","Pvalue","TValue"), collapse = "|")
Cols_Mean = grep(search.string, colnames(Allresult_full))

setCellStyle(wb, sheet = sheet, row = allRows, col = Cols_Mean, cellstyle = csMean)

writeNamedRegion(wb, data = Allresult_full, name = dataName, header = T)

saveWorkbook(wb)








# format demo result
Allresult_dcast_Mean_demo = rbindlist(lapply(1:length(AllResults_Mean_Demo), function(x){
  cbind(Demo = names(AllResults_Mean_Demo)[x], AllResults_Mean_Demo[[x]])
}))


names(Allresult_dcast_Mean_demo) = c("Demo",
                           "Name", 
                           "Altitude",
                           "Fall 2016 Valid Answer Count",
                           "Spring 2017 Valid Answer Count",
                           "Fall 2016 Top 2 Box Mean",
                           "Spring 2017 Top 2 BOx Mean",
                           "Diff",
                           "TValue",
                           "TTest.Pvalue",
                           "TSig05",
                           "TSig01")



# for(col in col_decast){
#   assign(paste0("dcast_allresult_demo", col), dcast.data.table(Allresult_full_demo, Demo + Name ~ Altitude, value.var = col))
#   
# }
# 
# setnames(`dcast_allresult_demoSpring 2017 Mean`, paste0(colnames(`dcast_allresult_demoSpring 2017 Mean`), '.x'))
# setnames(`dcast_allresult_demoFall 2016 Mean`, paste0(colnames(`dcast_allresult_demoFall 2016 Mean`), '.xx'))  
# setnames(dcast_allresult_demoDiff, paste0(colnames(dcast_allresult_demoDiff), '.xxx'))
# setnames(dcast_allresult_demoTValue, paste0(colnames(dcast_allresult_demoTValue), '.xxxx'))
# setnames(dcast_allresult_demoTTest.Pvalue, paste0(colnames(dcast_allresult_demoTTest.Pvalue), '.xxxxx'))
# setnames(dcast_allresult_demoTSig05, paste0(colnames(dcast_allresult_demoTSig05), '.xxxxxx'))
# setnames(dcast_allresult_demoTSig01, paste0(colnames(dcast_allresult_demoTSig01), '.xxxxxxx'))
# 
# 
# Allresult_full_demo_final = cbind(`dcast_allresult_demoSpring 2017 Mean`, `dcast_allresult_demoFall 2016 Mean`[, c(-1, -2)], 
#                                   dcast_allresult_demoDiff[, c(-1,-2)], dcast_allresult_demoTValue[, c(-1,-2)],
#                                   dcast_allresult_demoTTest.Pvalue[, c(-1,-2)], dcast_allresult_demoTSig05[, c(-1,-2)],
#                                   dcast_allresult_demoTSig01[, c(-1,-2)])
# 
# write.csv(Allresult_full_demo_final, "Fullresult_demo.csv", row.names = F)

col_decast = colnames(Allresult_dcast_Mean_demo)[c(-1,-2,-3)]


Allresult_full_demo = dcast.data.table(Allresult_dcast_Mean_demo, Demo + Name ~ Altitude, value.var = col_decast)

# reorder
full_colname_demo = colnames(Allresult_full_demo)

col = c()
for(x in 1:length(c(at_col, mag_col))){
  
  y = c(at_col, mag_col)[x]
  
  similar_col = grep(paste0('.*',c(y),'$'),full_colname_demo, value = T)
  col = c(col, similar_col)
  
  print(y)
} 

Allresult_full_demo = Allresult_full_demo[, c("Name","Demo",col),with = F]

# wbFilename = "Fullresult_Demo.xlsx"
# wbd = loadWorkbook(wbFilename, create = T)
# 
# sheet = "Full_Result_Demo"
# createSheet(wbd, name = sheet)
# 
# dataName = "overallresult.demo"
# createName(wbd, name = dataName, formula = paste(sheet, "$A$1", sep = "!"))
# 
# csMean = createCellStyle(wbd, name = "decimal")
# setDataFormat(csMean, format = "0.0000")
# 
# allRows = seq(length = nrow(Allresult_full_demo)) + 1
# 
# search.string = paste0(c("Mean","Diff","Pvalue","TValue"), collapse = "|")
# Cols_Mean = grep(search.string, colnames(Allresult_full_demo))
# 
# setCellStyle(wbd, sheet = sheet, row = allRows, col = Cols_Mean, cellstyle = csMean)
# 
# writeNamedRegion(wbd, data = Allresult_full_demo, name = dataName, header = T)
# 
# saveWorkbook(wbd)
# 
# options(java.parameters = "-Xmx1000m")



write.csv(Allresult_full_demo[1:300], "Fullresult_Demo1.csv", row.names = F)
write.csv(Allresult_full_demo[301:623], "Fullresult_Demo.csv", row.names = F)







## add in summary result
dt_wv1_sum = copy(data1)

dt_wv2_sum = copy(data2)

dt_wv1_sum = dt_wv1_sum[,c(1:67,107:110),with = F]
dt_wv2_sum = dt_wv2_sum[,c(1:67,107:110),with = F]


dt_wv1_sum[ , (at_col) := lapply(.SD,as.numeric), .SDcols = at_col]
dt_wv2_sum[ , (at_col) := lapply(.SD,as.numeric), .SDcols = at_col]


dt_wv1_sum[ , (mag_col) := lapply(.SD,as.numeric), .SDcols = mag_col]
dt_wv2_sum[ , (mag_col) := lapply(.SD,as.numeric), .SDcols = mag_col]

# sum over columns to find valid answers
dt_wv1_sum[, valid := apply(.SD,1, sum), .SDcols=c(at_col, mag_col)]
dt_wv2_sum[, valid := apply(.SD,1, sum), .SDcols=c(at_col, mag_col)]

# find the count
dt_wv1_count = sapply(Mag, function(x){
  dt_wv1_sum[Mag == x & valid > 0, length(unique(BOOK_ID))]
}, simplify = "vector")

dt_wv2_count = sapply(Mag, function(x){
  dt_wv2_sum[Mag == x & valid > 0, length(unique(BOOK_ID))]
}, simplify = "vector")

write.csv(dt_wv1_count, "Spring 2017 count.csv", row.names = F)
write.csv(dt_wv2_count, "Fall 2016 count.csv", row.names = F)
