#PDD outcome Reward Prediction Amplitdude Modulation -- edited with looping

library(readxl)
library(tidyr)
library(data.table)


#load SubList_stimfile

sublist_kandh = read.csv("C:/Users/jacobmer/Desktop/PDD_outcome/sublist_k_h_values.csv", TRUE)
sublist_kandh = data.frame(sublist_kandh)
head(sublist_kandh)

for (m in seq(nrow(sublist_kandh))){
  try({
  k = sublist_kandh$K[m]
  h = sublist_kandh$H[m]
  subject = sublist_kandh$Subject_ID_A[m]
  filepath1 = ("C:/Users/jacobmer/Desktop/PDD_outcome/rawfiles/")
  select_files = paste0(filepath1, subject, "_PDD_NTX_fMRI.csv")
  head(select_files)
  file1 = read.csv(select_files, FALSE)
  DATA1 = file1[-1,]
  DATA = DATA1[-1,]

  V1 = as.numeric(DATA$V1[1:64])
  V2 = as.numeric(DATA$V2[1:64])
  V3 = as.numeric(DATA$V3[1:64])
  V4 = as.numeric(DATA$V4[1:64])
  V5 = as.numeric(DATA$V5[1:64])
  V6 = as.numeric(DATA$V6[1:64])
  V7 = as.numeric(DATA$V7[1:64])
  V8 = as.numeric(DATA$V8[1:64])
  V9 = as.numeric(DATA$V9[1:64])
  V10 = as.numeric(DATA$V10[1:64])
  V11 = as.numeric(DATA$V11[1:64])

  #for(i in 1:64){
   # subjval = (V4[i])/((1 + k * V6[i])*(1 + h *((100 - (V5[i]))/(V5[i]))))
    #objval = (V4[i])}

  #j = DATA[1:64,1]
  #dw = DATA[1:64,1]
  #dl = DATA[1:64,1]
  #m = DATA[1:64,1]
  #c = DATA[1:64,1]

  for (row in nrow(DATA)){
    stim1 = V2 * 2 - 2 + 1  #ex: delay
    stim2 = V2 * 2 - 2 + 3.5 #ex: magnitude
    stim3 = V2 * 2 - 2  + 6  #ex: prob  (bc we dont care about the order)
    buttonA = V2 * 2 - 2 + 6 + (V11/1000)
    outcomeA = (V2 * 2 - 2) + 10 + V8
    objvalA = V4
    subjvalA = (V4)/((1 + k * V6)*(1 + h *((100 - (V5))/(V5))))}
 
  stim1A = data.frame(stim1)
  stim2A = data.frame(stim2)
  stim3A = data.frame(stim3)
  buttonA2 = data.frame(buttonA)
  outcomeA2 = data.frame(outcomeA)
  objvalA2 = data.frame(objvalA)
  subjvalA2 = data.frame(subjvalA)
  
  Stim1A_output = cbind(stim1A, 2, subjvalA2)
  write.csv(Stim1A_output,file = file.path("C:/Users/jacobmer/Desktop/PDD_outcome/Outputs/",
                                           paste(subject, "Stim1_out.csv",sep = "_")), row.names = FALSE) 
  Stim2A_output = cbind(stim2A, 2, subjvalA2)
  write.csv(Stim2A_output,file = file.path("C:/Users/jacobmer/Desktop/PDD_outcome/Outputs/",
                                           paste(subject, "Stim2_out.csv",sep = "_")), row.names = FALSE)
  Stim3A_output = cbind(stim3A, 2, subjvalA2)
  write.csv(Stim3A_output,file = file.path("C:/Users/jacobmer/Desktop/PDD_outcome/Outputs/",
                                           paste(subject, "Stim3_out.csv",sep = "_")), row.names = FALSE)
  button_output = cbind(buttonA2, 2, subjvalA2)
  write.csv(button_output,file = file.path("C:/Users/jacobmer/Desktop/PDD_outcome/Outputs/",
                                           paste(subject, "button_out.csv",sep = "_")), row.names = FALSE)
  outcome_output = cbind(outcomeA2, 2, subjvalA2)
  write.csv(outcome_output,file = file.path("C:/Users/jacobmer/Desktop/PDD_outcome/Outputs/",
                                            paste(subject, "outcome_out.csv",sep = "_")), row.names = FALSE)
  
  choose_immediate = subset.data.frame(DATA, V9 ==1) 
  choose_immediate_V2 = as.numeric(choose_immediate$V2)
  choose_immediate_V4 = as.numeric(choose_immediate$V4)
  choose_immediate_V5 = as.numeric(choose_immediate$V5)
  choose_immediate_V6 = as.numeric(choose_immediate$V6)
  choose_immediate_V8 = as.numeric(choose_immediate$V8)
  
  for (row in nrow(choose_immediate)){
    outcome_immediate =  (choose_immediate_V2 * 2 - 2) + 10 + choose_immediate_V8
    objval_chosen_imm = 20  #ignore print for now
    objval_notchosen_imm = choose_immediate_V4 #ignore print for now
    subval_imm = (choose_immediate_V4)/((1 + k * choose_immediate_V6)*
                                          (1 + h *((100 - (choose_immediate_V5))/(choose_immediate_V5))))}
  
  outcome_immediateA = data.frame(outcome_immediate)
  objval_chosen_immA = data.frame(objval_chosen_imm)
  objval_notchosen_immA = data.frame(objval_notchosen_imm)
  subval_immA = data.frame(subval_imm)
  
  
  outcome_immediate_output = cbind(outcome_immediateA, 2, subval_immA) #--fix this line 
  write.csv(outcome_output,file = file.path("C:/Users/jacobmer/Desktop/PDD_outcome/Outputs/",
                                            paste(subject, "choose_immediate_out.csv",sep = "_")), row.names = FALSE)
  
  choose_delaywon = subset.data.frame(DATA, V9 == 2 & V10 == 1) 
  choose_delaywon_V2 = as.numeric(choose_delaywon$V2)
  choose_delaywon_V4 = as.numeric(choose_delaywon$V4)
  choose_delaywon_V5 = as.numeric(choose_delaywon$V5)
  choose_delaywon_V6 = as.numeric(choose_delaywon$V6)
  choose_delaywon_V8 = as.numeric(choose_delaywon$V8)
  
  for (row in nrow(choose_delaywon)){
    outcome_delaywon =  (choose_delaywon_V2 * 2 - 2) + 10 + choose_delaywon_V8
    objval_notchosen_delaywon = choose_delaywon_V4 #ignore print for now
    subval_delaywon = (choose_delaywon_V4)/((1 + k * choose_delaywon_V6)*(1 + h *((100 - (choose_delaywon_V5))/
                                                                                    (choose_delaywon_V5))))}
  
  outcome_delaywonA = data.frame(outcome_delaywon)
  objval_notchosen_delaywonA = data.frame(objval_notchosen_delaywon)
  subval_delaywonA = data.frame(subval_delaywon)
  
  delaywon_output = cbind(outcome_delaywonA, 2, subval_delaywonA) 
  write.csv(delaywon_output,file = file.path("C:/Users/jacobmer/Desktop/PDD_outcome/Outputs/",
                                             paste(subject, "delay_won_out.csv",sep = "_")), row.names = FALSE)
  
  choose_delaylost = subset.data.frame(DATA, V9 == 2 & V10 == 0)
  choose_delaylost_V2 <- as.numeric(choose_delaylost$V2)
  choose_delaylost_V4 = as.numeric(choose_delaylost$V4)
  choose_delaylost_V5 = as.numeric(choose_delaylost$V5)
  choose_delaylost_V6 = as.numeric(choose_delaylost$V6)
  choose_delaylost_V8 = as.numeric(choose_delaylost$V8)
  
  
  for (row in nrow(choose_delaylost)){
    outcome_delaylost =  (choose_delaylost_V2 * 2 - 2) + 10 + (choose_delaylost_V8)
    objval_notchosen_delaylost = choose_delaylost_V4/140
    subval_delaylost = (choose_delaylost_V4)/((1 + k * choose_delaylost_V6)*
                                                (1 + h *((100 - (choose_delaylost_V5))/(choose_delaylost_V5))))}
  
  outcome_delaylostA = data.frame(outcome_delaylost)
  objval_notchosen_delaylostA = data.frame(objval_notchosen_delaylost)
  subval_delaylostA = data.frame(subval_delaylost)
  
  delaylost_output = cbind(outcome_delaylostA, 2, subval_delaylostA) 
  write.csv(delaylost_output,file = file.path("C:/Users/jacobmer/Desktop/PDD_outcome/Outputs/",
                                              paste(subject, "delay_lost_out.csv",sep = "_")), row.names = FALSE)
  
  miss_trial = subset.data.frame(DATA, V9 == 0) 
  miss_V2 = as.numeric(miss_trial$V2)
  
  for (row in nrow(miss_trial)){
    miss_trial_edit =  ((miss_V2) * 2 - 2) + 6}
  #think about subjective value?? can we put 1 col into FSL?
  #add -write.csv later
  
  miss_trialA = data.frame(miss_trial_edit)})}



