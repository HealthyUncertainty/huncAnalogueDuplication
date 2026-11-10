# ==============================================================================
# app.R - Shiny display of Original vs Duplicate HTA model results
# One tab per model (CAIS, CISS, CNRD, DMSS, NCD) plus an Overview tab.
# Self-contained: data are embedded (deterministic base case). Run with shiny::runApp("app.R").
# Needs: shiny, dplyr, tidyr, ggplot2, scales (DT optional, for sortable tables).
# ==============================================================================

BASIS <- "ncd"   # Original calculated on the common NCD life table

for (p in c("shiny", "dplyr", "tidyr", "ggplot2", "scales"))
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
suppressPackageStartupMessages({ library(shiny); library(dplyr); library(tidyr); library(ggplot2) })
HAS_GT <- FALSE; TABLE_MODE <- "view"

# ---- Embedded data -----------------------------------------------------------
rd <- function(x) read.csv(text = x, stringsAsFactors = FALSE)

# Totals per arm. _O = Original, _D = Duplicate. LYd/LYu = LYs discounted/undiscounted, Q = QALYs, C = cost.
tot_ncd <- rd(r"---("model","arm","LYd_O","LYd_D","LYu_O","LYu_D","Q_O","Q_D","C_O","C_D"
"CAIS","UsualCare",12.9242863,12.9904529,17.3516168,17.4689698,8.42601873,8.52779027,99333.363,95601.3008
"CAIS","Infectaguard",13.191439,13.2224078,17.8363806,17.89311,8.92629959,8.97355849,112564.155,110933.997
"CISS","BSC",4.8045057,4.54642282,5.18439527,4.88426163,3.02943465,2.86599718,80221.8593,76075.7681
"CISS","Dravokimab",4.8045057,4.54642282,5.18439527,4.88426163,3.51976113,3.33979206,161543.675,151131.805
"CISS","Barisetinib",4.8045057,4.54642282,5.18439527,4.88426163,3.23515566,3.06555021,98045.9683,91325.8442
"CISS","Tremacept",4.8045057,4.54642282,5.18439527,4.88426163,3.28215445,3.11096983,118798.76,110120.071
"CISS","Uvestinib",4.8045057,4.54642282,5.18439527,4.88426163,3.45034529,3.2726866,195654.908,182407.541
"CISS","Dorilumab",4.8045057,4.54642282,5.18439527,4.88426163,3.42251018,3.24642178,130224.197,121569.751
"CNRD","Velonix",17.1522397,17.1466246,25.8870287,25.7297324,10.3844535,10.4675259,337369.234,326101.705
"CNRD","Doralene",17.0373763,17.0244632,25.6347044,25.4635045,10.2117213,10.2833983,318091.171,293728.151
"DMSS","Cardivex",17.4519867,17.4797231,26.4374079,26.4998167,15.0856193,15.0745744,1435505.43,1430509.53
"DMSS","SoC",17.4519867,17.4797231,26.4374079,26.4998167,14.1147984,14.1200291,408814.879,400547.275
"DMSS","Zonapride",17.4519867,17.4797231,26.4374079,26.4998167,14.3657686,14.3914925,482232.203,471793.676
"DMSS","Septoplasty",17.260861,17.2882923,26.1474425,26.2091649,15.1335463,15.1641484,362159.61,351685.471
"DMSS","Ablatherm",17.2956111,17.3230979,26.2001635,26.2620107,15.1489606,15.1793619,302665.079,292171.859
"NCD","UsualCare",20.3458863,20.3458863,32.7017622,32.7017622,13.9585867,13.9585867,141524.138,141524.138
"NCD","Zelravine",20.4496548,20.4496548,32.9714562,32.9714562,14.7555949,14.7555949,196234.208,196234.208)---")
tot_own <- rd(r"---("model","arm","LYd_O","LYd_D","LYu_O","LYu_D","Q_O","Q_D","C_O","C_D"
"CAIS","UsualCare",13.5753529,12.9904529,18.4822667,17.4689698,8.83930335,8.52779027,105119.783,95601.3008
"CAIS","Infectaguard",13.8408276,13.2224078,18.971508,17.89311,9.35701835,8.97355849,118707.861,110933.997
"CISS","BSC",4.84617194,4.54642282,5.23047954,4.88426163,3.05548364,2.86599718,80929.2074,76075.7681
"CISS","Dravokimab",4.84617194,4.54642282,5.23047954,4.88426163,3.54993029,3.33979206,162846.041,151131.805
"CISS","Barisetinib",4.84617194,4.54642282,5.23047954,4.88426163,3.26284171,3.06555021,98850.8574,91325.8442
"CISS","Tremacept",4.84617194,4.54642282,5.23047954,4.88426163,3.31031589,3.11096983,119750.505,110120.071
"CISS","Uvestinib",4.84617194,4.54642282,5.23047954,4.88426163,3.47958672,3.2726866,197126.284,182407.541
"CISS","Dorilumab",4.84617194,4.54642282,5.23047954,4.88426163,3.45181035,3.24642178,131275.576,121569.751
"CNRD","Velonix",17.6301998,17.1466246,26.9514334,25.7297324,10.6479516,10.4675259,348316.47,326101.705
"CNRD","Doralene",17.5175894,17.0244632,26.6988981,25.4635045,10.474556,10.2833983,329259.243,293728.151
"DMSS","Cardivex",17.8035115,17.4797231,26.9676168,26.4998167,15.3883317,15.0745744,1464399.86,1430509.53
"DMSS","SoC",17.8035115,17.4797231,26.9676168,26.4998167,14.3978957,14.1200291,417041.904,400547.275
"DMSS","Zonapride",17.8035115,17.4797231,26.9676168,26.4998167,14.6539278,14.3914925,491781.004,471793.676
"DMSS","Septoplasty",17.6085191,17.2882923,26.6718192,26.2091649,15.4379849,15.1641484,367217.309,351685.471
"DMSS","Ablatherm",17.6439722,17.3230979,26.7256006,26.2620107,15.453045,15.1793619,307733.006,292171.859
"NCD","UsualCare",20.3458863,20.3458863,32.7017622,32.7017622,13.9585867,13.9585867,141524.138,141524.138
"NCD","Zelravine",20.4496548,20.4496548,32.9714562,32.9714562,14.7555949,14.7555949,196234.208,196234.208)---")
# Cost components per arm
parts_ncd <- rd(r"---("model","arm","part","O","D"
"CAIS","UsualCare","Background_UC",58159.2885,58457.038
"CAIS","UsualCare","Drug",0.0,0.0
"CAIS","UsualCare","Chronic_infection",28733.7543,24752.9097
"CAIS","UsualCare","Exacerbation_events",12440.3202,12391.3531
"CAIS","Infectaguard","Background_UC",59361.4757,59500.835
"CAIS","Infectaguard","Drug",26382.8781,26444.8156
"CAIS","Infectaguard","Chronic_infection",18965.6594,17152.9502
"CAIS","Infectaguard","Exacerbation_events",7854.14144,7835.39651
"CISS","BSC","Drug",0.0,0.0
"CISS","BSC","State_mgmt",80221.8593,76075.7681
"CISS","BSC","Injection_training",0.0,0.0
"CISS","Dravokimab","Drug",101012.02,93787.5732
"CISS","Dravokimab","State_mgmt",60531.6553,57344.2319
"CISS","Dravokimab","Injection_training",0.0,0.0
"CISS","Barisetinib","Drug",26370.037,23405.0615
"CISS","Barisetinib","State_mgmt",71675.9313,67920.7828
"CISS","Barisetinib","Injection_training",0.0,0.0
"CISS","Tremacept","Drug",48961.9348,43968.9711
"CISS","Tremacept","State_mgmt",69811.9383,66126.0997
"CISS","Tremacept","Injection_training",24.8865704,25.0
"CISS","Uvestinib","Drug",132233.966,122226.731
"CISS","Uvestinib","State_mgmt",63420.9416,60180.8105
"CISS","Uvestinib","Injection_training",0.0,0.0
"CISS","Dorilumab","Drug",65923.7973,60663.9318
"CISS","Dorilumab","State_mgmt",64275.5136,60880.8188
"CISS","Dorilumab","Injection_training",24.8865704,25.0
"CNRD","Velonix","Drug",40733.7452,35812.7695
"CNRD","Velonix","State_mgmt",134965.88,134186.092
"CNRD","Velonix","Relapse",108466.549,98800.0657
"CNRD","Velonix","Hospitalisation",53203.0591,57302.7783
"CNRD","Doralene","Drug",6003.13881,646.929602
"CNRD","Doralene","State_mgmt",142273.82,141891.52
"CNRD","Doralene","Relapse",109150.769,99669.1991
"CNRD","Doralene","Hospitalisation",60663.4431,51520.5022
"DMSS","Cardivex","Drug",1116909.7,1118684.8
"DMSS","Cardivex","Grade_state",306727.222,311541.334
"DMSS","Cardivex","Procedure",0.0,0.0
"DMSS","Cardivex","Zona_hospitalisation",0.0,0.0
"DMSS","Cardivex","Cardiac_scans",283.398835,283.398835
"DMSS","Cardivex","Background_meds",11585.1123,0.0
"DMSS","SoC","Drug",0.0,0.0
"DMSS","SoC","Grade_state",397088.847,399670.472
"DMSS","SoC","Procedure",0.0,0.0
"DMSS","SoC","Zona_hospitalisation",0.0,0.0
"DMSS","SoC","Cardiac_scans",875.397077,876.803008
"DMSS","SoC","Background_meds",10850.6346,0.0
"DMSS","Zonapride","Drug",86117.8144,86349.8321
"DMSS","Zonapride","Grade_state",376588.357,376767.041
"DMSS","Zonapride","Procedure",0.0,0.0
"DMSS","Zonapride","Zona_hospitalisation",7800.0,7800.0
"DMSS","Zonapride","Cardiac_scans",875.397077,876.803008
"DMSS","Zonapride","Background_meds",10850.6346,0.0
"DMSS","Septoplasty","Drug",0.0,0.0
"DMSS","Septoplasty","Grade_state",241377.42,240817.268
"DMSS","Septoplasty","Procedure",110000.0,110000.0
"DMSS","Septoplasty","Zona_hospitalisation",0.0,0.0
"DMSS","Septoplasty","Cardiac_scans",866.81271,868.203175
"DMSS","Septoplasty","Background_meds",9915.37639,0.0
"DMSS","Ablatherm","Drug",0.0,0.0
"DMSS","Ablatherm","Grade_state",241861.344,241302.092
"DMSS","Ablatherm","Procedure",50000.0,50000.0
"DMSS","Ablatherm","Zona_hospitalisation",0.0,0.0
"DMSS","Ablatherm","Cardiac_scans",868.373504,869.766781
"DMSS","Ablatherm","Background_meds",9935.36188,0.0
"NCD","UsualCare","State",141524.138,141524.138
"NCD","UsualCare","Initial_course",0.0,0.0
"NCD","UsualCare","Retreatment",0.0,0.0
"NCD","Zelravine","State",128357.357,128357.357
"NCD","Zelravine","Initial_course",60000.0,60000.0
"NCD","Zelravine","Retreatment",7876.85115,7876.85115)---")
parts_own <- rd(r"---("model","arm","part","O","D"
"CAIS","UsualCare","Background_UC",61089.0878,58457.038
"CAIS","UsualCare","Drug",0.0,0.0
"CAIS","UsualCare","Chronic_infection",30927.9566,24752.9097
"CAIS","UsualCare","Exacerbation_events",13102.7385,12391.3531
"CAIS","Infectaguard","Background_UC",62283.7241,59500.835
"CAIS","Infectaguard","Drug",27681.6552,26444.8156
"CAIS","Infectaguard","Chronic_infection",20476.7385,17152.9502
"CAIS","Infectaguard","Exacerbation_events",8265.74302,7835.39651
"CISS","BSC","Drug",0.0,0.0
"CISS","BSC","State_mgmt",80929.2074,76075.7681
"CISS","BSC","Injection_training",0.0,0.0
"CISS","Dravokimab","Drug",101768.254,93787.5732
"CISS","Dravokimab","State_mgmt",61077.7865,57344.2319
"CISS","Dravokimab","Injection_training",0.0,0.0
"CISS","Barisetinib","Drug",26533.781,23405.0615
"CISS","Barisetinib","State_mgmt",72317.0765,67920.7828
"CISS","Barisetinib","Injection_training",0.0,0.0
"CISS","Tremacept","Drug",49291.3795,43968.9711
"CISS","Tremacept","State_mgmt",70434.239,66126.0997
"CISS","Tremacept","Injection_training",24.8865704,25.0
"CISS","Uvestinib","Drug",133121.066,122226.731
"CISS","Uvestinib","State_mgmt",64005.2182,60180.8105
"CISS","Uvestinib","Injection_training",0.0,0.0
"CISS","Dorilumab","Drug",66396.3781,60663.9318
"CISS","Dorilumab","State_mgmt",64854.3113,60880.8188
"CISS","Dorilumab","Injection_training",24.8865704,25.0
"CNRD","Velonix","Drug",41073.9021,35812.7695
"CNRD","Velonix","State_mgmt",141850.53,134186.092
"CNRD","Velonix","Relapse",112188.979,98800.0657
"CNRD","Velonix","Hospitalisation",53203.0591,57302.7783
"CNRD","Doralene","Drug",6202.97344,646.929602
"CNRD","Doralene","State_mgmt",149502.579,141891.52
"CNRD","Doralene","Relapse",112890.248,99669.1991
"CNRD","Doralene","Hospitalisation",60663.4431,51520.5022
"DMSS","Cardivex","Drug",1139406.94,1118684.8
"DMSS","Cardivex","Grade_state",312890.771,311541.334
"DMSS","Cardivex","Procedure",0.0,0.0
"DMSS","Cardivex","Zona_hospitalisation",0.0,0.0
"DMSS","Cardivex","Cardiac_scans",283.435972,283.398835
"DMSS","Cardivex","Background_meds",11818.7216,0.0
"DMSS","SoC","Drug",0.0,0.0
"DMSS","SoC","Grade_state",405080.553,399670.472
"DMSS","SoC","Procedure",0.0,0.0
"DMSS","SoC","Zona_hospitalisation",0.0,0.0
"DMSS","SoC","Cardiac_scans",891.913505,876.803008
"DMSS","SoC","Background_meds",11069.4377,0.0
"DMSS","Zonapride","Drug",87854.347,86349.8321
"DMSS","Zonapride","Grade_state",384165.306,376767.041
"DMSS","Zonapride","Procedure",0.0,0.0
"DMSS","Zonapride","Zona_hospitalisation",7800.0,7800.0
"DMSS","Zonapride","Cardiac_scans",891.913505,876.803008
"DMSS","Zonapride","Background_meds",11069.4377,0.0
"DMSS","Septoplasty","Drug",0.0,0.0
"DMSS","Septoplasty","Grade_state",246218.84,240817.268
"DMSS","Septoplasty","Procedure",110000.0,110000.0
"DMSS","Septoplasty","Zona_hospitalisation",0.0,0.0
"DMSS","Septoplasty","Cardiac_scans",883.147456,868.203175
"DMSS","Septoplasty","Background_meds",10115.3215,0.0
"DMSS","Ablatherm","Drug",0.0,0.0
"DMSS","Ablatherm","Grade_state",246712.554,241302.092
"DMSS","Ablatherm","Procedure",50000.0,50000.0
"DMSS","Ablatherm","Zona_hospitalisation",0.0,0.0
"DMSS","Ablatherm","Cardiac_scans",884.741283,869.766781
"DMSS","Ablatherm","Background_meds",10135.7113,0.0
"NCD","UsualCare","State",141524.138,141524.138
"NCD","UsualCare","Initial_course",0.0,0.0
"NCD","UsualCare","Retreatment",0.0,0.0
"NCD","Zelravine","State",128357.357,128357.357
"NCD","Zelravine","Initial_course",60000.0,60000.0
"NCD","Zelravine","Retreatment",7876.85115,7876.85115)---")
# Bridge: Original moved one change at a time to the Duplicate (cumulative; last step = Duplicate)
wf <- rd(r"---("model","step","label","arm","LY_disc","LY_undisc","QALY","Cost"
"CAIS",0.0,"0 Original","UsualCare",13.5753529,18.4822667,8.83930335,105119.783
"CAIS",0.0,"0 Original","Infectaguard",13.8408276,18.971508,9.35701835,118707.861
"CAIS",1.0,"1 Mortality table","UsualCare",12.9242863,17.3516168,8.42601873,99333.363
"CAIS",1.0,"1 Mortality table","Infectaguard",13.191439,17.8363806,8.92629959,112564.155
"CAIS",2.0,"2 Exac. RR in PathA/PathB swapped","UsualCare",12.9246116,17.3522238,8.42678314,99320.8391
"CAIS",2.0,"2 Exac. RR in PathA/PathB swapped","Infectaguard",13.1915705,17.8366328,8.92661247,112559.196
"CAIS",3.0,"3 Transition structure","UsualCare",12.9904529,17.4689698,8.52779027,95601.3008
"CAIS",3.0,"3 Transition structure","Infectaguard",13.2224078,17.89311,8.97355849,110933.997
"CISS",0.0,"0 Original","BSC",4.84617194,5.23047954,3.05548364,80929.2074
"CISS",0.0,"0 Original","Dravokimab",4.84617194,5.23047954,3.54993029,162846.041
"CISS",0.0,"0 Original","Barisetinib",4.84617194,5.23047954,3.26284171,98850.8574
"CISS",0.0,"0 Original","Tremacept",4.84617194,5.23047954,3.31031589,119750.505
"CISS",0.0,"0 Original","Uvestinib",4.84617194,5.23047954,3.47958672,197126.284
"CISS",0.0,"0 Original","Dorilumab",4.84617194,5.23047954,3.45181035,131275.576
"CISS",1.0,"1 Background mortality (NCD @45)","BSC",4.8045057,5.18439527,3.02943465,80221.8593
"CISS",1.0,"1 Background mortality (NCD @45)","Dravokimab",4.8045057,5.18439527,3.51976113,161543.675
"CISS",1.0,"1 Background mortality (NCD @45)","Barisetinib",4.8045057,5.18439527,3.23515566,98045.9683
"CISS",1.0,"1 Background mortality (NCD @45)","Tremacept",4.8045057,5.18439527,3.28215445,118798.76
"CISS",1.0,"1 Background mortality (NCD @45)","Uvestinib",4.8045057,5.18439527,3.45034529,195654.908
"CISS",1.0,"1 Background mortality (NCD @45)","Dorilumab",4.8045057,5.18439527,3.42251018,130224.197
"CISS",2.0,"2 No deaths in induction cycle","BSC",4.80926821,5.18953435,3.0324376,80301.3801
"CISS",2.0,"2 No deaths in induction cycle","Dravokimab",4.80926821,5.18953435,3.52325012,161703.807
"CISS",2.0,"2 No deaths in induction cycle","Barisetinib",4.80926821,5.18953435,3.23836255,98143.1574
"CISS",2.0,"2 No deaths in induction cycle","Tremacept",4.80926821,5.18953435,3.28540792,118916.496
"CISS",2.0,"2 No deaths in induction cycle","Uvestinib",4.80926821,5.18953435,3.45376548,195848.853
"CISS",2.0,"2 No deaths in induction cycle","Dorilumab",4.80926821,5.18953435,3.42590278,130353.259
"CISS",3.0,"3 Start-of-cycle discounting","BSC",4.8311882,5.18953435,3.04625904,80667.3828
"CISS",3.0,"3 Start-of-cycle discounting","Dravokimab",4.8311882,5.18953435,3.53930862,162440.831
"CISS",3.0,"3 Start-of-cycle discounting","Barisetinib",4.8311882,5.18953435,3.25312256,98590.4805
"CISS",3.0,"3 Start-of-cycle discounting","Tremacept",4.8311882,5.18953435,3.30038236,119458.501
"CISS",3.0,"3 Start-of-cycle discounting","Uvestinib",4.8311882,5.18953435,3.46950727,196741.505
"CISS",3.0,"3 Start-of-cycle discounting","Dorilumab",4.8311882,5.18953435,3.44151758,130947.391
"CISS",4.0,"4 Half-cycle correction (first/last row)","BSC",4.54642282,4.88426163,2.86599718,76075.7681
"CISS",4.0,"4 Half-cycle correction (first/last row)","Dravokimab",4.54642282,4.88426163,3.33979206,151131.805
"CISS",4.0,"4 Half-cycle correction (first/last row)","Barisetinib",4.54642282,4.88426163,3.06555021,91325.8442
"CISS",4.0,"4 Half-cycle correction (first/last row)","Tremacept",4.54642282,4.88426163,3.11096983,110120.071
"CISS",4.0,"4 Half-cycle correction (first/last row)","Uvestinib",4.54642282,4.88426163,3.2726866,182407.541
"CISS",4.0,"4 Half-cycle correction (first/last row)","Dorilumab",4.54642282,4.88426163,3.24642178,121569.751
"DMSS",0.0,"0 Original","Cardivex",17.8035115,26.9676168,15.3883317,1464399.86
"DMSS",0.0,"0 Original","SoC",17.8035115,26.9676168,14.3978957,417041.904
"DMSS",0.0,"0 Original","Zonapride",17.8035115,26.9676168,14.6539278,491781.004
"DMSS",0.0,"0 Original","Septoplasty",17.6085191,26.6718192,15.4379849,367217.309
"DMSS",0.0,"0 Original","Ablatherm",17.6439722,26.7256006,15.453045,307733.006
"DMSS",1.0,"1 Mortality table","Cardivex",17.4519867,26.4374079,15.0856193,1435505.43
"DMSS",1.0,"1 Mortality table","SoC",17.4519867,26.4374079,14.1147984,408814.879
"DMSS",1.0,"1 Mortality table","Zonapride",17.4519867,26.4374079,14.3657686,482232.203
"DMSS",1.0,"1 Mortality table","Septoplasty",17.260861,26.1474425,15.1335463,362159.61
"DMSS",1.0,"1 Mortality table","Ablatherm",17.2956111,26.2001635,15.1489606,302665.079
"DMSS",2.0,"2 Mortality age lag (cycle t-1)","Cardivex",17.4797231,26.4998167,15.1093069,1437785.29
"DMSS",2.0,"2 Mortality age lag (cycle t-1)","SoC",17.4797231,26.4998167,14.1369384,409464.119
"DMSS",2.0,"2 Mortality age lag (cycle t-1)","Zonapride",17.4797231,26.4998167,14.3883079,482985.735
"DMSS",2.0,"2 Mortality age lag (cycle t-1)","Septoplasty",17.2882923,26.2091649,15.1573728,362558.779
"DMSS",2.0,"2 Mortality age lag (cycle t-1)","Ablatherm",17.3230979,26.2620107,15.1727583,303065.056
"DMSS",3.0,"3 Grade split before freeze cycle","Cardivex",17.4797231,26.4998167,15.0745744,1442113.08
"DMSS",3.0,"3 Grade split before freeze cycle","SoC",17.4797231,26.4998167,14.1200291,411415.173
"DMSS",3.0,"3 Grade split before freeze cycle","Zonapride",17.4797231,26.4998167,14.3914925,482566.575
"DMSS",3.0,"3 Grade split before freeze cycle","Septoplasty",17.2882923,26.2091649,15.1650159,361552.795
"DMSS",3.0,"3 Grade split before freeze cycle","Ablatherm",17.3230979,26.2620107,15.1804013,302059.072
"DMSS",4.0,"4 Background medication cost excluded","Cardivex",17.4797231,26.4998167,15.0745744,1430509.53
"DMSS",4.0,"4 Background medication cost excluded","SoC",17.4797231,26.4998167,14.1200291,400547.275
"DMSS",4.0,"4 Background medication cost excluded","Zonapride",17.4797231,26.4998167,14.3914925,471698.676
"DMSS",4.0,"4 Background medication cost excluded","Septoplasty",17.2882923,26.2091649,15.1650159,351621.642
"DMSS",4.0,"4 Background medication cost excluded","Ablatherm",17.3230979,26.2620107,15.1804013,292107.902
"DMSS",5.0,"5 Zonapride cycle-1 drug cost","Cardivex",17.4797231,26.4998167,15.0745744,1430509.53
"DMSS",5.0,"5 Zonapride cycle-1 drug cost","SoC",17.4797231,26.4998167,14.1200291,400547.275
"DMSS",5.0,"5 Zonapride cycle-1 drug cost","Zonapride",17.4797231,26.4998167,14.3914925,471793.676
"DMSS",5.0,"5 Zonapride cycle-1 drug cost","Septoplasty",17.2882923,26.2091649,15.1650159,351621.642
"DMSS",5.0,"5 Zonapride cycle-1 drug cost","Ablatherm",17.3230979,26.2620107,15.1804013,292107.902
"DMSS",6.0,"6 Septoplasty disutility timing","Cardivex",17.4797231,26.4998167,15.0745744,1430509.53
"DMSS",6.0,"6 Septoplasty disutility timing","SoC",17.4797231,26.4998167,14.1200291,400547.275
"DMSS",6.0,"6 Septoplasty disutility timing","Zonapride",17.4797231,26.4998167,14.3914925,471793.676
"DMSS",6.0,"6 Septoplasty disutility timing","Septoplasty",17.2882923,26.2091649,15.1651857,351621.642
"DMSS",6.0,"6 Septoplasty disutility timing","Ablatherm",17.3230979,26.2620107,15.1804013,292107.902
"DMSS",7.0,"7 Surgical-arm grade II/III split (0.19/0.09 vs 0.1904/0.0896)","Cardivex",17.4797231,26.4998167,15.0745744,1430509.53
"DMSS",7.0,"7 Surgical-arm grade II/III split (0.19/0.09 vs 0.1904/0.0896)","SoC",17.4797231,26.4998167,14.1200291,400547.275
"DMSS",7.0,"7 Surgical-arm grade II/III split (0.19/0.09 vs 0.1904/0.0896)","Zonapride",17.4797231,26.4998167,14.3914925,471793.676
"DMSS",7.0,"7 Surgical-arm grade II/III split (0.19/0.09 vs 0.1904/0.0896)","Septoplasty",17.2882923,26.2091649,15.1641484,351685.471
"DMSS",7.0,"7 Surgical-arm grade II/III split (0.19/0.09 vs 0.1904/0.0896)","Ablatherm",17.3230979,26.2620107,15.1793619,292171.859
"CNRD",0.0,"0 Original","Velonix",17.6301998,26.9514334,10.6479516,348316.47
"CNRD",0.0,"0 Original","Doralene",17.5175894,26.6988981,10.474556,329259.243
"CNRD",1.0,"1 Mortality table","Velonix",17.1522397,25.8870287,10.3844535,337369.234
"CNRD",1.0,"1 Mortality table","Doralene",17.0373763,25.6347044,10.2117213,318091.171
"CNRD",2.0,"2 No-treatment relapse prob (0.40 -> 0.12)","Velonix",17.1522397,25.8870287,10.4624628,327725.105
"CNRD",2.0,"2 No-treatment relapse prob (0.40 -> 0.12)","Doralene",17.0373763,25.6347044,10.2879364,308668.861
"CNRD",3.0,"3 Line 2/3 drug cost placeholder ($38/yr)","Velonix",17.1522397,25.8870287,10.4624628,322665.371
"CNRD",3.0,"3 Line 2/3 drug cost placeholder ($38/yr)","Doralene",17.0373763,25.6347044,10.2879364,303313.143
"CNRD",4.0,"4 Hospitalisation costing","Velonix",17.1522397,25.8870287,10.4624628,326871.563
"CNRD",4.0,"4 Hospitalisation costing","Doralene",17.0373763,25.6347044,10.2879364,294236.911
"CNRD",5.0,"5 Discontinuation timing within cycle","Velonix",17.1604109,25.9048978,10.4739862,326330.055
"CNRD",5.0,"5 Discontinuation timing within cycle","Doralene",17.0394122,25.6391129,10.2908291,294058.869
"CNRD",6.0,"6 Stabilisation cycle / age offset","Velonix",17.0883325,25.7414965,10.4331615,324833.13
"CNRD",6.0,"6 Stabilisation cycle / age offset","Doralene",16.9670353,25.4761013,10.2501343,292522.743
"CNRD",7.0,"7 Discounting timing","Velonix",17.1515881,25.7414965,10.4717818,325832.807
"CNRD",7.0,"7 Discounting timing","Doralene",17.0298419,25.4761013,10.2880771,293445.751
"CNRD",8.0,"8 Discontinuation/blend cutoff cycle","Velonix",17.1516486,25.7416822,10.4717485,325836.798
"CNRD",8.0,"8 Discontinuation/blend cutoff cycle","Doralene",17.0298461,25.4761136,10.2879689,293433.285
"CNRD",9.0,"9 Mortality cap / SMR combination form","Velonix",17.1516486,25.7416822,10.4717485,325836.798
"CNRD",9.0,"9 Mortality cap / SMR combination form","Doralene",17.0298461,25.4761136,10.2879689,293433.285
"CNRD",10.0,"10 MetabX -> DiabX/CardioY split","Velonix",17.1466246,25.7297324,10.4675259,326101.705
"CNRD",10.0,"10 MetabX -> DiabX/CardioY split","Doralene",17.0244632,25.4635045,10.2833983,293728.151
"CNRD",11.0,"11 Half-cycle weight on last row","Velonix",17.1466246,25.7297324,10.4675259,326101.705
"CNRD",11.0,"11 Half-cycle weight on last row","Doralene",17.0244632,25.4635045,10.2833983,293728.151)---")
# Discounted LYs and QALYs by health state, both versions and both bases (sums reconcile to the totals above)
states <- rd(r"---("basis","model","arm","version","measure","state","value"
"ncd","CAIS","UsualCare","Original","LY","Stable",6.18969544
"ncd","CAIS","UsualCare","Original","LY","Stable_Exac",2.4196258
"ncd","CAIS","UsualCare","Original","LY","PathA",1.58783766
"ncd","CAIS","UsualCare","Original","LY","PathA_Exac",0.831151578
"ncd","CAIS","UsualCare","Original","LY","PathB",1.25915317
"ncd","CAIS","UsualCare","Original","LY","PathB_Exac",0.636822682
"ncd","CAIS","UsualCare","Original","QALY","Stable",4.45658072
"ncd","CAIS","UsualCare","Original","QALY","Stable_Exac",1.42854707
"ncd","CAIS","UsualCare","Original","QALY","PathA",1.00605394
"ncd","CAIS","UsualCare","Original","QALY","PathA_Exac",0.431826465
"ncd","CAIS","UsualCare","Original","QALY","PathB",0.77966764
"ncd","CAIS","UsualCare","Original","QALY","PathB_Exac",0.323342896
"ncd","CAIS","UsualCare","Duplicate","LY","Stable",6.67306693
"ncd","CAIS","UsualCare","Duplicate","LY","Stable_Exac",2.59675211
"ncd","CAIS","UsualCare","Duplicate","LY","PathA",1.38852618
"ncd","CAIS","UsualCare","Duplicate","LY","PathA_Exac",0.712703298
"ncd","CAIS","UsualCare","Duplicate","LY","PathB",1.05656194
"ncd","CAIS","UsualCare","Duplicate","LY","PathB_Exac",0.562842435
"ncd","CAIS","UsualCare","Duplicate","QALY","Stable",4.80460819
"ncd","CAIS","UsualCare","Duplicate","QALY","Stable_Exac",1.53312244
"ncd","CAIS","UsualCare","Duplicate","QALY","PathA",0.879770189
"ncd","CAIS","UsualCare","Duplicate","QALY","PathA_Exac",0.370286424
"ncd","CAIS","UsualCare","Duplicate","QALY","PathB",0.654223156
"ncd","CAIS","UsualCare","Duplicate","QALY","PathB_Exac",0.285779869
"ncd","CAIS","Infectaguard","Original","LY","Stable",8.52050864
"ncd","CAIS","Infectaguard","Original","LY","Stable_Exac",1.83106981
"ncd","CAIS","Infectaguard","Original","LY","PathA",1.21003395
"ncd","CAIS","Infectaguard","Original","LY","PathA_Exac",0.345496083
"ncd","CAIS","Infectaguard","Original","LY","PathB",1.00647726
"ncd","CAIS","Infectaguard","Original","LY","PathB_Exac",0.277853309
"ncd","CAIS","Infectaguard","Original","QALY","Stable",6.13476622
"ncd","CAIS","Infectaguard","Original","QALY","Stable_Exac",1.08106361
"ncd","CAIS","Infectaguard","Original","QALY","PathA",0.766677509
"ncd","CAIS","Infectaguard","Original","QALY","PathA_Exac",0.179503181
"ncd","CAIS","Infectaguard","Original","QALY","PathB",0.623210717
"ncd","CAIS","Infectaguard","Original","QALY","PathB_Exac",0.141078351
"ncd","CAIS","Infectaguard","Duplicate","LY","Stable",8.76798706
"ncd","CAIS","Infectaguard","Duplicate","LY","Stable_Exac",1.88453224
"ncd","CAIS","Infectaguard","Duplicate","LY","PathA",1.10774295
"ncd","CAIS","Infectaguard","Duplicate","LY","PathA_Exac",0.306399344
"ncd","CAIS","Infectaguard","Duplicate","LY","PathB",0.898116366
"ncd","CAIS","Infectaguard","Duplicate","LY","PathB_Exac",0.257629825
"ncd","CAIS","Infectaguard","Duplicate","QALY","Stable",6.31295068
"ncd","CAIS","Infectaguard","Duplicate","QALY","Stable_Exac",1.11262784
"ncd","CAIS","Infectaguard","Duplicate","QALY","PathA",0.701865931
"ncd","CAIS","Infectaguard","Duplicate","QALY","PathA_Exac",0.159190392
"ncd","CAIS","Infectaguard","Duplicate","QALY","PathB",0.556113654
"ncd","CAIS","Infectaguard","Duplicate","QALY","PathB_Exac",0.130809998
"ncd","CISS","BSC","Original","LY","NonResponder",4.51641887
"ncd","CISS","BSC","Original","LY","Resp_Low",0.128719646
"ncd","CISS","BSC","Original","LY","Resp_Moderate",0.0858130972
"ncd","CISS","BSC","Original","LY","Resp_High",0.0735540833
"ncd","CISS","BSC","Original","QALY","NonResponder",2.8001797
"ncd","CISS","BSC","Original","QALY","Resp_Low",0.0978309831
"ncd","CISS","BSC","Original","QALY","Resp_Moderate",0.0697268561
"ncd","CISS","BSC","Original","QALY","Resp_High",0.0616971055
"ncd","CISS","BSC","Duplicate","LY","NonResponder",4.29327106
"ncd","CISS","BSC","Duplicate","LY","Resp_Low",0.113110364
"ncd","CISS","BSC","Duplicate","LY","Resp_Moderate",0.0754069095
"ncd","CISS","BSC","Duplicate","LY","Resp_High",0.0646344938
"ncd","CISS","BSC","Duplicate","QALY","NonResponder",2.66182805
"ncd","CISS","BSC","Duplicate","QALY","Resp_Low",0.0869337764
"ncd","CISS","BSC","Duplicate","QALY","Resp_Moderate",0.0621571886
"ncd","CISS","BSC","Duplicate","QALY","Resp_High",0.0550781635
"ncd","CISS","Dravokimab","Original","LY","NonResponder",2.26955483
"ncd","CISS","Dravokimab","Original","LY","Resp_Low",0.553404767
"ncd","CISS","Dravokimab","Original","LY","Resp_Moderate",0.624811834
"ncd","CISS","Dravokimab","Original","LY","Resp_High",1.35673427
"ncd","CISS","Dravokimab","Original","QALY","NonResponder",1.407124
"ncd","CISS","Dravokimab","Original","QALY","Resp_Low",0.427861406
"ncd","CISS","Dravokimab","Original","QALY","Resp_Moderate",0.518951576
"ncd","CISS","Dravokimab","Original","QALY","Resp_High",1.16582415
"ncd","CISS","Dravokimab","Duplicate","LY","NonResponder",2.15499735
"ncd","CISS","Dravokimab","Duplicate","LY","Resp_Low",0.522071759
"ncd","CISS","Dravokimab","Duplicate","LY","Resp_Moderate",0.589435857
"ncd","CISS","Dravokimab","Duplicate","LY","Resp_High",1.27991786
"ncd","CISS","Dravokimab","Duplicate","QALY","NonResponder",1.33609835
"ncd","CISS","Dravokimab","Duplicate","QALY","Resp_Low",0.40530828
"ncd","CISS","Dravokimab","Duplicate","QALY","Resp_Moderate",0.492164581
"ncd","CISS","Dravokimab","Duplicate","QALY","Resp_High",1.10622085
"ncd","CISS","Barisetinib","Original","LY","NonResponder",3.52236871
"ncd","CISS","Barisetinib","Original","LY","Resp_Low",0.422682523
"ncd","CISS","Barisetinib","Original","LY","Resp_Moderate",0.380414271
"ncd","CISS","Barisetinib","Original","LY","Resp_High",0.479040193
"ncd","CISS","Barisetinib","Original","QALY","NonResponder",2.1838686
"ncd","CISS","Barisetinib","Original","QALY","Resp_Low",0.326020453
"ncd","CISS","Barisetinib","Original","QALY","Resp_Moderate",0.315003992
"ncd","CISS","Barisetinib","Original","QALY","Resp_High",0.410262618
"ncd","CISS","Barisetinib","Duplicate","LY","NonResponder",3.34427659
"ncd","CISS","Barisetinib","Duplicate","LY","Resp_Low",0.396311945
"ncd","CISS","Barisetinib","Duplicate","LY","Resp_Moderate",0.356680751
"ncd","CISS","Barisetinib","Duplicate","LY","Resp_High",0.449153538
"ncd","CISS","Barisetinib","Duplicate","QALY","NonResponder",2.07345149
"ncd","CISS","Barisetinib","Duplicate","QALY","Resp_Low",0.307277163
"ncd","CISS","Barisetinib","Duplicate","QALY","Resp_Moderate",0.297327215
"ncd","CISS","Barisetinib","Duplicate","QALY","Resp_High",0.387494347
"ncd","CISS","Tremacept","Original","LY","NonResponder",3.30848531
"ncd","CISS","Tremacept","Original","LY","Resp_Low",0.46650098
"ncd","CISS","Tremacept","Original","LY","Resp_Moderate",0.45041474
"ncd","CISS","Tremacept","Original","LY","Resp_High",0.579104665
"ncd","CISS","Tremacept","Original","QALY","NonResponder",2.05126089
"ncd","CISS","Tremacept","Original","QALY","Resp_Low",0.360321247
"ncd","CISS","Tremacept","Original","QALY","Resp_Moderate",0.37363609
"ncd","CISS","Tremacept","Original","QALY","Resp_High",0.496936218
"ncd","CISS","Tremacept","Duplicate","LY","NonResponder",3.13825692
"ncd","CISS","Tremacept","Duplicate","LY","Resp_Low",0.439105497
"ncd","CISS","Tremacept","Duplicate","LY","Resp_Moderate",0.423963928
"ncd","CISS","Tremacept","Duplicate","LY","Resp_High",0.545096479
"ncd","CISS","Tremacept","Duplicate","QALY","NonResponder",1.94571929
"ncd","CISS","Tremacept","Duplicate","QALY","Resp_Low",0.340717672
"ncd","CISS","Tremacept","Duplicate","QALY","Resp_Moderate",0.353760469
"ncd","CISS","Tremacept","Duplicate","QALY","Resp_High",0.470772398
"ncd","CISS","Uvestinib","Original","LY","NonResponder",2.620646
"ncd","CISS","Uvestinib","Original","LY","Resp_Low",0.366324853
"ncd","CISS","Uvestinib","Original","LY","Resp_Moderate",0.479040193
"ncd","CISS","Uvestinib","Original","LY","Resp_High",1.33849466
"ncd","CISS","Uvestinib","Original","QALY","NonResponder",1.62480052
"ncd","CISS","Uvestinib","Original","QALY","Resp_Low",0.282551059
"ncd","CISS","Uvestinib","Original","QALY","Resp_Moderate",0.396671694
"ncd","CISS","Uvestinib","Original","QALY","Resp_High",1.14632202
"ncd","CISS","Uvestinib","Duplicate","LY","NonResponder",2.49881111
"ncd","CISS","Uvestinib","Duplicate","LY","Resp_Low",0.343470352
"ncd","CISS","Uvestinib","Duplicate","LY","Resp_Moderate",0.449153538
"ncd","CISS","Uvestinib","Duplicate","LY","Resp_High",1.25498783
"ncd","CISS","Uvestinib","Duplicate","QALY","NonResponder",1.54926289
"ncd","CISS","Uvestinib","Duplicate","QALY","Resp_Low",0.266306875
"ncd","CISS","Uvestinib","Duplicate","QALY","Resp_Moderate",0.374412049
"ncd","CISS","Uvestinib","Duplicate","QALY","Resp_High",1.08270479
"ncd","CISS","Dorilumab","Original","LY","NonResponder",2.68607802
"ncd","CISS","Dorilumab","Original","LY","Resp_Low",0.52960692
"ncd","CISS","Dorilumab","Original","LY","Resp_Moderate",0.580859202
"ncd","CISS","Dorilumab","Original","LY","Resp_High",1.00796156
"ncd","CISS","Dorilumab","Original","QALY","NonResponder",1.66536837
"ncd","CISS","Dorilumab","Original","QALY","Resp_Low",0.409299085
"ncd","CISS","Dorilumab","Original","QALY","Resp_Moderate",0.482199662
"ncd","CISS","Dorilumab","Original","QALY","Resp_High",0.865643065
"ncd","CISS","Dorilumab","Duplicate","LY","NonResponder",2.54847989
"ncd","CISS","Dorilumab","Duplicate","LY","Resp_Low",0.499485732
"ncd","CISS","Dorilumab","Duplicate","LY","Resp_Moderate",0.547823061
"ncd","CISS","Dorilumab","Duplicate","LY","Resp_High",0.950634136
"ncd","CISS","Dorilumab","Duplicate","QALY","NonResponder",1.58005753
"ncd","CISS","Dorilumab","Duplicate","QALY","Resp_Low",0.387691179
"ncd","CISS","Dorilumab","Duplicate","QALY","Resp_Moderate",0.457294449
"ncd","CISS","Dorilumab","Duplicate","QALY","Resp_High",0.821378621
"ncd","CNRD","Velonix","Original","LY","No_Comorbidity",5.36859054
"ncd","CNRD","Velonix","Original","LY","MetabX_Only",7.01289554
"ncd","CNRD","Velonix","Original","LY","DiabX_Only",2.84293933
"ncd","CNRD","Velonix","Original","LY","CardioY_Only",1.11367873
"ncd","CNRD","Velonix","Original","LY","DiabX_CardioY",0.814135604
"ncd","CNRD","Velonix","Original","QALY","No_Comorbidity",3.81169929
"ncd","CNRD","Velonix","Original","QALY","MetabX_Only",4.59344658
"ncd","CNRD","Velonix","Original","QALY","DiabX_Only",1.74840769
"ncd","CNRD","Velonix","Original","QALY","CardioY_Only",0.684912418
"ncd","CNRD","Velonix","Original","QALY","DiabX_CardioY",0.423350514
"ncd","CNRD","Velonix","Original","QALY","Relapse_disutility",-0.877362983
"ncd","CNRD","Velonix","Duplicate","LY","No_Comorbidity",5.46665937
"ncd","CNRD","Velonix","Duplicate","LY","MetabX_Only",6.95654796
"ncd","CNRD","Velonix","Duplicate","LY","DiabX_Only",2.81257119
"ncd","CNRD","Velonix","Duplicate","LY","CardioY_Only",1.11010977
"ncd","CNRD","Velonix","Duplicate","LY","DiabX_CardioY",0.800736306
"ncd","CNRD","Velonix","Duplicate","QALY","No_Comorbidity",3.88132815
"ncd","CNRD","Velonix","Duplicate","QALY","MetabX_Only",4.55653891
"ncd","CNRD","Velonix","Duplicate","QALY","DiabX_Only",1.72973128
"ncd","CNRD","Velonix","Duplicate","QALY","CardioY_Only",0.682717506
"ncd","CNRD","Velonix","Duplicate","QALY","DiabX_CardioY",0.416382879
"ncd","CNRD","Velonix","Duplicate","QALY","Relapse_disutility",-0.799172841
"ncd","CNRD","Doralene","Original","LY","No_Comorbidity",4.24487827
"ncd","CNRD","Doralene","Original","LY","MetabX_Only",7.51974442
"ncd","CNRD","Doralene","Original","LY","DiabX_Only",3.13155153
"ncd","CNRD","Doralene","Original","LY","CardioY_Only",1.22098275
"ncd","CNRD","Doralene","Original","LY","DiabX_CardioY",0.920219288
"ncd","CNRD","Doralene","Original","QALY","No_Comorbidity",3.01386357
"ncd","CNRD","Doralene","Original","QALY","MetabX_Only",4.9254326
"ncd","CNRD","Doralene","Original","QALY","DiabX_Only",1.92590419
"ncd","CNRD","Doralene","Original","QALY","CardioY_Only",0.750904389
"ncd","CNRD","Doralene","Original","QALY","DiabX_CardioY",0.47851403
"ncd","CNRD","Doralene","Original","QALY","Relapse_disutility",-0.882897493
"ncd","CNRD","Doralene","Duplicate","LY","No_Comorbidity",4.27939858
"ncd","CNRD","Doralene","Duplicate","LY","MetabX_Only",7.49169557
"ncd","CNRD","Doralene","Duplicate","LY","DiabX_Only",3.11705345
"ncd","CNRD","Doralene","Duplicate","LY","CardioY_Only",1.22416635
"ncd","CNRD","Doralene","Duplicate","LY","DiabX_CardioY",0.912149277
"ncd","CNRD","Doralene","Duplicate","QALY","No_Comorbidity",3.03837299
"ncd","CNRD","Doralene","Duplicate","QALY","MetabX_Only",4.9070606
"ncd","CNRD","Doralene","Duplicate","QALY","DiabX_Only",1.91698787
"ncd","CNRD","Doralene","Duplicate","QALY","CardioY_Only",0.752862305
"ncd","CNRD","Doralene","Duplicate","QALY","DiabX_CardioY",0.474317624
"ncd","CNRD","Doralene","Duplicate","QALY","Relapse_disutility",-0.806203078
"ncd","DMSS","Cardivex","Original","LY","Grade_I",8.34003055
"ncd","DMSS","Cardivex","Original","LY","Grade_II",7.61046038
"ncd","DMSS","Cardivex","Original","LY","Grade_III",1.50149579
"ncd","DMSS","Cardivex","Original","QALY","Grade_I",7.67197279
"ncd","DMSS","Cardivex","Original","QALY","Grade_II",6.39253466
"ncd","DMSS","Cardivex","Original","QALY","Grade_III",1.02111183
"ncd","DMSS","Cardivex","Duplicate","LY","Grade_I",8.13398059
"ncd","DMSS","Cardivex","Duplicate","LY","Grade_II",7.73449245
"ncd","DMSS","Cardivex","Duplicate","LY","Grade_III",1.61125006
"ncd","DMSS","Cardivex","Duplicate","QALY","Grade_I",7.48011692
"ncd","DMSS","Cardivex","Duplicate","QALY","Grade_II",6.49768724
"ncd","DMSS","Cardivex","Duplicate","QALY","Grade_III",1.09677023
"ncd","DMSS","SoC","Original","LY","Grade_I",3.822514
"ncd","DMSS","SoC","Original","LY","Grade_II",9.78234333
"ncd","DMSS","SoC","Original","LY","Grade_III",3.84712939
"ncd","DMSS","SoC","Original","QALY","Grade_I",3.51632086
"ncd","DMSS","SoC","Original","QALY","Grade_II",8.02104674
"ncd","DMSS","SoC","Original","QALY","Grade_III",2.57743082
"ncd","DMSS","SoC","Duplicate","LY","Grade_I",3.72807444
"ncd","DMSS","SoC","Duplicate","LY","Grade_II",9.85271656
"ncd","DMSS","SoC","Duplicate","LY","Grade_III",3.8989321
"ncd","DMSS","SoC","Duplicate","QALY","Grade_I",3.42838692
"ncd","DMSS","SoC","Duplicate","QALY","Grade_II",8.0791221
"ncd","DMSS","SoC","Duplicate","QALY","Grade_III",2.6125201
"ncd","DMSS","Zonapride","Original","LY","Grade_I",5.21251909
"ncd","DMSS","Zonapride","Original","LY","Grade_II",8.32283799
"ncd","DMSS","Zonapride","Original","LY","Grade_III",3.91662964
"ncd","DMSS","Zonapride","Original","QALY","Grade_I",4.79498299
"ncd","DMSS","Zonapride","Original","QALY","Grade_II",6.90763042
"ncd","DMSS","Zonapride","Original","QALY","Grade_III",2.66315516
"ncd","DMSS","Zonapride","Duplicate","LY","Grade_I",5.24391693
"ncd","DMSS","Zonapride","Duplicate","LY","Grade_II",8.32034819
"ncd","DMSS","Zonapride","Duplicate","LY","Grade_III",3.91545797
"ncd","DMSS","Zonapride","Duplicate","QALY","Grade_I",4.82401014
"ncd","DMSS","Zonapride","Duplicate","QALY","Grade_II",6.90526475
"ncd","DMSS","Zonapride","Duplicate","QALY","Grade_III",2.66221766
"ncd","DMSS","Septoplasty","Original","LY","Grade_I",12.3724353
"ncd","DMSS","Septoplasty","Original","LY","Grade_II",3.32412948
"ncd","DMSS","Septoplasty","Original","LY","Grade_III",1.56429622
"ncd","DMSS","Septoplasty","Original","QALY","Grade_I",11.3349012
"ncd","DMSS","Septoplasty","Original","QALY","Grade_II",2.74263691
"ncd","DMSS","Septoplasty","Original","QALY","Grade_III",1.05600823
"ncd","DMSS","Septoplasty","Duplicate","LY","Grade_I",12.4475705
"ncd","DMSS","Septoplasty","Duplicate","LY","Grade_II",3.28477554
"ncd","DMSS","Septoplasty","Duplicate","LY","Grade_III",1.55594631
"ncd","DMSS","Septoplasty","Duplicate","QALY","Grade_I",11.3999079
"ncd","DMSS","Septoplasty","Duplicate","QALY","Grade_II",2.71267922
"ncd","DMSS","Septoplasty","Duplicate","QALY","Grade_III",1.05156137
"ncd","DMSS","Ablatherm","Original","LY","Grade_I",12.3974554
"ncd","DMSS","Ablatherm","Original","LY","Grade_II",3.3307459
"ncd","DMSS","Ablatherm","Original","LY","Grade_III",1.56740984
"ncd","DMSS","Ablatherm","Original","QALY","Grade_I",11.3426905
"ncd","DMSS","Ablatherm","Original","QALY","Grade_II",2.74813944
"ncd","DMSS","Ablatherm","Original","QALY","Grade_III",1.05813061
"ncd","DMSS","Ablatherm","Duplicate","LY","Grade_I",12.4726305
"ncd","DMSS","Ablatherm","Duplicate","LY","Grade_II",3.2913886
"ncd","DMSS","Ablatherm","Duplicate","LY","Grade_III",1.55907881
"ncd","DMSS","Ablatherm","Duplicate","QALY","Grade_I",11.4118314
"ncd","DMSS","Ablatherm","Duplicate","QALY","Grade_II",2.71523053
"ncd","DMSS","Ablatherm","Duplicate","QALY","Grade_III",1.05230001
"ncd","NCD","UsualCare","Original","LY","Remission",3.1990344
"ncd","NCD","UsualCare","Original","LY","Mild",5.64837475
"ncd","NCD","UsualCare","Original","LY","Moderate",6.45528543
"ncd","NCD","UsualCare","Original","LY","Severe",5.04319174
"ncd","NCD","UsualCare","Original","QALY","Remission",2.81515027
"ncd","NCD","UsualCare","Original","QALY","Mild",4.46221605
"ncd","NCD","UsualCare","Original","QALY","Moderate",4.26048838
"ncd","NCD","UsualCare","Original","QALY","Severe",2.42073203
"ncd","NCD","Zelravine","Original","LY","Remission",5.116672
"ncd","NCD","Zelravine","Original","LY","Mild",6.05249322
"ncd","NCD","Zelravine","Original","LY","Moderate",5.64899368
"ncd","NCD","Zelravine","Original","LY","Severe",3.63149593
"ncd","NCD","Zelravine","Original","QALY","Remission",4.50267136
"ncd","NCD","Zelravine","Original","QALY","Mild",4.78146965
"ncd","NCD","Zelravine","Original","QALY","Moderate",3.72833583
"ncd","NCD","Zelravine","Original","QALY","Severe",1.74311805
"ncd","NCD","UsualCare","Duplicate","LY","Remission",3.1990344
"ncd","NCD","UsualCare","Duplicate","LY","Mild",5.64837475
"ncd","NCD","UsualCare","Duplicate","LY","Moderate",6.45528543
"ncd","NCD","UsualCare","Duplicate","LY","Severe",5.04319174
"ncd","NCD","UsualCare","Duplicate","QALY","Remission",2.81515027
"ncd","NCD","UsualCare","Duplicate","QALY","Mild",4.46221605
"ncd","NCD","UsualCare","Duplicate","QALY","Moderate",4.26048838
"ncd","NCD","UsualCare","Duplicate","QALY","Severe",2.42073203
"ncd","NCD","Zelravine","Duplicate","LY","Remission",5.116672
"ncd","NCD","Zelravine","Duplicate","LY","Mild",6.05249322
"ncd","NCD","Zelravine","Duplicate","LY","Moderate",5.64899368
"ncd","NCD","Zelravine","Duplicate","LY","Severe",3.63149593
"ncd","NCD","Zelravine","Duplicate","QALY","Remission",4.50267136
"ncd","NCD","Zelravine","Duplicate","QALY","Mild",4.78146965
"ncd","NCD","Zelravine","Duplicate","QALY","Moderate",3.72833583
"ncd","NCD","Zelravine","Duplicate","QALY","Severe",1.74311805
"own","CAIS","UsualCare","Original","LY","Stable",6.41736179
"own","CAIS","UsualCare","Original","LY","Stable_Exac",2.514303
"own","CAIS","UsualCare","Original","LY","PathA",1.70620472
"own","CAIS","UsualCare","Original","LY","PathA_Exac",0.893597854
"own","CAIS","UsualCare","Original","LY","PathB",1.35718057
"own","CAIS","UsualCare","Original","LY","PathB_Exac",0.686704925
"own","CAIS","UsualCare","Original","QALY","Stable",4.62050049
"own","CAIS","UsualCare","Original","QALY","Stable_Exac",1.48444449
"own","CAIS","UsualCare","Original","QALY","PathA",1.08105131
"own","CAIS","UsualCare","Original","QALY","PathA_Exac",0.464270552
"own","CAIS","UsualCare","Original","QALY","PathB",0.840366206
"own","CAIS","UsualCare","Original","QALY","PathB_Exac",0.348670305
"own","CAIS","UsualCare","Duplicate","LY","Stable",6.67306693
"own","CAIS","UsualCare","Duplicate","LY","Stable_Exac",2.59675211
"own","CAIS","UsualCare","Duplicate","LY","PathA",1.38852618
"own","CAIS","UsualCare","Duplicate","LY","PathA_Exac",0.712703298
"own","CAIS","UsualCare","Duplicate","LY","PathB",1.05656194
"own","CAIS","UsualCare","Duplicate","LY","PathB_Exac",0.562842435
"own","CAIS","UsualCare","Duplicate","QALY","Stable",4.80460819
"own","CAIS","UsualCare","Duplicate","QALY","Stable_Exac",1.53312244
"own","CAIS","UsualCare","Duplicate","QALY","PathA",0.879770189
"own","CAIS","UsualCare","Duplicate","QALY","PathA_Exac",0.370286424
"own","CAIS","UsualCare","Duplicate","QALY","PathB",0.654223156
"own","CAIS","UsualCare","Duplicate","QALY","PathB_Exac",0.285779869
"own","CAIS","Infectaguard","Original","LY","Stable",8.86553671
"own","CAIS","Infectaguard","Original","LY","Stable_Exac",1.90967645
"own","CAIS","Infectaguard","Original","LY","PathA",1.30417862
"own","CAIS","Infectaguard","Original","LY","PathA_Exac",0.372734487
"own","CAIS","Infectaguard","Original","LY","PathB",1.08806756
"own","CAIS","Infectaguard","Original","LY","PathB_Exac",0.300633762
"own","CAIS","Infectaguard","Original","QALY","Stable",6.38318643
"own","CAIS","Infectaguard","Original","QALY","Stable_Exac",1.12747297
"own","CAIS","Infectaguard","Original","QALY","PathA",0.826327573
"own","CAIS","Infectaguard","Original","QALY","PathA_Exac",0.193654948
"own","CAIS","Infectaguard","Original","QALY","PathB",0.673731432
"own","CAIS","Infectaguard","Original","QALY","PathB_Exac",0.152644989
"own","CAIS","Infectaguard","Duplicate","LY","Stable",8.76798706
"own","CAIS","Infectaguard","Duplicate","LY","Stable_Exac",1.88453224
"own","CAIS","Infectaguard","Duplicate","LY","PathA",1.10774295
"own","CAIS","Infectaguard","Duplicate","LY","PathA_Exac",0.306399344
"own","CAIS","Infectaguard","Duplicate","LY","PathB",0.898116366
"own","CAIS","Infectaguard","Duplicate","LY","PathB_Exac",0.257629825
"own","CAIS","Infectaguard","Duplicate","QALY","Stable",6.31295068
"own","CAIS","Infectaguard","Duplicate","QALY","Stable_Exac",1.11262784
"own","CAIS","Infectaguard","Duplicate","QALY","PathA",0.701865931
"own","CAIS","Infectaguard","Duplicate","QALY","PathA_Exac",0.159190392
"own","CAIS","Infectaguard","Duplicate","QALY","PathB",0.556113654
"own","CAIS","Infectaguard","Duplicate","QALY","PathB_Exac",0.130809998
"own","CISS","BSC","Original","LY","NonResponder",4.55697465
"own","CISS","BSC","Original","LY","Resp_Low",0.129215811
"own","CISS","BSC","Original","LY","Resp_Moderate",0.086143874
"own","CISS","BSC","Original","LY","Resp_High",0.0738376063
"own","CISS","BSC","Original","QALY","NonResponder",2.82532428
"own","CISS","BSC","Original","QALY","Resp_Low",0.0982154599
"own","CISS","BSC","Original","QALY","Resp_Moderate",0.0700023875
"own","CISS","BSC","Original","QALY","Resp_High",0.0619415097
"own","CISS","BSC","Duplicate","LY","NonResponder",4.29327106
"own","CISS","BSC","Duplicate","LY","Resp_Low",0.113110364
"own","CISS","BSC","Duplicate","LY","Resp_Moderate",0.0754069095
"own","CISS","BSC","Duplicate","LY","Resp_High",0.0646344938
"own","CISS","BSC","Duplicate","QALY","NonResponder",2.66182805
"own","CISS","BSC","Duplicate","QALY","Resp_Low",0.0869337764
"own","CISS","BSC","Duplicate","QALY","Resp_Moderate",0.0621571886
"own","CISS","BSC","Duplicate","QALY","Resp_High",0.0550781635
"own","CISS","Dravokimab","Original","LY","NonResponder",2.29166603
"own","CISS","Dravokimab","Original","LY","Resp_Low",0.557673825
"own","CISS","Dravokimab","Original","LY","Resp_Moderate",0.629631738
"own","CISS","Dravokimab","Original","LY","Resp_High",1.36720035
"own","CISS","Dravokimab","Original","QALY","NonResponder",1.42083294
"own","CISS","Dravokimab","Original","QALY","Resp_Low",0.431187534
"own","CISS","Dravokimab","Original","QALY","Resp_Moderate",0.522994494
"own","CISS","Dravokimab","Original","QALY","Resp_High",1.17491532
"own","CISS","Dravokimab","Duplicate","LY","NonResponder",2.15499735
"own","CISS","Dravokimab","Duplicate","LY","Resp_Low",0.522071759
"own","CISS","Dravokimab","Duplicate","LY","Resp_Moderate",0.589435857
"own","CISS","Dravokimab","Duplicate","LY","Resp_High",1.27991786
"own","CISS","Dravokimab","Duplicate","QALY","NonResponder",1.33609835
"own","CISS","Dravokimab","Duplicate","QALY","Resp_Low",0.40530828
"own","CISS","Dravokimab","Duplicate","QALY","Resp_Moderate",0.492164581
"own","CISS","Dravokimab","Duplicate","QALY","Resp_High",1.10622085
"own","CISS","Barisetinib","Original","LY","NonResponder",3.55520232
"own","CISS","Barisetinib","Original","LY","Resp_Low",0.425594381
"own","CISS","Barisetinib","Original","LY","Resp_Moderate",0.383034943
"own","CISS","Barisetinib","Original","LY","Resp_High",0.482340299
"own","CISS","Barisetinib","Original","QALY","NonResponder",2.20422544
"own","CISS","Barisetinib","Original","QALY","Resp_Low",0.328288085
"own","CISS","Barisetinib","Original","QALY","Resp_Moderate",0.317200881
"own","CISS","Barisetinib","Original","QALY","Resp_High",0.413127305
"own","CISS","Barisetinib","Duplicate","LY","NonResponder",3.34427659
"own","CISS","Barisetinib","Duplicate","LY","Resp_Low",0.396311945
"own","CISS","Barisetinib","Duplicate","LY","Resp_Moderate",0.356680751
"own","CISS","Barisetinib","Duplicate","LY","Resp_High",0.449153538
"own","CISS","Barisetinib","Duplicate","QALY","NonResponder",2.07345149
"own","CISS","Barisetinib","Duplicate","QALY","Resp_Low",0.307277163
"own","CISS","Barisetinib","Duplicate","QALY","Resp_Moderate",0.297327215
"own","CISS","Barisetinib","Duplicate","QALY","Resp_High",0.387494347
"own","CISS","Tremacept","Original","LY","NonResponder",3.33914521
"own","CISS","Tremacept","Original","LY","Resp_Low",0.469933068
"own","CISS","Tremacept","Original","LY","Resp_Moderate",0.453728479
"own","CISS","Tremacept","Original","LY","Resp_High",0.583365188
"own","CISS","Tremacept","Original","QALY","NonResponder",2.07027003
"own","CISS","Tremacept","Original","QALY","Resp_Low",0.362994778
"own","CISS","Tremacept","Original","QALY","Resp_Moderate",0.376414989
"own","CISS","Tremacept","Original","QALY","Resp_High",0.50063609
"own","CISS","Tremacept","Duplicate","LY","NonResponder",3.13825692
"own","CISS","Tremacept","Duplicate","LY","Resp_Low",0.439105497
"own","CISS","Tremacept","Duplicate","LY","Resp_Moderate",0.423963928
"own","CISS","Tremacept","Duplicate","LY","Resp_High",0.545096479
"own","CISS","Tremacept","Duplicate","QALY","NonResponder",1.94571929
"own","CISS","Tremacept","Duplicate","QALY","Resp_Low",0.340717672
"own","CISS","Tremacept","Duplicate","QALY","Resp_Moderate",0.353760469
"own","CISS","Tremacept","Duplicate","QALY","Resp_High",0.470772398
"own","CISS","Uvestinib","Original","LY","NonResponder",2.64726764
"own","CISS","Uvestinib","Original","LY","Resp_Low",0.368848464
"own","CISS","Uvestinib","Original","LY","Resp_Moderate",0.482340299
"own","CISS","Uvestinib","Original","LY","Resp_High",1.34771554
"own","CISS","Uvestinib","Original","QALY","NonResponder",1.64130594
"own","CISS","Uvestinib","Original","QALY","Resp_Low",0.28451634
"own","CISS","Uvestinib","Original","QALY","Resp_Moderate",0.399438147
"own","CISS","Uvestinib","Original","QALY","Resp_High",1.15432629
"own","CISS","Uvestinib","Duplicate","LY","NonResponder",2.49881111
"own","CISS","Uvestinib","Duplicate","LY","Resp_Low",0.343470352
"own","CISS","Uvestinib","Duplicate","LY","Resp_Moderate",0.449153538
"own","CISS","Uvestinib","Duplicate","LY","Resp_High",1.25498783
"own","CISS","Uvestinib","Duplicate","QALY","NonResponder",1.54926289
"own","CISS","Uvestinib","Duplicate","QALY","Resp_Low",0.266306875
"own","CISS","Uvestinib","Duplicate","QALY","Resp_Moderate",0.374412049
"own","CISS","Uvestinib","Duplicate","QALY","Resp_High",1.08270479
"own","CISS","Dorilumab","Original","LY","NonResponder",2.71183912
"own","CISS","Dorilumab","Original","LY","Resp_Low",0.533583205
"own","CISS","Dorilumab","Original","LY","Resp_Moderate",0.58522029
"own","CISS","Dorilumab","Original","LY","Resp_High",1.01552933
"own","CISS","Dorilumab","Original","QALY","NonResponder",1.68134025
"own","CISS","Dorilumab","Original","QALY","Resp_Low",0.41239685
"own","CISS","Dorilumab","Original","QALY","Resp_Moderate",0.485857339
"own","CISS","Dorilumab","Original","QALY","Resp_High",0.872215909
"own","CISS","Dorilumab","Duplicate","LY","NonResponder",2.54847989
"own","CISS","Dorilumab","Duplicate","LY","Resp_Low",0.499485732
"own","CISS","Dorilumab","Duplicate","LY","Resp_Moderate",0.547823061
"own","CISS","Dorilumab","Duplicate","LY","Resp_High",0.950634136
"own","CISS","Dorilumab","Duplicate","QALY","NonResponder",1.58005753
"own","CISS","Dorilumab","Duplicate","QALY","Resp_Low",0.387691179
"own","CISS","Dorilumab","Duplicate","QALY","Resp_Moderate",0.457294449
"own","CISS","Dorilumab","Duplicate","QALY","Resp_High",0.821378621
"own","CNRD","Velonix","Original","LY","No_Comorbidity",5.39234826
"own","CNRD","Velonix","Original","LY","MetabX_Only",7.15370216
"own","CNRD","Velonix","Original","LY","DiabX_Only",3.01227452
"own","CNRD","Velonix","Original","LY","CardioY_Only",1.17114283
"own","CNRD","Velonix","Original","LY","DiabX_CardioY",0.900732042
"own","CNRD","Velonix","Original","QALY","No_Comorbidity",3.82856726
"own","CNRD","Velonix","Original","QALY","MetabX_Only",4.68567492
"own","CNRD","Velonix","Original","QALY","DiabX_Only",1.85254883
"own","CNRD","Velonix","Original","QALY","CardioY_Only",0.720252839
"own","CNRD","Velonix","Original","QALY","DiabX_CardioY",0.468380662
"own","CNRD","Velonix","Original","QALY","Relapse_disutility",-0.907472927
"own","CNRD","Velonix","Duplicate","LY","No_Comorbidity",5.46665937
"own","CNRD","Velonix","Duplicate","LY","MetabX_Only",6.95654796
"own","CNRD","Velonix","Duplicate","LY","DiabX_Only",2.81257119
"own","CNRD","Velonix","Duplicate","LY","CardioY_Only",1.11010977
"own","CNRD","Velonix","Duplicate","LY","DiabX_CardioY",0.800736306
"own","CNRD","Velonix","Duplicate","QALY","No_Comorbidity",3.88132815
"own","CNRD","Velonix","Duplicate","QALY","MetabX_Only",4.55653891
"own","CNRD","Velonix","Duplicate","QALY","DiabX_Only",1.72973128
"own","CNRD","Velonix","Duplicate","QALY","CardioY_Only",0.682717506
"own","CNRD","Velonix","Duplicate","QALY","DiabX_CardioY",0.416382879
"own","CNRD","Velonix","Duplicate","QALY","Relapse_disutility",-0.799172841
"own","CNRD","Doralene","Original","LY","No_Comorbidity",4.25956985
"own","CNRD","Doralene","Original","LY","MetabX_Only",7.65402005
"own","CNRD","Doralene","Original","LY","DiabX_Only",3.30862714
"own","CNRD","Doralene","Original","LY","CardioY_Only",1.28025607
"own","CNRD","Doralene","Original","LY","DiabX_CardioY",1.01511633
"own","CNRD","Doralene","Original","QALY","No_Comorbidity",3.02429459
"own","CNRD","Doralene","Original","QALY","MetabX_Only",5.01338313
"own","CNRD","Doralene","Original","QALY","DiabX_Only",2.03480569
"own","CNRD","Doralene","Original","QALY","CardioY_Only",0.787357481
"own","CNRD","Doralene","Original","QALY","DiabX_CardioY",0.52786049
"own","CNRD","Doralene","Original","QALY","Relapse_disutility",-0.913145343
"own","CNRD","Doralene","Duplicate","LY","No_Comorbidity",4.27939858
"own","CNRD","Doralene","Duplicate","LY","MetabX_Only",7.49169557
"own","CNRD","Doralene","Duplicate","LY","DiabX_Only",3.11705345
"own","CNRD","Doralene","Duplicate","LY","CardioY_Only",1.22416635
"own","CNRD","Doralene","Duplicate","LY","DiabX_CardioY",0.912149277
"own","CNRD","Doralene","Duplicate","QALY","No_Comorbidity",3.03837299
"own","CNRD","Doralene","Duplicate","QALY","MetabX_Only",4.9070606
"own","CNRD","Doralene","Duplicate","QALY","DiabX_Only",1.91698787
"own","CNRD","Doralene","Duplicate","QALY","CardioY_Only",0.752862305
"own","CNRD","Doralene","Duplicate","QALY","DiabX_CardioY",0.474317624
"own","CNRD","Doralene","Duplicate","QALY","Relapse_disutility",-0.806203078
"own","DMSS","Cardivex","Original","LY","Grade_I",8.50876246
"own","DMSS","Cardivex","Original","LY","Grade_II",7.76337368
"own","DMSS","Cardivex","Original","LY","Grade_III",1.5313754
"own","DMSS","Cardivex","Original","QALY","Grade_I",7.82658874
"own","DMSS","Cardivex","Original","QALY","Grade_II",6.5204223
"own","DMSS","Cardivex","Original","QALY","Grade_III",1.04132063
"own","DMSS","Cardivex","Duplicate","LY","Grade_I",8.13398059
"own","DMSS","Cardivex","Duplicate","LY","Grade_II",7.73449245
"own","DMSS","Cardivex","Duplicate","LY","Grade_III",1.61125006
"own","DMSS","Cardivex","Duplicate","QALY","Grade_I",7.48011692
"own","DMSS","Cardivex","Duplicate","QALY","Grade_II",6.49768724
"own","DMSS","Cardivex","Duplicate","QALY","Grade_III",1.09677023
"own","DMSS","SoC","Original","LY","Grade_I",3.89984946
"own","DMSS","SoC","Original","LY","Grade_II",9.97919723
"own","DMSS","SoC","Original","LY","Grade_III",3.92446485
"own","DMSS","SoC","Original","QALY","Grade_I",3.58718651
"own","DMSS","SoC","Original","QALY","Grade_II",8.18174663
"own","DMSS","SoC","Original","QALY","Grade_III",2.6289626
"own","DMSS","SoC","Duplicate","LY","Grade_I",3.72807444
"own","DMSS","SoC","Duplicate","LY","Grade_II",9.85271656
"own","DMSS","SoC","Duplicate","LY","Grade_III",3.8989321
"own","DMSS","SoC","Duplicate","QALY","Grade_I",3.42838692
"own","DMSS","SoC","Duplicate","QALY","Grade_II",8.0791221
"own","DMSS","SoC","Duplicate","QALY","Grade_III",2.6125201
"own","DMSS","Zonapride","Original","LY","Grade_I",5.31797654
"own","DMSS","Zonapride","Original","LY","Grade_II",8.4901638
"own","DMSS","Zonapride","Original","LY","Grade_III",3.9953712
"own","DMSS","Zonapride","Original","QALY","Grade_I",4.89161796
"own","DMSS","Zonapride","Original","QALY","Grade_II",7.04589858
"own","DMSS","Zonapride","Original","QALY","Grade_III",2.7164113
"own","DMSS","Zonapride","Duplicate","LY","Grade_I",5.24391693
"own","DMSS","Zonapride","Duplicate","LY","Grade_II",8.32034819
"own","DMSS","Zonapride","Duplicate","LY","Grade_III",3.91545797
"own","DMSS","Zonapride","Duplicate","QALY","Grade_I",4.82401014
"own","DMSS","Zonapride","Duplicate","QALY","Grade_II",6.90526475
"own","DMSS","Zonapride","Duplicate","QALY","Grade_III",2.66221766
"own","DMSS","Septoplasty","Original","LY","Grade_I",12.6227491
"own","DMSS","Septoplasty","Original","LY","Grade_II",3.39032357
"own","DMSS","Septoplasty","Original","LY","Grade_III",1.59544639
"own","DMSS","Septoplasty","Original","QALY","Grade_I",11.5637691
"own","DMSS","Septoplasty","Original","QALY","Grade_II",2.79720228
"own","DMSS","Septoplasty","Original","QALY","Grade_III",1.07701353
"own","DMSS","Septoplasty","Duplicate","LY","Grade_I",12.4475705
"own","DMSS","Septoplasty","Duplicate","LY","Grade_II",3.28477554
"own","DMSS","Septoplasty","Duplicate","LY","Grade_III",1.55594631
"own","DMSS","Septoplasty","Duplicate","QALY","Grade_I",11.3999079
"own","DMSS","Septoplasty","Duplicate","QALY","Grade_II",2.71267922
"own","DMSS","Septoplasty","Duplicate","QALY","Grade_III",1.05156137
"own","DMSS","Ablatherm","Original","LY","Grade_I",12.6482754
"own","DMSS","Ablatherm","Original","LY","Grade_II",3.39707385
"own","DMSS","Ablatherm","Original","LY","Grade_III",1.59862299
"own","DMSS","Ablatherm","Original","QALY","Grade_I",11.571323
"own","DMSS","Ablatherm","Original","QALY","Grade_II",2.80263052
"own","DMSS","Ablatherm","Original","QALY","Grade_III",1.0790915
"own","DMSS","Ablatherm","Duplicate","LY","Grade_I",12.4726305
"own","DMSS","Ablatherm","Duplicate","LY","Grade_II",3.2913886
"own","DMSS","Ablatherm","Duplicate","LY","Grade_III",1.55907881
"own","DMSS","Ablatherm","Duplicate","QALY","Grade_I",11.4118314
"own","DMSS","Ablatherm","Duplicate","QALY","Grade_II",2.71523053
"own","DMSS","Ablatherm","Duplicate","QALY","Grade_III",1.05230001
"own","NCD","UsualCare","Original","LY","Remission",3.1990344
"own","NCD","UsualCare","Original","LY","Mild",5.64837475
"own","NCD","UsualCare","Original","LY","Moderate",6.45528543
"own","NCD","UsualCare","Original","LY","Severe",5.04319174
"own","NCD","UsualCare","Original","QALY","Remission",2.81515027
"own","NCD","UsualCare","Original","QALY","Mild",4.46221605
"own","NCD","UsualCare","Original","QALY","Moderate",4.26048838
"own","NCD","UsualCare","Original","QALY","Severe",2.42073203
"own","NCD","Zelravine","Original","LY","Remission",5.116672
"own","NCD","Zelravine","Original","LY","Mild",6.05249322
"own","NCD","Zelravine","Original","LY","Moderate",5.64899368
"own","NCD","Zelravine","Original","LY","Severe",3.63149593
"own","NCD","Zelravine","Original","QALY","Remission",4.50267136
"own","NCD","Zelravine","Original","QALY","Mild",4.78146965
"own","NCD","Zelravine","Original","QALY","Moderate",3.72833583
"own","NCD","Zelravine","Original","QALY","Severe",1.74311805
"own","NCD","UsualCare","Duplicate","LY","Remission",3.1990344
"own","NCD","UsualCare","Duplicate","LY","Mild",5.64837475
"own","NCD","UsualCare","Duplicate","LY","Moderate",6.45528543
"own","NCD","UsualCare","Duplicate","LY","Severe",5.04319174
"own","NCD","UsualCare","Duplicate","QALY","Remission",2.81515027
"own","NCD","UsualCare","Duplicate","QALY","Mild",4.46221605
"own","NCD","UsualCare","Duplicate","QALY","Moderate",4.26048838
"own","NCD","UsualCare","Duplicate","QALY","Severe",2.42073203
"own","NCD","Zelravine","Duplicate","LY","Remission",5.116672
"own","NCD","Zelravine","Duplicate","LY","Mild",6.05249322
"own","NCD","Zelravine","Duplicate","LY","Moderate",5.64899368
"own","NCD","Zelravine","Duplicate","LY","Severe",3.63149593
"own","NCD","Zelravine","Duplicate","QALY","Remission",4.50267136
"own","NCD","Zelravine","Duplicate","QALY","Mild",4.78146965
"own","NCD","Zelravine","Duplicate","QALY","Moderate",3.72833583
"own","NCD","Zelravine","Duplicate","QALY","Severe",1.74311805)---")

COMP <- c(CAIS = "UsualCare", CISS = "BSC", CNRD = "Doralene", DMSS = "SoC", NCD = "UsualCare")
MODELS <- names(COMP)
COL <- c(Original = "#4C78A8", Duplicate = "#E8833A")
METRICS <- c(LYd = "LY (discounted)", LYu = "LY (undiscounted)", Q = "QALY", C = "Cost ($)")

# ---- Helpers -----------------------------------------------------------------
tot_raw   <- function(basis = BASIS) if (basis == "ncd") tot_ncd else tot_own
parts_raw <- function(basis = BASIS) if (basis == "ncd") parts_ncd else parts_own
arm_levels <- function() unique(tot_ncd$arm)

# Long form: model, arm, metric, Original, Duplicate
tidy_tot <- function(basis = BASIS) {
  tot_raw(basis) |>
    pivot_longer(-c(model, arm), names_to = c("metric", ".v"), names_sep = "_") |>
    pivot_wider(names_from = .v, values_from = value) |>
    transmute(model, arm = factor(arm, arm_levels()),
              metric = factor(metric, names(METRICS), METRICS),
              Original = O, Duplicate = D, Delta = D - O)
}

show_tbl <- function(df, title, group = "model", digits = 3) {
  if (TABLE_MODE == "gt") {
    g <- gt::gt(df, groupname_col = group) |>
      gt::tab_header(title = title, subtitle = if (BASIS == "ncd")
        "Original recalculated on the NCD life table" else "Original with its own life table") |>
      gt::fmt_number(columns = tidyselect::where(is.numeric) & !tidyselect::contains("Cost"), decimals = digits) |>
      gt::fmt_number(columns = tidyselect::contains("Cost") & tidyselect::where(is.numeric), decimals = 0) |>
      gt::sub_missing(missing_text = "-")
    print(g)
  } else {
    View(df, title)
  }
  invisible(df)
}

# ==== TABLES ====================================================================

# Outputs per arm: LYs, QALYs, cost for Original, Duplicate and difference (Duplicate - Original)
tbl_outputs <- function(model = NULL, basis = BASIS) {
  d <- tot_raw(basis) |>
    transmute(model, arm,
      `LY disc. Orig` = LYd_O, `LY disc. Dup` = LYd_D, `LY disc. Diff` = LYd_D - LYd_O,
      `LY undisc. Orig` = LYu_O, `LY undisc. Dup` = LYu_D, `LY undisc. Diff` = LYu_D - LYu_O,
      `QALY Orig` = Q_O, `QALY Dup` = Q_D, `QALY Diff` = Q_D - Q_O,
      `Cost Orig` = C_O, `Cost Dup` = C_D, `Cost Diff` = C_D - C_O)
  if (!is.null(model)) d <- filter(d, model %in% !!model)
  d
}

# Cost components per arm
tbl_components <- function(model = NULL, basis = BASIS) {
  d <- parts_raw(basis) |>
    filter(O != 0 | D != 0) |>
    transmute(model, arm, component = part, `Cost Orig` = O, `Cost Dup` = D, `Cost Diff` = D - O)
  if (!is.null(model)) d <- filter(d, model %in% !!model)
  d
}

# Incremental results vs the model's comparator, with ICERs
tbl_incremental <- function(model = NULL, basis = BASIS) {
  d <- tot_raw(basis) |> filter(arm != COMP[model]) |> 
    left_join(tot_raw(basis) |> mutate(comp = COMP[model]) |> filter(arm == comp) |>
                select(model, cO = C_O, cD = C_D, qO = Q_O, qD = Q_D), by = "model")
  d |> transmute(model, comparison = paste(arm, "vs", COMP[model]),
      `Cost inc. Orig` = C_O - cO, `Cost inc. Dup` = C_D - cD,
      `QALY inc. Orig` = Q_O - qO, `QALY inc. Dup` = Q_D - qD,
      `ICER Orig` = icer_txt(`Cost inc. Orig`, `QALY inc. Orig`),
      `ICER Dup`  = icer_txt(`Cost inc. Dup`,  `QALY inc. Dup`)) |>
    filter(is.null(!!model) | model %in% !!model)
}
icer_txt <- function(dc, dq) ifelse(dc < 0 & dq > 0, "dominant",
                              ifelse(dc > 0 & dq < 0, "dominated",
                                     format(round(dc / dq), big.mark = ",", trim = TRUE)))

# Bridge: effect of each remaining difference (cumulative, in listed order)
tbl_bridge <- function(model, basis = BASIS) {
  w <- bridge_data(model, basis)
  w |> transmute(model, step = label, arm, `LY disc. Diff` = LY_disc, `QALY Diff` = QALY, `Cost Diff` = Cost)
}
bridge_data <- function(model, basis = BASIS) {
  w <- wf |> filter(model == !!model); w_all <- w
  if (!nrow(w)) stop(model, ": no bridge (outputs are identical in both versions).")
  start <- if (basis == "ncd") 1 else 0
  w <- w |> filter(step >= start) |> arrange(match(arm, unique(arm)), step) |>
    group_by(arm) |>
    mutate(across(c(LY_disc, LY_undisc, QALY, Cost), ~ .x - lag(.x))) |> ungroup()
  base <- filter(w_all, step == start)
  last <- filter(w_all, step == max(step))
  tot  <- tibble(model = model, step = Inf, label = "TOTAL (Duplicate - Original)", arm = base$arm,
                 LY_disc = last$LY_disc - base$LY_disc, LY_undisc = last$LY_undisc - base$LY_undisc,
                 QALY = last$QALY - base$QALY, Cost = last$Cost - base$Cost)
  bind_rows(filter(w, step > start), tot)
}

# ==== FIGURES ===================================================================
theme_set(theme_minimal(base_size = 11) +
          theme(legend.position = "bottom", strip.text = element_text(face = "bold"),
                panel.grid.minor = element_blank()))
fmt_axis <- function(metric) if (metric == "Cost ($)") scales::label_dollar() else scales::label_number()

# Duplicate minus Original, every arm, every metric
plot_diff <- function(basis = BASIS) {
  d <- tidy_tot(basis) |> mutate(dir = ifelse(Delta >= 0, "Duplicate higher", "Duplicate lower"))
  ggplot(d, aes(arm, Delta, fill = dir)) +
    geom_hline(yintercept = 0, colour = "grey40") + geom_col(width = 0.7) +
    facet_grid(metric ~ model, scales = "free", space = "free_x") +
    scale_fill_manual(values = c("Duplicate higher" = "#2A9D8F", "Duplicate lower" = "#C0504D"), name = NULL) +
    scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
    labs(x = NULL, y = "Duplicate - Original",
         title = "Difference in outputs, Duplicate vs Original",
         subtitle = if (basis == "ncd") "Original on the NCD life table" else "Original with its own life table") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Original vs Duplicate for one metric ("LYd", "LYu", "Q" or "C"), all models
plot_outputs <- function(key = "C", basis = BASIS) {
  lab <- METRICS[[key]]
  d <- tidy_tot(basis) |> filter(metric == lab)
  long <- d |> pivot_longer(c(Original, Duplicate), names_to = "version", values_to = "value")
  ggplot(d) +
    geom_segment(aes(x = arm, xend = arm, y = Original, yend = Duplicate), colour = "grey60") +
    geom_point(data = long, aes(arm, value, colour = version), size = 3) +
    facet_wrap(~model, scales = "free", nrow = 1) +
    scale_colour_manual(values = COL, name = NULL) +
    scale_y_continuous(labels = fmt_axis(lab)) +
    labs(x = NULL, y = lab, title = paste(lab, "- Original vs Duplicate")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Cost components, stacked, Original vs Duplicate, one model
plot_components <- function(model, basis = BASIS) {
  d <- parts_raw(basis) |> filter(model == !!model) |>
    pivot_longer(c(O, D), names_to = "version", values_to = "value") |>
    mutate(version = factor(version, c("O", "D"), c("Original", "Duplicate")))
  ggplot(d, aes(arm, value, fill = part)) + geom_col(width = 0.7) +
    facet_wrap(~version) +
    scale_y_continuous(labels = scales::label_dollar()) +
    scale_fill_brewer(palette = "Set2", name = NULL) +
    labs(x = NULL, y = "Cost per patient", title = paste(model, "- cost components")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Duplicate minus Original by cost component, one model
plot_component_delta <- function(model, basis = BASIS) {
  d <- parts_raw(basis) |> filter(model == !!model) |> mutate(delta = D - O)
  ggplot(d, aes(delta, part, fill = delta >= 0)) + geom_col() + geom_vline(xintercept = 0) +
    facet_wrap(~arm) +
    scale_x_continuous(labels = scales::label_dollar()) +
    scale_fill_manual(values = c("TRUE" = "#2A9D8F", "FALSE" = "#C0504D"), guide = "none") +
    labs(x = "Duplicate - Original", y = NULL, title = paste(model, "- cost difference by component"))
}

# Cost-effectiveness plane: each comparison, Original -> Duplicate (arrow)
plot_ceplane <- function(wtp = 100000, basis = BASIS) {
  inc <- tot_raw(basis) |> group_by(model) |>
    mutate(across(c(C_O, C_D, Q_O, Q_D), ~ .x - .x[arm == COMP[model[1]]])) |> ungroup() |>
    filter(arm != COMP[model])
  ggplot(inc) +
    geom_abline(slope = wtp, intercept = 0, linetype = "dashed", colour = "grey60") +
    geom_hline(yintercept = 0, colour = "grey80") + geom_vline(xintercept = 0, colour = "grey80") +
    geom_segment(aes(x = Q_O, y = C_O, xend = Q_D, yend = C_D),
                 arrow = arrow(length = unit(0.12, "cm")), colour = "grey50") +
    geom_point(aes(Q_O, C_O, colour = "Original"), size = 2.5) +
    geom_point(aes(Q_D, C_D, colour = "Duplicate"), size = 2.5) +
    geom_text(aes(Q_D, C_D, label = arm), size = 3, vjust = -0.9, check_overlap = TRUE) +
    facet_wrap(~model, scales = "free") +
    scale_colour_manual(values = COL, name = NULL) +
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.15))) +
    scale_y_continuous(labels = scales::label_dollar(), expand = expansion(mult = c(0.05, 0.15))) +
    labs(x = "Incremental QALYs vs comparator", y = "Incremental cost vs comparator",
         title = "Cost-effectiveness plane", subtitle = paste0("Dashed line: WTP $", format(wtp, big.mark = ",", scientific = FALSE)," per QALY"))
}

# Bridge for one model: effect of each remaining difference on LYs, QALYs, cost
plot_bridge <- function(model, basis = BASIS) {
  w <- bridge_data(model, basis) |>
    pivot_longer(c(LY_disc, QALY, Cost), names_to = "metric", values_to = "delta") |>
    mutate(metric = factor(metric, c("LY_disc", "QALY", "Cost"), c("LY (discounted)", "QALY", "Cost ($)")),
           label = factor(label, rev(unique(label))), arm = factor(arm, unique(arm)))
  ggplot(w, aes(delta, label, fill = arm)) +
    geom_col(position = position_dodge2(reverse = TRUE, preserve = "single")) + geom_vline(xintercept = 0) +
    facet_wrap(~metric, scales = "free_x") +
    scale_y_discrete(labels = scales::label_wrap(32)) +
    scale_x_continuous(labels = scales::label_number(big.mark = ",")) +
    scale_fill_brewer(palette = "Set2", name = NULL) +
    labs(x = "Change when the difference is applied (Original -> Duplicate)", y = NULL,
         title = paste(model, "- bridge from Original to Duplicate"),
         subtitle = "Cumulative steps; the size of a step depends on the order, the total does not")
}


# One model, one figure: cost by type, LYs by state, QALYs by state - Original vs Duplicate, per arm.
# free_y = TRUE gives each arm its own y-axis (useful when one arm dwarfs the others, e.g. DMSS cost).
# Returns the three ggplots invisibly (cost, ly, qaly) so any of them can be edited or saved.
plot_model <- function(model, basis = BASIS, free_y = FALSE) {
  okabe <- c("#0072B2", "#E69F00", "#009E73", "#CC79A7", "#56B4E9", "#D55E00", "#F0E442", "#999999")
  nice  <- function(x) gsub("_", " ", x)
  ver   <- function(x) factor(x, c("Original", "Duplicate"), c("Orig.", "Dup."))
  cost <- parts_raw(basis) |> filter(model == !!model) |>
    pivot_longer(c(O, D), names_to = "version", values_to = "value") |>
    mutate(version = ver(c(O = "Original", D = "Duplicate")[version]), comp = factor(part, unique(part)), arm = factor(arm, unique(arm))) |>
    group_by(comp) |> filter(any(value != 0)) |> ungroup()
  mk <- function(me) states |> filter(basis == !!basis, model == !!model, measure == me) |>
    mutate(version = ver(version), comp = factor(state, unique(state)), arm = factor(arm, unique(arm)))
  ly <- mk("LY"); qy <- mk("QALY")
  lv <- as.character(unique(qy$comp))
  st_col <- setNames(scales::viridis_pal(end = 0.92)(sum(lv != "Relapse_disutility")), lv[lv != "Relapse_disutility"])
  if ("Relapse_disutility" %in% lv) st_col <- c(st_col, Relapse_disutility = "#C0504D")
  co_col <- setNames(okabe[seq_along(levels(droplevels(cost$comp)))], levels(droplevels(cost$comp)))
  panel <- function(d, ttl, ylab, cols, labfun, ylabels) {
    tot <- d |> group_by(arm, version) |>
      summarise(top = sum(pmax(value, 0)), net = sum(value), .groups = "drop")
    ggplot(d, aes(version, value, fill = comp)) +
      geom_col(width = 0.75) +
      geom_text(data = tot, aes(version, top, label = labfun(net)), inherit.aes = FALSE, vjust = -0.4, size = 2.6) +
      facet_wrap(~arm, nrow = 1, strip.position = "bottom", scales = if (free_y) "free_y" else "fixed") +
      scale_fill_manual(values = cols, labels = nice, name = NULL) +
      scale_y_continuous(labels = ylabels, expand = expansion(mult = c(0.02, 0.14))) +
      labs(title = ttl, x = NULL, y = ylab) +
      theme(legend.position = "right", strip.placement = "outside", panel.spacing = grid::unit(0.3, "lines"),
            axis.text.x = element_text(size = 7, angle = 90, vjust = 0.5, hjust = 1),
            plot.title = element_text(size = 10, face = "bold"))
  }
  p_cost <- panel(cost, "Cost by type", "Cost per patient ($)", co_col,
                  function(x) paste0("$", format(round(x), big.mark = ",", trim = TRUE)), scales::label_dollar())
  p_ly   <- panel(ly,  "Life years by state (discounted)", "Life years", st_col,
                  function(x) sprintf("%.2f", x), scales::label_number())
  p_qaly <- panel(qy,  "QALYs by state (discounted)", "QALYs", st_col,
                  function(x) sprintf("%.2f", x), scales::label_number())
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(4, 1,
    heights = grid::unit(c(1.6, 1, 1, 1), c("lines", "null", "null", "null")))))
  grid::grid.text(paste0(model, " - Original vs Duplicate (", if (basis == "ncd") "Original on NCD life table" else "Original with own life table", ")"),
                  x = 0.01, just = "left", vp = grid::viewport(layout.pos.row = 1), gp = grid::gpar(fontface = "bold", fontsize = 13))
  print(p_cost, vp = grid::viewport(layout.pos.row = 2))
  print(p_ly,   vp = grid::viewport(layout.pos.row = 3))
  print(p_qaly, vp = grid::viewport(layout.pos.row = 4))
  invisible(list(cost = p_cost, ly = p_ly, qaly = p_qaly))
}

# LYs and QALYs by state: Original, Duplicate and difference
tbl_states <- function(model = NULL, basis = BASIS) {
  d <- states |> filter(basis == !!basis) |>
    pivot_wider(names_from = version, values_from = value) |>
    transmute(model, arm, measure, state, Orig = Original, Dup = Duplicate, Diff = Duplicate - Original)
  if (!is.null(model)) d <- filter(d, model %in% !!model)
  d
}

# Difference (Duplicate - Original) by component, as % of the Original total for that arm and measure.
# Components sum to the "Total" bar, so models are comparable on one scale.
diff_components <- function(model, basis = BASIS) {
  cost <- parts_raw(basis) |> filter(model == !!model) |>
    transmute(arm, measure = "Cost", comp = part, Original = O, Duplicate = D)
  st <- states |> filter(basis == !!basis, model == !!model) |>
    pivot_wider(names_from = version, values_from = value) |>
    transmute(arm, measure, comp = state, Original, Duplicate)
  d <- bind_rows(cost, st) |> mutate(comp = factor(comp, unique(comp)), delta = Duplicate - Original)
  d <- d |> group_by(measure, comp) |> filter(any(Original != 0 | Duplicate != 0)) |> ungroup()
  base <- d |> group_by(arm, measure) |> summarise(base = sum(Original), .groups = "drop")
  tot  <- d |> group_by(arm, measure) |> summarise(Original = sum(Original), Duplicate = sum(Duplicate), delta = sum(delta), .groups = "drop") |>
    mutate(comp = "Total")
  lv <- c(levels(droplevels(d$comp)), "Total")
  bind_rows(mutate(d, comp = as.character(comp)), tot) |>
    left_join(base, by = c("arm", "measure")) |>
    mutate(pct = 100 * delta / base, comp = factor(comp, lv), arm = factor(arm, unique(arm)),
           measure = factor(measure, c("Cost", "LY", "QALY")))
}

# Table: differences by component, absolute and as % of the Original total
tbl_diff_pct <- function(model = NULL, basis = BASIS) {
  models <- if (is.null(model)) MODELS else model
  bind_rows(lapply(models, function(m) mutate(diff_components(m, basis), model = m))) |>
    transmute(model, arm, measure, component = comp, Orig = Original, Dup = Duplicate, Diff = delta, `% of Orig total` = pct)
}

# Bar charts: Duplicate minus Original by cost type, LY state and QALY state, in % of the Original total.
# Bars left of the dotted line add up to "Total". common_scale = TRUE uses the same y-range in every model.
plot_model_diff <- function(model, basis = BASIS, common_scale = TRUE) {
  d <- diff_components(model, basis); nice <- function(x) gsub("_", " ", x)
  lim <- if (common_scale) {
    all <- bind_rows(lapply(MODELS, function(m) diff_components(m, basis)))
    all |> group_by(measure) |> summarise(lo = min(pct, 0), hi = max(pct, 0), .groups = "drop")
  }
  panel <- function(me, ttl) {
    x <- filter(d, measure == me) |> mutate(comp = droplevels(comp), dir = ifelse(pct >= 0, "Duplicate higher", "Duplicate lower"))
    lab <- filter(x, comp == "Total")
    pad <- if (common_scale) { l <- filter(lim, measure == me); c(l$lo, l$hi) } else range(c(0, x$pct))
    pad <- pad + c(-1, 1) * 0.28 * diff(pad)
    ggplot(x, aes(comp, pct, fill = dir)) +
      geom_hline(yintercept = 0, colour = "grey40") +
      geom_vline(xintercept = nlevels(x$comp) - 0.5, linetype = "dotted", colour = "grey60") +
      geom_col(width = 0.7) +
      geom_text(data = filter(lab, pct >= 0), aes(label = sprintf("%+.1f%%", round(pct, 1) + 0)), vjust = -0.4, size = 2.8, fontface = "bold") +
      geom_text(data = filter(lab, pct < 0),  aes(label = sprintf("%+.1f%%", round(pct, 1) + 0)), vjust = 1.3,  size = 2.8, fontface = "bold") +
      facet_wrap(~arm, nrow = 1) +
      scale_fill_manual(values = c("Duplicate higher" = "#2A9D8F", "Duplicate lower" = "#C0504D"), name = NULL) +
      scale_x_discrete(labels = nice) +
      scale_y_continuous(labels = function(v) paste0(v, "%")) +
      coord_cartesian(ylim = pad) +
      labs(title = ttl, x = NULL, y = "% of Original total") +
      theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1), legend.position = "right",
            plot.title = element_text(size = 10, face = "bold"))
  }
  p_cost <- panel("Cost", "Cost by type")
  p_ly   <- panel("LY",   "Life years by state (discounted)")
  p_qaly <- panel("QALY", "QALYs by state (discounted)")
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(4, 1,
    heights = grid::unit(c(1.6, 1, 1, 1), c("lines", "null", "null", "null")))))
  grid::grid.text(paste0(model, " - Duplicate vs Original, % of Original total (", if (basis == "ncd") "Original on NCD life table" else "Original with own life table", ")"),
                  x = 0.01, just = "left", vp = grid::viewport(layout.pos.row = 1), gp = grid::gpar(fontface = "bold", fontsize = 13))
  print(p_cost, vp = grid::viewport(layout.pos.row = 2))
  print(p_ly,   vp = grid::viewport(layout.pos.row = 3))
  print(p_qaly, vp = grid::viewport(layout.pos.row = 4))
  invisible(list(cost = p_cost, ly = p_ly, qaly = p_qaly))
}

# All models on one page: total % difference per arm for cost, LYs and QALYs
plot_diff_pct <- function(basis = BASIS) {
  d <- bind_rows(lapply(MODELS, function(m) mutate(diff_components(m, basis), model = m))) |>
    filter(comp == "Total") |> mutate(dir = ifelse(pct >= 0, "Duplicate higher", "Duplicate lower"))
  ggplot(d, aes(arm, pct, fill = dir)) +
    geom_hline(yintercept = 0, colour = "grey40") + geom_col(width = 0.7) +
    facet_grid(measure ~ model, scales = "free_x", space = "free_x") +
    scale_fill_manual(values = c("Duplicate higher" = "#2A9D8F", "Duplicate lower" = "#C0504D"), name = NULL) +
    scale_y_continuous(labels = function(v) paste0(v, "%")) +
    labs(x = NULL, y = "Duplicate vs Original, % of Original total",
         title = "Total difference as % of the Original, all models",
         subtitle = if (basis == "ncd") "Original on the NCD life table" else "Original with its own life table") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# ==== SHINY APP ===================================================================
# Run:  shiny::runApp("app.R")   (or press "Run App" in RStudio)

HAS_DT <- requireNamespace("DT", quietly = TRUE)
SHOW_BASIS_TOGGLE <- FALSE   # TRUE adds a switch to show the Original with its own life table

MODEL_INFO <- list(
  CAIS = "Infectaguard vs usual care",
  CISS = "Best supportive care vs five targeted therapies",
  CNRD = "Velonix vs Doralene",
  DMSS = "Cardivex, standard of care, Zonapride, Septoplasty, Ablatherm",
  NCD  = "Zelravine vs usual care")

# Format a table for display: costs in whole dollars, others to 3 decimals, % to 2
fmt_df <- function(df) {
  df <- as.data.frame(df)
  for (n in names(df)) if (is.numeric(df[[n]])) {
    x <- df[[n]]
    df[[n]] <-
      if (grepl("%", n, fixed = TRUE)) sprintf("%.2f%%", round(x, 2) + 0)
      else if (grepl("Cost", n))      format(round(x), big.mark = ",", trim = TRUE)
      else if ("measure" %in% names(df))
        ifelse(df$measure == "Cost", format(round(x), big.mark = ",", trim = TRUE), sprintf("%.3f", round(x, 3) + 0))
      else sprintf("%.3f", round(x, 3) + 0)
  }
  df[] <- lapply(df, function(v) if (is.factor(v)) as.character(v) else v)
  df
}
tbl_out <- function(id) if (HAS_DT) DT::DTOutput(id) else shiny::tableOutput(id)
tbl_render <- function(expr_fn) {
  if (HAS_DT) DT::renderDT(DT::datatable(fmt_df(expr_fn()), rownames = FALSE, filter = "top",
        options = list(pageLength = 15, scrollX = TRUE, dom = "tip")))
  else shiny::renderTable(fmt_df(expr_fn()), striped = TRUE, hover = TRUE, spacing = "s")
}

bridge_height <- function(model) {
  w <- wf[wf$model == model, ]
  if (!nrow(w)) return(300)
  max(420, 150 + 15 * (length(unique(w$step)) ) * length(unique(w$arm)))
}

model_tab <- function(m) {
  shiny::tabPanel(m,
    shiny::div(class = "model-head", shiny::h3(m), shiny::span(MODEL_INFO[[m]])),
    shiny::tabsetPanel(type = "pills",
      shiny::tabPanel("Outputs by arm",
        shiny::p(class = "note", "Cost by type, life years by state and QALYs by state (discounted). Original and Duplicate bars sit side by side for each arm."),
        shiny::plotOutput(paste0(m, "_stack"), height = "800px")),
      shiny::tabPanel("Difference (% of Original)",
        shiny::p(class = "note", "Duplicate minus Original by component, as a percentage of the Original total for that arm. Bars left of the dotted line add up to the Total."),
        shiny::plotOutput(paste0(m, "_diff"), height = "800px")),
      shiny::tabPanel("Bridge",
        if (m %in% unique(wf$model)) shiny::tagList(
          shiny::p(class = "note", "Effect of each remaining difference when the Original is moved to the Duplicate one change at a time. Step sizes depend on the order; the total does not."),
          shiny::plotOutput(paste0(m, "_bridge"), height = "auto"),
          shiny::h4("Bridge table"), tbl_out(paste0(m, "_bridge_tbl")))
        else shiny::p(class = "note", "No bridge: outputs are identical in both versions.")),
      shiny::tabPanel("Tables",
        shiny::h4("Outputs per arm"),             tbl_out(paste0(m, "_t_out")),
        shiny::h4("Cost components"),             tbl_out(paste0(m, "_t_comp")),
        shiny::h4("Life years and QALYs by state"), tbl_out(paste0(m, "_t_state")),
        shiny::h4("Difference by component"),     tbl_out(paste0(m, "_t_diff")))
    ))
}

ui <- shiny::fluidPage(
  shiny::tags$head(shiny::tags$style(shiny::HTML("
    body { font-size: 14px; }
    .note { color: #555; margin: 8px 0 12px; }
    .model-head { margin: 10px 0 4px; } .model-head h3 { display: inline-block; margin: 0 12px 0 0; }
    .model-head span { color: #666; }
    .controls { background: #f6f6f4; border-radius: 6px; padding: 8px 14px; margin-bottom: 10px; }
    .nav-tabs > li > a { font-weight: 600; }"))),
  shiny::titlePanel("Original vs Duplicate HTA models"),
  shiny::p(class = "note", "Deterministic base case. Differences are Duplicate minus Original; the Original is the reference and is calculated on the common NCD life table."),
  shiny::div(class = "controls", shiny::fluidRow(
    if (SHOW_BASIS_TOGGLE) shiny::column(4, shiny::radioButtons("basis", "Original calculated with",
        c("Common NCD life table" = "ncd", "Its own life table" = "own"), inline = TRUE)),
    shiny::column(4, shiny::checkboxInput("free_y", "Separate y-axis for each arm", FALSE)),
    shiny::column(4, shiny::checkboxInput("common", "Same % scale across models (difference charts)", TRUE)))),
  do.call(shiny::tabsetPanel, c(list(id = "tabs", type = "tabs",
    shiny::tabPanel("Overview",
      shiny::tabsetPanel(type = "pills",
        shiny::tabPanel("Total difference (% of Original)",
          shiny::p(class = "note", "Total difference per arm for cost, life years and QALYs, on one scale across models."),
          shiny::plotOutput("ov_pct", height = "620px")),
        shiny::tabPanel("Original vs Duplicate",
          shiny::selectInput("ov_metric", NULL, c("Cost" = "C", "QALYs" = "Q", "Life years (discounted)" = "LYd", "Life years (undiscounted)" = "LYu"), width = "260px"),
          shiny::plotOutput("ov_out", height = "420px"),
          shiny::h4("Outputs per arm"), tbl_out("ov_tbl")),
        shiny::tabPanel("ICERs",
          shiny::h4("Incremental results vs comparator"), tbl_out("ov_icer"),
          shiny::h4("Cost-effectiveness plane"), shiny::plotOutput("ov_ce", height = "560px")))
    )), lapply(MODELS, model_tab))),
  shiny::hr(), shiny::p(class = "note", "Teal bars: Duplicate higher. Red bars: Duplicate lower. Blue and orange: Original and Duplicate. In the bridge chart, colours distinguish arms.")
)

server <- function(input, output, session) {
  bs   <- shiny::reactive(if (SHOW_BASIS_TOGGLE && !is.null(input$basis)) input$basis else BASIS)
  fy   <- shiny::reactive(isTRUE(input$free_y))
  cmn  <- shiny::reactive(isTRUE(input$common))

  output$ov_pct  <- shiny::renderPlot({ print(plot_diff_pct(bs())) }, res = 96)
  output$ov_out  <- shiny::renderPlot({ print(plot_outputs(input$ov_metric, bs())) }, res = 96)
  output$ov_ce   <- shiny::renderPlot({ print(plot_ceplane(basis = bs())) }, res = 96)
  output$ov_tbl  <- tbl_render(function() tbl_outputs(basis = bs()))
  output$ov_icer <- tbl_render(function() tbl_incremental(basis = bs()))

  for (m in MODELS) local({
    mm <- m
    output[[paste0(mm, "_stack")]] <- shiny::renderPlot({ plot_model(mm, bs(), free_y = fy()); invisible(NULL) }, res = 96)
    output[[paste0(mm, "_diff")]]  <- shiny::renderPlot({ plot_model_diff(mm, bs(), common_scale = cmn()); invisible(NULL) }, res = 96)
    output[[paste0(mm, "_t_out")]]   <- tbl_render(function() dplyr::select(tbl_outputs(mm, bs()), -dplyr::any_of("model")))
    output[[paste0(mm, "_t_comp")]]  <- tbl_render(function() dplyr::select(tbl_components(mm, bs()), -dplyr::any_of("model")))
    output[[paste0(mm, "_t_state")]] <- tbl_render(function() dplyr::select(tbl_states(mm, bs()), -dplyr::any_of("model")))
    output[[paste0(mm, "_t_diff")]]  <- tbl_render(function() dplyr::select(tbl_diff_pct(mm, bs()), -dplyr::any_of("model")))
    if (mm %in% unique(wf$model)) {
      output[[paste0(mm, "_bridge")]] <- shiny::renderPlot({ print(plot_bridge(mm, bs())) },
                                                           height = bridge_height(mm), res = 96)
      output[[paste0(mm, "_bridge_tbl")]] <- tbl_render(function() dplyr::select(tbl_bridge(mm, bs()), -dplyr::any_of("model")))
    }
  })
}

shiny::shinyApp(ui, server)
