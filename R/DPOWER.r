#' Find the week number of year
#'
#' Takes in any date and show the week number
#' @param month,maxdate,curdate,bpem/ month,maxdate,curdate and bpem need to be entered
#' @return return the week number in year
#' @export



DPOWER<-function(month,maxdate,curdate,bpem)
{
  ##########################Manupulate the data/Change the format for Date##########################
  bpem1<-bpem
  bpem1$FirstAttempt<-as.Date(bpem1$FirstAttempt,"%m/%d/%Y")
  bpem1$UploadDate<-as.Date(bpem1$UploadDate,"%m/%d/%Y")
  bpem1$DateTime<-as.Date(bpem1$DateTime,"%m/%d/%Y")
  bpem1$SchedBD<-as.Date(bpem1$SchedBD,"%m/%d/%Y")
  bpem1$CreationDate<-as.Date(bpem1$CreationDate,"%m/%d/%Y")


  ############################Filter the Require Data########################
  bpem1<-bpem1[!(is.na(bpem1$CaseID) | bpem1$CaseID==""), ]
  str(bpem1)
  bpem2<-bpem1[format(bpem1$CreationDate,'%m')==month,1:17]
  bpem3<-bpem2[bpem2$CreationDate<=maxdate,1:17]

  # rm(bpem)
  # rm(bpem1)
  # rm(bpem2)

  ######################Data Processing######################################
  bpem3$Action_Date<-bpem3$DateTime
  bpem3$Action_Date1<-bpem3$DateTime


  #########################Find the week number##############################
  my_week <- function(x){
    # fst monday of the same year
    first_sun <- as.POSIXct(paste0(year(x),"-01-Mon"), format = "%Y-%U-%a")
    (yday(x) + (7 - yday(first_sun) )) %/% 7+1
  }

  bpem3$Week_Num<-my_week(bpem3$CreationDate)
  bpem3$Month_Count<-format(bpem3$CreationDate,'%m')
  #############################Check the condition##########################

  bpem3$Exception_Impact<-toupper(bpem3$Exception.Impact)

  bpem3$Impact_Hierarchy_Order<-
    with(bpem3, ifelse(bpem3$Exception_Impact=="STOPS REGISTRATION",bpem3$Impact_Hierarchy_Order<-1,
                       ifelse(bpem3$Exception_Impact=="REGULATORY",bpem3$Impact_Hierarchy_Order<-2,
                              ifelse(bpem3$Exception_Impact=="CUSTOMER SAFETY",bpem3$Impact_Hierarchy_Order<-3,
                                     ifelse(bpem3$Exception_Impact=="STOPS BILLING",bpem3$Impact_Hierarchy_Order<-4,
                                            ifelse(bpem3$Exception_Impact=="INCORRECT BILLING",bpem3$Impact_Hierarchy_Order<-5,
                                                   ifelse(bpem3$Exception_Impact=="INDUSTRY UPDATE",bpem3$Impact_Hierarchy_Order<-6,7)))))))
  bpem3$Action_Date<-
    as.Date( with(bpem3,
                  ifelse(bpem3$Exception_Impact=="STOPS REGISTRATION",bpem3$FirstAttempt,bpem3$Action_Date)),origin="1970-01-01")

  bpem3$Exception_Impact<-
    with(bpem3,ifelse(bpem3$Exception_Impact=="STOPS BILLING","STOPS BILLING-(BPEM)",Exception_Impact))

  ############################BGB_Exception_View_1########################

  BGB_Exception_View_1<-bpem3
  BGB_Exception_View_1$Process_Name<-bpem3$Process;


  BGB_Exception_View_1$Process_Name_New<-
    with(BGB_Exception_View_1,
         ifelse(toupper(BGB_Exception_View_1$Process_Name)=="OBJECTION","Switch-Objection",
                ifelse(toupper(BGB_Exception_View_1$Process_Name)=="REJECTION","Switch-Rejection",
                       ifelse(toupper(BGB_Exception_View_1$Process_Name)=="FIRST BILL","Switch-First_Bill",
                              ifelse(toupper(BGB_Exception_View_1$Process_Name)=="FINAL BILL","Switch-Final_Bill",
                                     ifelse(toupper(BGB_Exception_View_1$Process_Name)=="DEVICE","Device",
                                            ifelse(toupper(BGB_Exception_View_1$Process_Name)=="EIDE","IDE",

                                                   ifelse((toupper(BGB_Exception_View_1$Process_Name)=="READS") & (toupper(BGB_Exception_View_1$Exception)=="IMR"),"Read-IMR",
                                                          ifelse((toupper(BGB_Exception_View_1$Process_Name)=="READS") & (toupper(BGB_Exception_View_1$Exception)=="M100"),"Read-IMR",
                                                                 ifelse((toupper(BGB_Exception_View_1$Process_Name)=="READS") & (toupper(BGB_Exception_View_1$Exception)=="M101"),"Read-IMR",
                                                                        ifelse((toupper(BGB_Exception_View_1$Process_Name)=="READS") & (toupper(BGB_Exception_View_1$Exception)=="M102"),"Read-IMR",
                                                                               ifelse((toupper(BGB_Exception_View_1$Process_Name)=="READS") & (toupper(BGB_Exception_View_1$Exception)=="M103"),"Read-IMR",

                                                                                      ifelse((toupper(BGB_Exception_View_1$Process_Name)=="READS") & (toupper(BGB_Exception_View_1$Exception)!="IMR"),"Reads Non-IMR",
                                                                                             ifelse((toupper(BGB_Exception_View_1$Process_Name)=="READS") & (toupper(BGB_Exception_View_1$Exception)!="M100"),"Reads Non-IMR",
                                                                                                    ifelse((toupper(BGB_Exception_View_1$Process_Name)=="READS") & (toupper(BGB_Exception_View_1$Exception)!="M101"),"Reads Non-IMR",
                                                                                                           ifelse((toupper(BGB_Exception_View_1$Process_Name)=="READS") & (toupper(BGB_Exception_View_1$Exception)!="M102"),"Reads Non-IMR",
                                                                                                                  ifelse((toupper(BGB_Exception_View_1$Process_Name)=="READS") & (toupper(BGB_Exception_View_1$Exception)!="M103"),"Reads Non-IMR",

                                                                                                                         ifelse((toupper(BGB_Exception_View_1$Process_Name)=="BILLING") & (toupper(BGB_Exception_View_1$Exception)=='OUTSORT'),"Outsort",
                                                                                                                                ifelse((toupper(BGB_Exception_View_1$Process_Name)=="BILLING") & (toupper(BGB_Exception_View_1$Exception)=='B100'),"Outsort",

                                                                                                                                       ifelse((toupper(BGB_Exception_View_1$Process_Name)=="BILLING") & (toupper(BGB_Exception_View_1$Exception)!='OUTSORT'),"BPEM",
                                                                                                                                              ifelse((toupper(BGB_Exception_View_1$Process_Name)=="BILLING") & (toupper(BGB_Exception_View_1$Exception)!='B100'),"BPEM",""
                                                                                                                                              )))))))))))))))))))))

  # rm(bpem3)
  ###########################################BGB_Exception_View_2############################################

  BGB_Exception_View_2<-arrange(BGB_Exception_View_1,desc(Process_Name_New))

  BGB_Exception_View_2$Customer_Metric<-
    with(BGB_Exception_View_2,
         ifelse(BGB_Exception_View_2$Exception_Impact=="STOPS REGISTRATION",BGB_Exception_View_2$Customer_Metric<-"24 hours SLA",
                ifelse(BGB_Exception_View_2$Exception_Impact=="REGULATORY",BGB_Exception_View_2$Customer_Metric<-"24 hours SLA",
                       ifelse(BGB_Exception_View_2$Exception_Impact=="CUSTOMER SAFETY",BGB_Exception_View_2$Customer_Metric<-"24 hours SLA",
                              ifelse(BGB_Exception_View_2$Exception_Impact=="STOPS BILLING-(BPEM)",BGB_Exception_View_2$Customer_Metric<-"Bill Date",
                                     ifelse(BGB_Exception_View_2$Exception_Impact=="INCORRECT BILLING",BGB_Exception_View_2$Customer_Metric<-"Bill Date",
                                            ifelse(BGB_Exception_View_2$Exception_Impact=="INDUSTRY UPDATE",BGB_Exception_View_2$Customer_Metric<-"Bill Date",
                                                   ifelse(BGB_Exception_View_2$Exception_Impact=="IMBALANCE",BGB_Exception_View_2$Customer_Metric<-"Bill Date","Bill Date"
                                                   ))))))))

  # rm(BGB_Exception_View_1)
  #########################################BGB_Exception_View_3###################################################
  BGB_Exception_View_3<-BGB_Exception_View_2

  BGB_Exception_View_3$Customer_Metric<-
    with(BGB_Exception_View_3,
         ifelse(((BGB_Exception_View_3$Process_Name_New=="Read-IMR") & (BGB_Exception_View_3$Exception_Impact=="STOPS BILLING-(BPEM)")),"Extraction Date +5Days",
                ifelse(((BGB_Exception_View_3$Process_Name_New=="Outsort") & (BGB_Exception_View_3$Exception_Impact=="STOPS BILLING-(BPEM)")),"Extraction Date +5Days",BGB_Exception_View_3$Customer_Metric
                )))


  BGB_Exception_View_3$Exception_Impact<-
    with(BGB_Exception_View_3,
         ifelse(((BGB_Exception_View_3$Process_Name_New=="Read-IMR") & (BGB_Exception_View_3$Exception_Impact=="STOPS BILLING-(BPEM)")),"STOP BILLING-(Read-IMR)",
                ifelse(((BGB_Exception_View_3$Process_Name_New=="Outsort") & (BGB_Exception_View_3$Exception_Impact=="STOPS BILLING-(BPEM)")),"STOP BILLING-(Outsort)",BGB_Exception_View_3$Exception_Impact
                )))

  BGB_Exception_View_3$Past_Sched=BGB_Exception_View_3$SchedBD-BGB_Exception_View_3$CreationDate;

  BGB_Exception_View_3$Past_Sched<-as.numeric(BGB_Exception_View_3$Past_Sched,"days")

  BGB_Exception_View_3$Customer_Metric<-
    with(BGB_Exception_View_3,
         ifelse(((BGB_Exception_View_3$Customer_Metric=="Bill Date") & (BGB_Exception_View_3$Past_Sched<0)),"Bill Date Non-Aligned",
                ifelse(((BGB_Exception_View_3$Customer_Metric=="Bill Date") & (BGB_Exception_View_3$Past_Sched>=0)),"Bill Date Aligned",BGB_Exception_View_3$Customer_Metric
                )))
  # rm(BGB_Exception_View_2)
  ################################################BGB_Exception_View_4###########################################
  BGB_Exception_View_4<-BGB_Exception_View_3
  BGB_Exception_View_4$New_CreationDate<-BGB_Exception_View_4$CreationDate;
  BGB_Exception_View_4$week_day<-wday(BGB_Exception_View_4$CreationDate)

  BGB_Exception_View_4$New_CreationDate<-
    as.Date(with(BGB_Exception_View_4,
                 ifelse(BGB_Exception_View_4$week_day==1,BGB_Exception_View_4$CreationDate+1,
                        ifelse(BGB_Exception_View_4$week_day==7,BGB_Exception_View_4$CreationDate+2,BGB_Exception_View_4$CreationDate
                        ))),,origin="1970-01-01")
  # rm(BGB_Exception_View_3)

  #####################Adding 5 days in all where SLA is Bill Date###################################

  Add_Workday<-function(Date_to_add,Days_wanted_to_Add)
  {
    Date_to_add<-as.Date(ifelse((wday(Date_to_add)==1),Date_to_add+1,
                                ifelse((wday(Date_to_add)==7),Date_to_add+2,Date_to_add
                                )),origin="1970-01-01")
    cal <- Calendar(weekdays=c('saturday', 'sunday'))
    bizdays::offset(Date_to_add,Days_wanted_to_Add , cal)
  }

  BGB_Exception_View_4$New_SchedBD<-BGB_Exception_View_4$SchedBD;

  BGB_Exception_View_4$New_SchedBD<-
    as.Date(with(BGB_Exception_View_4,
                 ifelse(
                   (toupper(Exception_Impact)=="STOPS BILLING") |
                     (toupper(Exception_Impact)=="STOP BILLING-(READ-IMR)") |
                     (toupper(Exception_Impact)=="STOP BILLING-(OUTSORT)") |
                     (toupper(Exception_Impact)=="STOPS BILLING-(BPEM)") |
                     (toupper(Exception_Impact)=="IMBALANCE") |
                     (toupper(Exception_Impact)=="INCORRECT BILLING") |
                     (toupper(Exception_Impact)=="INDUSTRY UPDATE"),
                   Add_Workday(BGB_Exception_View_4$SchedBD,5),BGB_Exception_View_4$SchedBD
                 )),origin="1970-01-01")

  BGB_Exception_View_4$New_SchedBD<-
    as.Date( with(BGB_Exception_View_4,
                  ifelse(
                    (wday(BGB_Exception_View_4$New_SchedBD))==1,(BGB_Exception_View_4$New_SchedBD+1),
                    ifelse(
                      (wday(BGB_Exception_View_4$New_SchedBD))==7,(BGB_Exception_View_4$New_SchedBD+2),
                      BGB_Exception_View_4$New_SchedBD
                    ))),origin="1970-01-01")
  ####################################BGB_Exception_View_5####################################################

  BGB_Exception_View_5<-BGB_Exception_View_4

  BGB_Exception_View_5$New_SchedBD<-
    as.Date(with(BGB_Exception_View_5,
                 ifelse(
                   (toupper(Exception_Impact)=="STOPS REGISTRATION") |
                     (toupper(Exception_Impact)=="REGULATORY") |
                     (toupper(Exception_Impact)=="CUSTOMER SAFETY"),

                   Add_Workday(BGB_Exception_View_5$New_CreationDate,2),BGB_Exception_View_5$New_SchedBD
                 )),origin="1970-01-01")
  # rm(BGB_Exception_View_4)
  ##############################################BGB_Exception_View_6#######################################

  BGB_Exception_View_6<-BGB_Exception_View_5
  BGB_Exception_View_6$Process_Day_Difference=BGB_Exception_View_6$New_SchedBD-BGB_Exception_View_6$Action_Date;

  #####################################Find out the week in month#############################################

  week_of_month<-function(Put_The_Date)
  {
    # Put_The_Date=as.Date("2017/01/07")
    week(Put_The_Date)
    First_Day_Month<-cut(Put_The_Date,"month")
    First_Day_Month
    Week_First_Day_Month<-my_week(First_Day_Month)
    Week_First_Day_Month

    Week_Assign_Date<-my_week(Put_The_Date)
    Week_Assign_Date

    Week_in_the_Month<- (Week_Assign_Date-Week_First_Day_Month) +1
  }

  # week_of_month(as.Date("2017/01/02"))


  ############################################################################################################
  BGB_Exception_View_6$Week_Number<-week_of_month(BGB_Exception_View_6$CreationDate)

  BGB_Exception_View_6$Week<-
    with(BGB_Exception_View_6,
         ifelse(BGB_Exception_View_6$Week_Number==1,"1st Week of Month",
                ifelse(BGB_Exception_View_6$Week_Number==2,"2nd Week of Month",
                       ifelse(BGB_Exception_View_6$Week_Number==3,"3rd Week of Month",
                              ifelse(BGB_Exception_View_6$Week_Number==4,"4th Week of Month",
                                     ifelse(BGB_Exception_View_6$Week_Number==5,"5th Week of Month",
                                            ifelse(BGB_Exception_View_6$Week_Number==6,"6th Week of Month","Check the date format for CreationDate"
                                            )))))))

  BGB_Exception_View_6$unique_key<-gsub("[[:space:]]", "",paste(BGB_Exception_View_6$InstallationNo,BGB_Exception_View_6$Month_Count,BGB_Exception_View_6$Impact_Hierarchy_Order))
  BGB_Exception_View_6$unique_key_installation<-gsub("[[:space:]]", "",paste(BGB_Exception_View_6$InstallationNo,BGB_Exception_View_6$Month_Count))
  # rm(BGB_Exception_View_5)
  ##########################################BGB_Exception_View_7############################################

  BGB_Exception_View_7<-BGB_Exception_View_6
  BGB_Exception_View_7$CreationDate_1<-format(BGB_Exception_View_7$CreationDate,'%d-%B-%Y')
  # rm(BGB_Exception_View_6)
  ############################BGB_Exception_View_8########################

  BGB_Exception_View_8<-BGB_Exception_View_7

  BGB_Exception_View_8$Compelation_Status<-
    with(BGB_Exception_View_8,
         ifelse(BGB_Exception_View_8$TerminationName=="Completed","Completed",
                ifelse(BGB_Exception_View_8$TerminationName=="Already Completed","Completed","Outstanding"
                )))

  BGB_Exception_View_8$Compelation_Status<-
    with(BGB_Exception_View_8,
         ifelse(((toupper(BGB_Exception_View_8$Exception_Impact)=="STOPS REGISTRATION") & (BGB_Exception_View_8$TerminationName == "Pending")),
                "Completed",BGB_Exception_View_8$Compelation_Status
         ))

  BGB_Exception_View_8$SLA_Status<-
    with(BGB_Exception_View_8,
         ifelse(
           (BGB_Exception_View_8$Compelation_Status=="Completed")& (BGB_Exception_View_8$Process_Day_Difference>=0),"Completed_Within_SLA",
           ifelse((BGB_Exception_View_8$Compelation_Status=="Completed") & (BGB_Exception_View_8$Process_Day_Difference<0),"Completed_Outside_SLA",
                  ifelse((BGB_Exception_View_8$Compelation_Status=="Outstanding") & (BGB_Exception_View_8$New_SchedBD>curdate),"Outstanding_Within_SLA",
                         ifelse((BGB_Exception_View_8$Compelation_Status=="Outstanding") & (BGB_Exception_View_8$New_SchedBD<=curdate),"Outstanding_Outside_SLA","NA"
                         )))))

  BGB_Exception_View_8$SLA_Status_Final<-
    with(BGB_Exception_View_8,
         ifelse(
           (BGB_Exception_View_8$Compelation_Status=="Completed")& (BGB_Exception_View_8$Process_Day_Difference>=0),"Within_SLA",
           ifelse((BGB_Exception_View_8$Compelation_Status=="Completed") & (BGB_Exception_View_8$Process_Day_Difference<0),"Outside_SLA",
                  ifelse((BGB_Exception_View_8$Compelation_Status=="Outstanding") & (BGB_Exception_View_8$New_SchedBD>curdate),"Within_SLA",
                         ifelse((BGB_Exception_View_8$Compelation_Status=="Outstanding") & (BGB_Exception_View_8$New_SchedBD<=curdate),"Outside_SLA","NA"
                         )))))

  # rm(BGB_Exception_View_7)
  ##############################################BGB_Exception_View_9#############################################
  BGB_Exception_View_9<-BGB_Exception_View_8


  BGB_Exception_View_9$Pending<-0;
  BGB_Exception_View_9$Unattempted<-0;

  BGB_Exception_View_9$Pending<-
    with(BGB_Exception_View_9,
         ifelse((BGB_Exception_View_9$TerminationName=="Pending"),1,
                ifelse((BGB_Exception_View_9$TerminationName=="Voice Call Required Customer"),1,
                       ifelse((BGB_Exception_View_9$TerminationName=="Voice Call Required -3rd Party/Industry"),1,BGB_Exception_View_9$Pending
                       ))))

  BGB_Exception_View_9$Pend_Status<-
    with(BGB_Exception_View_9,
         ifelse((BGB_Exception_View_9$TerminationName=="Pending"),"Pending",
                ifelse((BGB_Exception_View_9$TerminationName=="Voice Call Required Customer"),"Pending",
                       ifelse((BGB_Exception_View_9$TerminationName=="Voice Call Required -3rd Party/Industry"),"Pending",
                              ifelse((BGB_Exception_View_9$TerminationName=="Unattempted"),"Unattempted",""
                              )))))

  BGB_Exception_View_9$Unattempted<-
    with(BGB_Exception_View_9,
         ifelse((BGB_Exception_View_9$TerminationName=="Unattempted"),1,BGB_Exception_View_9$Unattempted
         ))
  # rm(BGB_Exception_View_8)
  ######################################BGB_Exception_View_10############################################

  BGB_Exception_View_10<-BGB_Exception_View_9

  BGB_Exception_View_10$Completed_Within_SLA=0;
  BGB_Exception_View_10$Completed_Outside_SLA=0;
  BGB_Exception_View_10$Outstanding_Within_SLA=0;
  BGB_Exception_View_10$Outstanding_Outside_SLA=0;
  BGB_Exception_View_10$Recieved=1;

  BGB_Exception_View_10$Completed_Within_SLA<-
    ifelse((BGB_Exception_View_10$SLA_Status=="Completed_Within_SLA" ),1,BGB_Exception_View_10$Completed_Within_SLA)
  BGB_Exception_View_10$Completed_Outside_SLA<-
    ifelse((BGB_Exception_View_10$SLA_Status=="Completed_Outside_SLA" ),1,BGB_Exception_View_10$Completed_Outside_SLA)
  BGB_Exception_View_10$Outstanding_Within_SLA<-
    ifelse((BGB_Exception_View_10$SLA_Status=="Outstanding_Within_SLA" ),1,BGB_Exception_View_10$Outstanding_Within_SLA)
  BGB_Exception_View_10$Outstanding_Outside_SLA<-
    ifelse((BGB_Exception_View_10$SLA_Status=="Outstanding_Outside_SLA" ),1,BGB_Exception_View_10$Outstanding_Outside_SLA)
  # rm(BGB_Exception_View_9)
  ################################################BGB_Exception_View_11##########################################

  BGB_Exception_View_11<-BGB_Exception_View_10
  BGB_Exception_View_11<-subset(BGB_Exception_View_11,BGB_Exception_View_11$TerminationName!="Data Cleansing Slingshot 3B",1:47)

  BGB_Exception_View_12<-data.table(BGB_Exception_View_11)

  BGB_Exception_View_12$First<-
    with(BGB_Exception_View_12,
         ifelse(duplicated(BGB_Exception_View_12$InstallationNo),0,1))
  # rm(BGB_Exception_View_10)
  # rm(BGB_Exception_View_11)

  #########################################Working####################
  BGB_Installation_View<-BGB_Exception_View_12

  BGB_Installation_View$Action_Date_Final<-BGB_Installation_View$Action_Date1
  BGB_Installation_View$First_Attempt_Final<-BGB_Installation_View$FirstAttempt
  BGB_Installation_View$Creation_Date_Final<-BGB_Installation_View$CreationDate
  BGB_Installation_View$Schedule_Bill_Date<-BGB_Installation_View$SchedBD
  BGB_Installation_View$Upload_date_New<-BGB_Installation_View$UploadDate
  BGB_Installation_View$New_Schedule_Bill_date<-BGB_Installation_View$New_SchedBD

  write.csv(BGB_Installation_View,file="d:/BGB_Installation_View.csv")

  BGB_Installation_View_Final<-
    subset(
      BGB_Installation_View,
      select=c("Action_Date_Final",
               "First_Attempt_Final",
               "Creation_Date_Final",
               "Schedule_Bill_Date",
               "New_Schedule_Bill_date",
               "Customer_Metric",
               "Week_Number",
               "Impact_Hierarchy_Order",
               "Exception_Impact",
               "TerminationName",
               "CaseID",
               "InstallationNo",
               "Exception",
               "Process",
               "Segment",
               "Collective",
               "Process_Name",
               "Process_Name_New",
               "Past_Sched",
               "week_day",
               "Week",
               "Compelation_Status",
               "SLA_Status",
               "SLA_Status_Final",
               "Pending",
               "Unattempted",
               "Pend_Status",
               "Completed_Within_SLA",
               "Completed_Outside_SLA",
               "Outstanding_Within_SLA",
               "Outstanding_Outside_SLA",
               "Recieved",
               "UserName",
               "SupervisorName",
               "ManagerName",
               "Upload_date_New",
               "First",
               "Month_Count"
      ))

  #View(BGB_Installation_View_Final)
  write.csv(BGB_Installation_View_Final,file="d:/BGB_Installation_View_Final.csv")


}
