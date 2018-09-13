library(RODBC)
library(stringr)
library(gdata) 
library(plyr)
uid = "ommited to post on github"
pwd = "ommited to post on github"
schedual=NULL



getPrograms = function(year, semester, college = 'E')
{
  if(!is.null(semester) && !is.null(year)){
    connection = odbcConnect("OIEA_SERVER",uid,pwd)
 
    query = paste0("select distinct 
                   Program
                   from sections2 
                   where cast(cast(",adjust_year(semester,year)," as char(4)) + cast(",semester," as char(1)) as int) = Year_Semester and 
                   College = '",college,"' and PSA = 0 order by Program")
    programs = sqlQuery(connection, query, stringsAsFactors = FALSE)$Program
    
    odbcClose(connection)

    return(programs)
  }
  return(NULL)
}

getCourseEnrollmentData = function(year, semester,program, department, discipline, course, college = 'E')
{
 
  if(!is.null(year) && !is.null(semester) && !is.null(department) && !is.null(discipline) && department!="Select Department" && discipline!="Select Discipline"
     && !is.null(course) && !is.null(program)){
  connection = odbcConnect("OIEA_SERVER",uid,pwd)
  
  query = paste0("select Start_Period,Meeting_Days,SGEC,Hybrid, Summer_Session, Weeks, Course, 
                 avg(Enrollment) as Average_Enrollment, sum(Standard_Hours) as Standard_Hours,Count(*) as Sections,MAX(Enrollment_Limit) as Enrollment_Limit
                 from sections2 where cast(cast(",adjust_year(semester,year)," as char(4)) + cast(",semester," as char(1)) as int) = Year_Semester and
                 Program = '",program,"' and Department = '",department,"' and Discipline = '",discipline,"' and ")
                 
  if(course!="None"){
    query=paste0(query,"Course = '",course,"' and ")
  }
                  
  query=paste0(query,"College = '",college,"' and PSA = 0 and Status = 'Open'
                 group by Start_Period,Meeting_Days,SGEC, Summer_Session, Weeks, Course, Hybrid")
    
  #write(query,"text.txt")
  sections = sqlQuery(connection, query, stringsAsFactors = FALSE)
 
  sections$Start_Period = factor(sections$Start_Period, levels = c("Early Morning", "Morning", 
                                                                   "College Hour", "Early Afternoon",
                                                                   "Afternoon", "Evening","Online","Hybrid","TBA"))
  
 
  
  if(!empty(sections[sections$Meeting_Days=="TRUE",])){
    sections[sections$Meeting_Days=="TRUE",]$Meeting_Days="T"
  }
  if(!empty(sections[sections$Meeting_Days==as.character("TRUE"),])){
    sections[sections$Meeting_Days=="FALSE",]$Meeting_Days="F"
  }
  
  if(nrow(sections[sections$Hybrid=="1",])!=0){
    sections[sections$Hybrid=="1",]$Meeting_Days="Hybrid"
    sections[sections$Hybrid=="1",]$Start_Period="Hybrid"
  }
  sections$Hybrid=NULL

  meeting_levels = c("MTWR","MTWRTF","MW","MWF","TR","FS","M","T","W","R","F","S","U","Online","Hybrid","TBA")
  sections$Meeting_Days = factor(sections$Meeting_Days,levels = c(meeting_levels, setdiff(levels(factor(sections$Meeting_Days)),meeting_levels)))
  odbcClose(connection)
  schedual<<-sections
  schedual<<-schedual[order(schedual$Start_Period),]
  schedual<<-schedual[order(schedual$Meeting_Days),]
 
  return(sections)
  }
  return(NULL)
}


getYears = function(college = 'E')
{
  connection = odbcConnect("OIEA_SERVER",uid,pwd)
  
  query = paste0("select distinct cast(substring(cast(Year_Semester as char),1,4) as int) as Year from sections2 
                  where Year_Semester >= 20060 and College = '",college,"' order by Year ")
  years = sqlQuery(connection, query, stringsAsFactors = FALSE)$Year
  
  odbcClose(connection)
  
  test1=paste0(rev(years),"-",rev(years+1))
  if(identical(getSemesters(test1),character(0))){
    years=paste0(rev(years-1),"-",rev(years))
  }else{
    years=paste0(rev(years),"-",rev(years+1))
  }
  
  return(years)
}

getSemesters = function(year, college = 'E')
{
  year=str_replace_all(year,"-","")
  connection = odbcConnect("OIEA_SERVER",uid,pwd)
 
  query = paste0("select distinct 
                 case when Semester = 0 then 'Winter'
                 when Semester = 1 then 'Spring'
                 when Semester = 2 then 'Summer'
                 when Semester = 3 then 'Fall' end as Semester
                 from sections2 
                 where Academic_Year = ",year," and College = '",college,"' and PSA = 0
                 ")
  semesters = sqlQuery(connection, query, stringsAsFactors = FALSE)$Semester
  odbcClose(connection)
  
  return(semesters)
}

getDepartments = function(year, semester, program, college = 'E')
{
  if(!is.null(program) && !is.null(semester) && !is.null(year)){
    connection = odbcConnect("OIEA_SERVER",uid,pwd)
    
    query = paste0("select distinct 
                   Department
                   from sections2 
                   where cast(cast(",adjust_year(semester,year)," as char(4)) + cast(",semester," as char(1)) as int) = Year_Semester and 
                   College = '",college,"' and Program = '",program,"' and PSA = 0 order by Department
                   ")
    departments = sqlQuery(connection, query, stringsAsFactors = FALSE)$Department
    
    odbcClose(connection)
    
    return(append("Select Department", departments))
  }
  return (NULL)
}

getDisciplines = function(year, semester, program, department, college = 'E')
{
  if(!is.null(year) && !is.null(semester) && !is.null(program) && !is.null(department) && department!="Select Department"){
    connection = odbcConnect("OIEA_SERVER",uid,pwd)
    
    query = paste0("select distinct 
                   Discipline
                   from sections2 
                   where cast(cast(",adjust_year(semester,year)," as char(4)) + cast(",semester," as char(1)) as int) = Year_Semester and 
                   College = '",college,"' and Department = '",department,"' and PSA = 0 
                   and Program = '",program,"' order by Discipline
                   ")
    disciplines = sqlQuery(connection, query, stringsAsFactors = FALSE)$Discipline
    
    odbcClose(connection)
    return(append("Select Discipline",disciplines))
    }
  return (c("Select Discipline"))
}

getCourses = function(year, semester, department, discipline,program, college = 'E')
{
  if(!is.null(year) && !is.null(semester) && !is.null(department) && !is.null(discipline) && discipline!="Select Discipline"){
    connection = odbcConnect("OIEA_SERVER",uid,pwd)
    
    query = paste0("select distinct 
                   Course
                   from sections2 
                   where cast(cast(",adjust_year(semester,year)," as char(4)) + cast(",semester," as char(1)) as int) = Year_Semester and 
                   College = '",college,"' and Department = '",department,"' and Discipline = '",discipline,"'and Program = '",program,"'
                    and PSA = 0 and Status = 'Open' order by Course
                   ")
    courses = sqlQuery(connection, query, stringsAsFactors = FALSE)$Course
    
    odbcClose(connection)
    return(append("Select Course",courses))
  }
  return (c("Select Course"))
}

getSchedulingPlots = function(schedule, year, semester, sgec = 0)
{
  library(ggplot2)
  library(grid)
  library(gridExtra)
  library(gtable)

  
  #Create a term variable
  schedule$Term = paste0(semester," ", year)
  
  ##Filter for weeks and Summer Session, SGEC
  
    k=1
    scheduleFiltered = schedule %>% filter(SGEC == sgec)
    sessions = unique(scheduleFiltered$Summer_Session)
    
    sessions = factor(sessions,levels = c("Not summer", "Session 1", "Session 2"))
    sessions = sessions[order(sessions)]
    sessions = as.character(sessions)
    
    sessionGrob = vector(mode="list",length = length(sessions))
    
    ##Do multiple summer sessions with a nested layout
    for(sess in sessions)
    {
      scheduleBySession = scheduleFiltered %>% filter(Summer_Session == sess)
      
      scheduleBySession = scheduleBySession[order(scheduleBySession$Weeks),]
      
      weeks = unique(scheduleBySession$Weeks)
      ##Plot list is separated by weeks
      plotList= vector(mode="list",length = length(weeks))
      
      i=1
      for(w in weeks)
      {
        finalSchedule = scheduleBySession %>% filter(Weeks == w)
       # write.xlsx(finalSchedule, "schedual_example.xlsx")
        g = plotCourseSchedulingGrid(finalSchedule)
        
        ##Extract the legend
        legend = gtable_filter(ggplot_gtable(ggplot_build(g)), "guide-box")
        
        g = g + theme(legend.position="none")
        plotList[[i]] = g
        
        i = i+1
      }
      
      ##Label string
      label = ifelse(finalSchedule$Summer_Session != "Not summer", as.character(unique(finalSchedule$Summer_Session)),"")
      
      sessionGrob[[k]] = do.call("arrangeGrob", c(plotList,list(nrow=1,left = textGrob(label))))
      k=k+1
    }
    if(sgec)
    {
      location = "SGEC"
    }
    else
    {
      location = "Main Campus"
    }
    
    rowCount = length(sessions)
    
    ##Label string
    label = paste0(unique(scheduleFiltered$Term),": ",unique(scheduleFiltered$Course), " @ ",location)
    
    if(length(sessionGrob) == 0)
    {
      finalGrob = textGrob("No courses scheduled at this site")
    }
    else
    {
      nestedGrob = do.call("arrangeGrob", c(sessionGrob,list(nrow=rowCount,top = textGrob(label))))
      finalGrob = arrangeGrob(nestedGrob, legend, 
                                widths=unit.c(unit(1, "npc") - legend$width, legend$width), 
                                nrow=1) 
    }
  
  return(finalGrob)
}

plotCourseSchedulingGrid = function(sections,mid=34)
{
  library(RColorBrewer)
  library(gridExtra)
  library(ggplot2)
  
  weeks = unique(sections$Weeks)
  term = unique(sections$Term)
  
  colors = brewer.pal(5,"RdBu")
  values = c(0,16/80,34/80,45/80,80/80)
  
  g = ggplot(sections, aes(x=Meeting_Days,y=Start_Period, fill = Average_Enrollment)) +geom_tile(color = "black")+
    theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.key.size = unit(2, "cm"))+
    scale_fill_gradientn(colors = colors, values=values, limits = c(0,80),breaks=c(0,8,16,24,32,40,48,56,64,72,80)) +
    geom_text(aes(label=paste(sections$Standard_Hours, " hours")))  + 
    labs(fill = "Average Enrollment") +
    ggtitle(paste0(weeks, " weeks")) 
  
  return(g)
}


get_rankings=function(week,Sgec=0 ,data=NULL, advanced=NULL, filter_option=NULL){
  dataframe=NULL
  if(!is.null(data) && !identical(schedual,data)){
    dataframe=data
  }
  else{
    dataframe=schedual
  }
 
  
  order_time=c("Early Morning","Morning","College Hour","Early Afternoon", "Afternoon","Evening","Online","Hybrid")
  if(grepl("Session",week)){#summer
    
    if(nchar(week)==22)#allied health has 10 week classes throws off substring
      substr_weeks_available=substr(week,12,20)
    else
      substr_weeks_available=substr(week,13,21)

    
   result=dataframe[dataframe$SGEC==Sgec & dataframe$Summer_Session==substr_weeks_available
                   & dataframe$Weeks==as.numeric(substr(week,1,2)),]
 }else{#not summer
   result=dataframe[dataframe$SGEC==Sgec & dataframe$Weeks==as.numeric(substr(week[1],1,2)),]
                   
 }
 

  #result<<-factor(result$Start_Period,levels=order_time,ordered=TRUE)
  #result<<- result[order(result$Meeting_Days,result$Start_Period),]
  
  result<- result[c(1,2,7,8,9,10,6)]
  
  
  result=result[order(-result$Average_Enrollment),]
 #reset row numbers
  rownames(result) <- seq(length=nrow(result))
  schedual_example<<-result
  min=min(result$Standard_Hours)
 
  
  colnames(result)[1]="Start Period"
  colnames(result)[2]="Meeting Days"
  colnames(result)[3]="Average Enrollment"
  colnames(result)[4]="Standard Hours"
  colnames(result)[5]="Sections"
  colnames(result)[6]="Enrollment Limit"

  #since for advanced start period or meeting days is not necessary remove it
  if(!is.null(advanced)){
    result<-result[c(7,1:6)]
    result<-result[result[[advanced]]==filter_option,]
    result[[advanced]]=NULL
    
  }else{
    result$Course=NULL
  }
  
  row.names(result)<-c(1:nrow(result))
  return  (result)
}

adjust_year<-function(semester,year){
  
 if(!is.null(semester)){

  if(semester==0 || semester==1 || semester=="Spring" || semester=="Winter"){#spring or winter
    year=substr(year,6,9)
  }
  else{
    year=substr(year,1,4)
  }
 }
  return (year)
 
}

weeks_session<-function(semester, ELAC=T, data=NULL){
  if(is.null(data))
    dataframe=schedual
  else
    dataframe=data
  
  if(ELAC){
    result=dataframe[dataframe$SGEC==0,]
  }else{
    result=dataframe[dataframe$SGEC==1,]
    
  }
  
    if(semester=="Summer"){
      temp=paste(result$Weeks," weeks (",result$Summer_Session,")")
      temp=unique(sort(temp, decreasing=T))
      return (temp)
    }
    temp=unique(paste0(sort(result$Weeks, decreasing = T)," weeks"))
      return (temp)
    
}

box_length<-function(weeks_available,SGEC=F){
  week=substr(weeks_available,1,2)
  
  if(schedual$Summer_Session[1]=="Not summer"){
    temp<- schedual[schedual$SGEC==SGEC & schedual$Weeks==as.numeric(week),]
  }
  else{
    temp<-schedual[schedual$SGEC==SGEC & schedual$Summer_Session==substr(weeks_available,12,20)
                   & schedual$Weeks==as.numeric(substr(weeks_available,1,2)),]
  }
  
  if((length(unique(temp$Meeting_Days)))>=2 && length(unique(temp$Start_Period))>=2){
    return(12)
  }
  return (6)
}

get_graph<-function(length, week, SGEC=T, Enrollment_Limit=F){
  
  weeks_available=week
 
  week=substr(week,1,2)
  
  
  if(schedual$Summer_Session[1]=="Not summer"){
    temp<- schedual[schedual$SGEC==SGEC & schedual$Weeks==as.numeric(week),]
  }
  else{
    if(nchar(weeks_available)==22)#allied health has 10 week classes throws off substring
      substr_weeks_available=substr(weeks_available,12,20)
    else
      substr_weeks_available=substr(weeks_available,13,21)
     
    temp<-schedual[schedual$SGEC==SGEC & schedual$Summer_Session==substr_weeks_available
                   & schedual$Weeks==as.numeric(substr(weeks_available,1,2)),]
  }
  
  color=c('#700001','#c10001','#bcc0c5','#4E7E9D','#1D7DBB','#1D7DBB')
  
  enrollment_limit= 80
  colorscale=cbind(seq(0, 1, by=.20), color)
  if(Enrollment_Limit==T){
    color=c('#700001','#700001','#c10001','#bcc0c5','#1D7DBB')
    enrollment_limit=max(temp$Enrollment_Limit)
    colorscale=cbind(seq(0, 1, by=.25), color)
    }
  

  temp<-droplevels(temp)


  if(length==6){
    
    week=substr(week,1,2)
    
    if((length(unique(temp$Meeting_Days)))>=2 || length(unique(temp$Start_Period))>=2){
      p<-plot_ly(temp,
                 x = temp$Meeting_Days, y =temp$Start_Period,
                 z = temp$Average_Enrollment, type = "heatmap",
                 hoverinfo='text',
                 zauto=F,
                 zmin=0,
                 zmax=enrollment_limit,
                 colors = color,
                 colorbar=list(
                   title='Average Enrollment'
                 ),
                 colorscale=colorscale,
                 text=paste("Average Enrollment:",temp$Average_Enrollment ,"\nSections:",temp$Sections,
                            "\nHours:",temp$Standard_Hours)
      )%>%layout(margin=list(l = 110,pad = 2 ))%>%
        config(plot_ly(), displaylogo = FALSE,collaborate =FALSE,
               modeBarButtonsToRemove = list('sendDataToCloud','lasso2d','zoom2d','select2d',
                                             'hoverClosestCartesian','hoverCompareCartesian',
                                             'toImage','toggleSpikelines','pan2d','autoScale2d' ))
      return(p)
    }
   
     p<-plot_ly(temp,
               x = temp$Meeting_Days, y =temp$Start_Period,
               z = temp$Average_Enrollment, type = "heatmap",
               hoverinfo='text',
               zauto=F,
               zmin=0,
               zmax=enrollment_limit,
               colors = color,
               colorbar=list(
                 title='Average Enrollment'
               ),
               colorscale=colorscale,
               text=paste("Average Enrollment:",temp$Average_Enrollment ,"\nSections:",temp$Sections,
                          "\nHours:",temp$Standard_Hours)
    )%>%layout(margin=list(l = 110,pad = 2 ), xaxis=list(type="category",autorange=FALSE,range=c(-0.5,0.75)),
               yaxis=list(type="category",autorange=FALSE,range=c(-0.5,1)))%>%
      config(plot_ly(), displaylogo = FALSE,collaborate =FALSE,
             modeBarButtonsToRemove = list('sendDataToCloud','lasso2d','zoom2d','select2d',
                                           'hoverClosestCartesian','hoverCompareCartesian',
                                           'toImage','toggleSpikelines','pan2d','autoScale2d' ))
  }
  
  else{
    
    if((length(unique(temp$Meeting_Days)))>=2 || length(unique(temp$Start_Period))>=2){
      p<-plot_ly(temp,
                 x = temp$Meeting_Days, y =temp$Start_Period,
                 z = temp$Average_Enrollment, type = "heatmap",
                 hoverinfo='text',
                 zauto=FALSE,
                 zmin=0,
                 zmax=enrollment_limit,
                 colorbar=list(
                   title='Average Enrollment'
                 ),
                 colors = color,
                 colorscale=colorscale,
                 text=paste("Average Enrollment:",temp$Average_Enrollment ,"\nSections:",temp$Sections,
                            "\nHours:",temp$Standard_Hours)
      )%>%layout(margin=list(l = 110,pad = 2 ))%>%
        config(plot_ly(), displaylogo = FALSE,collaborate =FALSE,
               modeBarButtonsToRemove = list('sendDataToCloud','lasso2d','zoom2d','select2d',
                                             'hoverClosestCartesian','hoverCompareCartesian',
                                             'toImage','toggleSpikelines','pan2d','autoScale2d' ))
    }
   
     else{
      p<-plot_ly(temp,
                 x = temp$Meeting_Days, y =temp$Start_Period,
                 z = temp$Average_Enrollment, type = "heatmap",
                 hoverinfo='text',
                 zauto=FALSE,
                 zmin=0,
                 zmax=enrollment_limit,
                 colorbar=list(
                   title='Average Enrollment'
                 ),
                 colors = color,
                 colorscale=colorscale,
                 text=paste("Average Enrollment:",temp$Average_Enrollment ,"\nSections:",temp$Sections,
                            "\nHours:",temp$Standard_Hours)
      )%>%layout(margin=list(l = 110,pad = 2 ), xaxis=list(type="category",autorange=FALSE,range=c(-0.5,0.75)),
                 yaxis=list(type="category",autorange=FALSE,range=c(-0.5,1)))%>%
        config(plot_ly(), displaylogo = FALSE,collaborate =FALSE,
               modeBarButtonsToRemove = list('sendDataToCloud','lasso2d','zoom2d','select2d',
                                             'hoverClosestCartesian','hoverCompareCartesian',
                                             'toImage','toggleSpikelines','pan2d','autoScale2d' ))
    }
    
  }
  
  return(p)
  
  
}

#assume data is just data and has not been dissagregated by weeks or 
get_advaced_graph<-function(data, week, filter,filter_option, SGEC=F, Enrollment_Limit=F){
  copy_week=week
  week=substr(week,1,2)
  if(filter=="Start Period"){
    filter="Start_Period"
    y="Meeting_Days"
  }
  else{
    filter="Meeting_Days"
    y="Start_Period"
  }
  #not summer
  if(data$Summer_Session[1]=="Not summer"){
    temp<- data[data$SGEC==SGEC & data$Weeks==as.numeric(week) & data[[filter]]==filter_option,] #just need week, and filter option 
  }
  else{#summer
    #need copy_weeks and session
    if(nchar(copy_week)==22)#allied health has 10 copy_week classes throws off substring
      substr_copy_week=substr(copy_week,12,20)
    else
      substr_copy_week=substr(copy_week,13,21)

    temp<-data[data$SGEC==SGEC & data$Summer_Session==substr_copy_week
                   & data$Weeks==as.numeric(substr(copy_week,1,2)) & data[[filter]]==filter_option,]
  }
  
  color=c('#700001','#c10001','#bcc0c5','#4E7E9D','#1D7DBB','#1D7DBB')
  
  enrollment_limit= 80
  colorscale=cbind(seq(0, 1, by=.20), color)
  if(Enrollment_Limit==T){
    color=c('#700001','#700001','#c10001','#bcc0c5','#1D7DBB')
    enrollment_limit=max(temp$Enrollment_Limit)
    colorscale=cbind(seq(0, 1, by=.25), color)
  }
  
  temp<-droplevels(temp)
  
  if(nrow(temp)==1){
    p<-plot_ly(temp,
               x = temp$Course, y =temp[[y]],
               z = temp$Average_Enrollment, type = "heatmap",
               hoverinfo='text',
               zauto=FALSE,
               zmin=0,
               zmax=enrollment_limit,
               colorbar=list(
                 title='Average Enrollment'
               ),
               colors = color,
               colorscale=colorscale,
               text=paste("Average Enrollment:",temp$Average_Enrollment ,"\nSections:",temp$Sections,
                          "\nHours:",temp$Standard_Hours)
    )%>%layout(margin=list(l = 110,pad = 2 ), xaxis=list(type="category",autorange=FALSE,range=c(-0.5,0.75)),
               yaxis=list(type="category",autorange=FALSE,range=c(-0.5,1)))%>%
      config(plot_ly(), displaylogo = FALSE,collaborate =FALSE,
             modeBarButtonsToRemove = list('sendDataToCloud','lasso2d','zoom2d','select2d',
                                           'hoverClosestCartesian','hoverCompareCartesian',
                                           'toImage','toggleSpikelines','pan2d','autoScale2d' ))
    return(p)
  }
    p<-plot_ly(temp,
               x = temp$Course, y =temp[[y]],
               z = temp$Average_Enrollment, type = "heatmap",
               hoverinfo='text',
               zauto=FALSE,
               zmin=0,
               zmax=enrollment_limit,
               colorbar=list(
                 title='Average Enrollment'
               ),
               colors = color,
               colorscale=colorscale,
               text=paste("Average Enrollment:",temp$Average_Enrollment ,"\nSections:",temp$Sections,
                          "\nHours:",temp$Standard_Hours)
    )%>%layout(margin=list(l = 110,pad = 2 ))%>%
      config(plot_ly(), displaylogo = FALSE,collaborate =FALSE,
             modeBarButtonsToRemove = list('sendDataToCloud','lasso2d','zoom2d','select2d',
                                           'hoverClosestCartesian','hoverCompareCartesian',
                                           'toImage','toggleSpikelines','pan2d','autoScale2d' ))
  
  return (p)
}

#get start period or meeting days based on specified week
get_filter<-function(data,semester, week, selection){
 #prepare to get column
  if(selection=="Start Period")
    selection="Start_Period"
  else
    selection="Meeting_Days"
  
   #determine if in summer or not
  if(semester=="Summer"){
    if(nchar(week)==22){#allied health has 10 week classes throws off substring
      week_chosen=as.numeric(substr(week,1,2))
      session=substr(week,12,20)
    }
    else{
      week_chosen=as.numeric(substr(week,1,2))
      session=substr(week,13,21)
    }
    
    result=unique(data[data$Summer_Session==session & data$Weeks==week_chosen,][[selection]])
    return(sort(result,method=levels(result)))
  }else{
    week_chosen=as.numeric(gsub(x=week,pattern = " weeks", replacement = ""))
    result=unique(data[data$Weeks==week_chosen,][[selection]])
    return(sort(result,method=levels(result)))
  }
}

#if south gate is available will return true
south_gate_available<-function(data,semester, week, selection, filter_type){
  #prepare to get column
  if(selection=="Start Period")
    selection="Start_Period"
  else
    selection="Meeting_Days"
  
  #determine if in summer or not
  if(semester=="Summer"){
    if(nchar(week)==22){#allied health has 10 week classes throws off substring
      week_chosen=as.numeric(substr(week,1,2))
      session=substr(week,12,20)
    }
    else{
      week_chosen=as.numeric(substr(week,1,2))
      session=substr(week,13,21)
    }
    
    result=data[data$Summer_Session==session & data$Weeks==week_chosen & data[[selection]]==filter_type,]
    return(nrow(result[result$SGEC==1,])>0)
  }else{
    week_chosen=as.numeric(gsub(x=week,pattern = " weeks", replacement = ""))
    result=data[data$Weeks==week_chosen & data[[selection]]==filter_type,]
    return(nrow(result[result$SGEC==1,])>0)
  }
}
  
earliest_projection_year<-function(){
  current_month=as.integer(format(Sys.Date(),format="%m"))
  current_year=as.integer(format(Sys.Date(),format="%Y"))
  if(current_month<6){#
    return(paste0(current_year,"-",current_year+1))
  }
  else{#fall
    return(paste0(current_year+1,"-",current_year+2))
  }
}
