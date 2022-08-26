library(readxl)
library(pander)
filter=dplyr::filter
## [1] "序号"             "开课院系"         "课程编码"         "课程名称"        
## [5] "课程属性"         "所属学科/专业"    "课时/学分"        "限选人数"        
## [9] "已选人数"         "开课周"           "星期节次"         "教室"            
## [13] "授课方式"         "考试方式"         "首席教授"         "首席教授所属单位"
## [17] "主讲教师"        
raw_1<-read_excel("2022年秋季学期全校课表.xlsx", col_types = "text")
## 081201M05003H(超大规模集成电路基础), 0839X1M05005H(安全芯片技术), 081201M04002H(计算机体系结构), 081203M04001H(计算机算法设计与分析)
## my_courses<-c("0839X1M05005H","081201M05003H", "081201M04002H", "081203M04001H", "120400MGB001H-02", "083900MGB001H-02") #  "120400MGB001H-23", 学术道德与学术写作规范-通论
course_id<-c("081203M04002H","081201M04002H","081203M04001H","081201M05003H","081202M05010H","0839X1M05005H","120400MGB001H-02","010108MGB001H-20","030500MGB001H-21")


## > my_courses$`开课周`
## [1] "第2-12周"     "第2-20周"     "第2-16周"     "第2-20周"     "第8-12周"
## [6] "第2-6,8-15周" "第13-17周"
## > my_courses$`星期节次`
## [1] "周一(9-12)" "周一(9-11)" "周四(5-7)"  "周二(9-11)" "周二(9-10)"
## [6] "周三(9-11)" "周一(9-10)"

chinese_day_map=c("一"=1, "二"=2,"三"=3,"四"=4,"五"=5,"六"=6,"日"=7)
weekname=c("Mon","Tue","Wed","Thur","Fri","Sat","Sun")
check_and_print<-function(course_id, print_table=FALSE) {
    my_courses=raw_1%>%filter(`课程编码` %in% course_id) # course_id<-c("081201M05003H", "0839X1M05005H", "081201M04002H", "081203M04001H", "010108MGB001H-02", "120400MGB001H-23", "083900MGB001H-01")
timetable=list()
for(i in c(1:20)) { # 一共 20 周
    timetable[[i]]=matrix("",nrow=12,ncol=7, dimnames=list(str_c("第", as.character(c(1:12)), "节"), weekname)) # 一天 12 节课, 7 天
}

n=nrow(my_courses)
for(i in c(1:n)) {
  ## i =1 
  row = my_courses[i,]
  # 处理`开课周`
  course_str=str_wrap(sprintf("%s(%s)", row$`课程名称`, row$`教室`),width=8)
  week_info_list=str_split(str_split(str_sub(row$`开课周`, 2, -2),',')[[1]], '-') # 得到会是一个 list
  week_v = c()
  for(j in c(1:length(week_info_list))) {
      week_v=c(week_v, seq(week_info_list[[j]][1],week_info_list[[j]][2]))
}
  week_v=unique(week_v) # 存着周向量, 比如 c(2,3,4) 就是 2-4 周
  # day: 需要得到是第几天
  match_info=str_match(row$`星期节次`, "周(一|二|三|四|五|六|七)\\((\\d+)-(\\d+)\\)")[1,c(2:4)] # "周一(9-10)"
  day = chinese_day_map[match_info[1]]
  class_num = seq(as.integer(match_info[2]), as.integer(match_info[3]))
  for(j in week_v) {
      existing=str_c(timetable[[j]][class_num,day], collapse="")
      if(existing!="") {
          cat(sprintf("%s(%s) fail(%s) with existing %s", row$`课程名称`, row$`课程编码`, row$`星期节次`,existing))
return(FALSE)

}
      timetable[[j]][class_num,day]=course_str # 课程 string
}
}
if(print_table==TRUE) {
    pandoc.table(timetable[[2]], style = 'grid', keep.line.breaks = TRUE, emphasize.verbatim.rows=c())
}
return(TRUE)
}

check_and_print(course_id, TRUE)
# 修改 "课时/学分" 变量
my_courses$`课程名称`

## # %%
## # 选 学术道德与学术写作规范-通论
## tonglun=raw_1%>%dplyr::filter(`课程名称` == "学术道德与学术写作规范-通论")
## tonglun_all_id=tonglun$`课程编码`
## ## tonglun%>%filter(==1)
## tonglun_available=c()
## for(tonglun_id in tonglun_all_id) {
##     if(check_and_print(c(course_id, tonglun_id))) {
##         tonglun_available=c(tonglun_available, tonglun_id)
##     }
## }

## ## wa=raw_1%>%dplyr::filter(`课程名称` == "女子蛙泳")
## length(tonglun_available)
## length(tonglun_all_id)
