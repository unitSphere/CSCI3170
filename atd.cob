      ******************************************************************
      * Author: Ryustami Ubaydullo
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

                    *>
       *>  CSCI3180 Principles of Programming Languages
       *>
       *>  --- Declaration ---
       *>
       *>  I declare that the assignment here submitted is original
       *> except for source
       *>  material explicitly acknowledged. I also acknowledge that
       *> I am aware of
       *>  University policy and regulations on honesty in academic
       *> work, and of the
       *>  disciplinary guidelines and procedures applicable to
       *> breaches of such policy
       *>  and regulations, as contained in the website
       *>  http://www.cuhk.edu.hk/policy/academichonesty/
       *>
       *>  Assignment 1
       *>  Name : Rustami Ubaydullo
       *>  Student ID : 1155102622
       *>  Email Addr : 1155102622@link.cuhk.edu.hk


       PROGRAM-ID. CSCI3180ASG1.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           *> Here I am selecting 8 files with which I will be working
           SELECT IN-EMPLOY ASSIGN TO 'employees.txt'
           organization is line sequential
           file STATUS is fs.
           SELECT IN-ATTEND ASSIGN TO 'attendance.txt'
           organization is line sequential.
           SELECT IN-M-ATTEND ASSIGN TO 'monthly-attendance.txt'
           organization is line sequential
           file status is fs-month.

           SELECT OUT-M-ATTEND ASSIGN TO 'monthly-attendancecob.txt'
           organization is line sequential.
           SELECT OUT-SUMMARY ASSIGN TO 'summarycob.txt'
           organization is line sequential.

           select sorted-in-attend
               assign to 'sorted-attendance.txt'
               organization is line sequential
               file status is fs-2.

           select tmp-1
               assign to 'temp.txt'
               organization is line sequential.
           select tmp-2
               assign to 'temp.txt'
               organization is line sequential.

      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.



       FD IN-EMPLOY.
       01 staff-record.
           05 staff-id pic x(4).
           05 last-name pic x(10).
           05 first-name pic x(20).
           05 gender pic x(1).
           05 birth-date pic x(10).
           05 hiring-date pic x(10).
           05 department pic x(3).
           05 salary pic x(6).

       FD IN-ATTEND.
       01 attend-record-date.
           05 a-record-date pic x(10).
       01 attend-data.
           05 attend-staff-id pic x(10).
           05 attend-time pic x(16).
           05 attend-status pic x(6).



       FD IN-M-ATTEND.
       01 month-record-date.
           05 m-record-date pic x(7).
       01 month-record-data.
           05 month-staff-id pic x(4).
           05 days-absent pic x(3).
           05 m-num-late pic x(3).
           05 num-overtime pic x(3).



       FD OUT-SUMMARY.
       01 summery-record pic x(80).

       FD OUT-M-ATTEND.
       01 out-m-record pic x(15).


       *> The format of the two files belowwas introduced to
       *> me by Huzeyfe Kiran
       sd tmp-1.
       01 w-attendance-date-record.
           02 w-attendance-date pic x(10).
       01 w-attendance-info.
           02 w-attend-staff-id pic x(4).
           02 w-status-al pic x(6).
           02 w-time-al pic x(16).

       sd tmp-2.
       01 w-m-attendance-date-record.
           02 w-m-attendance-date pic x(7).
       01 w-staff-record.
           02 w-m-staff-id pic x(4).
           02 w-no-days-absent pic x(3).
           02 w-fifteen_period pic x(3).
           02 w-overtime_work_hour pic x(3).

      *-----------------------
       WORKING-STORAGE SECTION.
      *-----------------------

       01 fs pic 99.
       01 fs-2 pic 99.
       01 fs-month pic 99.


       01 ws-attendance.
           02 ws-attendance-info.
               03 ws-attendance-staff-number pic x(4).
               03 ws-status-al pic x(6).
               03 ws-year-al pic 9999.
               03 one-dash-1 pic x value '-'.
               03 ws-month-al pic 99.
               03 one-dash-2 pic x value '-'.
               03 ws-day-al pic x(2).
               03 one-dash-3 pic x value '-'.
               03 ws-hour-al pic 99.
               03 one-dash-4 pic x value '-'.
               03 ws-minute-al pic 99.


       01 ws-employees-table.
           05 ws-employees.
               10 ws-staff-number pic x(4).
               10 ws-last-name pic x(10).
               10 ws-first-name pic x(20).
               10 ws-gender pic x(1).
               10 ws-date-of-birth pic x(10).
               10 ws-hiring-date pic x(10).
               10 ws-department pic x(3).
               10 ws-monthly-salary pic x(6).

       *> Credit for the following format goes to Huzeyfe Kiran
       01 ws-m-table.
           02 ws-m-staff-id pic x(4).
           02 ws-no-days-absent pic 999.
           02 ws-fifteen_period pic 999.
           02 ws-overtime_work_hour pic 999.
           02 slash pic x(2) value "\r".


       01 summery-header pic x(24) VALUE "Daily Attendance Summary".
       01 summery-header-2.
           02 summery-constant-1 pic x(6) value "Date: ".
           02 summery-date-complete pic x(18).
           02 slash pic x(2) value "\r".
       01 summery-header-3.
           02 summery-header-3-1 pic x(13) value "Staff-ID Name".
           02 summery-header-3-2 pic x(29) value spaces.
           02 summery-header-3-3 pic x(17) value "Department Status".
           02 slash pic x(2) value "\r".
       01 summery-header-dash.
           02 dash-1 pic x(21) value '---------------------'.
           02 dash-2 pic x(21) value '---------------------'.
           02 dash-3 pic x(20) value '--------------------'.
           02 slash pic x(2) value "\r".

       01 summery-footer-1.
           02 summery-footer-1-1 pic x(22) value "Number of Presences:".

           02 summery-footer-1-3 pic ZZZZ.
           02 slash pic x(2) value "\r".
       01 summery-footer-2.
           02 summery-footer-2-1 pic x(19) value "Number of Absences:".
           02 summery-footer-2-3 pic ZZZZ.
           02 slash pic x(2) value "\r".
       01 summery-footer-3.
           02 summery-footer-3-1 pic x(24) value
           "Number of Late Arrivals:".


           02 summery-footer-3-3 pic ZZZZ.
           02 slash pic x(2) value "\r".
       01 summery-footer-4.
           02 summery-footer-4-1 pic x(31) value
           "Number of Suspicious Records:".
           02 summery-footer-4-3 pic ZZZZ.
           02 slash pic x(2) value "\r".




       01 summery-date-year pic x(4).
       01 summery-date-month pic x(8).
       01 summery-date-day pic x(2).
       01 summery-tmp pic x(80).

       01 summery-date-day1 pic x(1).

       01 summery-id-checker pic x(4).

       01 act-tracker pic 99 value 00.
       01 o-status pic x(10).
       01 num-present pic 9999 value 0000.
       01 num-late pic 9999 value 0000.
       01 num-absent pic 9999 value 0000.
       01 num-suspicious pic 9999 value 0000.
       *> These variables are responsible for Summery file
       01 o-summery-record.
           02 summery-staff-id pic x(4).
           02 space-1 pic x(5) value spaces.
           02 summery-lname pic x(10).
           02 summery-fname pic x(20).
           02 space-2 pic x(3) value spaces.
           02 summery-dept pic x(3).
           02 space-1 pic x(8) value spaces.
           02 summery-status pic x(10).
           02 slash-r pic x(2) value "/r".


       01 ws-month-date pic x(7).


       01 num pic 999 value 000.
       01 num2 pic 99 value 6.
       *> This variable will be helpful while calculateing Late periods
       01 dummy-late pic 999.
       *> This variable decides whether we are in the beginning of
       *> monthly-attendencecob.txt or not
       01 beginning pic 9 value 0.
       01 absent-dummy pic 9 value 0.
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
      **
      * The main procedure of the program
      **   open the files and read, sort, iterate and write
            open input IN-M-ATTEND.
            PERFORM read-IN-M-ATTEND-date.
            PERFORM read-monthly-data.
            PERFORM sort-IN-ATTEND-file.
            open input sorted-in-attend.
            open input IN-EMPLOY.
            open OUTPUT OUT-SUMMARY
            open OUTPUT OUT-M-ATTEND
            PERFORM read-IN-ATTEND-file.
            PERFORM read-IN-EMPLOY-file
            PERFORM write-summery.
            *> Close the files if they are still open
            close OUT-SUMMARY
            close sorted-in-attend
            close IN-EMPLOY
            close IN-M-ATTEND
            close OUT-M-ATTEND
            STOP RUN.


       write-summery.
           *> Writes the summery by writing the header part and then
           *> calling main-summery which is responsible for the summery data
           perform summery-date.
           write summery-record from summery-header
           write summery-record from summery-header-2
           write summery-record from summery-header-3
           WRITE summery-record from summery-header-dash

           perform main-summery.

        *> Simply decides the months by looking at the number from attendance file
       summery-date.
           if ws-month-al = "01" THEN
               move "January" to summery-date-month.
           if ws-month-al = "02" THEN
               move "February" to summery-date-month.
           if ws-month-al = "03" THEN
               move "March" to summery-date-month.
           if ws-month-al = "04" THEN
               move "April" to summery-date-month.
           if ws-month-al = "05" THEN
               move "May" to summery-date-month.
           if ws-month-al = "06" THEN
               move "June" to summery-date-month.
           if ws-month-al = "07" THEN
               move "July" to summery-date-month.
           if ws-month-al = "08" THEN
               move "August" to summery-date-month.
           if ws-month-al = "09" THEN
               move "September" to summery-date-month.
           if ws-month-al = "10" THEN
               move "October" to summery-date-month.
           if ws-month-al = "11" THEN
               move "November" to summery-date-month.
           if ws-month-al = "12" THEN
               move "December" to summery-date-month.
          *> If day is > 10 then 2 digits are displayed, otherwise 1 digit is displayed
           if ws-day-al < 10 then
           move ws-day-al(2:1) to summery-date-day1
           string summery-date-month delimited by space
                  " " delimited by size
                  summery-date-day1 delimited by size
                  ", " delimited by size
                  ws-year-al delimited by size
                  into summery-date-complete
           end-string
           END-IF
           if ws-day-al >= 10 THEN


           move ws-day-al to summery-date-day
           string summery-date-month delimited by space
                  " " delimited by size
                  summery-date-day delimited by size
                  ", " delimited by size
                  ws-year-al delimited by size
                  into summery-date-complete
           end-string
           end-if.


      ******************************
       sort-IN-ATTEND-file.
           sort tmp-1 on ASCENDING key w-attend-staff-id
           using IN-ATTEND giving sorted-in-attend.

       read-IN-ATTEND-file.
           read sorted-in-attend into ws-attendance
           end-read.

       read-IN-M-ATTEND-date.
          read IN-M-ATTEND into ws-month-date
          end-read.



       read-monthly-data.
           read IN-M-ATTEND into ws-m-table
           end-read.



       read-IN-EMPLOY-file.
           read IN-EMPLOY into ws-employees-table
           END-READ

           *> Close the files if employees.txt was iterated completely
           if fs = 10 THEN
                 perform write-summery-footer
                 close OUT-SUMMARY
                 close sorted-in-attend
                 close IN-EMPLOY
                 close IN-M-ATTEND
                 close OUT-M-ATTEND

                 STOP RUN
           END-IF.


       *> This function is responsible for writing the lines about emloyees in summerycob.txt
       summery-info-section.

               move ws-staff-number to summery-staff-id

               move ws-last-name to summery-lname

               move ws-first-name to summery-fname

               move ws-department to summery-dept

               write summery-record from o-summery-record.
      ** add other procedures here

       *> multiple-arrive-checker.

           *> if ws-attendance-staff-number = ws-staff-number THEN
               *> if ws-status-al = "ARRIVE" THEN
                   *> perform read-IN-ATTEND-file
                   *> if ws-attendance-staff-number NOT = ws-staff-number
                       *> THEN
                       *> DISPLAY "Next attend person"
                       *> move 00 to act-tracker
                       *> add 1 to num-suspicious
                       *> move "SUSPICIOUS" to summery-status
                       *> perform summery-info-section
                       *> perform write-monthly-data
                       *> perform read-IN-EMPLOY-file
                       *> perform read-monthly-data
                       *> DISPLAY "YOYO"
                       *> DISPLAY ws-m-staff-id

                    *> END-IF
                   *> if ws-attendance-staff-number = ws-staff-number THEN
                       *> move 11 to act-tracker
                       *> perform multiple-arrive-checker
                   *> end-if
               *> END-IF
               *> if ws-status-al = "LEAVE" and act-tracker = 11 THEN
                   *> DISPLAY "EMPLOYEE LEFT multiple-arrive-checker"
               *> END-IF
               *> if ws-status-al = "LEAVE" and act-tracker = 00 THEN
                   *> move 00 to act-tracker
                       *> add 1 to num-suspicious
                       *> move "SUSPICIOUS" to summery-status
                       *> perform summery-info-section
                       *> perform write-monthly-data
                       *> perform read-IN-EMPLOY-file
                       *> perform read-monthly-data
               *> end-if
           *> end-if.

       *> multiple-arrive-checker.
           *> if ws-attendance-staff-number = ws-staff-number THEN
               *> if ws-status-al = "ARRIVE" THEN
                   *> move ws-attendance to tmp-ws-attendance
                   *> perform read-IN-ATTEND-file
                   *> perform multiple-arrive-checker
               *> END-IF
               *> if ws-status-al = "LEAVE" THEN
                   *> DISPLAY "Employee left"

               *> END-IF
           *> END-IF

           *> if ws-attendance-staff-number NOT = ws-staff-number THEN
               *> DISPLAY "NEXT EMPLOYEE multiple-arrive-checker"
               *> add 1 to num-suspicious


           *> END-IF.



       *> next-employee.
           *> if ws-attendance-staff-number = ws-staff-number THEN
               *> perform read-IN-ATTEND-file
               *> perform next-employee
           *> END-IF
           *> if ws-attendance-staff-number NOT = ws-staff-number THEN
               *> DISPLAY "Look"
               *> DISPLAY ws-attendance-staff-number
           *> END-IF.
       *> The summery footer which consists of the lines with total number of PRESENT, LATE, SUSPICIOUS
       *> and ABSENT are written by this function
       write-summery-footer.
           WRITE summery-record from summery-header-dash
              move num-present to summery-footer-1-3
              write summery-record from summery-footer-1
              move num-absent to summery-footer-2-3
              write summery-record from summery-footer-2
              move num-late to summery-footer-3-3
              write summery-record from summery-footer-3

              move num-suspicious to summery-footer-4-3
              write summery-record from summery-footer-4.

       *> Write the information in monthlyattandencecob.txt. If it has just been opened
           *> write the date, otherwise write the employee record
       write-monthly-data.
           if beginning = 0
               write out-m-record from ws-month-date
               add 1 to beginning
           end-if
           if beginning > 0
               write out-m-record from ws-m-table
           end-if.

       *> This is my initial algorithm
       *> main-summery.

           *> if ws-status-al (1:1) = "-" THEN
               *> DISPLAY "NOTHINGSSSS"
               *> PERFORM read-IN-ATTEND-file
           *> END-IF
           *> if ws-attendance-staff-number = ws-staff-number THEN
               *> display "loking for 1983"
               *> display ws-attendance-staff-number


               *> if ws-status-al = "LEAVE" and act-tracker = 00 THEN
                   *> move "SUSPICIOUS" to summery-status
                   *> add 1 to num-suspicious
                   *> DISPLAY "GGGGSSDASDASDASDASDASDasd"
                   *> DISPLAY ws-staff-number
                   *> perform summery-info-section
                   *> perform write-monthly-data
                   *> perform next-employee

                   *> perform read-IN-EMPLOY-file

                   *> perform read-monthly-data
                   *> perform main-summery

               *> END-IF
               *> if ws-status-al = "ARRIVE" AND act-tracker = 00 THEN
                   *> if ws-hour-al = 10 and ws-minute-al >= 15
                   *> or ws-hour-al > 10 THEN
                       *> move "LATE" to summery-status
                       *> move 11 to act-tracker
                       *> perform late-calculator
                       *> *> add 1 to num-late
                       *> perform multiple-arrive-checker
                       *> perform main-summery
                   *> END-IF
                   *> if ws-hour-al < 10 OR
                   *> ws-hour-al =10 and ws-minute-al < 15
                       *> move "PRESENT" to summery-status
                       *> move 11 to act-tracker

                       *> *> add 1 to num-present
                       *> perform multiple-arrive-checker
                       *> perform main-summery
                   *> END-IF

               *> END-IF
               *> if ws-status-al = "LEAVE" and act-tracker = 11 THEN
                   *> if summery-status = "PRESENT"
                       *> add 1 to num-present
                   *> end-if
                   *> if summery-status = "LATE"
                       *> add 1 to num-late
                   *> end-if
                   *> perform summery-info-section
                   *> move 00 to act-tracker

                   *> DISPLAY ws-overtime_work_hour
                    *> perform overtime-calculator

                    *> DISPLAY "HEEEEEEE"
                    *> DISPLAY ws-overtime_work_hour
                   *> add dummy-late to ws-fifteen_period
                   *> DISPLAY ws-fifteen_period
                   *> perform write-monthly-data
                   *> perform next-employee

                   *> perform read-IN-EMPLOY-file
                   *> display fs
                   *> perform read-monthly-data
                   *> perform main-summery
               *> END-IF
           *> END-IF
           *> if ws-attendance-staff-number NOT = ws-staff-number THEN
               *> if act-tracker = 00
                   *> add 1 to num-absent
                   *> move "ABSENT" to summery-status
                   *> add 1 to ws-no-days-absent
                   *> perform summery-info-section
                   *> perform write-monthly-data
                   *> display fs

                   *> perform read-IN-EMPLOY-file

                   *> perform read-monthly-data

                   *> perform main-summery

               *> END-IF
               *> if act-tracker = 11

                   *> move 'SUSPICIOUS' to summery-status
                   *> add 1 to num-suspicious
                   *> perform summery-info-section
                   *> perform write-monthly-data
                   *> perform read-IN-EMPLOY-file
                   *> perform read-monthly-data


                   *> move 00 to act-tracker
                   *> perform next-employee
                   *> perform main-summery
               *> END-IF

           *> END-IF.



       *> Algorithm fo printing the summery
       *> As my algorithm (above) struggled to print the correct results I got
       *> limited help from Huzeyfe Kiran to write new algorithm

       main-summery.

           if ws-attendance-staff-number = ws-staff-number THEN
               *> If the person just arrived
               if act-tracker = 00 and ws-status-al = "ARRIVE" THEN
                   move 10 to act-tracker
                   *> See if he is late
                   if ws-hour-al = 10 and ws-minute-al < 15
                       or ws-hour-al < 10 THEN
                       move "PRESENT" to summery-status

                   END-IF

                   if ws-hour-al = 10 and ws-minute-al >=15
                       or ws-hour-al > 10 THEN
                       move "LATE" to summery-status
                       perform late-calculator
                       add dummy-late to ws-fifteen_period
                   END-IF
                   *> check the next attendance record
                   perform read-IN-ATTEND-file
                   perform main-summery
               END-IF
               *> if the person has arrived but had already arrived before

                   if act-tracker = 10 and ws-status-al = "ARRIVE" THEN
                   *> Ignore him
                   perform read-IN-ATTEND-file
                   perform main-summery
               END-IF
               *> if the person had left before but had arrived again
               if act-tracker = 01 and ws-status-al = "ARRIVE" THEN
                  *> Ignore him
                   perform read-IN-ATTEND-file
                   perform main-summery
               END-IF
               *> If a person had arrived before and left but then arrived again
               if act-tracker = 11 and ws-status-al = "ARRIVE" THEN
                   *> Reset the act-tracker and move to the next employee
                   move 00 to act-tracker
                   perform read-IN-ATTEND-file
                   perform read-IN-EMPLOY-file
                   perform read-monthly-data
                   perform main-summery
               END-IF
               *> if the person had not arrived before but is leaving
                if act-tracker = 00 and ws-status-al = "LEAVE" THEN
                   *> Write suspicious in month output file and move to the next attendance record
                    move "SUSPICIOUS" to summery-status
                    add 1 to num-suspicious
                    move 01 to act-tracker
                    perform summery-info-section
                    perform write-monthly-data
                    perform read-IN-ATTEND-file
                    perform main-summery
                END-IF

               *> if the person had arrived before and is leaving now
               if act-tracker = 10 and ws-status-al = "LEAVE" THEN
                   move 11 to act-tracker
                   *> increment Late/PRESENT depending on employee and
                   *> write the result in month output
                   if summery-status = "LATE" THEN
                       add 1 to num-late
                   end-if
                   if summery-status = "PRESENT" THEN
                       add 1 to num-present
                   END-IF
                   perform summery-info-section
                   perform overtime-calculator
                   perform write-monthly-data
                   perform read-IN-ATTEND-file
                   PERFORM main-summery
                END-IF
                *> If the person had not arrived before but has left
                    *> and is leaving again
                if act-tracker = 01 and ws-status-al = "LEAVE" THEN
                    *> Ignore him
                    perform read-IN-ATTEND-file
                    perform main-summery
                END-IF

                if act-tracker = 11 and ws-status-al = "LEAVE" THEN
                    PERFORM read-IN-ATTEND-file
                    perform main-summery
                END-IF
           END-IF
           *> If the ID's of Attendence and Employees files don't match
           if ws-attendance-staff-number NOT = ws-staff-number THEN
               *> If the person had not come or left at all
               if act-tracker = 00 then
                   *> Mark as ABSENT
                   move "ABSENT" to summery-status
                   add 1 to num-absent
                   add 1 to ws-no-days-absent
                   perform write-monthly-data
                   perform summery-info-section

                   perform read-IN-EMPLOY-file
                   PERFORM read-monthly-data
                   perform main-summery
               END-IF
               *> If the person had left ones
               if act-tracker = 01 THEN
                   *> Ignore him, because the SUSPICIOUS was written in the file already
                   perform read-IN-EMPLOY-file
                   move 00 to act-tracker
                   PERFORM read-monthly-data
                   perform main-summery
               END-IF
               *> If the person had arrived but has no leave record
               if act-tracker = 10 THEN
                   *> Write suspicious in month output file and move to the next emloyee
                   move "SUSPICIOUS" to summery-status
                   add 1 to num-suspicious
                   perform write-monthly-data
                   perform summery-info-section
                   move 00 to act-tracker
                   perform read-IN-EMPLOY-file
                   PERFORM read-monthly-data
                   perform main-summery
               END-IF
               *> If the person had come and left accordingly
               if act-tracker = 11 THEN
                   *> reset act-tracker and move on, because he was handled already
                   MOVE 00 to act-tracker
                   PERFORM read-IN-EMPLOY-file
                   PERFORM read-monthly-data
                   PERFORM main-summery
               END-IF
           END-IF.


       overtime-calculator.
       *> Calculates the overtime hours and assigns the increments
       *> ws-overtime_work_hour
           if ws-hour-al > 17 and
               ws-attendance-staff-number = ws-staff-number
               subtract 17 from ws-hour-al
               add ws-hour-al to ws-overtime_work_hour
               if ws-overtime_work_hour > 30 and
                   ws-attendance-staff-number = ws-staff-number
                   move 30 to ws-overtime_work_hour
               end-if
           end-if.

       late-calculator.
        *> Calculates the number of quarters and assigns the
        *> result to dummy-late
           move 0 to dummy-late
           if ws-hour-al >= 10
               subtract 10 from ws-hour-al
               multiply ws-hour-al by 4 giving num
               add num to dummy-late
               divide ws-minute-al by 15 giving num
               add num to dummy-late
           end-if.
       END PROGRAM CSCI3180ASG1.
