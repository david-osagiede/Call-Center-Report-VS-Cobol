       identification division.
       program-id. A7-CallCenterOpReport.
       author. David Osagiede.
       date-written. 2022-03-31.
      *Program Description: This program will read records from a data 
      *file, interprets the data using an array, performs analysis
      * and writes a formatted report output to report on the activity 
      * of our call centre operators
      *
       environment division.
       input-output section.
       file-control.
      *
           select emp-file
               assign to '../../../data/A7.dat'
               organization is line sequential.
      *
           select report-file
               assign to '../../../data/A7-CallCenterOpReport.out'
               organization is line sequential.
      *
       data division.
       file section.
      *
       fd emp-file
           data record is emp-rec
           record contains 51 characters.
      *
       01 emp-rec.
           05 emp-rec-num              pic x(3).
           05 emp-rec-name             pic x(12).
           05 emp-rec-calls            pic 9(3) occurs 12.
         
      *

       fd report-file
           data record is report-line
           record contains 132 characters.
      *
       01 report-line                  pic x(132).
      *
       working-storage section.
      *
      *create the necessary working storage variables
      *
       01 ws-months.
           05 filler                   pic x(3)
             value 'Jul'.
           05 filler                   pic x(3)
             value 'Aug'.
           05 filler                   pic x(3)
             value 'Sep'.
           05 filler                   pic x(3)
             value 'Oct'.
           05 filler                   pic x(3)
             value 'Nov'.
           05 filler                   pic x(3)
             value 'Dec'.
           05 filler                   pic x(3)
             value 'Jan'.
           05 filler                   pic x(3)
             value 'Feb'.
           05 filler                   pic x(3)
             value 'Mar'.
           05 filler                   pic x(3)
              value 'Apr'.
           05 filler                   pic x(3)
             value 'May'.
           05 filler                   pic x(3)
             value 'Jun'.
           05 filler                   pic x(96)
             value spaces.
       01 ws-r-month-names redefines ws-months.
           05 ws-month-name            pic x(3) occurs 12 times.
       01 ws-constants.
           05 ws-number-of-months      pic 99   value 12.
           05 ws-ctr                   pic 99   value 0.
      *
       01 ws-calculated-fields.
           05 ws-non-zero-month-count  pic 9(2) value 0.
           05 ws-zero-month-count      pic 9(2) value 0.
      *
       01 ws-eof-flag                  pic x    value 'n'.
           88 ws-end-of-file                    value "y".
      *
       01 ws-totals.
           05 ws-op-highest-avg        pic 9(5) value 0.
           05 ws-op-lowest-avg         pic 9(5) value 0.
           05 ws-highest-avg           pic 9(5) value 0.
           05 ws-emp-total             pic 9(5) value 0.
           05 ws-total-no-calls        pic 9(5) value 0.
           05 ws-total-no-calls-mths   pic 9(5) value 0.
           05 ws-highest-ops           pic 9(3) value 0.
           05 ws-lowest-ops            pic 9(3) value 0.
           05 ws-month-highest-ops     pic 9(3) value 0.
      *    05 ws-count                 pic 9(3) value 0.
        
         
           05 ws-rem-emp               pic 99   value 0.
           05 ws-non-zero-calls        pic 99   value 12.
           05 ws-zero-calls            pic 99   value 0.
           05 ws-total-call            pic 9(5) occurs 12 value 0.
           05 ws-total-amount          pic 9(5) occurs 12 value 0.
           05 ws-total-averages        pic 9(5) occurs 12 value 0.
           05 ws-calc-average          pic 9(5) value 0.
      *
       01 ws-name-line.
           05 filler                   pic x(5)
               value spaces.
           05 filler                   pic x(25)
               value '    David Osagiede    '.
      *               ----+----1----+----2----+
           05 filler                   pic x(29)
               value '                        lab 7'.
      *               ----+----1----+----2----+----
           05 filler                   pic x(5)
               value spaces.
           05 ws-name-line-date        pic 9(6).
           05 filler                   pic x(4)
               value spaces.
           05 ws-name-line-time        pic 9(8).
           05 filler                   pic x(50)
               value spaces.
      *
       01 ws-report-heading.
           05 filler                   pic x(40)
               value spaces.
           05 filler                   pic x(40)
               value 'call centre volumes for july - june     '.
      *               ----+----1----+----2----+----3----+----4
           05 filler                   pic x(40)
               value spaces.
           05 filler                   pic x(12)
               value spaces.
      *
       01 ws-heading-line1.
           05 filler                   pic x(2) value spaces.
           05 filler                   pic x(8) value 'operator'.
           05 filler                   pic x(2) value spaces.
           05 filler                   pic x(8) value 'operator'.
           05 filler                   pic x(7) value spaces.
           05 filler                   pic x(3) value 'jul'.
           05 filler                   pic x(4) value spaces.
           05 filler                   pic x(3) value 'aug'.
           05 filler                   pic x(4) value spaces.
           05 filler                   pic x(3) value 'sep'.
           05 filler                   pic x(4) value spaces.
           05 filler                   pic x(3) value 'oct'.
           05 filler                   pic x(4) value spaces.
           05 filler                   pic x(3) value 'nov'.
           05 filler                   pic x(4) value spaces.
           05 filler                   pic x(3) value 'dec'.
           05 filler                   pic x(4) value spaces.
           05 filler                   pic x(3) value 'jan'.
           05 filler                   pic x(4) value spaces.
           05 filler                   pic x(3) value 'feb'.
           05 filler                   pic x(4) value spaces.
           05 filler                   pic x(3) value 'mar'.
           05 filler                   pic x(4) value spaces.
           05 filler                   pic x(3) value 'apr'.
           05 filler                   pic x(4) value spaces.
           05 filler                   pic x(3) value 'may'.
           05 filler                   pic x(4) value spaces.
           05 filler                   pic x(3) value 'jun'.
           05 filler                   pic x(4) value spaces.
           05 filler                   pic x(5) value 'total'.
           05 filler                   pic x(4) value spaces.
           05 filler                   pic x(3) value 'avg'.
           05 filler                   pic x(4) value spaces.
           05 filler                   pic x(3) value 'rem'.
           05 filler                   pic x(2) value spaces.
      *
       01 ws-heading-line2.
           05 filler                   pic x(5) value spaces.
           05 filler                   pic x(1) value '#'.
           05 filler                   pic x(8) value spaces.
           05 filler                   pic x(4) value 'name'.
           05 filler                   pic x(114) 
               value spaces.
      *
       01 ws-detail-line.
           05 filler                   pic x(4) 
               value spaces.
           05 ws-detail-line-num       pic x(3).
           05 filler                   pic x(4) 
               value spaces.
           05 ws-detail-line-name      pic x(12).
          
           05 ws-detail-line-months    pic z(6)9 occurs 12.
           05 filler                   pic x(4) 
               value spaces.
           05 ws-detail-line-total     pic zzzz9.
           05 filler                   pic x(2) 
               value spaces.
           05 ws-detail-line-avg       pic zzzz9.
           05 ws-detail-line-avg-r redefines ws-detail-line-avg.

           10 filler                   pic x.

           10 ws-detail-line-zero-lit  pic x(4).
           05 filler                   pic x(6) 
               value spaces.
           05 ws-detail-line-rem       pic 9.
           05 filler                   pic x(2) 
              value spaces.
       01 ws-total-line11.
         05 filler                     pic x(4)
              value spaces.
         05 filler                     pic x(25)
              value "Operators with calls     ".
      *               ----+----1----+----2----+----3----+
         05 ws-total-calls             pic zzzz9 OCCURS 12.
         05 filler                     pic x(43)
              value spaces.

       01 ws-total-line12.
         05 filler                     pic x(4)
              value spaces.
         05 filler                     pic x(22)
              value "Totals                ".
      *               ----+----1----+----2----+----3----+
           05 ws-total                 pic zzzz9 OCCURS 12.
           05 filler                   pic x(46)
              value spaces.

       01 ws-total-line13.
           05 filler                   pic x(4)
             value spaces.
           05 filler                   pic x(24)
               value  "Averages                ".
      *               ----+----1----+----2----+----3----+
           05 ws-average OCCURS 12.
               10 ws-average-r         pic 9(3)
               value 0.
           10 filler                   pic x(3)
               value spaces.
           
           05 filler                   pic x(32)
               value spaces.

       01 ws-total-line1.
           05 filler                   pic x(6) 
               value spaces.
           05 filler                   pic x(35)
               value "number of operators with no calls: ".
      *               ----+----1----+----2----+----3----+
           05 ws-total-line-no-calls   pic zzzz9.
           05 filler                   pic x(86) 
               value spaces.
      *
       01 ws-total-line2.
           05 filler                   pic x(6) 
               value spaces.
           05 filler                   pic x(35)
               value "number of months with no calls:    ".
      *               ----+----1----+----2----+----3----+

           05 ws-total-line-zero-mths  pic zzzz9.
           05 filler                   pic x(86) 
               value spaces.
      *
       01 ws-total-line3.
           05 filler                   pic x(6)             
               value spaces.
           05 filler                   pic x(39) 
               value "month with the Highest Monthly Average:    ".
      *               ----+----1----+----2----+----3----+
       
           05 ws-total-line-highest-avg
                                       pic zzzz9.
           05 filler                   pic x(1)
               value spaces.
           05 ws-total-line-month-index
                                       pic 99.
         
           05 filler                   pic x(79)             
               value spaces.
      *

       01 ws-total-line4.
           05 filler                   pic x(6)                
               value spaces.
           05 filler                   pic x(45) 
               value "Operator with the Lowest Monthly Average:    ".
      *               ----+----1----+----2----+----3----+
           
           05 ws-total-line-lowest-avg pic zzzz9.
           05 filler                   pic x(1)
               value spaces.
           05 ws-total-line-lowest-index
                                       pic 99.
           05 filler                   pic x(73)                
               value spaces.
      *

       01 ws-total-line5.
           05 filler                   pic x(6)                    
               value spaces.
           05 filler                   pic x(46)                    
               value "Operator with the Highest Monthly Average:    ".
      *               ----+----1----+----2----+----3----+
          
           05 ws-total-line-op-highest-avg                            
                                       pic zzzz9.
           05 filler                   pic x(1)
               value spaces.
           05 ws-total-line-highest-index
                                       pic 99.
           05 filler                   pic x(72)              
               value spaces.
      *
      *
       01 ws-total-line6.
           05 filler                   pic x(6) 
               value spaces.
           05 filler                   pic x(35)
               value "overall total calls:               ".
      *               ----+----1----+----2----+----3----+
           05 ws-total-line-calls      pic zzzz9.
           05 filler                   pic x(86) 
               value spaces.
     
       procedure division.
      *
       000-main.
      *
      *open files  
           open input  emp-file,
                output report-file.
      *
      *get the current date & time
           accept ws-name-line-date from date.
           accept ws-name-line-time from time.
      *
      *output first headings
           perform 100-print-headings.
      *
      *process input file & output results
           perform 200-read-input-file.
      *

           perform 300-process-records
               until ws-end-of-file.
      *
      *output total lines
           perform 400-print-totals.
      *
      *close files
           close emp-file
                 report-file.
      *
           stop run.
      *
       100-print-headings.
      *
           write report-line from ws-name-line 
               after advancing 1 line.
      *
           write report-line from ws-report-heading
               after advancing 1 line.
      *
           write report-line from ws-heading-line1 
               after advancing 2 lines.
      *
           write report-line from ws-heading-line2 
               after advancing 1 line.
      *
       200-read-input-file.
      *reads a line from input file & stores it in emp-rec
      * - unless eof is encountered in which case it sets
      *    ws-eof-flag to y                                              
           read emp-file 
           	   at end move 'y'         to ws-eof-flag.

       300-process-records.
      * TODO: Use Perform Varying to loop through monthly calls
      *       in each record to calculate the required values
      *       for each record and accumulate the required data
      *       for total lines
           perform varying ws-ctr  from 1 by 1
             until ws-ctr > ws-number-of-months
               add emp-rec-calls(ws-ctr)
                                       to ws-emp-total
           end-perform.


      * TODO: Implement average calculation logic
      *       as outlined in the requirments
      * divide ws-total-emp by ws-emp-non-zero-calls
      *giving ws-avg-emp remainder ws-rem-emp
           
           divide ws-emp-total by ws-non-zero-calls
             giving ws-average-r(12) remainder ws-rem-emp.

           move spaces                 to ws-detail-line-avg-r

           if ws-average-r(12) > 0

             then
               move ws-average-r(12)   to ws-detail-line-avg

           else
               move 'ZERO'             to ws-detail-line-zero-lit.

         

      * TODO: Move required data to detail line for output
      *  
           move emp-rec-num            to ws-detail-line-num.
           move emp-rec-name           to ws-detail-line-name.
           move ws-rem-emp             to ws-detail-line-rem.
           move ws-emp-total           to ws-detail-line-total.
      *    move ws-average             to ws-detail-line-avg.

           perform varying ws-ctr from 1 by 1
             until ws-ctr > ws-number-of-months
               move emp-rec-calls(ws-ctr)
                                       to ws-detail-line-months(
               ws-ctr)
           end-perform.

           add emp-rec-calls(12)       to ws-total-amount(12).
      *       this adds the calls for each month from the current
      *      operator input
      *       one at a time to the respective monthly
      *    call total
      *      * then you can output ws-total-call table of 12 items
      *      across the bottom of the output
      *       in an 01 output group setup with a ws-total-call-output
      *      of 12 items in the same way
      *        before the
      *    write from statement for the 01 output group
             
           perform varying ws-ctr from 1 by 1
             until ws-ctr > ws-number-of-months
               add emp-rec-calls(ws-ctr)
                 to ws-emp-total
               add emp-rec-calls(ws-ctr)
                 to ws-total-amount(ws-ctr)
               if emp-rec-calls(ws-ctr) > 0
                   add 1 to ws-total-call(ws-ctr)
               end-if
           end-perform.
      *
      * find highest & lowest monthly average 
           if ws-average-r(12) > ws-op-lowest-avg
               move emp-rec-num
                                       to ws-op-lowest-avg
               move emp-rec-num        to ws-total-line-lowest-index
           end-if.
           move 0 to ws-average-r(12)
           if ws-average-r(12) <= ws-op-highest-avg
               move emp-rec-num
                                       to ws-op-highest-avg
               
               move emp-rec-num        to ws-total-line-highest-index
           end-if.
           move 1 to ws-average-r(12)
           if ws-average-r(12) > ws-highest-avg
               move ws-month-name(12)
                                       to ws-highest-avg
               subtract 1 from ws-ctr 
               move ws-ctr             to ws-total-line-month-index
             
           end-if.
      * checks to see operaters & months with no calls
           if emp-rec-calls(12) = 0
              
               Add 1                   to ws-total-no-calls-mths
           end-if.
           if ws-detail-line-zero-lit equals 'ZERO' then
               add 1 to ws-zero-month-count

           end-if.
      *    Calculate the average
           compute ws-average-r(12) = ws-emp-total / ws-zero-month-count
          
           
      *
      * print detail line
           write report-line from ws-detail-line
               after advancing 1 lines.
      *
      * TODO: reset fields for next record
           move 0                      to ws-emp-total.
           move 0                      to ws-non-zero-month-count.
           
         

      *
      * read next record (if any)
           perform 200-read-input-file.
      *
       400-print-totals.
      *
      * TODO: Move required data to total lines for output
      *             
           move ws-zero-month-count    to ws-total-line-no-calls.
           move ws-total-amount(12)    to ws-total-line-calls.
           move ws-op-highest-avg      to ws-total-line-op-highest-avg.
           move ws-op-lowest-avg       to ws-total-line-lowest-avg.
           move ws-highest-avg         to ws-total-line-highest-avg.
           move ws-total-no-calls-mths to ws-total-line-zero-mths.
           
           perform varying ws-ctr from 1 by 1
             until ws-ctr > ws-number-of-months
               move ws-total-call(ws-ctr) to ws-total-calls(ws-ctr)
               move ws-total-amount(ws-ctr) to ws-total(ws-ctr)
               divide ws-total-amount(ws-ctr) by ws-total-call(ws-ctr)
                 giving ws-calc-average rounded
               move ws-calc-average to ws-average(ws-ctr)
           end-perform.
      *
           write report-line from ws-total-line11
               after advancing 2 lines.
           write report-line from ws-total-line12
               after advancing 2 lines.
           write report-line from ws-total-line13
               after advancing 2 lines.
           write report-line from ws-total-line1
               after advancing 2 lines.
           write report-line from ws-total-line2
               after advancing 2 lines.
           write report-line from ws-total-line3
               after advancing 2 lines.
           write report-line from ws-total-line4
               after advancing 2 lines.
           write report-line from ws-total-line5
               after advancing 2 lines.
           write report-line from ws-total-line6
               after advancing 2 lines.
      *
       end program A7-CallCenterOpReport.