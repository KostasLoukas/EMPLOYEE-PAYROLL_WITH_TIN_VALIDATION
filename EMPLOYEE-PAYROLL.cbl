       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPLOYEE-PAYROLL.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
               DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PAYROLL-FILE ASSIGN TO DISK
             FILE STATUS IS PAYROLL-FILE-STATUS.

           SELECT VALID-PAYROLL-FILE ASSIGN TO DISK
             FILE STATUS IS VALID-PAYROLL-FILE-STATUS.

           SELECT INVALID-PAYROLL-FILE ASSIGN TO DISK
             FILE STATUS IS INVALID-PAYROLL-FILE-STATUS.

           SELECT STATS-FILE ASSIGN TO DISK
             FILE STATUS IS STATS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  PAYROLL-FILE
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 38 CHARACTERS
           DATA RECORD IS PAYROLL-IN.
       01  PAYROLL-IN.
           05 PAYROLL-EMPLOYEE-NAME            PIC X(20).
           05 PAYROLL-HOURS-WORKED             PIC 9(03).
           05 PAYROLL-HOUR-RATE                PIC 99V99.
           05 PAYROLL-AFM                      PIC X(09).
           05 PAYROLL-CRLF                     PIC X(02).
       FD  INVALID-PAYROLL-FILE
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 112 CHARACTERS
           DATA RECORD IS VALID-PAYROLL-OUT.
       01  INVALID-PAYROLL-OUT.
           05 INVALID-PAYROLL-EMPLOYEE-NAME      PIC X(20).
           05 FILLER                             PIC X(02).
           05 INVALID-PAYROLL-HOURS-WORKED       PIC Z(11)9.
           05 FILLER                             PIC X(02).
           05 INVALID-PAYROLL-HOUR-RATE          PIC Z(02).Z(02)9,99.
           05 FILLER                             PIC X(02).
           05 INVALID-PAYROLL-AFM                PIC X(09).
           05 FILLER                             PIC X(02).
           05 INVALID-PAYROLL-AFM-STATUS         PIC X(10).
           05 FILLER                             PIC X(02).
           05 INVALID-PAYROLL-AFM-CAUSE          PIC X(40).
           05 INVALID-PAYROLL-CRLF               PIC X(02).
       01  INVALID-PAYROLL-OUT-TITLE.
           05 INVALID-EMPLOYEE-NAME-TITLE        PIC X(20).
           05 FILLER                             PIC X(02).
           05 INVALID-HOURS-WORKED-TITLE         PIC X(12).
           05 FILLER                             PIC X(02).
           05 INVALID-HOUR-RATE-TITLE            PIC X(09).
           05 FILLER                             PIC X(02).
           05 INVALID-AFM-TITLE                  PIC X(09).
           05 FILLER                             PIC X(02).
           05 INVALID-AFM-STATUS-TITLE           PIC X(10).
           05 FILLER                             PIC X(02).
           05 INVALID-AFM-CAUSE-TITLE            PIC X(40).
           05 INVALID-PAYROLL-TITLE-CRLF         PIC X(02).
       01  INVALID-PAYROLL-OUT-DASHES.
           05 INVALID-OUT-DASHES                 PIC X(110).
           05 INVALID-OUT-DASHES-CRLF            PIC X(02).
       FD  VALID-PAYROLL-FILE
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 112 CHARACTERS
           DATA RECORD IS VALID-PAYROLL-OUT.
       01  VALID-PAYROLL-OUT.
           05 VALID-PAYROLL-EMPLOYEE-NAME      PIC X(20).
           05 FILLER                           PIC X(02).
           05 VALID-PAYROLL-HOURS-WORKED       PIC Z(11)9.
           05 FILLER                           PIC X(02).
           05 VALID-PAYROLL-HOUR-RATE          PIC Z(05)9,99.
           05 FILLER                           PIC X(02).
           05 VALID-PAYROLL-AFM                PIC X(09).
           05 FILLER                           PIC X(02).
           05 VALID-PAYROLL-AFM-STATUS         PIC X(10).
           05 FILLER                           PIC X(02).
           05 VALID-PAYROLL-GROSS
                   PIC Z(02).Z(03).Z(02)9,99.
           05 FILLER                           PIC X(02).
           05 VALID-PAYROLL-TAXES
                   PIC Z(02).Z(03).Z(02)9,99.
           05 FILLER                           PIC X(02).
           05 VALID-PAYROLL-NET                PIC Z(03).Z(02)9,99.
           05 VALID-PAYROLL-CRLF               PIC X(02).
       01  VALID-PAYROLL-OUT-TITLE.
           05 VALID-EMPLOYEE-NAME-TITLE      PIC X(20).
           05 FILLER                         PIC X(02).
           05 VALID-HOURS-WORKED-TITLE       PIC X(12).
           05 FILLER                         PIC X(02).
           05 VALID-HOUR-RATE-TITLE          PIC X(09).
           05 FILLER                         PIC X(02).
           05 VALID-AFM-TITLE                PIC X(09).
           05 FILLER                         PIC X(02).
           05 VALID-AFM-STATUS-TITLE         PIC X(10).
           05 FILLER                         PIC X(02).
           05 VALID-GROSS-TITLE              PIC X(13).
           05 FILLER                         PIC X(02).
           05 VALID-TAXES-TITLE              PIC X(13).
           05 FILLER                         PIC X(02).
           05 VALID-NET-TITLE                PIC X(10).
           05 VALID-CRLF-TITLE               PIC X(02).
       01  VALID-PAYROLL-OUT-DASHES.
           05 VALID-OUT-DASHES               PIC X(110).
           05 VALID-OUT-DASHES-CRLF          PIC X(02).
       FD  STATS-FILE
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 53 CHARACTERS
           DATA RECORD IS STATS-OUT.
       01  STATS-OUT.
           05 TOTAL-TITLE                      PIC X(40).
           05 FILLER                           PIC X.
           05 TOTAL-AMOUNT                     PIC Z(03).Z(02)9,99.
           05 STATS-CRLF                       PIC X(02).
       WORKING-STORAGE SECTION.
       01  WS-VARIABLES.
           05 PAYROLL-FILE-STATUS              PIC 99.
           05 VALID-PAYROLL-FILE-STATUS        PIC 99.
           05 INVALID-PAYROLL-FILE-STATUS      PIC 99.
           05 STATS-FILE-STATUS                PIC 99.
           05 DATA-REMAINS-SWITCH              PIC X(02) VALUE SPACES.
           05 TOTAL-GROSS                      COMP PIC 9(08)V99
                   VALUE 0.
           05 TOTAL-TAXES                      COMP PIC 9(08)V99
                   VALUE 0.
           05 TOTAL-NET                        COMP PIC 9(08)V99
                   VALUE 0.
           05 GROSS-SALARY                     PIC 9(08)V99.
           05 TAXES                            PIC 9(08)V99.
           05 NET-SALARY                       PIC 9(08)V99.
           05 GROSS-SALARY-Z                   PIC Z(03).Z(02)9,99.
           05 TAXES-Z                          PIC Z(03).Z(02)9,99.
           05 NET-SALARY-Z                     PIC Z(03).Z(02)9,99.
           05 TOTAL-GROSS-Z                    PIC Z(03).Z(02)9,99.
           05 TOTAL-TAXES-Z                    PIC Z(03).Z(02)9,99.
           05 TOTAL-NET-Z                      PIC Z(03).Z(02)9,99.
           05 AFM                              PIC X(09).
           05 AFM-STATUS                       PIC X(07).
           05 AFM-CAUSE                        PIC X(40).

       PROCEDURE DIVISION.

           PERFORM INITIALIZE-RTN.
           PERFORM MAIN-RTN.
           PERFORM FINAL-RTN.
           PERFORM STOPRUN.

       STOPRUN.
           CLOSE PAYROLL-FILE, VALID-PAYROLL-FILE, INVALID-PAYROLL-FILE,
               STATS-FILE.
           STOP RUN.

       INITIALIZE-RTN.
           INITIALIZE WS-VARIABLES.

           OPEN INPUT PAYROLL-FILE, OUTPUT VALID-PAYROLL-FILE,
               OUTPUT INVALID-PAYROLL-FILE, OUTPUT STATS-FILE.
           IF PAYROLL-FILE-STATUS NOT = 0
              DISPLAY 'ERROR OPENING INPUT FILE: PAYROLL-FILE!!!'
              DISPLAY 'STATUS-CODE = ' PAYROLL-FILE-STATUS
              GO TO STOPRUN.
           IF VALID-PAYROLL-FILE-STATUS NOT = 0
              DISPLAY 'ERROR OPENING OUTPUT FILE: VALID-PAYROLL-FILE!!!'
              DISPLAY 'STATUS-CODE = ' VALID-PAYROLL-FILE-STATUS
              GO TO STOPRUN.
           IF INVALID-PAYROLL-FILE-STATUS NOT = 0
              DISPLAY 'ERROR OPENING OUTPUT FILE: INVALID-PAYROLL-FILE'
                   '!!!'
              DISPLAY 'STATUS-CODE = ' INVALID-PAYROLL-FILE-STATUS
              GO TO STOPRUN.
           IF STATS-FILE-STATUS NOT = 0
              DISPLAY 'ERROR OPENING OUTPUT FILE: STATS-FILE!!!'
              DISPLAY 'STATUS-CODE = ' STATS-FILE-STATUS
              GO TO STOPRUN.
           READ PAYROLL-FILE
                AT END MOVE 'NO' TO DATA-REMAINS-SWITCH.

       MAIN-RTN.
           PERFORM PRINT-VALID-RECORD-TITLE.
           PERFORM PRINT-INVALID-RECORD-TITLE.
           PERFORM PROCESS-PAYROLLS
               UNTIL DATA-REMAINS-SWITCH = 'NO'.

       PRINT-INVALID-RECORD-TITLE.
           MOVE SPACES                   TO INVALID-PAYROLL-OUT-TITLE
           MOVE 'EMPLOYEE NAME'          TO INVALID-EMPLOYEE-NAME-TITLE
           MOVE 'HOURS WORKED'           TO INVALID-HOURS-WORKED-TITLE
           MOVE 'HOUR RATE'              TO INVALID-HOUR-RATE-TITLE
           MOVE '   AFM'                 TO INVALID-AFM-TITLE
           MOVE 'AFM STATUS'             TO INVALID-AFM-STATUS-TITLE
           MOVE 'AFM STATUS CAUSE'       TO INVALID-AFM-CAUSE-TITLE
           MOVE X'0D0A'                  TO INVALID-PAYROLL-TITLE-CRLF
           WRITE INVALID-PAYROLL-OUT-TITLE
           IF INVALID-PAYROLL-FILE-STATUS NOT = 0
              DISPLAY '***ERROR WRITING INVALID PAYROLL FILE!!!'
              DISPLAY 'STATUS-CODE = ' INVALID-PAYROLL-FILE-STATUS
              GO TO STOPRUN
           END-IF
           MOVE SPACES                   TO INVALID-PAYROLL-OUT-DASHES
           MOVE ALL '='                  TO INVALID-OUT-DASHES
           MOVE X'0D0A'                  TO INVALID-OUT-DASHES-CRLF
           WRITE INVALID-PAYROLL-OUT-DASHES
           IF INVALID-PAYROLL-FILE-STATUS NOT = 0
              DISPLAY '***ERROR WRITING INVALID PAYROLL FILE!!!'
              DISPLAY 'STATUS-CODE = ' INVALID-PAYROLL-FILE-STATUS
              GO TO STOPRUN
           END-IF.

       PRINT-VALID-RECORD-TITLE.
           MOVE SPACES                   TO VALID-PAYROLL-OUT-TITLE
           MOVE 'EMPLOYEE NAME'          TO VALID-EMPLOYEE-NAME-TITLE
           MOVE 'HOURS WORKED'           TO VALID-HOURS-WORKED-TITLE
           MOVE 'HOUR RATE'              TO VALID-HOUR-RATE-TITLE
           MOVE '   AFM'                 TO VALID-AFM-TITLE
           MOVE 'AFM STATUS'             TO VALID-AFM-STATUS-TITLE
           MOVE ' GROSS SALARY'          TO VALID-GROSS-TITLE
           MOVE 'TAXES APPLIED'          TO VALID-TAXES-TITLE
           MOVE 'NET SALARY'             TO VALID-NET-TITLE
           MOVE X'0D0A'                  TO VALID-CRLF-TITLE
           WRITE VALID-PAYROLL-OUT-TITLE
           IF VALID-PAYROLL-FILE-STATUS NOT = 0
              DISPLAY '***ERROR WRITING VALID PAYROLL FILE!!!'
              DISPLAY 'STATUS-CODE = ' VALID-PAYROLL-FILE-STATUS
              GO TO STOPRUN
           END-IF
           MOVE SPACES                   TO VALID-PAYROLL-OUT-DASHES
           MOVE ALL '='                  TO VALID-OUT-DASHES
           MOVE X'0D0A'                  TO VALID-OUT-DASHES-CRLF
           WRITE VALID-PAYROLL-OUT-DASHES
           IF VALID-PAYROLL-FILE-STATUS NOT = 0
              DISPLAY '***ERROR WRITING VALID PAYROLL FILE!!!'
              DISPLAY 'STATUS-CODE = ' VALID-PAYROLL-FILE-STATUS
              GO TO STOPRUN
           END-IF.

       PROCESS-PAYROLLS.
      *----Check if the employee is valid:
           MOVE PAYROLL-AFM TO AFM
           CALL 'AFM-VALIDATION-SUBRTN' USING BY REFERENCE
                   AFM, AFM-STATUS, AFM-CAUSE
           IF AFM-STATUS = 'INVALID'
               PERFORM PRINT-INVALID-RECORD
           ELSE
               COMPUTE GROSS-SALARY =
                   PAYROLL-HOURS-WORKED*PAYROLL-HOUR-RATE*14
               IF GROSS-SALARY <= 10000
                   MOVE 0 TO TAXES
                   ELSE IF GROSS-SALARY <= 20000
                           COMPUTE TAXES ROUNDED =
                           (GROSS-SALARY - 10000)*10/100
                         ELSE
                             COMPUTE TAXES ROUNDED = (10000*10/100)
                             + (GROSS-SALARY - 20000)*25/100
                         END-IF
               END-IF
               COMPUTE NET-SALARY = GROSS-SALARY - TAXES
               PERFORM PRINT-VALID-RECORD

               ADD GROSS-SALARY TO TOTAL-GROSS
               ADD TAXES        TO TOTAL-TAXES
               ADD NET-SALARY   TO TOTAL-NET

               MOVE GROSS-SALARY TO GROSS-SALARY-Z
               MOVE TAXES TO TAXES-Z
               MOVE NET-SALARY TO NET-SALARY-Z
               DISPLAY PAYROLL-EMPLOYEE-NAME ' ' PAYROLL-HOURS-WORKED
                   ' ' PAYROLL-HOUR-RATE ' ' PAYROLL-AFM ' '
                   VALID-PAYROLL-AFM-STATUS ' ' GROSS-SALARY-Z ' '
                   TAXES-Z ' ' NET-SALARY-Z
           END-IF.
           READ PAYROLL-FILE
                    AT END MOVE 'NO' TO DATA-REMAINS-SWITCH.

       PRINT-INVALID-RECORD.
           MOVE SPACES                   TO INVALID-PAYROLL-OUT
           MOVE PAYROLL-EMPLOYEE-NAME    TO
                   INVALID-PAYROLL-EMPLOYEE-NAME
           MOVE PAYROLL-HOURS-WORKED     TO INVALID-PAYROLL-HOURS-WORKED
           MOVE PAYROLL-HOUR-RATE        TO INVALID-PAYROLL-HOUR-RATE
           MOVE PAYROLL-AFM              TO INVALID-PAYROLL-AFM
           MOVE AFM-STATUS               TO INVALID-PAYROLL-AFM-STATUS
           MOVE AFM-CAUSE                TO INVALID-PAYROLL-AFM-CAUSE
           MOVE X'0D0A'                  TO INVALID-PAYROLL-CRLF
           WRITE INVALID-PAYROLL-OUT
           IF INVALID-PAYROLL-FILE-STATUS NOT = 0
              DISPLAY '***ERROR WRITING INVALID PAYROLL FILE!!!'
              DISPLAY 'STATUS-CODE = ' INVALID-PAYROLL-FILE-STATUS
              GO TO STOPRUN
           END-IF.

       PRINT-VALID-RECORD.
           MOVE SPACES                   TO VALID-PAYROLL-OUT
           MOVE PAYROLL-EMPLOYEE-NAME    TO VALID-PAYROLL-EMPLOYEE-NAME
           MOVE PAYROLL-HOURS-WORKED     TO VALID-PAYROLL-HOURS-WORKED
           MOVE PAYROLL-HOUR-RATE        TO VALID-PAYROLL-HOUR-RATE
           MOVE PAYROLL-AFM              TO VALID-PAYROLL-AFM
           MOVE AFM-STATUS               TO VALID-PAYROLL-AFM-STATUS
           MOVE GROSS-SALARY             TO VALID-PAYROLL-GROSS
           MOVE TAXES                    TO VALID-PAYROLL-TAXES
           MOVE NET-SALARY               TO VALID-PAYROLL-NET
           MOVE X'0D0A'                  TO VALID-PAYROLL-CRLF
           WRITE VALID-PAYROLL-OUT
           IF VALID-PAYROLL-FILE-STATUS NOT = 0
              DISPLAY '***ERROR WRITING VALID PAYROLL FILE!!!'
              DISPLAY 'STATUS-CODE = ' VALID-PAYROLL-FILE-STATUS
              GO TO STOPRUN
           END-IF.

       FINAL-RTN.
           MOVE TOTAL-GROSS TO TOTAL-GROSS-Z
           MOVE TOTAL-TAXES TO TOTAL-TAXES-Z
           MOVE TOTAL-NET   TO TOTAL-NET-Z
           DISPLAY 'TOTAL GROSS OF ALL EMPLOYEES: ' TOTAL-GROSS-Z
           DISPLAY 'TOTAL TAXES APPLIED: ' TOTAL-TAXES-Z
           DISPLAY 'TOTAL NET SALARY OF ALL EMPLOYEES: ' TOTAL-NET-Z

           PERFORM PRINT-STATS.

       PRINT-STATS.
           MOVE SPACES                            TO STATS-OUT
           MOVE 'TOTAL GROSS FOR ALL EMPLOYEES: ' TO TOTAL-TITLE
           MOVE TOTAL-GROSS-Z                     TO TOTAL-AMOUNT
           MOVE X'0D0A'                           TO STATS-CRLF
           WRITE STATS-OUT
           IF STATS-FILE-STATUS NOT = 0
              DISPLAY '***ERROR WRITING STATS FILE!!!'
              DISPLAY 'STATUS-CODE = ' STATS-FILE-STATUS
              GO TO STOPRUN
           END-IF
           MOVE SPACES                            TO STATS-OUT
           MOVE 'TOTAL TAXES APPLIED: '           TO TOTAL-TITLE
           MOVE TOTAL-TAXES-Z                     TO TOTAL-AMOUNT
           MOVE X'0D0A'                           TO STATS-CRLF
           WRITE STATS-OUT
           IF STATS-FILE-STATUS NOT = 0
              DISPLAY '***ERROR WRITING STATS FILE!!!'
              DISPLAY 'STATUS-CODE = ' STATS-FILE-STATUS
              GO TO STOPRUN
           END-IF
           MOVE SPACES                            TO STATS-OUT
           MOVE 'TOTAL NET FOR ALL EMPLOYEES: '   TO TOTAL-TITLE
           MOVE TOTAL-NET-Z                       TO TOTAL-AMOUNT
           MOVE X'0D0A'                           TO STATS-CRLF
           WRITE STATS-OUT
           IF STATS-FILE-STATUS NOT = 0
              DISPLAY '***ERROR WRITING STATS FILE!!!'
              DISPLAY 'STATUS-CODE = ' STATS-FILE-STATUS
              GO TO STOPRUN
           END-IF.


       END PROGRAM EMPLOYEE-PAYROLL.
