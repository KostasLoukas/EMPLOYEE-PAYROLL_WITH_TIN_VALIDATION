       IDENTIFICATION DIVISION.
       PROGRAM-ID. AFM-VALIDATION-SUBRTN.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  AFM-IN                           PIC X(9).
       01  AFM-ARRAY REDEFINES AFM-IN.
           05 AFM-DIGIT OCCURS 9 TIMES      PIC 9.
       01  WS-VARIABLES.
           05  IDX                          PIC 99   VALUE 0.
           05  SUM-DIGITS                   PIC 9(5) VALUE 0.
           05  MULTIPLIER                   PIC 9    VALUE 8.
           05  YPOL                         PIC 99.
           05  COUNT-NUMBERS                PIC 99   VALUE 0.
           05  COUNT-CHARS                  PIC 99   VALUE 0.
           05  COUNT-SPACES                 PIC 99   VALUE 0.
           05  ERROR-SW                     PIC  9   VALUE 0.
       LINKAGE SECTION.
       01  AFM                              PIC  X(009).
       01  AFM-STATUS                       PIC  X(007).
       01  AFM-CAUSE                        PIC  X(040).

       PROCEDURE DIVISION USING AFM, AFM-STATUS, AFM-CAUSE.
           PERFORM VALIDATE-AFM-RTN.
           PERFORM CALCULATE-AFM-RTN.
           GOBACK.
      ***------------------------------------------------------------***
      ***   PERFORMS VALIDATION OF THE INPUTTED AFM AND ISSUES ERROR ***
      ***   SPECIFIC MESSAGES                                        ***
      ***------------------------------------------------------------***
       VALIDATE-AFM-RTN.
           INITIALIZE WS-VARIABLES.
           MOVE    AFM    TO        AFM-IN.
           INSPECT AFM-IN REPLACING ALL X'0D' BY SPACES
           INSPECT AFM-IN REPLACING ALL X'0A' BY SPACES
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 9
               EVALUATE TRUE
                 WHEN  AFM-DIGIT(IDX) IS NUMERIC
                   ADD 1 TO COUNT-NUMBERS
                 WHEN  AFM-DIGIT(IDX) = SPACES
                   ADD 1 TO COUNT-SPACES
                 WHEN OTHER
                   ADD 1 TO COUNT-CHARS
               END-EVALUATE
           END-PERFORM.
           EVALUATE TRUE
             WHEN COUNT-NUMBERS = 9 AND AFM-IN =  ALL '0'
                  MOVE  '***ZERO AFM IS NOT SUPPORTED OR VALID!!!' TO
                        AFM-CAUSE
                  MOVE 1 TO ERROR-SW
             WHEN COUNT-NUMBERS < 9 AND COUNT-CHARS  > 0  AND
                  COUNT-SPACES > 0
                  MOVE  '***NO CHARS AND SPACES ALLOWED!!!'        TO
                        AFM-CAUSE
                  MOVE 1 TO ERROR-SW
             WHEN COUNT-NUMBERS < 9 AND COUNT-SPACES > 0
                  MOVE '***NO SPACES ALLOWED!!!'                   TO
                        AFM-CAUSE
                  MOVE 1 TO ERROR-SW
             WHEN COUNT-NUMBERS < 9 AND COUNT-CHARS  > 0
                  MOVE  '***NO CHARS ALLOWED!!!'                  TO
                        AFM-CAUSE
                  MOVE 1 TO ERROR-SW
             WHEN COUNT-NUMBERS = 9
                  CONTINUE
           END-EVALUATE.
           IF ERROR-SW = 1
              MOVE 'INVALID'  TO AFM-STATUS
              GOBACK
           END-IF.

      ***------------------------------------------------------------***
      ***   PERFORMS CALCULATIONS (AFM DIGITS IN THE POWER OF TWO AND***
      ***  APPLIES MODULO 11. IF REMAINDER EQUALS TO THE 9-TH AFM DI-***
      ***  GIT SIGNAL AFM AS VALID OTHERWISE AS INVALID              ***
      ***------------------------------------------------------------***
       CALCULATE-AFM-RTN.
           MOVE 0 TO SUM-DIGITS
           MOVE 8 TO MULTIPLIER
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 8
               COMPUTE SUM-DIGITS =
               SUM-DIGITS + AFM-DIGIT(IDX) * (2**(MULTIPLIER))
               SUBTRACT 1 FROM MULTIPLIER
           END-PERFORM.
           COMPUTE YPOL = FUNCTION MOD(SUM-DIGITS,11)
           IF YPOL = 10
               MOVE 0 TO YPOL
           END-IF
           IF YPOL = AFM-DIGIT(9)
              MOVE 'VALID'   TO AFM-STATUS
           ELSE
              MOVE 'INVALID' TO AFM-STATUS
              MOVE '***AFM CALCULATIONS LEAD TO  CHECK-DIGIT-ERROR!!'
                TO AFM-CAUSE
           END-IF.
       END PROGRAM AFM-VALIDATION-SUBRTN.
