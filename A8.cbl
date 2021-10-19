      ******************************************************************
      * Author: Blaine McShea
      * Date: 10/19/2021
      * Purpose: This program generates a report card for students.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. A8.
       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT STUMAST  ASSIGN TO "STUMAST.DAT".
           SELECT HONORRPT ASSIGN TO "HONORRPT.PRN".

       DATA DIVISION.

       FILE SECTION.

       FD  STUMAST.

       01  STUDENT-MASTER-RECORD.
           05  SM-STUDENT-ID                   PIC 9(9).
           05  SM-STUDENT-STATUS               PIC X.
               88  ENROLLED                    VALUE "E".
               88  INACTIVE                    VALUE "I".
           05  SM-STUDENT-NAME-AND-ADDRESS.
               10  SM-STUDENT-NAME             PIC X(25).
               10  SM-DATE-OF-BIRTH.
                   15  SM-DOB-YEAR             PIC 9(4).
                   15  SM-DOB-MONTH            PIC 9(2).
                   15  SM-DOB-DAY              PIC 9(2).
               10  SM-STUDENT-ADDRESS          PIC X(25).
               10  SM-STUDENT-CITY             PIC X(11).
               10  SM-STUDENT-STATE            PIC X(2).
               10  SM-STUDENT-ZIP-CODE         PIC 9(5).
               10  SM-STUDENT-ZIP-CODE-EXT     PIC 9(4).
           05  SM-STUDENT-PROGRESS-SUMMARY.
               10  SM-CLASS-STANDING           PIC 9.
                   88  FRESHMAN                VALUE 1.
                   88  SOPHOMORE               VALUE 2.
                   88  JUNIOR                  VALUE 3.
                   88  SENIOR                  VALUE 4.
               10  SM-MAJOR                    PIC X(4).
               10  SM-UNITS-COMPLETED          PIC 9(3).
               10  SM-TOTAL-GRADE-POINTS       PIC 9(3).
               10  SM-UNITS-IN-PROGRESS        PIC 9(3).
      *
       FD  HONORRPT.
      *
       01  PRINT-AREA      PIC X(132).
      *
       WORKING-STORAGE SECTION.
      *
       01  SWITCHES.
           05  STUMAST-EOF-SWITCH     PIC X    VALUE "N".
      *
       01  WORK-FIELDS.
           05  STUDENT-ID.
               10  STUDENT-ID-1        PIC 9(3).
               10  STUDENT-ID-2        PIC 9(2).
               10  STUDENT-ID-3        PIC 9(4).
           05  STUDENT-GPA             PIC S9V99.
      *
       01  PRINT-FIELDS.
           05  PAGE-COUNT      PIC S9(3)   VALUE ZERO.
           05  LINES-ON-PAGE   PIC S9(3)   VALUE +55.
           05  LINE-COUNT      PIC S9(3)   VALUE +99.
           05  SPACE-CONTROL   PIC S9.
      *
       01  TOTAL-FIELDS.
           05  TOTAL-STUDENTS         PIC S9(5)  VALUE ZERO.
           05  RANKING-SCHOLARS       PIC S9(5)  VALUE ZERO.
      *
       01  CURRENT-DATE-AND-TIME.
           05  CD-YEAR         PIC 9999.
           05  CD-MONTH        PIC 99.
           05  CD-DAY          PIC 99.
           05  CD-HOURS        PIC 99.
           05  CD-MINUTES      PIC 99.
           05  FILLER          PIC X(9).
      *
       01  HEADING-LINE-1.
           05  FILLER          PIC X(7)    VALUE "DATE:  ".
           05  HL1-MONTH       PIC 9(2).
           05  FILLER          PIC X(1)    VALUE "/".
           05  HL1-DAY         PIC 9(2).
           05  FILLER          PIC X(1)    VALUE "/".
           05  HL1-YEAR        PIC 9(4).
           05  FILLER          PIC X(9)    VALUE SPACE.
           05  FILLER          PIC X(20)   VALUE "HONOR STUDENT REPORT".
           05  FILLER          PIC X(17)   VALUE SPACE.
           05  FILLER          PIC X(6)    VALUE "PAGE: ".
           05  HL1-PAGE-NUMBER PIC ZZZ9.
           05  FILLER          PIC X(59)   VALUE SPACE.
      *
       01  HEADING-LINE-2.
           05  FILLER          PIC X(7)    VALUE "TIME:  ".
           05  HL2-HOURS       PIC 9(2).
           05  FILLER          PIC X(1)    VALUE ":".
           05  HL2-MINUTES     PIC 9(2).
           05  FILLER          PIC X(51)   VALUE SPACE.
           05  FILLER          PIC X(7)    VALUE "SP03-2R".
           05  FILLER          PIC X(62)   VALUE SPACE.
      *
       01  HEADING-LINE-3.
           05  FILLER      PIC X(11)   VALUE "STUDENT ID".
           05  FILLER      PIC X(2)    VALUE SPACE.
           05  FILLER      PIC X(25)   VALUE "STUDENT NAME".
           05  FILLER      PIC X(2)    VALUE SPACE.
           05  FILLER      PIC X(9)    VALUE "CLASS".
           05  FILLER      PIC X(2)    VALUE SPACE.
           05  FILLER      PIC X(4)    VALUE "GPA".
           05  FILLER      PIC X(77)   VALUE SPACE.
      *
       01  HEADING-LINE-4.
           05  FILLER      PIC X(11)  VALUE "-----------".
           05  FILLER      PIC X(2)   VALUE SPACE.
           05  FILLER      PIC X(25)  VALUE "-------------------------".
           05  FILLER      PIC X(2)   VALUE SPACE.
           05  FILLER      PIC X(9)   VALUE "---------".
           05  FILLER      PIC X(2)   VALUE SPACE.
           05  FILLER      PIC X(4)   VALUE "----".
           05  FILLER      PIC X(77)  VALUE SPACE.
      *
       01  STUDENT-LINE.
           05  SL-STUDENT-ID-1     PIC 9(3).
           05  FILLER              PIC X(1)     VALUE "-".
           05  SL-STUDENT-ID-2     PIC 9(2).
           05  FILLER              PIC X(1)     VALUE "-".
           05  SL-STUDENT-ID-3     PIC 9(4).
           05  FILLER              PIC X(2)     VALUE SPACE.
           05  SL-STUDENT-NAME     PIC X(25).
           05  FILLER              PIC X(2)     VALUE SPACE.
           05  SL-CLASS            PIC X(9).
           05  FILLER              PIC X(2)     VALUE SPACE.
           05  SL-GPA              PIC 9.99.
           05  FILLER              PIC X(3)     VALUE SPACE.
           05  SL-SCHOLARS         PIC X(15).
           05  FILLER              PIC X(59).
      *
       01  STUDENT-TOTAL-LINE.
           05  FILLER             PIC X(18)  VALUE "TOTAL STUDENTS:   ".
           05  STL-TOTAL-STUDENTS PIC ZZ,ZZ9.
      *
       01  RANKING-SCHOLAR-TOTAL-LINE.
           05  FILLER             PIC X(18)  VALUE "RANKING SCHOLARS: ".
           05  RSL-TOTAL-SCHOLARS PIC ZZ,ZZ9.
      *
       PROCEDURE DIVISION.
      *
       000-PREPARE-HONOR-REPORT.
      *
           OPEN INPUT  STUMAST
                OUTPUT HONORRPT.
           PERFORM 100-FORMAT-REPORT-HEADING.
           PERFORM 200-PREPARE-STUDENT-LINES
               UNTIL STUMAST-EOF-SWITCH = "Y".
           PERFORM 300-PRINT-TOTALS.
           CLOSE STUMAST
                 HONORRPT.
           STOP RUN.
      *
       100-FORMAT-REPORT-HEADING.
      *
           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME.
           MOVE CD-MONTH   TO HL1-MONTH.
           MOVE CD-DAY     TO HL1-DAY.
           MOVE CD-YEAR    TO HL1-YEAR.
           MOVE CD-HOURS   TO HL2-HOURS.
           MOVE CD-MINUTES TO HL2-MINUTES.
      *
       200-PREPARE-STUDENT-LINES.
      *
           PERFORM 210-READ-STUDENT-RECORD.
           IF STUMAST-EOF-SWITCH = "N"
               COMPUTE STUDENT-GPA ROUNDED =
                   SM-TOTAL-GRADE-POINTS / SM-UNITS-COMPLETED
               IF STUDENT-GPA >= 3.5
                   PERFORM 220-PRINT-STUDENT-LINE.
      *
       210-READ-STUDENT-RECORD.
      *
           READ STUMAST
               AT END
                   MOVE "Y" TO STUMAST-EOF-SWITCH.
      *
       220-PRINT-STUDENT-LINE.

           IF LINE-COUNT >= LINES-ON-PAGE
               PERFORM 230-PRINT-HEADING-LINES.
           MOVE SM-STUDENT-ID TO STUDENT-ID.
           MOVE STUDENT-ID-1 TO SL-STUDENT-ID-1.
           MOVE STUDENT-ID-2 TO SL-STUDENT-ID-2.
           MOVE STUDENT-ID-3 TO SL-STUDENT-ID-3.
           MOVE SM-STUDENT-NAME TO SL-STUDENT-NAME.


               IF SM-CLASS-STANDING = 1
                   MOVE "FRESHMAN" TO SL-CLASS
               ELSE
               IF SM-CLASS-STANDING = 2
                   MOVE "SOPHOMORE" TO SL-CLASS
               ELSE
               IF SM-CLASS-STANDING = 3
                      MOVE "JUNIOR" TO SL-CLASS
               ELSE
                      MOVE "SENIOR" TO SL-CLASS.
           MOVE STUDENT-GPA  TO SL-GPA.

               IF STUDENT-GPA >= 3.8
                   MOVE "RANKING SCHOLAR" TO SL-SCHOLARS
                   ADD 1 TO RANKING-SCHOLARS
           ELSE
               MOVE SPACE TO SL-SCHOLARS.
           MOVE STUDENT-LINE TO PRINT-AREA.
           WRITE PRINT-AREA AFTER ADVANCING SPACE-CONTROL LINES.
           ADD 1  TO LINE-COUNT.
           ADD 1  TO TOTAL-STUDENTS.
           MOVE 1 TO SPACE-CONTROL.

       230-PRINT-HEADING-LINES.
      *
           ADD 1 TO PAGE-COUNT.
           MOVE PAGE-COUNT     TO HL1-PAGE-NUMBER.
           MOVE HEADING-LINE-1 TO PRINT-AREA.
           WRITE PRINT-AREA AFTER ADVANCING PAGE.
           MOVE HEADING-LINE-2 TO PRINT-AREA.
           WRITE PRINT-AREA AFTER ADVANCING 1 LINES.
           MOVE HEADING-LINE-3 TO PRINT-AREA.
           WRITE PRINT-AREA AFTER ADVANCING 2 LINES.
           MOVE HEADING-LINE-4 TO PRINT-AREA.
           WRITE PRINT-AREA AFTER ADVANCING 1 LINES.
           MOVE ZERO TO LINE-COUNT.
           MOVE 2 TO SPACE-CONTROL.
      *
       300-PRINT-TOTALS.
      *
           MOVE TOTAL-STUDENTS       TO STL-TOTAL-STUDENTS.
           MOVE STUDENT-TOTAL-LINE   TO PRINT-AREA.
           WRITE PRINT-AREA AFTER ADVANCING 2 LINES.
           MOVE RANKING-SCHOLARS     TO RSL-TOTAL-SCHOLARS.
           MOVE RANKING-SCHOLAR-TOTAL-LINE TO PRINT-AREA.
           WRITE PRINT-AREA AFTER ADVANCING 1 LINES.
       END PROGRAM A8.
