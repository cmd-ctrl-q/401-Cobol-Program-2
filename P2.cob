       >>SOURCE FORMAT FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. P2.
AUTHOR. xyz robotics
DATE-WRITTEN. Sept 30nd 2021 

ENVIRONMENT DIVISION. 
INPUT-OUTPUT SECTION. 
FILE-CONTROL.
       SELECT InFile ASSIGN TO "P2In.dat" 
           ORGANIZATION IS LINE SEQUENTIAL. 
       SELECT OutFile ASSIGN TO "P2Out.dat" 
           ORGANIZATION IS LINE SEQUENTIAL. 

DATA DIVISION.
FILE SECTION. 
FD OutFile. 
01 PrintLine PIC X(80). 

FD InFile. 
01 Student. 
       02 FullName.
           03 FirstName PIC X(8).
           03 LastName PIC X(10). 
       02 WNumber PIC X(8). 
       02 Semester PIC X(12). 
       02 AClass.
           03 ShortName PIC X(10).
           03 LongName PIC X(28).
       02 Grade PIC A.
       02 Credits PIC 9.
01 EOF PIC A.

WORKING-STORAGE SECTION. 
01 Header. 
       02 school PIC X(33) VALUE "SOUTHEASTERN LOUISIANA UNIVERSITY".
       02 schoolAddress PIC X(17) VALUE "HAMMOND, LA 70402". 
01 StudentInfo.
       02 PrintFull.
           03 PrintFirst PIC X(8).
           03 PrintLast PIC X(15).
       02 PrintWNumber PIC X(8).
01 ASemester PIC X(12).
01 Categories. 
       02 COURSE PIC A(13) VALUE "COURSE".
       02 TITLES PIC A(33) VALUE "TITLE".
       02 GR PIC A(6) VALUE "GR".
       02 EARNED PIC A(13) VALUE "EARNED".
       02 QPTS PIC A(4) VALUE "QPTS".
       02 FILLER PIC X(5) VALUE SPACE. 
01 ClassInfo.
       02 PrintShort PIC X(8). 
       02 FILLER PIC X(5) VALUE SPACE. 
       02 PrintLong PIC X(28). 
       02 FILLER PIC X(5) VALUE SPACE. 
       02 PrintGrade PIC A. 
       02 FILLER PIC X(5) VALUE SPACE. 
       02 PrintCredits PIC Z9.99.
       02 FILLER PIC X(9) VALUE SPACE. 
       02 PrintQpts PIC Z9.99.
01 CreditsEarned PIC 9V99.
01 Qpoints PIC 99V99.
01 DS. 
       02 DSemester PIC A(51) VALUE "SEMESTER". 
       02 TotalSemesterCredits PIC Z99.99. 
       02 FILLER PIC X(8) VALUE SPACE.
       02 TotalSemQPts PIC Z99.99.
       02 FILLER PIC X(3) VALUE SPACE.
       02 SGPA_fixed PIC 9.99. 
01 SemCreditEarned PIC 99V99.
01 SemQPts PIC 99V99.
01 SGPA PIC 9V99.
01 DC. *> overall 
       02 DCumulative PIC A(51) VALUE "CUMULATIVE". 
       02 TotalCumulativeCredits PIC Z99.99.
       02 FILLER PIC X(8) VALUE SPACE.
       02 TotalCumQPts_fixed PIC Z99.99.
       02 FILLER PIC X(3) VALUE SPACE.
       02 CGPA_fixed PIC 9.99.
01 TotalCumCredits PIC 999V99.
01 TotalCumQPts PIC 999V99.
01 CGPA PIC 9V99.

01 TempA PIC X(75). 
01 TempB PIC X(75). 
01 TempC PIC X(75). 
01 TF PIC A VALUE 'T'. 
01 LC PIC 99 VALUE 0. *> line count
01 GradeVal PIC 9 VALUE 0.
01 Floaty PIC 9.99. 
01 Kursor PIC 99 VALUE 22.
01 SLength PIC 99.

PROCEDURE DIVISION.
OPEN INPUT InFile.
       OPEN OUTPUT OutFile.
               PERFORM PrintHead 
               READ InFile 
                   AT END MOVE 'Y' TO EOF 
               END-READ
               PERFORM PrintClass UNTIL EOF EQUALS 'Y'
CLOSE InFile, OutFile. 

STOP RUN. 

PrintHead. 
       INSPECT school TALLYING SLength FOR CHARACTERS
       COMPUTE Kursor = (75 - SLength) / 2
       STRING school DELIMITED BY SIZE INTO TempA WITH POINTER Kursor
       WRITE PrintLine FROM TempA AFTER ADVANCING 2 LINES
       DISPLAY PrintLine 

       COMPUTE SLength = 0
       INSPECT schoolAddress TALLYING SLength FOR CHARACTERS
       COMPUTE Kursor = (75 - SLength) / 2
       STRING schoolAddress DELIMITED BY SIZE INTO TempB WITH POINTER Kursor
       WRITE PrintLine FROM TempB AFTER ADVANCING 1 LINES
       DISPLAY PrintLine 
       DISPLAY TempC.

PrintStudentInfo.
       WRITE PrintLine FROM PrintFull AFTER ADVANCING 2 LINES
       DISPLAY PrintLine 
       WRITE PrintLine FROM PrintWNumber AFTER ADVANCING 1 LINE
       DISPLAY PrintLine 
       DISPLAY TempC.

PrintSemesterYear. 
       WRITE PrintLine FROM ASemester AFTER ADVANCING 2 LINES
       DISPLAY PrintLine. 

PrintCategories. 
       WRITE PrintLine FROM ASemester AFTER ADVANCING 2 LINES
       DISPLAY PrintLine 
       WRITE PrintLine From Categories AFTER ADVANCING 1 LINE
       DISPLAY PrintLine.

PrintClass. 
       PERFORM MoveAll
       IF TF EQUALS 'T' THEN 
           MOVE 'F' TO TF
           PERFORM PrintStudentInfo 
           PERFORM PrintCategories 
       END-IF
       IF ASemester NOT EQUAL Semester THEN
           MOVE Semester TO ASemester
           WRITE PrintLine FROM ClassInfo AFTER ADVANCING 1 LINE
           DISPLAY PrintLine 
           *> compute semester gpa
           COMPUTE SGPA = SemQPts / SemCreditEarned
           MOVE SGPA TO SGPA_fixed
           *> compute cumulative gpa 
           COMPUTE CGPA = TotalCumQPts / TotalCumCredits
           MOVE CGPA TO CGPA_fixed
           MOVE TotalCumQPts TO TotalCumQPts_fixed

           WRITE PrintLine FROM DS AFTER ADVANCING 1 LINE
           DISPLAY PrintLine *> display in shell
           WRITE PrintLine FROM DC AFTER ADVANCING 1 LINE
           DISPLAY PrintLine *> display in shell
           DISPLAY TempC

           *> reset semester credits 
           MOVE 0 TO SemCreditEarned
           *> reset Semester GPA
           MOVE 0 TO SGPA
           *> reset total semester Qpts 
           MOVE 0 TO SemQPts

           PERFORM PrintSemesterYear 

       ELSE IF LC EQUALS 8 THEN 
           WRITE PrintLine FROM ClassInfo AFTER ADVANCING 1 LINE
           DISPLAY PrintLine 

           *> compute semester gpa 
           COMPUTE SGPA = SemQPts / SemCreditEarned
           MOVE SGPA TO SGPA_fixed

           *> compute cumulative gpa 
           COMPUTE CGPA = TotalCumQPts / TotalCumCredits
           MOVE CGPA TO CGPA_fixed
           MOVE TotalCumQPts TO TotalCumQPts_fixed

        *>    print semester and cumulative
           WRITE PrintLine FROM DS AFTER ADVANCING 1 LINE
           DISPLAY PrintLine 
           WRITE PrintLine FROM DC AFTER ADVANCING 1 LINE
           DISPLAY PrintLine 
           DISPLAY TempC
       ELSE
           WRITE PrintLine FROM ClassInfo AFTER ADVANCING 1 LINE
           DISPLAY PrintLine 
       END-IF
       COMPUTE LC = LC + 1.

MoveAll. 
       MOVE FirstName TO PrintFirst 
       MOVE LastName TO PrintLast 
       MOVE WNumber TO PrintWNumber
       MOVE Semester TO ASemester 
       MOVE ShortName TO PrintShort
       MOVE LongName TO PrintLong
       MOVE Grade TO PrintGrade 
       MOVE Credits TO CreditsEarned
       COMPUTE SemCreditEarned = SemCreditEarned + Credits
       COMPUTE TotalCumCredits = TotalCumCredits + Credits
       PERFORM CheckGradeValue

    *>    calculate qpts 
       COMPUTE Qpoints = GradeVal * CreditsEarned 
       COMPUTE SemQPts = SemQPts + Qpoints
       COMPUTE TotalCumQPts = TotalCumQPts + Qpoints
       MOVE Qpoints TO PrintQpts
       MOVE CreditsEarned TO PrintCredits
       MOVE SemCreditEarned TO TotalSemesterCredits
       MOVE TotalCumCredits TO TotalCumulativeCredits
       MOVE SemQPts TO TotalSemQPts.

CheckGradeValue.
       IF Grade EQUAL 'A' THEN 
           MOVE 4.0 TO GradeVal
       ELSE IF Grade EQUAL 'B' THEN 
           MOVE 3.0 TO GradeVal
       ELSE IF Grade EQUAL 'C' THEN 
           MOVE 2.0 TO GradeVal
       ELSE IF Grade EQUAL 'D' THEN 
           MOVE 1.0 TO GradeVal
       ELSE 
           MOVE 0.0 TO GradeVal
       END-IF.


READ InFile 
       AT END MOVE 'Y' TO EOF
END-READ. 

