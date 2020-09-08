       >>SOURCE FORMAT FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. P2.
AUTHOR. Pay-pao Mafia. 
DATE-WRITTEN. Sept 6nd 2020 

ENVIRONMENT DIVISION. 
*> define custom data classification 
INPUT-OUTPUT SECTION. 
FILE-CONTROL.
       SELECT InFile ASSIGN TO "P2In.dat" 
           ORGANIZATION IS LINE SEQUENTIAL. *> each piece of data takes up its own line 
       SELECT OutFile ASSIGN TO "P2Out.dat" 
           ORGANIZATION IS LINE SEQUENTIAL. 

DATA DIVISION.
FILE SECTION. 
FD OutFile. 
01 PrintLine PIC X(75). *> default for printing a line

FD InFile. 
01 Student. 
       02 FullName.
           03 FirstName PIC X(8).
           03 LastName PIC X(10). 
       02 WNum PIC X(8). 
       02 Semester PIC X(12). 
       02 AClass.
           03 ShortName PIC X(10).
           03 LongName PIC X(28).
       02 Grade PIC A.
       02 Credits PIC 9.
01 EOF PIC A.

WORKING-STORAGE SECTION. 
*> define custom report 
01 Header. 
       *> header
       02 Slu PIC X(33) VALUE "SOUTHEASTERN LOUISIANA UNIVERSITY".
       02 SluAddress PIC X(17) VALUE "HAMMOND, LA 70402". 
01 StudentInfo. *> 23
       02 PrintFull.
           03 PrintFirst PIC X(8).
           03 PrintLast PIC X(15).
       02 PrintWNum PIC X(8).
01 ASemester PIC X(12).
01 Categories. 
       02 COURSE PIC A(13) VALUE "COURSE".
       02 TITLES PIC A(33) VALUE "TITLE".
       02 GR PIC A(6) VALUE "GR".
       02 EARNED PIC A(10) VALUE "EARNED".
       02 QPTS PIC A(4) VALUE "QPTS".
       02 FILLER PIC X(5) VALUE SPACE. 
01 ClassInfo.
       02 PrintShort PIC X(8). 
       02 FILLER PIC X(5) VALUE SPACE. 
       02 PrintLong PIC X(28). 
       02 FILLER PIC X(5) VALUE SPACE. 
       02 PrintGrade PIC A. 
       02 FILLER PIC X(5) VALUE SPACE. 
       02 PrintCredits PIC 9 VALUE 0.
       02 FILLER PIC X(9) VALUE SPACE. 
       02 PrintQpts PIC 99 VALUE 0.
01 DS. *> semester
       02 DSemester PIC A(52) VALUE "SEMESTER". 
       02 TotalSemesterCredits PIC 99. 
       02 FILLER PIC X(8) VALUE SPACE.
       02 TotalSemQPts PIC 99 VALUE 0.
       02 FILLER PIC X(3) VALUE SPACE.
       02 SGPA_fixed PIC 9.99. 
01 SGPA pIC 9V99.
01 DC. *> overall 
       02 DCumulative PIC A(52) VALUE "CUMULATIVE". 
       02 TotalCumulativeCredits PIC 99.
       02 FILLER PIC X(7) VALUE SPACE.
       02 TotalCumQPts_fixed PIC Z99.
       02 FILLER PIC X(3) VALUE SPACE.
       02 CGPA_fixed PIC 9.99.
01 TotalCumQPts PIC 999 VALUE 0.
01 CGPA PIC 9V99.

*> other vars
01 GradeVal PIC 9 VALUE 0.
01 AFloat PIC 9.99. *> use to display float values
01 TF PIC A VALUE 'T'. *> true/false
01 LineCount PIC 99 VALUE 0.
01 Ptr PIC 99 VALUE 22.
01 SLength PIC 99.
01 TempInt PIC 99.
01 TempLine1 PIC X(75). *> temp print line
01 TempLine2 PIC X(75). *> temp print line

PROCEDURE DIVISION.
*> open student file (P2In.dat)
OPEN INPUT InFile.
       *> open student output file (P2Out.dat) 
       OPEN OUTPUT OutFile.
               PERFORM PrettifyHeader 
               READ InFile *> read P2In.dat file 
                   AT END MOVE 'Y' TO EOF 
               END-READ
               PERFORM PrintClass UNTIL EOF EQUALS 'Y' *> loop
CLOSE InFile, OutFile. 

STOP RUN. 



*> Paragraphs 

PrettifyHeader. 
       *> SLU Title
       *> get size of Slu string
       INSPECT Slu TALLYING SLength FOR CHARACTERS
       *> formula
       COMPUTE Ptr = (75 - SLength) / 2
       *> add Slu string into PrintLine at pointer 
       STRING Slu DELIMITED BY SIZE INTO TempLine1 WITH POINTER Ptr
       WRITE PrintLine FROM TempLine1 AFTER ADVANCING 2 LINES
       DISPLAY PrintLine *> display in shell

       *> SLU Address
       *> get size of Slu string
       COMPUTE SLength = 0
       INSPECT SluAddress TALLYING SLength FOR CHARACTERS
       *> formula
       COMPUTE Ptr = (75 - SLength) / 2
       *> add address into PrintLine at pointer 
       STRING SluAddress DELIMITED BY SIZE INTO TempLine2 WITH POINTER Ptr
       WRITE PrintLine FROM TempLine2 AFTER ADVANCING 1 LINES
       DISPLAY PrintLine. *> display in shell

PrintStudentInfo.
       WRITE PrintLine FROM PrintFull AFTER ADVANCING 2 LINES
       DISPLAY PrintLine *> display in shell
       WRITE PrintLine FROM PrintWNum AFTER ADVANCING 1 LINE
       DISPLAY PrintLine. *> display in shell

PrintSemesterYear. 
       WRITE PrintLine FROM ASemester AFTER ADVANCING 2 LINES
       DISPLAY PrintLine. *> display in shell

PrintCategories. 
       WRITE PrintLine FROM ASemester AFTER ADVANCING 2 LINES
       DISPLAY PrintLine *> display in shell
       WRITE PrintLine From Categories AFTER ADVANCING 1 LINE
       DISPLAY PrintLine. *> display in shell

PrintClass. 
       PERFORM MoveAll
       *> done once to print student info and header
       IF TF EQUALS 'T' THEN 
           MOVE 'F' TO TF
           PERFORM PrintStudentInfo *> print student data
           PERFORM PrintCategories *> print header
       END-IF
       IF ASemester NOT EQUAL Semester THEN
           MOVE Semester TO ASemester
           WRITE PrintLine FROM ClassInfo AFTER ADVANCING 1 LINE
           DISPLAY PrintLine *> display in shell
           *> compute semester gpa
           COMPUTE SGPA = TotalSemQPts / TotalSemesterCredits
           MOVE SGPA TO SGPA_fixed
           *> compute cumulative gpa 
           COMPUTE CGPA = TotalCumQPts / TotalCumulativeCredits
           MOVE CGPA TO CGPA_fixed
           MOVE TotalCumQPts TO TotalCumQPts_fixed
        *>    print semester and cumulative
           WRITE PrintLine FROM DS AFTER ADVANCING 1 LINE
           DISPLAY PrintLine *> display in shell
           WRITE PrintLine FROM DC AFTER ADVANCING 1 LINE
           DISPLAY PrintLine *> display in shell
           *> reset semester credits 
           MOVE 0 TO TotalSemesterCredits
           *> reset Semester GPA
           MOVE 0 TO SGPA
           *> reset total semester Qpts 
           MOVE 0 TO TotalSemQPts
           PERFORM PrintSemesterYear 
       ELSE IF LineCount EQUALS 8 THEN 
           WRITE PrintLine FROM ClassInfo AFTER ADVANCING 1 LINE
           DISPLAY PrintLine *> display in shell
           *> compute semester gpa 
           COMPUTE SGPA = TotalSemQPts / TotalSemesterCredits
           MOVE SGPA TO SGPA_fixed
           *> compute cumulative gpa 
           COMPUTE CGPA = TotalCumQPts / TotalCumulativeCredits
           MOVE CGPA TO CGPA_fixed
           MOVE TotalCumQPts TO TotalCumQPts_fixed
        *>    print semester and cumulative
           WRITE PrintLine FROM DS AFTER ADVANCING 1 LINE
           DISPLAY PrintLine *> display in shell
           WRITE PrintLine FROM DC AFTER ADVANCING 1 LINE
           DISPLAY PrintLine *> display in shell
       ELSE
           WRITE PrintLine FROM ClassInfo AFTER ADVANCING 1 LINE
           DISPLAY PrintLine *> display in shell
       END-IF
       COMPUTE LineCount = LineCount + 1.


MoveAll. 
       MOVE FirstName TO PrintFirst 
       MOVE LastName TO PrintLast 
       MOVE WNum TO PrintWNum
       MOVE Semester TO ASemester 
       MOVE ShortName TO PrintShort
       MOVE LongName TO PrintLong
       MOVE Grade TO PrintGrade 
       MOVE Credits TO PrintCredits
       COMPUTE TotalSemesterCredits = TotalSemesterCredits + Credits
       COMPUTE TotalCumulativeCredits = TotalCumulativeCredits + Credits
       PERFORM CheckGradeValue
       COMPUTE PrintQpts = GradeVal * PrintCredits *> calculate qpts
       COMPUTE TotalSemQPts = TotalSemQPts + PrintQpts
       COMPUTE TotalCumQPts = TotalCumQPts + PrintQpts.


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


*> finally
READ InFile 
       AT END MOVE 'Y' TO EOF
END-READ. 

