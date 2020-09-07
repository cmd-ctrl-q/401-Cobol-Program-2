       >>SOURCE FORMAT FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. P2.
AUTHOR. Batman. 
DATE-WRITTEN. Sept 6nd 2020 

ENVIRONMENT DIVISION. 
*> define custom data classification 
INPUT-OUTPUT SECTION. 
FILE-CONTROL.
       SELECT InFile ASSIGN TO "P2In.dat" 
           ORGANIZATION IS LINE SEQUENTIAL.
       SELECT OutFile ASSIGN TO "P2Out.dat" 
           ORGANIZATION IS LINE SEQUENTIAL. *> each piece of data takes up its own line 

DATA DIVISION.
FILE SECTION. 
FD OutFile. 
01 PrintLine PIC X(65). *> default for printing a line

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
       02 Credits PIC 9V99. *> total = 77
01 WSEOF PIC A.

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
       02 PrintCredits PIC 9V99 VALUE 0.
       02 FILLER PIC X(7) VALUE SPACE. 
       02 PrintQpts PIC 99V99 VALUE 0.
01 DS. *> semester
       02 DSemester PIC A(52) VALUE "SEMESTER". 
       02 TotalSemesterCredits PIC 99V99. 
       02 FILLER PIC X(6) VALUE SPACE.
       02 SGPA PIC 99V99 VALUE 0. 
01 DC. *> overall 
       02 DCumulative PIC A(52) VALUE "CUMULATIVE". 
       02 TotalCumulativeCredits PIC 99V99.
       02 FILLER PIC X(6) VALUE SPACE.
       02 CGPA PIC 99V99 VALUE 0.
01 GradeVal PIC 9V99 VALUE 0.

PROCEDURE DIVISION.
*> open student file (P2In.dat)
OPEN INPUT InFile.
       *> open student output file (P2Out.dat) 
       OPEN OUTPUT OutFile.
               PERFORM PrintHeader *> print header
               READ InFile *> read P2In.dat file 
                   AT END MOVE 'Y' TO WSEOF 
               END-READ
               PERFORM MoveAll
               PERFORM PrintStudentInfo
               PERFORM PrintCategories
               PERFORM PrintClass UNTIL WSEOF EQUALS 'Y' *> loop
CLOSE InFile, OutFile. 

STOP RUN. 



*> Paragraphs 

PrintHeader. 
       WRITE PrintLine FROM Slu AFTER ADVANCING 2 LINE
       WRITE PrintLine FROM SluAddress AFTER ADVANCING 1 LINE.

PrintStudentInfo.
       WRITE PrintLine FROM PrintFull AFTER ADVANCING 2 LINES
       WRITE PrintLine FROM PrintWNum AFTER ADVANCING 1 LINE.

PrintSemesterYear. 
       WRITE PrintLine FROM ASemester AFTER ADVANCING 2 LINES.

PrintCategories. 
       WRITE PrintLine FROM ASemester AFTER ADVANCING 2 LINES
       WRITE PrintLine From Categories AFTER ADVANCING 1 LINE.

PrintClass. 
       IF ASemester NOT EQUAL Semester THEN 
           *> print one more class
           WRITE PrintLine FROM ClassInfo AFTER ADVANCING 1 LINE
           *> compute semester gpa 
           DISPLAY "SGPA " SGPA " Sem Credits " TotalSemesterCredits
           COMPUTE SGPA ROUNDED = SGPA / TotalSemesterCredits
           DISPLAY SGPA
           *> compute cumulative gpa 
           COMPUTE CGPA ROUNDED = CGPA * TotalCumulativeCredits
           *> print semester and cumulative
           WRITE PrintLine FROM DS AFTER ADVANCING 1 LINE
           WRITE PrintLine FROM DC AFTER ADVANCING 1 LINE
           *> reset semester credits 
           MOVE 0 TO TotalSemesterCredits
           *> reset Semester GPA
           MOVE 0 TO SGPA
           PERFORM PrintSemesterYear 
       ELSE 
           WRITE PrintLine FROM ClassInfo AFTER ADVANCING 1 LINE
       END-IF
       PERFORM MoveAll.

MoveAll. 
       MOVE FirstName TO PrintFirst 
       MOVE LastName To PrintLast 
       MOVE WNum TO PrintWNum
       MOVE Semester TO ASemester 
       MOVE ShortName TO PrintShort
       MOVE LongName TO PrintLong
       MOVE Grade TO PrintGrade 
       MOVE Credits TO PrintCredits
       COMPUTE TotalSemesterCredits ROUNDED = TotalSemesterCredits + Credits
       COMPUTE TotalCumulativeCredits ROUNDED = TotalCumulativeCredits + Credits
       PERFORM CheckGradeValue
    *>    COMPUTE SGPA = SGPA + GradeVal *> sum semester GPA
       COMPUTE PrintQpts = GradeVal * PrintCredits. *> calculate qpts
    *>    COMPUTE SGPA = PrintQpts / PrintCredits.

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
       AT END MOVE 'Y' TO WSEOF
    *> AT END SET WSEOF TO TRUE 
END-READ. 

