      ******************************************************************
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. W2N.
      *AUTHOR. Sebastian Ritter.                                        *> no COBOL-2014 

      *=================================================================
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT config-file ASSIGN TO config_filename                     *> OpenCobol need _ instead of - to find our later defined file
              ORGANIZATION IS LINE SEQUENTIAL
              FILE STATUS IS CONFIG-STATUS.       

      *=================================================================
       DATA DIVISION.
       FILE SECTION.
       FD config-file
          RECORD CONTAINS 1 TO 80 CHARACTERS.
       01 config-line.                                                  *>length of maximum german word is 77 (siebenhundertsiebenundsiebzigtausendsiebenhundersiebenundsiebzig=777777)
         05 config-content   pic n(80).
         05 filler redefines config-content.
           10 filler           pic n.
               88 config-comment-descriptor     value x"0023". *> UTF-16BE value of #
           10 config-comment-value     pic n(79).
       
       
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
      * Like PASCAL, unlike Java or C does COBOL declare variables and
      * memories before programm definition.

       copy "TxtUtils.cpy".      

       77 CONFIG-KEY-VALUE-DELIMITER  PIC X(1) VALUE "=".
       77 CONFIG-PART-DELIMITER       PIC X(1) VALUE ":".

       01 filler.                                                       *> helper variables
         05 float-value        pic S9(1)V9(37) .*>usage is comp-1.
         05 double-value       pic S9(1)V9(37) .*>usage is comp-2.
         05 signed-bcd-value   pic S9(4) .*>usage is comp-3.
         05 unsigned-bcd-value pic 9(4) .*>usage is comp-6.
         05 helper-count-pos   pic 9(4).


       01 filler.
        03 number-system           occurs 256.
         05 number-system-text      pic x(256).
         05 number-system-value     pic 9(38).
       01 filler.
        03 normalize-data          occurs 256.
         05 normalize-data-text     pic x(256).
         05 normalize-data-value    pic 9(38).
       
       01 point-text              pic x(64).
       01 filler.
        03 decimal-words           pic x(64) occurs 10.
       01 filler.
        03 sorted-measuere-values  pic 9(38) occurs 128.




       01 config-file-namew     pic x(128).
       01 dynmaic-config-file-name.
         05 filler       value "data/".
         05 filler       value "config_".
         05 lang         value "en".
           88 lang-default value "en".
         05 filler       value ".properties".

       01 filler.
       copy "FILE-STATUS.CPY" replacing ==:FILE:== by ==CONFIG==.

      *LOCAL-STORAGE SECTION.
       LINKAGE SECTION.
       copy "W2N.cpy".

      *SCREEN SECTION.

      *=================================================================
       PROCEDURE DIVISION using by reference w2n-parameter 
                          *>returning w2n-returning.                    *> gnu-cobol 3.1.2.0 warning not implemented
                                by reference w2n-returning.
       main section.
         perform init

       exit section.

      *-----------------------------------------------------------------
       init section.

       display "enter W2N"

       move all spaces to w2n-result

       if lang-default then
         display "english"
       else
         display "other"
       end-if

       if w2n-number-sentences equal all low-values or 
          w2n-number-sentences equal all high-values or 
          w2n-number-sentences = all spaces
          then
            set w2n-returning-error to true
            exit program
          else 
            continue
       end-if

       display "config_filename" upon environment-name
       display dynmaic-config-file-name upon environment-value

       OPEN INPUT config-file.
       if CONFIG-NOT_EXISTS 
             display "File not found " config-file-namew
             exit program
       end-if
       
       perform with test before until not CONFIG-SUCCESS
           
              read config-file
              if not config-comment-descriptor 
                   display config-line 
              end-if


       end-perform
       close config-file.
              
              initialize txt-util-parameter
              move "   Zwölf   " to txt-util-input-output
              display ">>>" txt-util-input-output-byte(1:40) "<<<"
              display ">>>" txt-util-input-output (1:40) "<<<"
              set txt-util-func-trim to true
              call "TxtUtils" using by reference txt-util-parameter
                on exception 
                  display "MODUL TxtUtils not calling"
                  call "TxtUtils" using by reference txt-util-parameter
              end-call
       exit section.

       exit program.
       END PROGRAM W2N.
      * EOF
