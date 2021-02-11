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
      
      *-----

      *> 100%-pure COBOL text utilities Linkage
      *> Select the text util function over 88
      *> 
       01 text-util.
         05 text-util-function             pic 9(2).
           88 text-util-func-none            value zero.
           88 text-util-func-trim            value 10.
           88 text-util-func-indexof         value 20.
           88 text-util-func-startswith      value 25.
         05 filler.
           10 text-util-input-output-byte  pic x(199998).
           10 text-util-input-output       
              redefines text-util-input-output-byte pic n(99999).    
           10 text-util-input-xdata        pic n(99999).
           10 text-util-input-looking-for  
              redefines text-util-input-xdata pic n(99999). *> using for indexof
           10 text-util-input-starts-with  
              redefines text-util-input-xdata pic n(99999). *> using for indexof
         05 text-util-result.
           10 text-util-rc                 pic 9(2).
             88 text-util-rc-ok              value zero thru 89.
             88 text-util-rc-true            value 01.
             88 text-util-rc-false           value 02.
             88 text-util-rc-nothing-todo    value 80.
             88 text-util-rc-unknown-error   value 90.
             88 text-util-rc-not-impl        value 99.
           10 text-util-result-index       pic 9(5).
           10 text-util-result-trim-end    pic 9(5).
       
       01 text-util-work-counter           pic 99999.
      *-----
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
              
              move "   Zwölf   " to text-util-input-output
              display ">>>" text-util-input-output-byte(1:40) "<<<"
              display ">>>" text-util-input-output (1:40) "<<<"
              set text-util-func-trim to true
              perform text-util-main
              display "Text-util-rc: " text-util-rc
              display ">>>" text-util-input-output-byte(1:40) "<<<"
              display ">>>" text-util-input-output (1:40) "<<<"
              display 
                  ">>>" 
                  text-util-input-output (1:text-util-result-trim-end) 
                  "<<<"
              display 
                  ">L>" 
                  text-util-input-output
                  "<<<"

       close config-file.
       exit section.
      * exit program. W2N.


      *> TODO export
      *
       text-util-main section.

       evaluate true
         when text-util-func-trim          perform text-util-func-10
         when text-util-func-indexof       perform text-util-func-20
         when text-util-func-startswith    perform text-util-func-25
         when text-util-func-none      
           set text-util-rc-ok to true
         when other
           set text-util-rc-not-impl to true
       end-evaluate

       exit section.

      * Function:  trim
      * Input:     text-util-input-output
      * Output:    text-util-input-output, text-util-result-trim-end
      * Example:       
      *    IN:  SET text-util-func-trim TO TRUE
      *         MOVE " Hello text-utils! " TO text-util-input-output
      *    OUT: text-util-input-output [1:text-util-result-trim-end] EQUAL "Hello text-utils!"
       text-util-func-10 section. 
         if not text-util-input-output equal all spaces

           move zero to text-util-work-counter
           INSPECT text-util-input-output-byte
               TALLYING text-util-work-counter FOR LEADING SPACE 
           add 1 to text-util-work-counter                              *> because cobol count beginning is one      
           move text-util-input-output-byte (text-util-work-counter:)
             to text-util-input-output-byte

           move zero to text-util-work-counter
           INSPECT FUNCTION REVERSE (text-util-input-output-byte)
               TALLYING text-util-work-counter FOR LEADING SPACE
           move length of text-util-input-output-byte
             to text-util-result-trim-end
           subtract text-util-work-counter 
               from text-util-result-trim-end
           add 1 to text-util-result-trim-end                           *> because cobol count beginning is one      
           
      *> C Feature for example for gnu-cobol
      *> In result of C terminates Strings with x"00" aka low-value
      *> we set all spaces after our text to low-value
      *> and so we can use DISPLAY text-util-input-output
      *> instead of        DISPLAY text-util-input-output (1:text-util-result-trim-end)
           move text-util-result-trim-end to text-util-work-counter
           move low-values 
             to text-util-input-output-byte(text-util-result-trim-end:)

         else 
           set text-util-rc-nothing-todo to true
           move 1 to text-util-result-trim-end
         end-if
           
       exit section.

      * Function:  indexof
      * Input:     text-util-input-output, text-util-input-looking-for
      * Output:    text-util-result-index
      * Example:       
      *    IN:  SET text-util-func-indexof TO TRUE
      *         MOVE " Hello text-utils! " TO text-util-input-output
      *         MOVE "l" TO text-util-input-looking-for
      *    OUT: text-util-result-index = 4
       text-util-func-20 section.
       exit section.

      * Function:  startswith
      * Input:     text-util-input-output, text-util-input-starts-with
      * Output:    text-util-rc-true or text-util-rc-false
      * Example:       
      *    IN:  SET text-util-func-trim TO TRUE
      *         MOVE " Hello text-utils! " TO text-util-input-output
      *         PERFORM/CALL text-util-main/text-util
      *         SET text-util-func-startswith TO TRUE
      *         MOVE " Hello text-utils! " TO text-util-input-output
      *         MOVE "Hello text" TO text-util-input-looking-startswith
      *    OUT: text-util-rc-true
       text-util-func-25 section.
       exit section.

       exit program.
       END PROGRAM W2N.
      * EOF
