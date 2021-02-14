      ******************************************************************
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TxtUtils.
      *AUTHOR. Sebastian Ritter.                                        *> no COBOL-2014 

      *=================================================================
       ENVIRONMENT DIVISION.

      *=================================================================
       DATA DIVISION.
       FILE SECTION.       
       
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01 txt-util-work-counter           pic 99999.

       LINKAGE SECTION.
       copy "TxtUtils.cpy".

      *=================================================================
       PROCEDURE DIVISION using by reference txt-util-parameter.
                          *>      by reference txt-util-rc.
       main section.
         perform init

       exit section.

      *-----------------------------------------------------------------
       init section.

              
      *        move "   Zwölf   " to txt-util-input-output
      *       display ">>>" txt-util-input-output-byte(1:40) "<<<"
      *       display ">>>" txt-util-input-output (1:40) "<<<"
      *       set txt-util-func-trim to true
              perform txt-util-main
      *       display "Text-util-rc: " txt-util-rc
      *       display ">>>" txt-util-input-output-byte(1:40) "<<<"
      *       display ">>>" txt-util-input-output (1:40) "<<<"
      *       display 
      *           ">>>" 
      *           txt-util-input-output (1:txt-util-result-trim-end) 
      *           "<<<"
      *       display 
      *           ">L>" 
      *           txt-util-input-output
      *           "<<<"

       exit section.
      * exit program. W2N.


       txt-util-main section.

       evaluate true
         when txt-util-func-trim          perform txt-util-func-10
         when txt-util-func-indexof       perform txt-util-func-20
         when txt-util-func-startswith    perform txt-util-func-25
         when txt-util-func-none      
           set txt-util-rc-ok to true
         when other
           set txt-util-rc-not-impl to true
       end-evaluate

       exit section.

      * Function:  trim
      * Input:     txt-util-input-output
      * Output:    txt-util-input-output, txt-util-result-trim-end
      * Example:       
      *    IN:  SET txt-util-func-trim TO TRUE
      *         MOVE " Hello text-utils! " TO txt-util-input-output
      *    OUT: txt-util-input-output [1:txt-util-result-trim-end] EQUAL "Hello text-utils!"
       txt-util-func-10 section. 
         if not txt-util-input-output equal all spaces

           move zero to txt-util-work-counter
           INSPECT txt-util-input-output-byte
               TALLYING txt-util-work-counter FOR LEADING SPACE 
           add 1 to txt-util-work-counter                               *> because cobol count beginning is one      
           move txt-util-input-output-byte (txt-util-work-counter:)
             to txt-util-input-output-byte

           move zero to txt-util-work-counter
           INSPECT FUNCTION REVERSE (txt-util-input-output-byte)
               TALLYING txt-util-work-counter FOR LEADING SPACE
           move length of txt-util-input-output-byte
             to txt-util-result-trim-end
           subtract txt-util-work-counter 
               from txt-util-result-trim-end
           add 1 to txt-util-result-trim-end                            *> because cobol count beginning is one      
           
      *> C Feature for example for gnu-cobol
      *> In result of C terminates Strings with x"00" aka low-value
      *> we set all spaces after our text to low-value
      *> and so we can use DISPLAY txt-util-input-output
      *> instead of        DISPLAY txt-util-input-output (1:txt-util-result-trim-end)
           move txt-util-result-trim-end to txt-util-work-counter
           move low-values 
             to txt-util-input-output-byte(txt-util-result-trim-end:)

         else 
           set txt-util-rc-nothing-todo to true
           move 1 to txt-util-result-trim-end
         end-if
           
       exit section.

      * Function:  indexof
      * Input:     txt-util-input-output, txt-util-input-looking-for
      * Output:    txt-util-result-index
      * Example:       
      *    IN:  SET txt-util-func-indexof TO TRUE
      *         MOVE " Hello txt-utils! " TO text-util-input-output
      *         MOVE "l" TO txt-util-input-looking-for
      *    OUT: txt-util-result-index = 4
       txt-util-func-20 section.
         set txt-util-rc-not-impl to true
       exit section.

      * Function:  startswith
      * Input:     txt-util-input-output, txt-util-input-starts-with
      * Output:    txt-util-rc-true or txt-util-rc-false
      * Example:       
      *    IN:  SET txt-util-func-trim TO TRUE
      *         MOVE " Hello text-utils! " TO txt-util-input-output
      *         CALL txtutils
      *         SET txt-util-func-startswith TO TRUE
      *         MOVE " Hello text-utils! " TO txt-util-input-output
      *         MOVE "Hello text" TO txt-util-input-looking-startswith
      *    OUT: text-util-rc-true
       txt-util-func-25 section.
         set txt-util-rc-not-impl to true
       exit section.

       exit program.
       

       END PROGRAM TxtUtils.
      * EOF


      *       initialize txt-util-parameter
      *        move "   Zwölf   " to txt-util-input-output
      *        display ">>>" txt-util-input-output-byte(1:40) "<<<"
      *        display ">>>" txt-util-input-output (1:40) "<<<"
      *        set txt-util-func-trim to true
      *        call "TxtUtils" using by reference txt-util-parameter
      *          on exception 
      *            display "MODUL TxtUtils not calling"
      *            call "TxtUtils" using by reference txt-util-parameter
      *        end-call
