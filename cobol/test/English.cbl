      ******************************************************************
      * TestCases for english language
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. English.
      *AUTHOR. Sebastian Ritter. *> no COBOL-2014

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *>   SELECT
      *>   ASSIGN TO
      *>   ORGANIZATION IS
      *>   .

       DATA DIVISION.
       FILE SECTION.
      *>FD .
      *>    01 .

       WORKING-STORAGE SECTION.

       copy "W2N.cpy". *> for call

       01 expected-value.
           20 expected-value-integer    pic 9(38).
           20 expected-value-delmiter   pic n(1).
           20 expected-value-decimal    pic 9(38).         
             88 expected-is-integer         value zeros.


      *>Assertions.assertEquals(new W2N().wordToNum("nineteen"), 19);
      *>Assertions.assertEquals(new W2N().wordToNum("two thousand and nineteen"), 2019);
      *>Assertions.assertEquals(new W2N().wordToNum("two million three thousand and nineteen"), 2003019);
      *>Assertions.assertEquals(new W2N().wordToNum("three billion"), 3000000000L);
      *>Assertions.assertEquals(new W2N().wordToNum("three million"), 3000000);
      *>Assertions.assertEquals(new W2N().wordToNum("one hundred twenty three million four hundred fifty six thousand seven hundred and eighty nine"), 123456789);
      *>Assertions.assertEquals(new W2N().wordToNum("eleven"), 11);
      *>Assertions.assertEquals(new W2N().wordToNum("nineteen billion and nineteen"), 19000000019L);
      *>Assertions.assertEquals(new W2N().wordToNum("one hundred and forty two"), 142);
      *>Assertions.assertEquals(new W2N().wordToNum("112"), 112);
      *>Assertions.assertEquals(new W2N().wordToNum("11211234"), 11211234);
      *>Assertions.assertEquals(new W2N().wordToNum("five"), 5);
      *>Assertions.assertEquals(new W2N().wordToNum("two million twenty three thousand and forty nine"), 2023049);

       LOCAL-STORAGE SECTION.
       LINKAGE SECTION.
       SCREEN SECTION.

      ******************************************************************
       PROCEDURE DIVISION.
      * Like XUnit tests setup, test, tear-down
       main section.
         perform setup
         perform test-positives
         perform test-negatives
         perform tear-down
         goback.
       exit section.
      *----------------------------------------------------------------- 
      * setup 
      * initialize the linkage with our default values
      *-----------------------------------------------------------------
       setup section.
           set w2n-returning-ok to true
           initialize w2n-parameter
       exit section.
      *-----------------------------------------------------------------
      * test-positives
      * test cases for positive text values
      *----------------------------------------------------------------- 
       test-positives section.
       
           initialize expected-value
       move "two million three thousand nine hundred and eighty four"
         to  w2n-number-sentences
           move 2003984 to expected-value
           set expected-is-integer to true
           perform assert-true

       exit section.
      *-----------------------------------------------------------------
      * test-negatives
      * test cases for negative text values
      *----------------------------------------------------------------- 
       test-negatives section.
       exit section.
      *-----------------------------------------------------------------
      * tear down
      *-----------------------------------------------------------------
       tear-down section.
       exit section.

      *-----------------------------------------------------------------
      * inline helper 
      *-----------------------------------------------------------------
       assert-true section.
       call "W2N" using by reference w2n-parameter 
                        by reference w2n-returning
                     on exception display "99 bugs in a bottle..."
       end-call
       
       evaluate w2n-returning
         when low-value
           continue *> YEAH 
         when high-value
           if w2n-result is not equal all spaces 
             display w2n-result-value
           else 
             display "Unexpected result for >>>" w2n-number-sentences  
      -      "expected=" expected-value "<=>"
      -      "actually=" w2n-result-value    
             exit program
           end-if
         when other
           display "Unknown error"
               exit program
       end-evaluate

       display "OK: " w2n-result-value 
       
       exit section.       
       

      *- Bye bye -------------------------------------------------------
       exit program.
       END PROGRAM English.
      * EOF
