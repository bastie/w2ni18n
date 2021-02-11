       01 w2n-parameter.
         10 w2n-number-sentences            pic n(1024).
         10 w2n-result.
           15 filler                    pic x(1).
             88 w2n-result-is-none            value x"00".
           15 w2n-result-value.
             20 w2n-result-value-integer    pic 9(38).
             20 w2n-result-value-delmiter   pic n(1).
             20 w2n-result-value-decimal    pic 9(38).         
               88 w2n-result-is-integer         value zeros.
       
       01 w2n-returning             pic X.
         88 w2n-returning-ok          value low-value.
         88 w2n-returning-error       value high-value.
