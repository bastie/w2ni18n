      *> 100%-pure COBOL text utilities Linkage
      *> Select the text util function over 88
      *> 
       01 txt-util-parameter.
         05 txt-util-function             pic 9(2).
           88 txt-util-func-none            value zero.
           88 txt-util-func-trim            value 10.
           88 txt-util-func-indexof         value 20.
           88 txt-util-func-startswith      value 21.
           88 txt-util-func-split           value 22.
         05 filler.
           10 txt-util-input-output-byte  pic x(199998).
           10 txt-util-input-output       
              redefines txt-util-input-output-byte pic n(99999).    
           10 txt-util-input-xdata        pic n(99999).
           10 txt-util-input-looking-for  
              redefines txt-util-input-xdata pic n(99999). *> using for indexof
           10 txt-util-input-starts-with  
              redefines txt-util-input-xdata pic n(99999). *> using for indexof
           10 txt-util-output-split-data
              redefines txt-util-input-xdata.
             15 txt-util-split-rules           pic n(2000). *> 4000
             15 txt-util-split-output-next   occurs 400.
               20 txt-util-split-output-start  pic 9(5).    *> 2000
               20 txt-util-split-output-end    pic 9(5).    *> 2000
               20 txt-util-split-output-length pic 9(5).    *> 2000           
         05 txt-util-result.
           10 txt-util-rc                 pic 9(2).
             88 txt-util-rc-ok              value zero thru 89.
             88 txt-util-rc-true            value 01.
             88 txt-util-rc-false           value 02.
             88 txt-util-rc-nothing-todo    value 80.
             88 txt-util-rc-unknown-error   value 90.
             88 txt-util-rc-not-impl        value 99.
           10 txt-util-result-index       pic 9(5).
           10 txt-util-result-trim-end    pic 9(5).
       
