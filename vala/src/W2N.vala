using Gee;

errordomain NumberFormatException {
    FormatException
}


class word2number.W2N : GLib.Object {

    static string data_source;

    string lang = "";
    HashMap<string, long> numberSystem;
    ArrayList<string> decimalWords;
    HashMap<string,string> normalizeData;
    ArrayList<long> sortedMeasureValues;
    string localizedPointName = "";

    /**
     * Create a new instance of Word2Number i18n.
     * Optional it accepts a language parameter to select the
     * language over programming instead configuration.
     */
    public W2N (string? langParam = null) {
        string newLang = (langParam ?? // a nice lang feature
            GLib.Environment.get_variable("w2n.lang") ??
            GLib.Environment.get_variable("LANGUAGE") ??
            "en"); // fallback
        this.lang = newLang.substring(0,2);
        

        this.numberSystem = new HashMap<string, long>();
        this.decimalWords = new ArrayList<string>();
        this.normalizeData = new HashMap<string, string>();
        this.sortedMeasureValues = new ArrayList<long>();
    
        string config;
        try {
            FileUtils.get_contents (data_source+"data/config_"+this.lang+".properties", out config);
        }
        catch (FileError ignore) {
            config="";
        }
        string [] lines = Regex.split_simple("[\n\r]", config.strip());

        int zeroToNine = 0;

        foreach (string line in lines) {
            if (line[0]=='#') {
            }
            else {
                string [] keyValue = line.split ("=",2);
                string key = keyValue [0].strip();
                string value = keyValue [1].strip();
                const string REPLACE = "replace:";
                const string MEASURE = "measure:";
                if (key.length >= REPLACE.length && REPLACE == key.slice(0,8)) {
                    key = key.substring(REPLACE.length);
                    this.normalizeData[key]=value;
                }
                else if (key.length >= MEASURE.length && MEASURE == key.slice(0,8)) {
                    key = key.substring(MEASURE.length);
                    long number = long.parse (value+"\0");  
                    this.sortedMeasureValues.add (number);
                }
                else if (key == "point") {
                    this.localizedPointName = value;
                }
                else {
                    long number = long.parse (value+"\0");
                    this.numberSystem[key] = number;
                }
                if (zeroToNine<10) {
                    this.decimalWords.add (key);
                    zeroToNine++;
                }
            }
        }
        sortedMeasureValues.sort ((a, b) => { // sort reverse with lambda
            return a > b ? -1 : 1;
        });
    }


    /**
    * Method to form numeric multipliers for million, billion, thousand etc.
    * @param numberWords list of strings
    * @return value: integer
    */
    protected int numberFormation (ArrayList<string> numberWords) {
        ArrayList<int> digitValues = new ArrayList<int>();
        // calculate the three digit values (max)
        foreach (string word in numberWords) {
            digitValues.add((int)this.numberSystem[word]);
        }
        int hundredIndex = 100 in digitValues ? digitValues.index_of(100) : -1;
        if (hundredIndex == 1){
            digitValues[0] =  digitValues[0] * digitValues [1]; // this is equals to Python
            digitValues.remove(digitValues[1]);
        }
        if ((digitValues.size > 3) && (digitValues[0] < 100)) {
          digitValues[0] *= digitValues[1];
          digitValues.remove(digitValues[1]);
        }
        else if ((digitValues.size > 3) && (digitValues[0] > 100)) {
          digitValues[1] *= digitValues[2];
          digitValues.remove(digitValues[2]);
        }
        // add the three digits
        while (digitValues.size > 1) {
          digitValues[0] = digitValues[0] + digitValues[1];
          digitValues.remove(digitValues[1]);
        }
        // return the result

        return digitValues[0];
    }

    /**
    * Method to convert post decimal digit words to numerial digits
    * it returns a string to prevert from floating point conversation problem
    * @param decimalDigitWords list of strings
    * @return string
    */
    protected string getDecimalString(ArrayList<string> decimalDigitWords) {
        string decimalNumberStr = ""; 
        foreach (string decWord in decimalDigitWords) {
        if(!decimalWords.contains(decWord))
            return "0";
        else
            decimalNumberStr += this.numberSystem[decWord].to_string();
        }
        return decimalNumberStr;
    }
    

    /**
    * function to return integer for an input `newNumberValue` string
    * @param newNumberValue numberValue
    * @return Number or null
    */
    public Value wordToNum_from_long (long newNumberValue) {
      var result = Value(typeof(long));
      result.set_long (newNumberValue);
      return result;
    }
  
  /**
   * Method to return integer for an input `newNumberSentence` string
   * @param numberSentence text
   * @return Number, prefered Long and Double, or null
   */
   public Value wordToNum_from_string (string newNumberSentence) throws NumberFormatException {

      long long_result = -1; 
      double double_result = -1d;
      ArrayList<string> cleanNumbers = new ArrayList<string>();
      ArrayList<string> cleanDecimalNumbers = new ArrayList<string>();

      if (null == newNumberSentence)
        throw new NumberFormatException.FormatException("Type of input is null! Please enter a valid number word (eg. \'two million twenty three thousand and forty nine\')");

      string numberSentence = this.normalize(newNumberSentence.to_string());

      // return the number if user enters a number string
      long tempLong = -1;
      if (long.parse(numberSentence).to_string() == numberSentence) {
        long_result = long.parse(numberSentence);
      }
      else {
          double tempDouble;
          if (double.try_parse(numberSentence, out tempDouble)) {
              double_result = tempDouble;
          }
      }

      bool isDigit = long_result >= 0 || double_result >= 0; // maybe to optimize by compiler but to similar code to python 
    if (!isDigit) {
      string [] splitWords = new string [0];
      try {
        var regex = new Regex ("[\\s,]+");
        splitWords = regex.split_full (numberSentence);
      }
      catch (Error error) {
        throw new NumberFormatException.FormatException (@"$(error.message)");
      }

      // removing and, & etc.
      foreach (string word in splitWords) {
        if (this.numberSystem.has_key(word)) {
          cleanNumbers.add(word);
        }
        else if (word == this.localizedPointName){
          cleanNumbers.add(word);
        }
      }

      // Error message if the user enters invalid input!
      if (cleanNumbers.size== 0) 
          throw new NumberFormatException.FormatException("No valid number words found! Please enter a valid number word (eg. two million twenty three thousand and forty nine)");
      
      bool toMuchPoints = cleanNumbers.index_of(this.localizedPointName) != last_index_of(cleanNumbers,this.localizedPointName);
      if (toMuchPoints)
        throw new NumberFormatException.FormatException(@"Redundant point word $localizedPointName! Please enter a valid number word (eg. two million twenty three thousand and forty nine)");
  
        // separate decimal part of number (if exists)
      bool pointCount = cleanNumbers.index_of(this.localizedPointName)>-1;
      if (pointCount) {
        var temp_clean_decimal_numbers = new ArrayList<string>();
        temp_clean_decimal_numbers.add_all (cleanNumbers.slice(cleanNumbers.index_of(this.localizedPointName)+1, cleanNumbers.size-(cleanNumbers.index_of(this.localizedPointName)+1)));
        cleanDecimalNumbers = temp_clean_decimal_numbers;

        var temp_clean_numbers = new ArrayList<string>();
        temp_clean_numbers.add_all (cleanNumbers.slice(0,cleanNumbers.index_of(this.localizedPointName)));
        cleanNumbers = temp_clean_numbers;
      }

      // special case "point" without pre or post number   
      if (cleanDecimalNumbers.size == 0 && cleanNumbers.size == 0) return (int) 0;

      // check measure word errors
      ArrayList<int> measureWordsSequence = new ArrayList<int>();
      // check for to much measure words like "million million"
      foreach (long measureValueDoubleCheck in sortedMeasureValues) {
        if (measureValueDoubleCheck >= 1000) { // measure values under 1000 can be more than one in text
          checkDoubleInput(measureValueDoubleCheck, cleanNumbers);
          // save index for next check
          if (-1 != getIndexForNumber(measureValueDoubleCheck,cleanNumbers)){
            measureWordsSequence.add(getIndexForNumber(measureValueDoubleCheck,cleanNumbers));
          }
        }
      }
      
      ArrayList<int> sortedMeasureWordsSequence = new ArrayList<int>();
      sortedMeasureWordsSequence.add_all(measureWordsSequence);
      sortedMeasureWordsSequence.sort();
      for (int i = 0; i <measureWordsSequence.size;i++) {
        if (measureWordsSequence[i] != sortedMeasureWordsSequence[i]) {
          throw new NumberFormatException.FormatException ("Malformed number in result of false measure word sequence eg. trillion after thousand! Please enter a valid number word (eg. two million twenty three thousand and forty nine)");
        }
      }
      
      // normalize measure words with add localized value for 1 before than empty
      bool lastWasMeasureWord = true;
      ArrayList<string> normalizedCleanNumbers = new ArrayList<string>(); 
      ArrayList<string> measureNames = new ArrayList<string>();
      foreach (long value in sortedMeasureValues) {
        measureNames.add(getNameByNumberValue(value));
      }
      foreach (string nextPart in cleanNumbers) {
        if (nextPart in measureNames && lastWasMeasureWord) {
          normalizedCleanNumbers.add(this.getNameByNumberValue(1L));
          lastWasMeasureWord = true;
        }
        else {
          lastWasMeasureWord = nextPart in measureNames; 
        }
        normalizedCleanNumbers.add(nextPart);
      }
      cleanNumbers = normalizedCleanNumbers;


      // check no measure words in decimal numbers
      foreach (long measureValue in sortedMeasureValues) {
          string measure_name = getNameByNumberValue(measureValue);
          if (measure_name in cleanDecimalNumbers)
            throw new NumberFormatException.FormatException ("Malformed number in result of false measure word after point eg. trillion after thousand! Please enter a valid number word (eg. two million twenty three thousand and forty nine)");
      }
      
      // Now we calculate the pre-decimal value
      long_result = getNumberValue(cleanNumbers);
 
      // And add the post-decimal value
      if (cleanDecimalNumbers.size > 0){
        string decimalValue = getDecimalString(cleanDecimalNumbers);
        decimalValue = ""+long_result.to_string()+"."+decimalValue;
        if (decimalValue.substring(decimalValue.index_of(".")) == ".0") {
          decimalValue = decimalValue.substring(0, decimalValue.index_of("."));
          tempLong = long.parse (decimalValue);
          long_result = tempLong;
          if (tempLong < int.MAX &&
              tempLong > int.MIN) {
            long_result = (int)tempLong;
          }
        }
        else {
            double_result = double.parse (decimalValue);
        }
      }
    }
    if (double_result <0) { // ok than double not set it is -1
        if (long_result < int.MAX &&
            long_result > int.MIN) {
            long_result = (int)long_result;
        }
    }    

    // in result of given value, we want to give our return in dynamic way
    if (double_result <0) {
      Value return_value = Value (typeof (long));
      return_value.set_long (long_result);
      return return_value;
    }
    else {
      Value return_value = Value (typeof (double));
      return_value.set_double (double_result);
      return return_value;
    }
  }

  private int last_index_of (AbstractList<string> looking_in, string looking_for) {
    int i = looking_in.size-1;
    while (i > 0) {
      if (looking_in.get(i) == looking_for) {
        return i;
      }
      i--;
    }
    return -1;
  }

    /**
    * Method to normalize the whole(!) input text
    * @param numberSentence input string
    * @return normalized input
    */ 
    protected string normalize(string numberSentence) {
       string own_numberSentence = numberSentence;
       own_numberSentence = own_numberSentence.down();  // converting input to lowercase
       own_numberSentence = own_numberSentence.replace ("-"," ");

        // for examples: both is right "vingt et un" and "vingt-et-un"
        // we change this to composed value "vingt-et-un" over the localized data file "replace:" entry
        foreach (var nonAndComposedNumber in this.normalizeData.entries) {
            if (" " in nonAndComposedNumber.key) {
                own_numberSentence = own_numberSentence.replace(nonAndComposedNumber.key, nonAndComposedNumber.value);
            }
        }
        
        return own_numberSentence.strip();
    }
  
  
  /**
   * Method to check false redundant input
   *
   * <br/> Note: call this after normalize (lemma,replace,...) text
   * 
   * @param int new_number, string[] words - looking for count of localized name of new_numerb in words
   * @throws FormatException if redundant input error
   * @implNote this method has language configuration dependency
   * <br/> example 1: check_double_input (1000, "thousand thousand") with lang="en" throws a FormatException
   * <br/> example 2: check_double_input (1000, "thousand thousand") with lang="de" its ok
   * <br/> example 3: check_double_input (1000, "tausend tausend") with lang="de" throws a FormatException
   */
  protected void checkDoubleInput(long newNumber, ArrayList<string> cleanNumbers) throws NumberFormatException {
    if (cleanNumbers.size > 1) { // TODO Backport
      string localizedName =  this.getNameByNumberValue(newNumber);
      bool countGreaterOne = cleanNumbers.index_of(localizedName) != last_index_of(cleanNumbers,localizedName);
      if (countGreaterOne)
          throw new NumberFormatException.FormatException (@"Redundant number word $localizedName in! Please enter a valid number word (eg. two million twenty three thousand and forty nine)");
    }
  }
  

  /**
   * Method to get the value for the measure aka 1000, 1_000_000 ...
   * @param measureIndex index of measure
   * @param cleanNumbers
   * @return multiplier for measure
   */
  protected long getMeasureMultiplier (int measureIndex, ArrayList<string> cleanNumbers) {
    ArrayList<string> param = new ArrayList<string>();
    param.add_all (cleanNumbers.slice(0,measureIndex));
    if (param.size == 0) {
      param.add(this.getNameByNumberValue(1L));
    }
    long multiplier = this.numberFormation(param);
    return multiplier;
  }

  /**
   * Method to get the localized name form value
   * @param newNumber numeric value
   * @return name from language configuration or <code>null</code> if not found 
   */
  protected string? getNameByNumberValue (long newNumber) {
    foreach (var pair in this.numberSystem) {
      if (newNumber == pair.value) {
        return pair.key;
      }
    }
    return null;
  }
  
  /**
   * Method to get the index of name for given number
   * 
   * <br/> note: call this after normalize (lemma,replace,...) text
   * 
   * @param newNumber number to looking for
   * @param cleanNumbers
   * @return index or -1 if not found
   */
  protected int getIndexForNumber (long newNumber, ArrayList<string> cleanNumbers) {
    string localizedName = getNameByNumberValue(newNumber);
    return cleanNumbers.index_of(localizedName);
  }
  
      /**
      * Method to get the pre-decimal number from clean_number
      * @param sorted list with number words
      * @return number
      */
      protected long getNumberValue (ArrayList<string> cleanNumbers) {
        long result = 0L;

        /*
        * The simple algorithm based on the idea from NLP to work with tagging (key)words
        * but yes it is handmade implemented today.
        *
        * -- 2021-02-05 --
        * The tagging can be tested on for example https://parts-of-speech.info and tell for
        * nine trillion one billion two million twenty three thousand and forty nine point two three six nine
        * - "and" is a conjunction
        * - "point" is a none
        * - all other are numbers
        * But also contains this line these "measure words" for numbers:
        * - trillion
        * - billion
        * - million
        * - thousand
        * - hundred
        * This new algorithm split the word array from highest value to lowest 
        * (because hundred can be a measure and also a number). Then it work
        * only with number for this measure, simplify so the algorithm and
        * make it free from other measure part in the number.
        * Also it is no different to calculate a trillion or a million or other
        */
        foreach (long measureValue in sortedMeasureValues){
          int measureValueIndex = getIndexForNumber(measureValue, cleanNumbers);
          if (measureValueIndex > -1){
            result +=  getMeasureMultiplier(measureValueIndex, cleanNumbers) * measureValue;
            // we remove next manually the leading elements from lsit
            for (int i = 0 ; i <= measureValueIndex; i++) {
              cleanNumbers.remove_at(0);
            }
          }
        }
        // Now we add the value of less then hundred
        if (cleanNumbers.size > 0){
          int multiplier = this.numberFormation(cleanNumbers);
          result +=  multiplier * 1;
        }

        return result;
      }


    /**
     * The programm entry
     */
     public static int main(string[] args) {
      // next set the relative path for later file loading
      W2N.data_source = args[0].substring(0,(args[0].char_count() - "W2N".char_count()));

      try {
        W2N instance = new W2N();
        // integer values
        assert_equal (instance.wordToNum_from_long(777L),"777");
        assert_equal (instance.wordToNum_from_string("nineteen"), "19");
        assert_equal (instance.wordToNum_from_string("two thousand and nineteen"), "2019");
        assert_equal (instance.wordToNum_from_string("two million three thousand and nineteen"), "2003019");
        assert_equal (instance.wordToNum_from_string("three billion"), "3000000000");
        assert_equal (instance.wordToNum_from_string("three million"), "3000000");
        assert_equal (instance.wordToNum_from_string("one hundred twenty three million four hundred fifty six thousand seven hundred and eighty nine"), "123456789");
        assert_equal (instance.wordToNum_from_string("eleven"), "11");
        assert_equal (instance.wordToNum_from_string("nineteen billion and nineteen"), "19000000019");
        assert_equal (instance.wordToNum_from_string("one hundred and forty two"), "142");
        assert_equal (instance.wordToNum_from_long(112), "112");
        assert_equal (instance.wordToNum_from_long(11211234), "11211234");
        assert_equal (instance.wordToNum_from_string("five"), "5");
        assert_equal (instance.wordToNum_from_string("two million twenty three thousand and forty nine"), "2023049");
        assert_equal (instance.wordToNum_from_string("one hundred thirty-five"), "135");
        assert_equal (instance.wordToNum_from_string("hundred"), "100");
        assert_equal (instance.wordToNum_from_string("thousand"), "1000");
        println (instance.wordToNum_from_string("nine point nine nine nine"));
        assert_equal (instance.wordToNum_from_string("million"), "1000000");
        assert_equal (instance.wordToNum_from_string("billion"), "1000000000");
        assert_equal (instance.wordToNum_from_string("trillion"), "1000000000000");
        assert_equal (instance.wordToNum_from_string("one million and thousand"), "1001000");
        assert_equal (instance.wordToNum_from_string("seven million, eight hundred, and sixty three thousand, two hundred, and fifty four"), "7863254");
        assert_equal (instance.wordToNum_from_string("two hundreds"), "200");
        // non number points
        assert_equal (instance.wordToNum_from_string("point"), "0");
        assert_equal (instance.wordToNum_from_string("point nineteen"), "0");
        assert_equal (instance.wordToNum_from_string("seventh point nineteen"), "0");
        // decimal value
        assert_equal (instance.wordToNum_from_string("two million twenty three thousand and forty nine point two three six nine"), "2023049.2369");
        assert_equal (instance.wordToNum_from_string("one billion two million twenty three thousand and forty nine point two three six nine"), "1002023049.2369");
        assert_equal (instance.wordToNum_from_string("nine trillion one billion two million twenty three thousand and forty nine point two three six nine"), "9001002023049.2369");
        assert_equal (instance.wordToNum_from_string("two point three"), "2.3");
        assert_equal (instance.wordToNum_from_string("point one"), "0.1");
        assert_equal (instance.wordToNum_from_string("nine point nine nine nine"), "9.999");


      }
      catch (NumberFormatException ignored) {
        stderr.printf (@"Errorrrr: $(ignored.message)\n");
        return 6;
      }

      return 0;
  }

}


private void assert_equal (Value actually, string expected) {
  if (expected != pretty(actually)) {
    var not_so_nice = pretty(actually);
    stdout.printf (@"Assertation error for\nexpected:$(expected)\nactually:$(not_so_nice)\n");
    Process.exit(666);
  }
}
private void println (Value result) {
  var nice_looking = pretty(result);
  stdout.printf (@"$(nice_looking)\n");
}
private string pretty (Value dirty) {
  return dirty.type_name() == "glong" ? dirty.get_long().to_string() : dirty.get_double().to_string ();
}