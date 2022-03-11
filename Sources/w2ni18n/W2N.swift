/**
 * SPDX-FileCopyrightText: 2022 - Sebastian Ritter <bastie@users.noreply.github.com>
 * SPDX-License-Identifier: MIT
 */

import Foundation

/**
 Word2Number type
 */
public class W2N {
    
    fileprivate var numberSystem : [String: Int] = [:]
    fileprivate var normalizeData : [String: String] = [:]
    fileprivate var sortedMeasureValues : [Int] = [] // = [1_000_000_000_000,1_000_000_000,1_000_000,1_000,100]
    fileprivate var localizedPointName = ""
    fileprivate var decimalWords : [String] = []
    
    internal var lang : String? = "en"

    // MARK: - Constructor
    
    /**
     Construct the instance of type
     */
    public init (_ language : String?) {
        // first get programming language specific local spoken language
        lang = language
        if let lang = language {}
        else {
            lang = Locale.current.languageCode!
        }
        if let w2nEnvOverwriteSystemLanguage = ProcessInfo.processInfo.environment["w2n.lang"]{
            lang = w2nEnvOverwriteSystemLanguage
        }
        if lang == nil {
            if let envOverwriteSystemLanguage = ProcessInfo.processInfo.environment["LANGUAGE"]{
                lang = envOverwriteSystemLanguage
            }
        }
        if lang == nil {
            lang = "en"  // fallback
        }
        guard lang?.count == 2 else {
            Int8(128+128) // kill the programm without throw declaration
        }
        
        // Now analyse the configuration file for the local spoken language
        let dataFilePath = "./data/config_" + lang!;
        if let dataFileURL = Bundle.main.url(forResource: dataFilePath, withExtension: "properties") {
            var zeroToNine : Int = 0
            if let dataFileContent = try? String (contentsOfFile: dataFileURL.absoluteString, encoding: .utf8) {
                let numberSystemData = dataFileContent.split(whereSeparator: \.isNewline)
                for line in numberSystemData {
                    if line.starts(with: "#") {
                    }
                    else {
                        let parts = {
                               return (line.components(separatedBy: "=")[0], line.components(separatedBy: "=")[1] )
                            }()
                        var key = parts.0
                        let val = parts.1
                        if key.starts(with: "replace:") {
                            key = { return String (key.split(separator: ":",maxSplits: 1) [1] )}()
                            self.normalizeData[key] = val.trimmingCharacters(in: .whitespacesAndNewlines)
                        }
                        else {
                            if key.starts(with: "measure:"){
                                
                                self.sortedMeasureValues.append(
                                    Int(val.trimmingCharacters(in: .whitespacesAndNewlines))!)
                            }
                            else {
                                if "point" != key {
                                    let numericValue = Int(val.trimmingCharacters(in: .whitespacesAndNewlines))!
                                    self.numberSystem[key] = numericValue
                                }
                                else{
                                    self.localizedPointName = val.trimmingCharacters(in: .whitespacesAndNewlines)
                                }
                            }
                        }
                        if zeroToNine < 10 {
                            self.decimalWords.append(key)
                            zeroToNine+=1
                        }
                    }
                }
            }
        }
        self.sortedMeasureValues = self.sortedMeasureValues.sorted().reversed()
    }
    
    // MARK: - namberFormations
    /** [internal] function to form numeric multipliers
    
    input: list of strings
    return value: integer
    */
    fileprivate func numberFormation(numberWords : [String]) -> Int {
        var digitValues : [Int] = []
        // calculate the three digit values (max)
        for word in numberWords {
            var nextNumberCandidat = self.numberSystem[word]!
            digitValues.append(nextNumberCandidat)
        }
        let hundredIndex = digitValues.contains(100) ? digitValues.firstIndex(of:100) : -1
        if hundredIndex == 1 {
            digitValues[0] = digitValues[0] * digitValues[1] // this is like other languages need to do it
            digitValues.remove(at: 1)
        }
        if digitValues.count > 3 && digitValues[0] < 100 {
            digitValues[0] *= digitValues[1]
            digitValues.remove(at: 1)
        }
        else {
            if digitValues.count > 3 && digitValues[0] > 100 {
                digitValues[1] *= digitValues[2]
                digitValues.remove(at: 2)
            }
        }
        // add the three digits
        while digitValues.count > 1 {
            digitValues[0] += digitValues[1]
            digitValues.remove(at: 1)
        }
        // return the result
        return digitValues[0]
    }
    
    /** [internal] function to convert post decimal digit words to numerial digits
    it returns a string to prevert from floating point conversation problem
    
    input: list of strings
    output: string
    **/
    fileprivate func getDecimalString(decimalDigitWords : [String]) -> String {
        var decimalNumberString : String = ""
        for decWord in decimalDigitWords {
            if !self.decimalWords.contains(decWord) {
                return "0"
            }
            else {
                decimalNumberString += String(self.numberSystem[decWord]!)
            }
        }
        return decimalNumberString
    }

    // MARK: - normalize functions
    /*
     typesafe language unlike Python
     */
    fileprivate func normalize (numberSentence : Float) -> String {
        return String(numberSentence)
    }
    /*
     typesafe language unlike Python
     */
    fileprivate func normalize (numberSentence : Int) -> String {
        return String(numberSentence)
    }
    /** [internal] function to normalize the whole(!) input text
    
    input: string the full text
    output: string
    */
    fileprivate func normalize(numberSentence : String) -> String {
        // we need no check for numbers...
        // ...but if it is a string we need normalizing
        var internNumberSentence = numberSentence.lowercased()  // converting input to lowercase
    
        internNumberSentence = internNumberSentence.replacingOccurrences(of: "-", with: " ")
        // for examples: both is right "vingt et un" and "vingt-et-un"
        // we change this to composed value "vingt-et-un" over the localized data file "replace:" entry

        for nonComposedNumberValue in self.normalizeData.keys {
          if nonComposedNumberValue.contains(" ") {
            let composedNumberValue = self.normalizeData[nonComposedNumberValue]!
              internNumberSentence = internNumberSentence.replacingOccurrences(of: nonComposedNumberValue, with: composedNumberValue)
          }
        }
        
        var result : String = ""
        for word in internNumberSentence.split4W2N(regex: "[\\s,]+") {
            if let newWord = self.normalizeData[word] {
                result.append(newWord)
                result.append(" ")
            }
        }
        result = result.trimmingCharacters(in: .whitespacesAndNewlines)
        return result
    }
    
    
    // MARK: - checkDoubleInput
    /** [internal] function to check false redundant input
    note: this method has language configuration dependency
    
    note: call this after lemma text
    
    example: check_double_input (1000, "thousand thousand") with lang="en" throws a ValueError
    example: check_double_input (1000, "thousand thousand") with lang="de" its ok
    example: check_double_input (1000, "tausend tausend") with lang="de" throws a ValueError
    
    input: int new_number, string[] words - looking for count of localized name of new_numerb in words
    raise: if redundant input error
    */
    fileprivate func checkDoubleInput (newNumber : Int, cleanNumbers : [String]) throws {
        let localizedName = self.getNameByNumberValue(newNumber: newNumber)!
        guard cleanNumbers.lastIndex(of: localizedName) == cleanNumbers.firstIndex(of: localizedName) else {
            
            throw NSError(
                domain: Bundle.className(), code: 500, userInfo: ["description" :
                "Redundant number word \(localizedName) in! Please enter a valid number word (eg. two million twenty three thousand and forty nine)"])
            // i18n save für later:
            // de: "Redundantes Nummernwort! Bitte gebe ein zulässiges Nummernwort ein (z.B. zwei Millionen Dreiundzwanzigtausend und Neunundvierzig)"
            // ru: "Избыточное числовое слово! Введите правильное числовое слово (например, два миллиона двадцать три тысячи сорок девять)"
    
        }
    }

    // MARK: - getNameByNumberValue
    
    /** [internal] function to get the localized name form value
    
    input: numeric value
    output: name from language configuration or None if not found
    */
    fileprivate func getNameByNumberValue (newNumber : Int) -> String? {
        for key in self.numberSystem.keys {
            if newNumber == self.numberSystem[key] {
            return key;
          }
        }
        return nil;
    }
    
    // MARK: - getIndexForNumber
    
    /* [internal] function to get the index of name for given number
        note: call this after lemma text

        input: int number
        output: index or nil if not found
    
    */
    fileprivate func getIndexForNumber(newNumber : Int, cleanNumbers : [String]) -> Int? {
        // in result of get name by numeric value, the localized name came from dictionary
        // and we need no language specific code
        if let localizedName = self.getNameByNumberValue(newNumber: newNumber) {
            if let result = cleanNumbers.firstIndex(of:localizedName) {
                return result
            }
        }
        return nil
    }
    
    // MARK: - getNumberValue
    
    /** [internal] function to get the pre-decimal number from clean_number
    
        input: sorted array with number words
        output: int number
        raise: ValueError
    */
    fileprivate func getNumberValue (cleanNumbers : [String]) -> Int {
        var result = 0
    
        /*
        # The simple algorithm based on the idea from NLP to work with tagging (key)words
        # but yes it is handmade implemented today.
        #
        # -- 2021-02-05 --
        # The tagging can be tested on for example https://parts-of-speech.info and tell for
        # nine trillion one billion two million twenty three thousand and forty nine point two three six nine
        # - "and" is a conjunction
        # - "point" is a none
        # - all other are numbers
        # But also contains this line these "measure words" for numbers:
        # - trillion
        # - billion
        # - million
        # - thousand
        # - hundred
        # This new algorithm split the word array from highest value to lowest
        # (because hundred can be a measure and also a number). Then it work
        # only with number for this measure, simplify so the algorithm and
        # make it free from other measure part in the number.
        # Also it is no different to calculate a trillion or a million or other
        */
        var muatbleCleanNumbers = cleanNumbers
        for measureValue in self.sortedMeasureValues{
            if let measureValueIndex = self.getIndexForNumber(newNumber: measureValue, cleanNumbers: muatbleCleanNumbers) {
                result +=  self.getMeasureMultiplier(measureIndex: measureValueIndex, cleanNumbers: muatbleCleanNumbers) * measureValue
                muatbleCleanNumbers.remove(at: 0)
            }
        }
        // Now we add the value of less then hundred
        if muatbleCleanNumbers.count > 0{
            let multiplier = self.numberFormation(numberWords: muatbleCleanNumbers)
            result +=  multiplier * 1
        }
        return result
    }
    
    // MARK: - getMeasureMultiplier

    /**function to get the value for the measure aka 1000, 1_000_000 ...
    
    input: index of measure
    output: multiplier for measure
    */
    fileprivate func getMeasureMultiplier (measureIndex : Int, cleanNumbers : [String]) -> Int {
        var param : [String] = Array (cleanNumbers[0...measureIndex])
        if param.isEmpty {
            if let toAppend = self.getNameByNumberValue(newNumber: 1) {
                param.append(toAppend);
            }
            else {
                _ = UInt8(128+128)
            }
        }
        let multiplier : Int = self.numberFormation(numberWords: param);
        return multiplier;
    }
    
    // MARK: - wordToNum
    
    /** public function to return integer for an input `number_sentence` string
    This function return as result
    - the same float if float is input
    - the same int if int is given
    - None if no number can be extracted
    - Error if extracted number is formal incorrect
     
     Sample:
     switch wordToNum(numberSentence: "Take the three books.") {
     case let numericValue as Double:
        print ("Double value is \(numericValue).")
     case let numericValue as Int:
        print ("Int value is \(numericValue)!")
     default:
        print ("no numeric value")
     }


    preconditions: number_sentence is type of float, int or str
    input: string
    output: int or float or None
    raise: given number is formal incorrect
    */
    public func wordToNum(_ numberSentence : Any) -> Any {
        // check preconditions
        var numberSentenceString : String = numberSentence
        switch numberSentence {
        case let doubleValue as Double:
            return doubleValue
        case let intValue as Int:
            return intValue
        case let numberSentenceStr as String:
            numberSentenceString = numberSentenceStr
        default:
            let _ : UInt8 = UInt8(128+128)
            //raise ValueError("Type of input is not string! Please enter a valid number word (eg. \'two million twenty three thousand and forty nine\')")
        }
        numberSentenceString = numberSentenceString.trimmingCharacters(in: .whitespacesAndNewlines)

        var result : Any = 0
        var cleanNumbers : [String] = []
        var cleanDecimalNumbers : [String] = []
    
        
        numberSentenceString = self.normalize(numberSentence: numberSentenceString )
    
        if let isDigit = Int(numberSentenceString) {  // return the number if user enters a number string
            result = isDigit
        }
        else{
            splitWords = re.findall(r'\w+', numberSentenceString)  // strip extra spaces and comma and than split sentence into words
        
            // removing unknown words form text
            for word in splitWords{
                word = self.normalizeData.get(word,word) // replacing words and lemma text
                if word in self.numberSystem {
                    cleanNumbers.append(word)
                }
                else {
                    if word == self.localizedPointName {
                        cleanNumbers.append(word)
                    }
                }
            }
        
            // Error message if the user enters invalid input!
            if cleanNumbers.count == 0 {
                let _ = UInt8(128+128)
                //raise ValueError("No valid number words found! Please enter a valid number word (eg. two million twenty three thousand and forty nine)")
            }
        
            // check point count
            if cleanNumbers.firstIndex(of:self.localizedPointName) != cleanNumbers.lastIndex(of: self.localizedPointName) {
                 raise ValueError("Redundant point word "+self.localizedPointName+"! Please enter a valid number word (eg. two million twenty three thousand and forty nine)")
            }
                      
            // split in pre-decimal and post-decimal part
            var pointCount = cleanNumbers.count(self.localizedPointName)
            if pointCount == 1 {
                cleanDecimalNumbers = cleanNumbers[cleanNumbers.index(self.localizedPointName)+1:]
                cleanNumbers = cleanNumbers[:clean_numbers.index(self.localizedPointName)]
            }
    
            // check measure word errors
            var measureWordsSequence : [String] = []
            // check for to much measure words like "million million"
            for measureValueDoubleCheck in self.sortedMeasureValues {
                if measureValueDoubleCheck >= 1000 { // measure values under 1000 can be more than one in text
                    self.checkDoubleInput(measureValueDoubleCheck, cleanNumbers)
                    // save index for next check
                    if -1 != self.getIndexForNumber(measureValueDoubleCheck,cleanNumbers) {
                        measureWordsSequence.append(self.getIndexForNumber(measureValueDoubleCheck,cleanNumbers))
                    }
                }
            }
    
            // check generic measure words are in right sequence
            if measureWordsSequence != sorted(measureWordsSequence) {
                let _ = UInt8(128+128)
                //raise ValueError("Malformed number in result of false measure word sequence eg. trillion after thousand! Please enter a valid number word (eg. two million twenty three thousand and forty nine)")
            }
    
            // check no measure words in decimal numbers
            for measureValue in self.sortedMeasureValues {
                var measureName = self.getNameByNumberValue(newNumber: measureValue)!
                if measureName in cleanDecimalNumbers {
                    let _ = UInt8(128+128)
                    //raise ValueError("Malformed number in result of false measure word after point eg. trillion after thousand! Please enter a valid number word (eg. two million twenty three thousand and forty nine)")
                }
            }
    
            // Now we calculate the pre-decimal value
            result = self.getNumberValue(cleanNumbers: cleanNumbers)
            
            // And add the post-decimal value
            if cleanDecimalNumbers.count > 0 {
                var totalSumAsString = String(result)+"."+String(self.getDecimalString(cleanDecimalNumbers))
                result = Float(totalSumAsString)
            }
        }
        return result
    }
}

/**
 
 Sample:
 switch wordToNum(numberSentence: "Take the three books.") {
 case let numericValue as Double:
    print ("Double value is \(numericValue).")
 case let numericValue as Int:
    print ("Int value is \(numericValue)!")
 default:
    print ("no numeric value")
 }
 */
func wordToNum (numberSentence : String, langParam : String?) -> Any {
    let instance = W2N(langParam)
    return instance.wordToNum(numberSentence)
}

//EOF

// MARK: - unwanted String extension

// Unlike this project the https://github.com/crossroadlabs/Regex library is under more permissive license and not useable.
extension String {
    func split4W2N(regex pattern: String) -> [String] {
        let regex = try! NSRegularExpression(pattern: pattern)
        let matches = regex.matches(in: self, range: NSRange(0..<utf16.count))
        let ranges = [startIndex..<startIndex] + matches.map{Range($0.range, in: self)!} + [endIndex..<endIndex]
        return (0...matches.count).map {String(self[ranges[$0].upperBound..<ranges[$0+1].lowerBound])}
    }
}

