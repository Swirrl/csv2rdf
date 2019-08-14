(ns csv2rdf.tabular.csv.reader-test
  (:require [clojure.test :refer :all]
            [csv2rdf.tabular.csv.reader :refer :all])
  (:import [java.io StringReader PushbackReader]))

(deftest try-read-line-terminator-test
  (let [trie (line-terminators->trie ["\n" "\r\n"])]
    (testing "Read string"
      (are [expected s] (= expected (try-read-line-terminator (PushbackReader. (StringReader. s)) trie))
        [true "\n"] "\n"
        [true "\r\n"] "\r\n"
        [false ""] ""
        [false ""] "foo"
        [false "\r"] "\r"
        [false "\r"] "\rfoo"))

    (testing "Reader state"
      (are [expected s] (= expected (let [r (PushbackReader. (StringReader. s))
                                          _ (try-read-line-terminator r trie)]
                                      (.read r)))
        -1 "\n"
        -1 "\r\n"
        (int \f) "\nf"
        (int \f) "\r\nf"))))

(deftest parse-cell-double-quote-test
  (testing "Valid inputs"
    (are [row-contents start-index expected] (= expected (parse-cell-double-quote row-contents 1 start-index {:quoteChar \' :delimiter \, :trim-mode :all}))
      ;;No quotes to end
      "first,second" 6 {:cell "second" :next-index 12}

      ;;No quotes to delimiter
      "first,second" 0 {:cell "first" :next-index 5}

      ;;Quoted to end no escaped quotes
      "first,'some,text'" 6 {:cell "some,text" :next-index 17}

      ;;Quoted to delimiter no escaped quotes
      "'quoted, text',second" 0 {:cell "quoted, text" :next-index 14}

      ;;Unquoted with escaped quotes to end
      "first,escaped ''quote" 6 {:cell "escaped 'quote" :next-index 21}

      ;;Unquoted with escaped quotes to delimiter
      "escaped ''quotes'',second" 0 {:cell "escaped 'quotes'" :next-index 18}

      ;;Quoted with escaped quotes to end
      "first,'''quoted'' text'" 6 {:cell "'quoted' text" :next-index 23}

      ;;Quoted with escaped quotes to delimiter
      "'''quoted'' text',second" 0 {:cell "'quoted' text" :next-index 17}

      ;;Unquoted with escaped quotes to end
      "first,''''" 6 {:cell "''" :next-index 10}

      ;;Unquoted with escaped quotes to delimiter
      "'''',second" 0 {:cell "''" :next-index 4}

      ;;Returns raw cell text
      "  first  ,second" 0 {:cell "  first  " :next-index 9}

      ;;Empty cell at end of row
      "a,b," 4 {:cell "" :next-index 4}
      ))

  (testing "Unclosed quote"
    (is (thrown? IllegalArgumentException (parse-cell-double-quote "'Unclosed, quote :(" 1 0 {:quoteChar \' :delimiter \,}))))

  (testing "Non-delimiter character after closing quote"
    (is (thrown? IllegalArgumentException (parse-cell-double-quote "'Quoted text' followed by more stuff" 1 0 {:quoteChar \' :delimiter \,})))))

(deftest parse-cell-escape-test
  (testing "Valid inputs"
    (are [row-contents start-index expected] (= expected (parse-cell-escape row-contents 1 start-index {:quoteChar \' :escapeChar \\ :delimiter \,}))
      ;;No quotes to end
      "first,second" 6 {:cell "second" :next-index 12}

      ;;No quotes to delimiter
      "first,second" 0 {:cell "first" :next-index 5}

      ;;Quoted to end no escaped quotes
      "first,'some,text'" 6 {:cell "some,text" :next-index 17}

      ;;Quoted to delimiter no escaped quotes
      "'quoted, text',second" 0 {:cell "quoted, text" :next-index 14}

      ;;Unquoted with escaped quotes to end
      "first,escaped \\'quote" 6 {:cell "escaped 'quote" :next-index 21}

      ;;Unquoted with escaped quotes to delimiter
      "escaped \\'quotes\\',second" 0 {:cell "escaped 'quotes'" :next-index 18}

      ;;Quoted with escaped quotes to end
      "first,'\\'quoted\\' text'" 6 {:cell "'quoted' text" :next-index 23}

      ;;Quoted with escaped quotes to delimiter
      "'\\'quoted\\' text',second" 0 {:cell "'quoted' text" :next-index 17}

      ;;Unquoted with escaped quotes to end
      "first,\\'\\'" 6 {:cell "''" :next-index 10}

      ;;Unquoted with escaped quotes to delimiter
      "\\'\\',second" 0 {:cell "''" :next-index 4}

      ;;Returns raw cell text
      "  first  ,second" 0 {:cell "  first  " :next-index 9}

      ;;empty cell at end of line
      "a,b," 4 {:cell "" :next-index 4}))

  (testing "Unclosed quote"
    (is (thrown? IllegalArgumentException (parse-cell-escape "'Unclosed, quote :(" 1 0 {:quoteChar \' :escapeChar \\ :delimiter \,}))))

  (testing "Dangling escape character"
    (is (thrown? IllegalArgumentException (parse-cell-escape "Dangling escape\\" 1 0 {:quoteChar \' :escapeChar \\ :delimiter \,}))))

  (testing "Non-delimiter character after closing quote"
    (is (thrown? IllegalArgumentException (parse-cell-escape "'Quoted text' followed by more stuff" 1 0 {:quoteChar \' :escapeChar \\ :delimiter \,})))))

(deftest parse-row-cells-test
  (testing "Quote and escape characters differ"
    (let [opts {:escapeChar \\ :quoteChar \" :delimiter \, :trim-mode :all}]
      (testing "No quotes or spaces"
        (is (= ["a" "b" "c"] (parse-row-cells "a,b,c" 0 opts))))

      (testing "No quotes with spaces"
        (is (= ["a" "b" "c"] (parse-row-cells "a  , b ,    c" 0 opts))))

      (testing "With quotes no spaces"
        (is (= ["a,b" "c" "d"] (parse-row-cells "\"a,b\",c,d" 0 opts))))

      (testing "With quotes and spaces"
        (is (= ["a,b" "c" "d"] (parse-row-cells "\"  a,b  \",  c  ,    d" 0 opts))))

      (testing "With escape"
        (is (= ["a\"b" "c" "d"] (parse-row-cells "a\\\"b,c,d" 0 opts))))

      (testing "Dangling escape character"
        (is (thrown? IllegalArgumentException (parse-row-cells "a,b,c\\" 0 opts))))

      (testing "Quote beyond the start of cell value"
        (is (thrown? IllegalArgumentException (parse-row-cells "start \"text\",b,c" 0 opts))))

      (testing "Closing quote before the end of cell value"
        (is (thrown? IllegalArgumentException (parse-row-cells "\"quoted text\" and more,b,c" 0 opts))))))

  (testing "Quote and escape characters the same"
    (let [opts {:escapeChar \" :quoteChar \" :delimiter \, :trim-mode :none}]
      (testing "With quotes"
        (is (= ["a,b" "c" "d"] (parse-row-cells "\"a,b\",c,d" 0 opts))))

      (testing "Escaped quote"
        (is (= ["a\"b" "c" "d"] (parse-row-cells "a\"\"b,c,d" 0 opts))))

      (testing "Escaped quote at start of cell"
        (is (= ["\"Quoted text\"" "b" "c"] (parse-row-cells "\"\"Quoted text\"\",b,c" 0 opts))))

      (testing "Triple quotes"
        (is (= ["\"Quoted text\", and more" "" "c"] (parse-row-cells "\"\"\"Quoted text\"\", and more\",,c" 0 opts))))))

  (testing "Last cell empty"
    (is (= ["a" "b" ""] (parse-row-cells "a,b," 1 {:escapeChar \" :quoteChar \" :delimiter \, :trim-mode :none}))))

  (testing "Empty"
    (is (= [] (parse-row-cells "" 1 {:escapeChar \" :quoteChar \" :delimiter \, :trim-mode :none}))))

  (testing "Whitespace"
    (is (= ["   "] (parse-row-cells "   " 1 {:escapeChar \" :quoteChar \" :delimiter \, :trim-mode :none})))))

(deftest trim-cell-test
  (are [expected input mode] (= expected (trim-cell input mode))
       "foo" "  foo    " :all
       "foo\t\n" "  \tfoo\t\n" :start
       "\t\tfoo" "\t\tfoo  \r\n" :end
       "\tfoo\t" "\tfoo\t" :none))
