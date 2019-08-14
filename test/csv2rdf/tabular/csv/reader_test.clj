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
        (is (= ["a\"b" "c" "d"] (parse-row-cells "a\"\"b,c,d" 0 opts)))))))

(deftest trim-cell-test
  (are [expected input mode] (= expected (trim-cell input mode))
       "foo" "  foo    " :all
       "foo\t\n" "  \tfoo\t\n" :start
       "\t\tfoo" "\t\tfoo  \r\n" :end
       "\tfoo\t" "\tfoo\t" :none))