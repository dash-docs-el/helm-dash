(ert-deftest helm-dash-test/some-with-matches ()
  "Should return the first element of the list because it satisfies the function."
  (should (equal (helm-dash-some '(lambda (x) (equal 0 (mod x 2))) '(1 2 3 4))
		 2)))

(ert-deftest helm-dash-test/some-no-matches ()
  "Should return nil because it doesn't satisfy the function."
  (should (equal (helm-dash-some '(lambda (x) (equal 5 x)) '(1 2 3 4))
		 nil)))

(ert-deftest helm-dash-test/maybe-narrow-to-one-docset-filtered ()
  "Should return a list with filtered connections."
  (let ((pattern "Go ")
        (helm-dash-docsets-path "/tmp/.docsets")
        (helm-dash-common-docsets '("Redis" "Go" "CSS" "C" "C++"))
        (helm-dash-connections
         '(("Redis" "/tmp/.docsets/Redis.docset/Contents/Resources/docSet.dsidx" "DASH")
           ("Go" "/tmp/.docsets/Go.docset/Contents/Resources/docSet.dsidx" "DASH")
           ("C" "/tmp/.docsets/C.docset/Contents/Resources/docSet.dsidx" "DASH")
           ("C++" "/tmp/.docsets/C++.docset/Contents/Resources/docSet.dsidx" "DASH")
           ("CSS" "/tmp/.docsets/CSS.docset/Contents/Resources/docSet.dsidx" "ZDASH"))))
    (should (equal (helm-dash-maybe-narrow-to-one-docset pattern)
                   '(("Go" "/tmp/.docsets/Go.docset/Contents/Resources/docSet.dsidx" "DASH"))))

    (should (equal "C" (caar (helm-dash-maybe-narrow-to-one-docset "C foo"))))
		(should (equal "C++" (caar (helm-dash-maybe-narrow-to-one-docset "C++ foo"))))
		(should (equal "C" (caar (helm-dash-maybe-narrow-to-one-docset "c foo"))))))

(ert-deftest helm-dash-test/maybe-narrow-to-one-docset-not-filtered ()
  "Should return all current connections because the pattern doesn't match with any connection."
  (let ((pattern "FOOOO ")
	(helm-dash-docsets-path "/tmp/.docsets")
	(helm-dash-common-docsets '("Redis" "Go" "CSS"))
	(helm-dash-connections
	 '(("Redis" "/tmp/.docsets/Redis.docset/Contents/Resources/docSet.dsidx" "DASH")
	   ("Go" "/tmp/.docsets/Go.docset/Contents/Resources/docSet.dsidx" "DASH")
	   ("CSS" "/tmp/.docsets/CSS.docset/Contents/Resources/docSet.dsidx" "ZDASH"))))
    (should (equal (helm-dash-maybe-narrow-to-one-docset pattern) helm-dash-connections))))

(ert-deftest helm-dash-test/sub-docset-name-in-pattern-with-docset-name ()
  ""
  (let ((pattern "Redis BLPOP")
	(docset "Redis"))
    (should (equal (helm-dash-sub-docset-name-in-pattern pattern docset) "BLPOP"))))

(ert-deftest helm-dash-test/sub-docset-name-in-pattern-without-docset-name ()
  ""
  (let ((pattern "BLPOP")
	(docset "Redis"))
    (should (equal (helm-dash-sub-docset-name-in-pattern pattern docset) pattern))))

(ert-deftest helm-dash-test/sub-docset-name-in-pattern-with-special-docset-name ()
  ""
  (let ((pattern "C++ printf")
	(docset "C++"))
    (should (equal (helm-dash-sub-docset-name-in-pattern pattern docset) "printf"))))
