class MySuite extends munit.FunSuite {
  test("hello") {
    val obtained = 43
    val expected = 45
    assertEquals(obtained, expected)
  }
}
/*
+:MySuite:
==> X MySuite.hello  0.058s munit.ComparisonFailException: C:\Users\zenAndroid\Documents\gitRepos\hello-world\src\test\scala\Testtest.scala:5
4:    val expected = 45
5:    assertEquals(obtained, expected)
6:  }
values are not the same
=> Obtained
43
=> Diff (- obtained, + expected)
-43
+45
    at munit.FunSuite.assertEquals(FunSuite.scala:11)
    at MySuite.$init$$$anonfun$1(Testtest.scala:5)
Execution took 58ms
1 tests, 1 failed

*/