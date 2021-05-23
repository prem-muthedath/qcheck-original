-- | sample test results.
-- author Prem Muthedath.
*** QuickCheck pass test ***
+++ OK: passed 100 tests.

*** QuickCheck fail test ***
*** Failed! Falsifiable after 1 tests:
    []
    [-18,12,-27,-23,-13,-26,5,-23,-4,2,-14,-29,24,21]

*** QuickCheck evaluate test ***
+++ OK: passed 100 tests.

*** QuickCheck collect test ***
+++ OK: passed 100 tests.
3% 0
5% 1
2% 10
4% 11
3% 12
1% 13
4% 14
1% 15
3% 16
2% 18
4% 19
5% 2
2% 20
2% 21
4% 22
2% 23
5% 24
3% 25
3% 26
5% 27
7% 28
1% 29
7% 3
3% 30
3% 4
1% 5
2% 6
3% 7
5% 8
5% 9

*** QuickCheck classify test ***
+++ OK: passed 100 tests.
1% are empty
29% have < 10 elements
70% have >= 10 elements
71% have duplicate elements
29% have unique elements

*** QuickCheck implication (==>) test ***
+++ OK: passed 97 tests.
100% passed are >= -25

*** QuickCheck gave up! (==>) test ***
*** Gave up! Passed only 8 tests.

*** QuickCheck forAll test ***
+++ OK: passed 100 tests.
5% empty
70% have > 10 elements
73% have duplicates
100% obeying forAll condition `sorted`

*** QuickCheck frequency test ***
+++ OK: passed 100 tests.
18% empty
82% have >= 1 element

*** QuickCheck orderedList test ***
+++ OK: passed 100 tests.
2% empty
61% have > 10 elements
66% have duplicates
100% ordered

*** QuickCheck vector test ***
+++ OK: passed 100 tests.
100% have size = 10

*** QuickCheck listOf1 test ***
+++ OK: passed 100 tests.
3% 1
1% 10
2% 11
4% 12
2% 13
3% 14
2% 15
6% 16
2% 17
4% 18
4% 19
6% 20
6% 21
3% 22
3% 23
3% 24
3% 25
4% 26
2% 27
7% 28
4% 29
4% 3
3% 30
2% 4
8% 5
2% 6
3% 7
1% 8
3% 9
100% have >= 1 element

*** QuickCheck tree test ***
+++ OK: passed 100 tests.
100% are valid trees
21% have depth 1
4% have depth 2
75% have depth > 3

*** QuickCheck elements test ***
+++ OK: passed 100 tests.
100% in range [1, 10]

*** QuickCheck oneof test ***
+++ OK: passed 100 tests.
35% are [1, 2, 5]
34% are [11, 15, 4, 0]
31% are empty

*** QuickCheck sized test ***
+++ OK: passed 100 tests.
63% in (0, 30]
36% in [-30, 0)
1% zero

*** QuickCheck choose test ***
+++ OK: passed 100 tests.
100% in range [0, 15]
