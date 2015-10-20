Functional Programming: test benches
====================================

Version history
---------------

20 Oct 12:37
    Created test repository


How to get the test benches
---------------------------

Run this from a workstation::

    git clone https://github.com/uob-fp/fp fp-testbenches

You now have the latest test bench on your laptop in directory ``fp-testbenches``. This directory has a subdirectory ``Exercise2``. Move (or copy) your ``Exercise.hs`` into this directory, and run ::

    make
    
to test your submission. This will

1. Check if the types of the exercises are as they were;
2. If the types are right, it will run your functions with our inputs, and check if you get the expected outputs.

Then the test bench will give you an estimate of your mark for this submission.

From time to time, we may release new test benches. Specifically, we do not yet test all edge cases of ``cp`` as documented in the errata. **However, you should still follow the errata.** We will update the tests for ``cp`` soon so the test bench will give you a better indication of your mark. We will notify you when this newer test bench is ready.

----

You can also use Git from your laptop or home machine if you install it. As usual, you must ``module load ghc/7.6.3`` whenever you use ``ghc``. 
