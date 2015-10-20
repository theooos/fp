Functional Programming: test benches
====================================

Version history
---------------

20 Oct 21:10
	Added the `errata <Exercise2/errata.rst>`_; please check them out!
20 Oct 20:40
	You are now guaranteed to get only directories as input to ``find``. 
20 Oct 19:55
	Fixed ``testEntries`` and the test for ``lsTree``. 

	.. Our revisions: 00e46093c25824c78e7a97e2274337e82f10b0e5 and fe853c7753fa12c7cca55df7173214eac9deecfb

20 Oct 16:50
    Added info on contact; removed sentence on the test for ``cp`` as it worked already.
20 Oct 14:00
    Created repository with test bench for lab2.


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

From time to time, we may release new test benches to fix problems. You can update to a newer test bench simply by running ``git pull`` inside ``fp-testbenches``. We will notify you when newer test benches are available.

----

You can also use Git from your laptop or home machine if you install it. As usual, you must ``module load ghc/7.6.3`` whenever you use ``ghc``. 

In case of problems
-------------------

Although we prepare the test bench with care, it is always possible that there is a bug. As usual, if you suspect there might be a problem, please report it on Facebook.
