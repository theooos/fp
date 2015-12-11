Functional Programming: test benches
====================================

**Known issue**: `"error 1" <https://github.com/uob-fp/fp/issues/1>`_. We are investigating.

Version history
---------------

- 10 Dec 13:05: Published Test 8 and improved tests 5 and 6 (Test 7 stays the same).

- 4 Dec 1:05: Published Test 6.

- 3 Dec 13:20: You are now allowed to write either ``fromMove`` or ``fromMove'``. 

- 2 Dec 13:20: Fixed bug in test #1; we accidentally checked the order of positions in the list

- 2 Dec 11:00: Created repository with test bench for lab4. The test bench is not finished we're still working on Test 6 and Test 8. Also the marking is not decided yet.


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

In case of difficulty
---------------------

We will do our best to help you in the labs. Outside the labs, Facebook_ is often a very good resource. Please note that Bertie gives a tutorial every Monday 10:30 - 12:00.

Rooms: 

==============   ================
Week 4, 8-11     MechEng G28
Week 5           Strathcona LT4
Weeks 6-7        MechEng G36
==============   ================


.. This is a comment.
..
.. Link targets follow. 

.. _Facebook: https://www.facebook.com/groups/511767035624467/
