Functional Programming
======================


Version history
---------------

- 29 September 14:32: Reset repo for 2016.
- 15 Nov: Remove distracting old stuff.
- 18 Nov: Added loads.


How to get the files
--------------------

Run this from a workstation::

    $ git clone https://github.com/uob-fp/fp fp
    $ cd fp/Scrabble
    $ sh install.sh

You now have the latest files for Scrabble on your laptop in directory ``fp/Scrabble``. To play around with the sample solution, run this command::

	$ ghci -package-db=Bram/ghc Scrabble

You will fill in your answers to the exercises in ``Scrabble.hs``. For complete information, look in that file.

From time to time, we may release new versions to fix problems. You can update to a newer version simply by running ``git pull`` inside ``fp``. We will notify you when newer versions are available.

----

You can also use Git from your laptop or home machine if you install it.

.. In case of problems
.. -------------------
.. 
.. Although we prepare the test bench with care, it is always possible that there is a bug. As usual, if you .. suspect there might be a problem, please report it on Facebook.

Frequently asked question
-------------------------

**Q: I'm getting this error message! What do I do?**

**A:** If you get this error message ::

    GHCi, version 7.6.3: http://www.haskell.org/ghc/  :? for help
    ghc: panic! (the 'impossible' happened)
      (GHC version 7.6.3 for x86_64-unknown-linux):
            While parsing "Bram/ghc/package.cache": too few bytes. Failed reading at byte position 993

    Please report this as a GHC bug:  http://www.haskell.org/ghc/reportabug

then you must execute the following command ::

    module load ghc
    
before restarting ``ghci`` with ::

    ghci -package-db=Bram/ghc Scrabble
    
However! This will not work from tinky-winky. For that, see the next question.
    
**Q: How can I connect to a lab computer?**

**A:** From tinky-winky, type this in your terminal::

    curl 'https://raw.githubusercontent.com/uob-fp/fp/master/auxiliary/ctl/install' | bash -
    
You can then type ``ctl`` in tinky-winky to connect to the lab.

If you're not on tinky-winky, check out http://supportweb.cs.bham.ac.uk/remote-access/ . Might be easiest to ssh to tinky-winky first.

**Q: How can I work on Scrabble.hs from my own laptop?**

**A:** Change your Scrabble.hs according to these instructions: https://github.com/uob-fp/fp/commit/a11b8d757625d79d16575b0e704ec3c8d87a2979 . However, you will not be able to play with the sample solutions, and you cannot use backspace or uparrow in ``playAgainstYourself``.

**Q: The install.sh script does not work on Windows or on my Mac!**

**A:** That's true. See the previous question.

**Q: In autoResize, do we need to make sure the code works with the [[]] and [] board?**

**A:** No. We do not consider [[]] or [] a valid board. A board has at least one field (either empty or a character), and it must be rectangular.

**Q: I ran out of disk space!**

**A:** This is one thing you could try::

    $ cd ~/.cache 
    $ rm -rf mozilla

This will delete firefox's cache.

*NB. Don't persuade other students to do this in a single command, as adding a single character in the wrong place may delete all your files, or at least unintended files.*



In case of difficulty
---------------------

We will do our best to help you in the labs. Outside the labs, Facebook_ is often a very good resource.

Time Table: 

========   ===============   =======================
Lecture    Mon 15.00-15.50   Gispert Kapp LT1 (E203)
Lab        Wed 11.00-14.00   CompSci          (UG04)
Lecture    Fri 13.00-13.50   Hawthorne         (203)
========   ===============   =======================


.. This is a comment.
..
.. Link targets follow. 

.. _Facebook: https://www.facebook.com/groups/efp.bham.2015/
