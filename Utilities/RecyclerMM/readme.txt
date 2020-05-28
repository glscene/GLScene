RecyclerMM Public Release 2 - 26/07/04 - Eric Grange
egrange@glscene.org

RecyclerMM is an alternative memory manager for Delphi,
to use it, add "RecyclerMM" to the uses of your project 
as *first* unit.

The purpose of RecyclerMM is to provide:

- Improved performance (mileage may vary from a few percents
  to several orders of magnitude, depending on application)
- highly-aligned memory blocks, suitable for SSE operation
- high resilience to memory fragmentation
- fast 'Allocated' testing (see below)
- shared memory (like ShareMem, but no special DLL required)
- no memory management data hidden before or after 
  the allocated blocks

The features and behaviours of the memory manager can be tweaked 
via compile-time options in RecyclerMM.pas (see there for details).

Besides benchmark situations, improvements where noticed
in the following "real-world" test applications:
- XML DOM parser : about 30 to 1000% (higher figures 
  on larger XML files)
- OLAP Cube compiler : several orders of magnitude faster
  on large datasets (default manager would fragment heavily
  and use hundreds of MB)
- Entity Flow Simulator : about 20% (coming from increased
  cache coherency, all allocations happen once, before the
  simulation is run)
- FTP server : constant memory use under heavy multithreading
  (default manager would fragment over time, use more and more 
  memory, enter swapping hell, etc.)

A webpage on these applications will be setup once 
authorization from the parties involved is received
and time is found... especially time, volunteers welcome :)

What hasn't been tested/optimized in this version:
- out of order DLL loading/unloading
- multi-CPU performance
- platforms other than Win98/NT/XP/Win2k/Win2k3

Only downside found so far is an extra memory consumption 
(when default memory manager wouldn't fragment at all),
along with a one-time hit at startup (about 1-2 MB, depending
on options), so reports of cases where performance goes
down would be welcome.

Using FRMMUsageSnapShot:

This form can provide a view of the state of the RMM (a "SnapShot"),
to use it, instantiate it or use the autocreate function, invoke
"Show", then "Display" each time you want it refreshed.
The Display function takes a structure in parameter, which is obtained
from the RecyclerMM unit via the "RMMUsageSnapShot" function.

Using "Allocated":

RecyclerMM exposes an 'Allocated' function which takes a pointer
or object, and that will return True if the memory is allocated,
False if it isn't. This test happens at high speed and does not
rely on information hidden before/after the block, and thus will
be quite robust to wild pointers or buffer overruns/underruns.

---

Any reports of use are welcome, the more applications report
improvement (or lack of), the more RecyclerMM can be improved!

Eric Grange