EZDSL v3.1
==========

Easy classical data structures for pretty much any Delphi

Copyright (c) 1993-2015 Julian M. Bucknall


Introduction
----------------------------------------------------------------------
The EZDSL units provide an OOP interface for classical data structures
for Delphi: stacks, queues, priority queues, lists, binary trees, hash
tables and so forth.

My objective in writing these units was to provide myself with a set
of well-encapsulated classes that would manage the tedium of using
these types of data structures, leaving me to concentrate on coding my
latest application. Furthermore, since I was able to concentrate on
the structures one at a time, without the hazard of an application
awaiting my endeavors, I could research and use the latest and best
algorithms.

This library was rewritten from my EZSTRUCS library that I first
released in August 1994. EZSTRUCS was written for Borland Pascal 7 and
hence it was not a simple port to Delphi. I was determined to use some
of the whiz new syntax of Delphi: exceptions, virtual constructors,
properties and so on. The first version of EZDSL appeared in 1995,
version 2 was ported to 32 bits for the Delphi 2.0 compiler (of course
making sure that it could still be compiled with Delphi 1.0 in 16
bits) and now, support for later Delphis and packages.

Within this document I shall not spend too much time describing how
the data structures work and what they're for; any data structures
book will be able to provide that kind of information easily, and in
more depth than I could hope to do. The three books I have made a
great deal of use of (and that I recommend heartily) are

- _Algorithms_, by Robert Sedgewick; Addison-Wesley
- _Data Structures, Algorithms and Performance_, by Derek Wood;
    Addison-Wesley
- _Practical Data Structures in C++_, by Bryan Flamig; Wiley

Also, the four volumes of _The Art of Computer Programming_ by Donald
Knuth belong on every serious programmer's shelf.

Also, a small word of warning. Using these units require that you have
a fairly good grounding in using pointers, objects and classes in
Delphi, including typecasting. If you need to, please review your
Delphi documentation on how to use them.




DANGER, WILL ROBINSON!!!
----------------------------------------------------------------------
I no longer program in Delphi. Let me repeat that: I no longer program
in Delphi. I've forgotten more than I knew, and I haven't kept up with
the new stuff.

In the old days, I used to religiously install every version of Delphi
whenever I had a new PC, or repaved an existing one. No more, I'm
afraid. I could create a set of VMs and install a version of Delphi in
each, but... I have better things to do with my life. So I've got XE6
installed in a VM and perhaps in the future I'll have a later version.



What's new in version 3?
----------------------------------------------------------------------
To all the previous users of EZDSL, welcome to version 3.

What is new about this version 3? Without further ado:

- This documentation is now a Markdown file. Convert it to HTML and read 
  in your browser, or use a Markdown viewer.

- Support for pretty much any Delphi, including EZDSL packages for
  each (Delphi XE+ support is NEW FOR 3.04 and later).

- A hash table class, using linear probing for collision detection,
  automatic table expansion/shrinking, support for different hash
  functions;

- NEW FOR 3.01 A boolean array class (also known as a bitset, a bit
  array or a bitmap); enabling several million booleans in an array
  like structure to be set true, false, toggled, ANDed, ORed, XORed
  with other boolean arrays, navigation though all true or false
  booleans, backwards and forwards.

- Simplification of the use of Compare with sorted containers, now you
  can alter Compare with a non-empty container and the container will
  sort its data objects according to the new Compare function.

- Separate thread-safe containers for 32-bit programming, thread-safe
  node allocation.

- The pseudo random number generator has been revamped and documented.

- There's been a lot of code clean-up behind the scenes as well. For
  example, I've changed my coding style and I've used different naming
  conventions for the protected/private fields inside the containers.

- Also, a warning is due to old-time users of the library: the 
  TEZCollection has been dropped. After all it was originally written 
  to help BP7 programmers migrate to Delphi 1 and we're now at 
  Delphi XE8.




Container classes and Data objects
----------------------------------------------------------------------
The EZDSLxxx units define a set of data object containers. These are
containers into which you throw a bunch of data 'things' and retrieve
them at a later stage. The data 'things' can be anything:  integers,
strings, records, object instances, or whatever; we'll just use the
generic term data object. The different kinds of containers each have
different characteristics, some will keep the data objects sorted in
some order, some will give you fast serial access times, some will
just give you them back in the same order you put them in.  Some will
allow you to navigate through the structure (and will provide a
classic iterator method), some do not.

The containers can be separated into two types: those that own their
data objects, and those that do not. Being a data owner means that a
container will destroy its data objects when it is emptied or itself
destroyed. This distinction means that you could create two skip lists
for example, one a data owner and the other not, each with a different
sort sequence. You could then insert the same set of data objects into
both and know that, when you call the Empty method for both lists, the
data objects will get destroyed only once.

As you look at the code for the containers and at this documentation,
you'll see that the hierarchy tree is fairly flat (or narrow if you
look at it vertically): 

    TAbstractContainer                 (EZDSLBSE.PAS)
      +--TStack                        (EZDSLSTK.PAS)
      +--TQueue                        (EZDSLQUE.PAS)
      |    +--TDeque                   (EZDSLQUE.PAS)
      +--TPriorityQueue                (EZDSLPQU.PAS)
      +--TLinkList                     (EZDSLLST.PAS) - single linked list
      +--TDList                        (EZDSLDBL.PAS) - doubly linked list
      +--TSkipList                     (EZDSLSKL.PAS)
      +--TBinTree                      (EZDSLBTR.PAS)
      |    +--TBinSearchTree           (EZDSLBTR.PAS)
      |         +--TrbSearchTree       (EZDSLBTR.PAS)
      +--THashTable                    (EZDSLHSH.PAS)
    TBooleanArray                      (EZDSLBAR.PAS)

There is a single ancestor class that provides the important common
methods and fields (allocating/freeing a node, whether a container is
empty) and the base of the virtual methods hierarchy (Compare,
DisposeData, et al). Most of the other classes are descended directly
from this, with only a few further descendants. This is contrary to
lots of other container hierarchies where the author has endeavored to
arrange things so that everything descends from one another all the
way back to a doubly linked list or something. I felt that this type
of scheme was too restricting: if I found a better implementation of a
list I don't have to worry too much about breaking up my hierarchy (so
long as I interface the same methods). The other thing is that
deriving a stack from a linked list for example means that not only do
you get the `Push` and `Pop` methods but you also get a whole slew of
linked list methods appearing as well (`Prev`, `Next`, `Insert`, `Delete` or
whatever).

However I have tried to be consistent in my naming convention so that
for example, there is a method called `Next` for a `TLinkList`, `TDList` and
`TSkipList` but it works in different ways for each (and has a different
calling interface) - the `Next` method is not virtual.

There is one container that stands out from the crowd and from much of
the above: the boolean array. This data structure just provides an
array of boolean values, true or false, and no data objects are stored
inside. Remember that all of the information in this help file
relating to data objects (including ownership, disposal, duplication,
and so on) do not apply to the boolean array. It is a container truly
"on its own," since it doesn't descend from `TAbstractContainer` either.

The EZDSL library has been written assuming that the data objects are
pointers to something. This scheme makes it easy to have the
containers have lots of different kinds of objects and to be able to
deal with them properly.  However, there is nothing to stop you using
pointers to strings or even plain longints instead; just typecast to a
pointer when required (and typecast back again when the object is
retrieved from the container!). For example when using a stack you
could use integers as follows:

    var
      MyStack : TStack;
    begin
      MyStack := TStack.Create(False);
      try
        with MyStack do begin
          Push(pointer(19));
          Push(pointer(57));
          writeln(longint(Pop)); {outputs: 57}
          writeln(longint(Pop)); {outputs: 19}
        end;
      finally
        MyStack.Free;
      end;
    end;

Or you could write a container that would wrap up this slight
awkwardness and hide it from you. See the example programs EXINTQUE
and EXSTRSTK.



Data Objects and Polymorphism
----------------------------------------------------------------------
The first version of the EZSTRUCS unit (on which this EZDSL library
was originally based) had virtual methods called `Compare`, `DisposeData`
and `DupData`. However, I found that often the ONLY methods I was
overriding were... `Compare`, `DisposeData` and `DupData`! The data
container worked fine 'out of the box', it was such a pain to override
yet another class just to alter one of these methods.

I had two solutions to my perceived problem: make the data objects
derive from an ancestor object class or make the methods function
pointers. The first was out (I find it awkward), but the second was
just what I wanted. In fact Delphi itself is rife with this kind of
delegation model.

So, the data object related methods in the containers are in fact
procedure and function pointers held as properties in the container.
You set them when you initialize a new container, and they will be
used whenever two data objects need to be compared, or a data object
needs to be disposed of or you need to duplicate a data object.

To set them you first create your new container object by calling the
Create constructor. This sets the internal procedure and function
pointers of the object to 'do nothing' abstract routines. Then you set
the actual routines you want to use by modifying the required
properties: `Compare`, `DisposeData` and `DupData`.  EZDSL provides some
popular ones in EZDSLSUP.PAS:  comparison between two longints,
between two strings (short strings in Delphi 2/3), disposing and
duplicating the same. 

Although you can alter the `DisposeData` and `DupData` properties when the
container is not empty, it doesn't make much sense to do so. Altering
the `Compare` property has an interesting side effect: if the container
maintains its data objects in a sorted order, changing the `Compare`
method will cause the data objects to be resorted in the container.

The `Compare`, `DisposeData` and `DupData` routines that you write *must* be
global, declared 'far' in Delphi 1.0 and declared using the normal
Pascal fastcall calling convention (viz 'register') in Delphi 2/3. In
other words 

(a) they cannot be nested routines;
(b) in Delphi 1.0 they cannot be declared implicitly or 
    explicitly as near routines; and
(c) in 32-bit Delphi they cannot be declared 'cdecl' or 'stdcall'.

They must also follow the routine prototypes `TCompareFunc`,
`TDisposeDataProc` and `TDupDataFunc` respectively.

To facilitate cloning containers with the most flexibility, you can
clone a container and use a different `Compare` function than the
container you are cloning from (useful for maintaining trees or lists
with their data objects in two different orders).



Sorted Containers, Compare and Duplicate Data Objects
----------------------------------------------------------------------
A number of the containers in EZDSL maintain their data objects in
some kind of ordered sequence. The sequence that they are maintained
in is generally defined by the container's `Compare` property. Some
containers are automatically and always 'sorted': skip lists and
binary search trees are two examples. Some are never sorted: stacks
and queues for example. Some containers can be used sorted or
unsorted: examples are linked lists or double linked lists. One
container is kept in an ordered sequence, yet that sequence has
nothing to do with `Compare`: the hash table. The boolean array doesn't
support storing data objects at all, so this topic doesn't even apply.

The `Compare` property is a function that you write (or is one of the
supplied functions). The routine must compare two data objects and
return a negative number (e.g. -1) if the first data object is 'less'
than the second, zero if they are equal, and a positive number (e.g.
1) if the first is greater than the second. The `Compare` function is
used only by sorted containers to maintain the internal sorted
sequence, or by unsorted containers that implement a `Search` method. In
the latter case, the `Compare` method is just used as an equal or not-
equal test. Unsorted containers that do not have a `Search` method do
not use the `Compare` property, and so in these cases it does not have
to be set.

The exception to this rule is the hash table. Here data objects are
stored in a 'sequence' that enables them to be retrieved very quickly,
and yet this sequence has nothing to do with `Compare`. In fact, the
`Compare` property is not used with hash tables so it's pointless to set
it.

Once you accept that certain containers maintain data objects in an
ordered sequence, you have to consider the problem of duplicate data
objects. Two data objects are defined as duplicate if the container's
`Compare` function returns zero when called with them as parameters.
Well, EZDSL enforces a strict rule: sorted containers cannot contain
duplicate data objects. All sorted containers will raise an exception
(with string code `edsInsertDup`) if you attempt to insert a data object
that compares equal to an already inserted data object.

You might have decided that this is a somewhat harsh restriction.
Well, yes and no. Some containers just cannot be used with duplicate
data objects (the skip list is the main example; if you try to, some
very weird effects will occur).  Some containers will internally
reorganize themselves, and will destroy any arrival sequence that
duplicate data objects could have been inserted with (the red-black
binary search tree is the main example here). Rather than document
that some containers will accept duplicate items and some won't and if
they did what kind of weird effects could happen on deletion and
insertion, I decided to restrict all sorted containers in the same
manner: they will not accept duplicate data objects.

What to do? Suppose you had the kind of use for a skip list that
absolutely had to accept duplicate data objects? Firstly I would ask
you to redesign your data object so that duplicates could not occur
and secondly recode your `Compare` function so that maybe another field
of the data object could be used in the comparison to remove the
ambiguity.

If all fails, one solution could be to declare a sequence long integer
in your container descendant class. Set this to zero in your
constructor. Make sure that your data objects have a sequence field,
and that your `Compare` function checks this after your main comparison.
Override the `Insert` method of the container to increment the
container's sequence field and set the data object's sequence field to
it, and then call the inherited insert method.  This algorithm is
shown in the example program EXINSDUP.

As always with rules, there is one exception to the 'no duplicates'
rule:  the priority queue. This container will accept duplicate data
objects.  However duplicate data objects will *not* be popped off in
arrival sequence or, reverse arrival sequence, or any other
deterministic sequence; as far as the priority queue is concerned
duplicate data objects are exactly that, it cannot distinguish between
them in any way. Again if this matters to you, you'll have to redefine
your data object and `Compare` function to remove the anomaly.

A new feature of version 3 of EZDSL is the ability of containers to
sort themselves when you supply a new `Compare` function. If the
container is sorted and you set the `Compare` property to a new compare
function, then the container will reorder its data objects according
to that new function. It does this in the most efficient manner
possible by reusing the internal nodes and minimizing the amount of
extra heap space for the job. 



Nodes and Node Stores
----------------------------------------------------------------------
We all know that these classical data structures are generally
implemented with nodes. For a doubly linked list a node is usually
represented by a record structure of the form: 

    PMyNode = ^TMyNode;
    TMyNode = record
      Next : PMyNode;     {Link to next node in the chain}
      Prev : PMyNode;     {Link to previous node in the chain}
      Data : SomeRecord;
    end;

When writing the unit I wanted to get away from the dependence of
knowing about this typical node structure: I wanted to insert, append,
push, examine, delete, erase or pop data objects without having to be
continually reminded of the underlying algorithm. Also, I didn't want
to have to descend my data objects from a `TNode` object - I would be
forced to always use `TNode` descendants and couldn't use `PString`s or
something along those lines. Also, by divorcing myself from knowing
about (worrying about) the node structure, I was able to make numerous
economies in data storage and speed.

However, some of these data structures cry out for being able to
navigate through the structure. The navigation is performed by the
container class' methods. Sometimes where you are in the structure is
stored internally in the container object (an internal cursor),
sometimes you have explicit 'cursors' to help you move around the
container (external cursors). All containers have methods to move
these internal and external cursors around the object. Of course
stacks and queues and similar structures do not, you can only
reference the topmost object (they are inherently non-navigable).

Also because, the node structure is hidden I was able to implement a
node suballocator scheme (called a `TNodeStore`; see the source) to help
me and the containers manage blocks of nodes, rather than just one at
a time.  Because the nodes are generally all the same size for a given
container type, we can take advantage of this and speed up allocations
and deallocations of nodes, compared with using the heap. So if you
look at the `TNodeStore` code you'll see things like node reuse, several
containers using the same node store and so on.

In 32-bit land, the `TNodeStore` class is fully thread-safe. Hence, just
like the normal Delphi heap, you can use several different containers
in different threads and know that their node allocation method is
perfectly safe, despite the fact that they all use the same node
store.



Iterators
----------------------------------------------------------------------
For the navigable containers (i.e. those containers where a cursor is
defined) an iterator method is defined. This method is called `Iterate`
and can be used to perform "first that" type processing (find the
first data object that meets a criterion) or "for all" type processing
(for all data objects in the container perform this action).

Each iterator takes three parameters: an `Action` routine, a direction
flag and a `ExtraData` pointer. The `ExtraData` pointer is simply to
enable you to pass any kind of record structure to the `Action` routine:
it negates the need for global variables or for a nested `Action`
routine (c.f.  Borland's `TCollection` in BP7). The direction flag
(called `Backwards`) enables you to iterate through the container in
either direction: forwards or backwards.

The `Action` routine for a call to `Iterate` has to be of the form:

     TIteratorProc = function (C : TAbstractContainer;
                               aData : pointer;
                               ExtraData : pointer) : boolean;

where `C` will be the container itself, `aData` is the current data object
and `ExtraData` is the same extra data pointer you passed to `Iterate`.
The function must return `True` to cause the `Iterate` routine to continue
iterating or `False` to cause the `Iterate` routine to stop (and return
the data object that caused the `Action` routine to return `False`).

Remember that your `Action` procedure cannot be a nested routine (in
Delphi 1.0 it must be declared 'far'; in 32-bit Delphi it must be
declared as 'register' (i.e. the normal fastcall declaration)). A
quick example: suppose your data objects had an integer field called
`Value` and you wanted to find the sum of all the `Value` fields in a
list. Your `Action` procedure would look like:

     function SumValues(C : TAbstractContainer;
                        aData : pointer;
                        ExtraData : pointer) : boolean; far;
     var
       {ExtraData is a pointer to a longint: so typecast it}
       Sum :  ^longint absolute ExtraData;
     begin
       inc(Sum^, PMyObject(aData).Value);
       Result := true;
     end;

and you would call `Iterate` to do the summation as follows:

     var
       TotalValue : longint;
       MyList : TLinkList;
     begin
       {...}
       TotalValue := 0; {clear the total}
       MyList.Iterate(SumValues, false, @TotalValue);
       {...}
     end.

Notice that the `SumValues` procedure is explicitly declared 'far' in
Delphi 1.0 (in 32-bit Delphi this modifier is ignored). I have found
this method to be by far (!) the easiest method of ensuring that the
Delphi 1.0 compiler will compile my source code without relying on the
current value of the $F compiler define.

There is one important point that you must be aware of when you are
using the `Iterate` method. It is simply put: do not add or delete data
objects from the container in your `Action` routine. The `Iterate` method
maintains some state information whilst you are navigating through the
container. If you add or delete an object, this state information
becomes invalid since the container may restructure itself internally
to accommodate that change. The state information that `Iterate` depends
upon will become out-of-date and may result in you seeing the same
data objects twice, not at all, or even worse, cause access violations
and so on. The proper thing to do is to make a note elsewhere of the
data objects you want to add or remove from the object (use another
container, e.g. a stack or queue), and once the `Iterate` method has
completed to perform the additions and removals.

The boolean array `TBooleanArray` has its own form of the iterator,
designed for iterating though either the true values in the array, or
the false ones, and in either direction. In this case, the `Action`
routine for a call to `Iterate` has to be of the form:

     TBooleanArrayIterator = function(C : TBooleanArray;
                                      aIndex : longint;
                                      ExtraData : pointer) : boolean;

where `C` will be the boolean array itself, `aIndex` is the element number
of the current boolean, and `ExtraData` is the same extra data pointer
you passed to `Iterate`. The function must return `True` to cause the
Iterate routine to continue iterating or `False` to cause the `Iterate`
routine to stop (and return the index of the boolean value that caused
the `Action` routine to return `False`).  Remember that your `Action`
procedure cannot be a nested routine (in Delphi 1.0 it must be
declared 'far'; in 32-bit Delphi it must be declared as 'register'
(i.e. the normal fastcall declaration)).



Packages
----------------------------------------------------------------------
From the release of version 3.02, I included an EZDSL run-time
package for each of Delphi 3, 4, 5, and 6, or later.

The naming convention I am using is EZDSLnxc where n is
the major version number of EZDSL, x is the minor version number
expressed as a alphabetic character (.00=a, .01=b, etc), and c is the
Delphi compiler major version number. Hence EZDSL3a3.DPL is EZDSL
version 3.00 for Delphi 3. The Delphi 4 version of the package would
be EZDSL3a4.DPL.

The package name for Delphi XE or later is EZDSL3XE.BPL.

I hereby reserve the rights to names of the form EZDSLnxc.DPL. If you
alter EZDSL and create a run-time package from your altered code,
please do not call it the same name as the official one. If you do so,
you may cause compatibility problems on someone else's machine if they
have an application already using the official EZDSL package.

The package is a run-time package, in other words it does not install
directly into the Delphi IDE. In fact, it would only be used if you
were creating a program that used run-time packages. If you do so,
it's best to copy the package to a relevant directory on your path. 

Please refer to your Delphi documentation for more information on
packages.



Thread-safe programming in 32-bit
----------------------------------------------------------------------
32-bit Windows programming introduces a new concept (at least for
Windows programmers): multithreaded programming. You can define
sections of code that will run, for all intents and purposes, at the
same time as each other. The operating system will switch from thread
to thread in a round-robin fashion invisibly and out of your control
to give the illusion of many things happening at the same time. On
machines with multiple CPUs, each section of code (or thread) will run
on a different CPU, providing there's enough to go round, and the
threads will be executing at the same time.

This type of programming produces its own problems at the same time as
touting its benefits. The main one is called a race condition: two
different threads use and change the same variable at the same time.
Without some kind of synchronization control, the two threads will
trash the variable. For a classic example, let's assume that our two
threads are manipulating a single linked list (one of EZDSL's
`TLinkList` objects). Suppose that they both want to insert a new data
object into the linked list at the same time. The first thread gets
control, positions the linked list cursor and then calls `InsertBefore`.
Half way through the execution of the `InsertBefore` method, the second
thread gains control, and tries to position the cursor and call
`InsertBefore` itself. The problem is that `InsertBefore` breaks the
list's chain of pointers internally in order to insert the new data
object. If the thread doing the job loses control to another, the
linked list is in a precarious state: half broken. It is extremely
likely that the second thread doing its work will trash the linked
list permanently.

I won't go into pros and cons and synchronization of multithreaded
programs any further here, there are several books on the subject.

So what should we do? The simple answer is that we must synchronize
access to the container: a thread must be able to block all other
threads until it has finished doing its work, at which point it
relinquishes access so that another thread can do its stuff. When I
was designing the thread-safe support, I had the choice of several
paths to take:

     Add access locking and unlocking to every container method; 

     Add special routines to acquire and release locked access to the
       container; 

     Create separate thread-safe container classes

Let's discuss them in turn. The first option is the safest: every
method uses the following methodology:

     Acquire exclusive access to the container
     try
       do whatever the method is supposed to do
     finally
       Release exclusive access to the container
     end;

Basically, what happens here is that only one thread can get exclusive
access at one time (all other threads are blocked) and this ensures
that what the method is supposed to do is done safely. The one big
problem with this way that I didn't like is that, if you were not
using multithreaded programming at all, or if you were using different
containers in difference threads, you'd be locking and unlocking the
containers all the time, without reason to. Although the resource
locking code uses Win32's critical sections, and hence is fairly fast,
it still slows down the code unnecessarily for those who don't use it.

Another small variation on this theme is to use a boolean field
(called `BeThreadSafe` or something) to say whether to lock or not. If
the boolean was false, no locking would occur, if true, locking would
be done. I rejected this as well, since the try..finally block would
still have to be there and this consumes cycles unnecessarily. Also
it's too easy to set the boolean to true whilst a method is being
executed. This results is an unlock being performed, even though a
lock has not taken place. Adding code to counteract this is messy and
unnecessary given that there are better solutions.

The second option requires you, the programmer, to make sure that you
acquire access to the container before using it, and to release it
afterwards. Basically your code looks like this:

     MyStack.AcquireAccess;
     try
       ..do something with MyStack..
     finally
       MyStack.ReleaseAccess;
     end

The reason I rejected this option is that it's too easy to forget. For
example, moving the cursor in a single linked list will rearrange the
links in the list: you must protect the linked list before calling
`Next` or `Prev`. Forgetting this kind of gotcha is too easy. Another one
is `Iterate`: you must protect the container before navigating through
it, otherwise you'd have a complete disaster on your hands if thread 1
was navigating at the same time that thread 2 was adding data objects.

The option I finally went for was to have special thread-safe
containers. You use them like this:

     var
       MyThreadSafeStack : TThreadsafeStack;
       MyStack : TStack;
     begin
       MyThreadSafeStack := TThreadsafeStack.Create(false);
       ...
       {get the embedded stack}
       MyStack := MyThreadSafeStack.AcquireAccess;
       try
         ..use MyStack methods..
       finally
         MyThreadSafeStack.ReleaseAccess;
       end;
       {you no longer have access to the embedded stack}

Here the code is explicit: you acquire access to the stack and in
return you get a stack variable to use. Once you release access you no
longer have access to that variable. To get a stack variable to use,
you must call the `AcquireAccess` method of the special protected
object. You can't forget.

So this means you have the best of both worlds. Unprotected, fast
container classes for normal use, or for unshared containers in a
multithreaded program; fully protected classes for shared containers
over multiple threads.



Debugging and Errors and Exceptions
----------------------------------------------------------------------
When I started writing this library I had several goals, but there
were two which seemed incompatible: it had to be fast and it had to
have lots of checks built in to trap any errors that might occur.

This second goal is further complicated because there are broadly two
types of error that could occur: an error due to a programming mistake
and errors due to some run-time problem. This is necessarily a wishy-
washy definition, but generally the run-time problems would be things
like running out of memory whilst adding a data object, and the
programming mistakes would be things like trying to pop a data object
from an empty stack. Another way to view this would be to define
programming mistakes as being those things which would apply to every
machine, whereas the run-time problems would vary from user to user
and from machine to machine. Normal testing should identify
programming mistakes, whereas the other type of error are exceptions
to the norm.

I thought about how I might trap programming mistakes and decided to
use assertion checks as in the C language. An assertion is a simple
debugging routine that checks whether something is true.  If this is
the case the program continues. If it is false, the program is
terminated immediately by writing out a string explaining why (we
shall however be raising an exception).  In C this is accomplished by
the `Assert` macro.  Compile your C program in one way during testing
and the `Assert` macro expands to this kind of check; compile it in
another way (for your production app.) and the `Assert` macro 'expands'
to a null statement.  As you see, during testing you have the full set
of checks to aid in debugging (slow but safe), and once you have fully
tested the application you can turn them all off to obtain the full
speed. 

Unfortunately in Delphi these kind of preprocessor macros are not
available, at least not until Delphi 3. This release of the Delphi
compiler came with a proper `Assert` routine: compile it one way with a
compiler define set and assertion checks are performed, compile it
another way and the check 'disappears'. Unfortunately, with my goal of
supporting all the Delphis, I could not use this nifty language
feature. Instead, I worked round the problem by having a compiler
define called DEBUG which firstly automatically activates the $D+ and
$L+ compiler options, and secondly causes a lot of methods to have a
call to `Assert` at the start of the routine to check that various entry
conditions are met.

An example should make this clear. The `TStack` object has a method
called `Pop` to remove the topmost data object from the stack. If the
stack is empty, I count calling `Pop` as a programming mistake: you
really should check for the stack being empty in your program prior to
calling `Pop`. Of course `Pop` could have an if statement within it that
did this check for you, but in the *majority* of cases the stack won't
be empty when `Pop` is called and in the *majority* of cases when you
use `Pop`, you'll have some kind of loop in your program which is
continually checking whether the stack is empty or not anyway. In my
mind, having a check for an empty stack within `Pop` is safe but slow.
So, instead, `Pop` has a call to an `Assert` procedure at the start
(activated by the DEBUG compiler define) that checks to see whether
the stack is empty. Here is the code for `Pop`:

     function TStack.Pop : pointer;
     var
       Node : PNode;
     begin
       {$IFDEF DEBUG}
       EZAssert(not IsEmpty, ascEmptyPop);
       {$ENDIF}
       Node := stHead^.Link;
       stHead^.Link := Node^.Link;
       Result := Node^.Data;
       acDisposeNode(Node);
     end;

As you see, if the DEBUG compiler define is set the `EZAssert` procedure
is called, and this checks whether the stack is non-empty first. If
the assertion is true, the `Pop` method continues and executes the code
that pops the data object off the stack. If the stack is empty an
`EEZAssertionError` exception is raised inside the `EZAssert` procedure
(the constant `ascEmptyPop` is a string code for a string in a
stringtable resource).  If DEBUG is not set the code runs at full
speed.

In the method descriptions below, I will show when an assertion check
(activated with the DEBUG define) has been built into the method's
code. You may assume that if the DEBUG define is off and the condition
that the assertion check test for occurs, very bad things will happen
to your program. These could be as 'benign' as a memory leak, or as
dreadful as a memory overwrite or an access violation. It is up to you
to thoroughly test your program with the DEBUG define set, before you
turn it off.

The other type of error (that which occurs infrequently and is due to
some 'at the limit' problem) will cause an `EEZContainerError` exception
to be raised. The strings that the exception class uses are defined in
a stringtable resource (EZDSLCTS.RES, string codes in EZDSLCTS.PAS),
so that you may alter them at will (for example, to translate them
into another language).

You can rest assured that when exceptions are raised, resources will
be conserved via try..finally blocks. If you also have EZSTRUCS you
might find it amazing how easy try..finally blocks make your code
easier to understand compared with the 'old' method involving flags
and checks for errors all over the place.



Compiler Defines
----------------------------------------------------------------------
At the beginning of every EZDSL source code file you'll find the
following lines:

    {$I EZDSLDEF.INC}
    {---Place any compiler options you require here----------}


    {--------------------------------------------------------}
    {$I EZDSLOPT.INC}

The include file EZDSLDEF.INC contains the compiler defines for the
EZDSL library. These defines are described below. The include file
EZDSLOPT.INC contains the _unchangeable_ compiler options for the
EZDSL library; by unchangeable I mean that if you do change one or two
then EZDSL might still work, but on the other hand it might not. You
can change any other compiler options at will, and EZDSL will compile
and run. These changeable options should be placed inside the
indicated place.

The DEBUG compiler define has already been mentioned, but there is
another compiler define from EZDSLDEF.INC to consider. Here is the
full list.

DEBUG defines whether the unit will be compiled with debug information
(if on it automatically sets $D+ and $L+, if off these options are set
off) and with assertion checks activated.  It is on by default. 

SuppressWarnings is a 32-bit Delphi only compiler define. When active
(as it is by default) all warnings that are generated for the code in
EZDSL are suppressed and you won't see them; when inactive the
warnings are shown as normal. I have found the Delphi compiler
warnings to be a mixed blessing: they are great at finding those silly
but simple mistakes we all make (e.g. forgetting to set a function
result) but they can be fooled by moderately complex code. There are a
few places in EZDSL where the compiler generates a warning, but which
can be shown to be false. Maybe you'll have fun turning off the
SuppressWarnings define and working out why the compiler is wrong for
the warnings that are then shown.

In Delphi 3, the EZDSL3a3 package is compiled with the DEBUG define
off. In other words, if you are using the run-time EZDSL package you
will be using code without assertion checks.



Of Compilers and Caveats
----------------------------------------------------------------------
I have tested the EZDSL units with Borland Delphi versions 1, 2, 3, 4,
5, and 6. In each case, I used the version of the compiler with all
patches applied. Later Delphi versions? Not so much (remember: I don't
program much in Delphi any more).

The EZDSL library grew out of both my own personal work and my
work at TurboPower. If you are familiar with TurboPower products the
iterator idea is lifted from Orpheus' sparse array class.  In return the
container classes in TurboPower's SysTools (written by Kim Kokkonen)
take some of the ideas from EZDSL and move them on further. You could
say EZDSL reflects my programming and coding practices, and is hence
somewhat Julian-centric!



Example programs
----------------------------------------------------------------------
To see how to use the EZDSL library, check out the example units and
test programs in the same archive file. Each example program has a
header comment that describes the particular feature that's being
shown or tested.

There are a set of test programs (called something of the form
DTstXxx) that exercise the full set of features for each container
class. They are all console programs, and not only output their
reports to the console (to fast for the eye to read!), but also create
a log file called TEST.LOG that you can read at your leisure.


Programming documentation
=========================


The next section contains all the documented container classes and
their documented methods and fields. For the underlying undocumented
classes, fields, methods and algorithms see the source code (Use the
Source, Luke!).

It might also be beneficial at this point to review the naming
convention for the methods of these containers; it'll also provide a
summary of the important methods to know.

- `Create`	creates a new instance of the container, and prepares it for
        use. You define whether the container is to 'own' its data
        objects or whether it is just holding a reference to them.

- `Destroy`	destroys an object instance, releasing all memory used
        by the container including that held by the remaining data
        objects in the container (providing of course that the
        container owns its objects).

- `Insert`	inserts a data object into the container. For some
        containers there might also be other InsertXxxxx methods that
        insert data objects in certain other defined ways. For stacks
        and queues Insert is known as Push or Append.

- `Delete`	unlinks a data object from a container but does not
        dispose of the memory held by the data object. For stacks and
        queues Delete is known as Pop.

- `Erase`	unlinks a data object from a container and also disposes
        the memory held by the object (if the container owns its data
        objects, that is).

- `Examine`	returns the data object at the 'current position' of
        the container. Current position is defined in different ways
        for different containers: for a stack or queue it is the head
        of the stack or queue for example.

- `Empty`	empties the container by calling Erase for all data
        objects.

- `IsEmpty`	returns true if there are no data objects in the
        container, false if there is at least one.

- `Iterate`	calls its action routine for each data object in the
        container.

- `Clone`	creates an exact duplicate of a data container. All the
        data objects within the container are also duplicated if the
        new container is going to be a data owner, else only the
        pointers to the data objects are copied over. Note that an
        "exact lookalike" copy might not be created, a clone of a
        binary search tree might not look the same, even though all
        the nodes are in sorted InOrder sequence.

- `Join`	adds all the data objects in one container to another (in
        a fashion that makes sense according to the container type).
        The emptied container is disposed of.

- `Split`	splits a container into two, moving all the data objects
        from the split point to a newly created container of the same
        type is the first. In this version of EZDSL, Split has not
        been implemented for binary trees.



Global Types, Constants and Variables
----------------------------------------------------------------------

### Declaration
    const
      ezdsStartOffset = $E2D5;
      escTooManyItems   = ezdsStartOffset+1;
      escInsInvalidHere = ezdsStartOffset+2;
      escDelInvalidHere = ezdsStartOffset+3;
      escInsertDup      = ezdsStartOffset+4;
      escTreeStackError = ezdsStartOffset+5;
      escTreeQueueError = ezdsStartOffset+6;
      escCannotMoveHere = ezdsStartOffset+7;
      escIncompatible   = ezdsStartOffset+8;
      escNoCompare      = ezdsStartOffset+9;
      escNoDupData      = ezdsStartOffset+10;
      escNoDisposeData  = ezdsStartOffset+11;
      escBadSource      = ezdsStartOffset+12;
      escIndexError     = ezdsStartOffset+13;
      escBadCaseSwitch  = ezdsStartOffset+14;
      escKeyNotFound    = ezdsStartOffset+15;
      escTableFull      = ezdsStartOffset+16;
      escTableHasData   = ezdsStartOffset+17;
      escSortNeedsCmp   = ezdsStartOffset+18;
      escCmpNeeded      = ezdsStartOffset+19;

      ascFreeNilNode    = ezdsStartOffset+50;
      ascNewNodeSize0   = ezdsStartOffset+51;
      ascFreeNodeSize0  = ezdsStartOffset+52;
      ascEmptyExamine   = ezdsStartOffset+53;
      ascEmptyPop       = ezdsStartOffset+54;
      ascDeleteEdges    = ezdsStartOffset+55;
      ascExamineEdges   = ezdsStartOffset+56;
      ascInsertEdges    = ezdsStartOffset+57;
      ascReplaceEdges   = ezdsStartOffset+58;
      ascAlreadyAtEnd   = ezdsStartOffset+59;
      ascAlreadyAtStart = ezdsStartOffset+60;
      ascCannotJoinHere = ezdsStartOffset+61;
      ascCannotJoinData = ezdsStartOffset+62;
      ascSplitEdges     = ezdsStartOffset+63;
      ascOutOfRange     = ezdsStartOffset+64;
      ascExamineLeaf    = ezdsStartOffset+65;
      ascBadSkipLevel   = ezdsStartOffset+66;
      ascIsSortedList   = ezdsStartOffset+67;
      ascIsNotSortedList= ezdsStartOffset+68;
      ascHashFuncIsNil  = ezdsStartOffset+69;
#### Description
  Various string constants defining error and assertion conditions.
  The constants are defined in EZDSLCTS.PAS. The strings themselves
  are defined in EZDSLCTS.RC in a stringtable, and compiled into the
  EZDSLCTS.R16 resource file for Delphi 1, and EZDSLCTS.R32 for the
  32-bit Delphis. As Windows only allows one stringtable per
  application `ezdsStartOffset` can be altered to any constant value so
  that the string constants don't clash with your current application.

### Declaration
    const
      skMaxLevels = 16;
#### Description
  The maximum number of levels in a skip list. A skip list node will
  never have more than this number of forward pointers. Defined in
  EZDSLBSE.PAS.

### Declaration
    type
      TChild = (CLeft, CRight);
#### Description
  For binary trees: flags for left and right children. Defined in
  EZDSLBSE.PAS.

### Declaration
    type
      TCompareFunc = function (Data1, Data2 : pointer) : integer 
                                                       of object;
#### Description
  Function prototype for comparing two data objects. The function must
  return a negative number if `Data1` is less than `Data2`, 0 if they are
  equal, and a positive number if `Data1` is greater than `Data2`. The
  routine you write must be a method of a class.  Defined in
  EZDSLBSE.PAS.

### Declaration
    type
      TDisposeDataProc = procedure (aData : pointer) of object;
#### Description
  Procedure prototype for disposing a data object. The routine you
  write must be a method of a class. Defined in EZDSLBSE.PAS.

### Declaration
    type
      TDupDataFunc = function (aData : pointer) : pointer of object;
#### Description
  Function prototype for duplicating data objects. The function must
  create a duplicate to the `aData` data object and return it as the
  function result.  If the duplication fails for some reason, then the
  function must raise an exception. The routine you write must be a
  method of a class. Defined in EZDSLBSE.PAS.

### Declaration
    type
      TIterator = function (C : TAbstractContainer; aData : pointer;
                            ExtraData : pointer) : boolean;
#### Description
  Function prototype for an `Iterate` method iterator. `C` is the
  container whose `Iterate` method was called. `aData` is the current data
  object. `ExtraData` is the extra pointer you passed to `Iterate`. The
  function must return true if `Iterate` is to continue iterating, false
  if `Iterate` is to stop immediately.  Defined in EZDSLBSE.PAS.

### Declaration
    type
      TListCursor = longint;
#### Description
  Navigation cursor for `TDList` and `TSkipList` (double linked & skip
  lists). Defined in EZDSLBSE.PAS.

### Declaration
    type
      TTraversalType = (ttPreOrder, ttInOrder, ttPostOrder, ttLevelOrder);
#### Description
  For binary trees: the different methods of traversing their nodes.
  Defined in EZDSLBSE.PAS.

### Declaration
    type
      TTreeCursor = longint;
#### Description
  Navigation cursor for `TBinTree` and descendants (binary trees).
  Defined in EZDSLBSE.PAS.

### Declaration
    type
      TEZString = string[255];
      PEZString = ^TEZString;
#### Description
  Essentially for compatibility between Delphi 1 and Delphi 2/3: these
  provide a much needed single type for short strings. Defined in
  EZDSLSUP.PAS.



Stand-alone routines (all within EZDSLSUP.PAS)
----------------------------------------------------------------------

### Declaration
    function  EZIntCompare(Data1, Data2 : pointer) : integer;
#### Description
  Intended as a compare function for containers: compares two
  longints.

### Declaration
    procedure EZIntDisposeData(aData : pointer);
#### Description
  Intended as a data disposal procedure for containers: disposes a
  longint. In other words, does nothing!

### Declaration
    function  EZIntDupData(aData : pointer) : pointer;
#### Description
  Intended as a data duplication function for containers:  duplicates
  a longint. Essentially it returns `aData`.

### Declaration
    function  EZStrNew(const S : string) : PEZString;
#### Description
  Allocates a memory block on the heap, copies `S` to it, and returns
  the pointer to the memory block. Exactly equivalent to `NewStr` in
  Delphi 1.0. In Delphi 2/3 `EZStrNew` provides a pointer to a short
  string, not a long string.

### Declaration
    procedure EZStrDispose(PS : PEZString);
#### Description
  Disposes of a string allocated on the heap by `EZStrNew`.

### Declaration
    function  EZStrCompare(Data1, Data2 : pointer) : integer;
#### Description
  Intended as a compare function for containers: compares two
  strings in case-sensitive manner. The strings are assumed to have
  been assigned with the `EZStrNew` routine, in other words are short
  strings in Delphi 2/3.

### Declaration
    procedure EZStrDisposeData(aData : pointer);
#### Description
  Intended as a data disposal procedure for containers: disposes a
  string. The string is assumed to have been assigned with the
  `EZStrNew` routine.

### Declaration
    function  EZStrDupData(aData : pointer) : pointer;
#### Description
  Intended as a data duplication function for containers:  duplicates
  a string. The string is assumed to have been assigned with 
  `EZStrNew`.



Exception classes
----------------------------------------------------------------------
EZDSL defines two exception classes.

     EEZContainerError = class(Exception);

This is the ancestor exception class for the EZDSL library. All run-
time exceptions are of this type.

     EEZAssertionError = class(EEZContainerError);

This is the assertion exception class. It is only used in DEBUG mode.



TAbstractContainer (EZDSLBSE.PAS)
----------------------------------------------------------------------
This type is an abstract container class, an ancestor to all the other
containers. Most methods have to be or must be overridden, but it
forms a base object from which more complex objects can be derived.
Please review the descriptions of the properties defined below; the
most important are `Compare`, and `DisposeData`; `DupData` is only required
if you are going to be cloning containers.

Do not create an instance of this object type.


Properties
----------

### Declaration
    property Count : longint
#### Description
  READ ONLY. The number of data objects in the container.

### Declaration
    property Compare : TCompareFunc
#### Description
  The container's `Compare` function. If you don't 'override' it by
  setting the property to a function in your code, the default one
  raises an exception when `Compare` is used. 

  If a container's IsSorted property returns true, the `Compare`
  property must be set.

  If there are data objects present in the container, and the
  container is sorted, then setting the `Compare` property will cause
  the container to be sorted according to the new `Compare` function.

  For non-sorted containers, the `Compare` function will be used in a
  search to determine whether the data object you are looking for is
  present or not. In this restricted case, `Compare` is just used for
  its zero (i.e., equal) and non-zero (i.e., not equal) results.
  Setting `Compare` in a non-sorted container has no immediate effect.

  Note that the `THashTable` container does not use the `Compare` function
  under any circumstances.

### Declaration
    property DisposeData : TDisposeDataProc
#### Description
  The container's `DisposeData` procedure. If you don't 'override' it,
  the default one raises an exception if the container is a data owner
  and it tries to dispose of a data object.

  WARNING: your `DisposeData` procedure may be called with a nil pointer
  so please cater for this possibility and don't try to free a nil
  pointer. It doesn't work.

### Declaration
    property DupData : TDupDataFunc
#### Description
  The container's `DupData` function. If you don't 'override' it, the
  default one raises an exception when it is used. `DupData` is used by
  the `Clone` constructor when the new container is a data owner.

### Declaration
    property IsDataOwner : boolean
#### Description
  READ ONLY. True if the container was created as a data owner, False
  otherwise. It is not possible to change this property after the
  container has been created. Data owners will destroy data objects at
  certain times, e.g., during `Empty`.

### Declaration
    property IsSorted : boolean
#### Description
  Returns true if the container is sorted, false otherwise.

  Setting the `IsSorted` property may be ignored depending on the
  container. Certain containers in EZDSL are always non-sorted and so
  setting `IsSorted` to true will be ignored. Other containers in EZDSL
  are always sorted and so setting `IsSorted` to false will be ignored.
  If the container's 'sortedness' attribute can be changed, then
  setting it to true will cause the data objects in the container to
  be reordered according to the `Compare` function.

    Container       IsSorted values
    -------------------------------
    TStack          false
    TQueue          false
    TDeque          false
    TLinkList       true or false
    TDList          true or false
    TSkipList       true
    TBinTree        false
    TBinSearchTree  true
    TrbSearchTree   true
    THashTable      false


Interfaced methods
------------------

### Declaration
    constructor Create(DataOwner : boolean); virtual;
#### Description
  Creates the object. Descendants will (must) set the `NodeSize` field
  before calling this as an inherited constructor. If non-zero, this
  method gets a pointer to the relevant node store and stores it in a
  field in the container for use when allocating or deallocating
  nodes. If `NodeSize` is zero this constructor assumes that the
  descendant will be taking care of allocating & freeing nodes.  

  Create sets the internal count of items to zero.

  The `DataOwner` parameter determines whether the container is to own
  (i.e., can dispose of) its data objects. If true, then the container
  will dispose of data objects by calling `DisposeData` when required.

### Declaration
    destructor Destroy; override;
#### Description
  Destroys the container. First it calls the virtual `Empty` method,
  enabling the descendant to clean up properly. If the container is a
  data owner, all data objects will be destroyed as well by calling
  `DisposeData`, `Destroy` detaches the container from the node store if
  one was being use for node allocation.

### Declaration
    constructor Clone(Source : TAbstractContainer;
                    DataOwner : boolean; NewCompare : TCompareFunc);
                                                    virtual; abstract;
#### Description
  Constructor to create a 'clone' (ie an exact copy) of the `Source`
  container and all its data objects.  Descendants will override this
  constructor without fail. If you are going to use this method you
  must override the `DupData` function as well.  You may specify a new
  `Compare` function for the cloned container, in which case, if the
  container maintains a sorted order, the new container will have its
  data objects in another order. If `DataOwner` is true, the new
  container will own its data objects and hence all the objects in the
  original container will be duplicated. If false, the data objects
  (i.e., the pointers) will be copied over.  

### Declaration
    procedure Empty; virtual; abstract;
#### Description
  Abstract method that empties the container; each container
  descendant will have its own preferred efficient method of doing
  this.

  NOTE: `DisposeData` will be called for all objects in the container if
  the container owns its data objects.  

### Declaration
    function IsEmpty : boolean;
#### Description
  Returns true if the container is empty; i.e., it contains no data
  objects. The method is just a shorthand for checking that the `Count`
  property is zero.



TStack (EZDSLSTK.PAS)
----------------------------------------------------------------------
A stack is a LIFO container (last in first out): the last data object
pushed on will be the first to be popped off. You can also examine
(peek at) the next item to be popped off. However you cannot navigate
through the stack, the data objects underneath the top one are hidden
from you until you pop the ones above off.


Interfaced methods
------------------

### Declaration
    constructor Create(DataOwner : boolean); override;
#### Description
  Creates the stack by calling the ancestor's `Create` after setting a
  node size of 8.  If `DataOwner` is true, the new stack will own its
  data objects. Stacks are created as unsorted objects (`IsSorted` will
  return false).

### Declaration
    constructor Clone(Source : TAbstractContainer;
                    DataOwner : boolean; NewCompare : TCompareFunc);
                                                            override;
#### Description
  Creates a copy of a `Source` stack. If `DataOwner` is true, the data
  objects in the Source stack are duplicated for the new one. If
  false, the data objects are just copied over to the new stack. The
  data objects in the new stack will be popped off in the same order
  as the original stack.

  If `Source` is not a `TStack` instance an exception (`escBadSource`) is
  raised.

### Declaration
    procedure Empty; override;
#### Description
  Repeatedly calls the `Pop` method until the stack is empty. If the
  stack is a data owner, `DisposeData` will be called for each data
  object to destroy it.

### Declaration
    function Examine : pointer;
#### Description
  Returns the data object at the top of the stack without popping it. 

  In DEBUG mode, an assertion error (`ascEmptyExamine`) will occur if
  the stack is empty.  

### Declaration
    function Pop : pointer;
#### Description
  Pops the data object from the top of the stack and returns it.

  In DEBUG mode, an assertion error (`ascEmptyPop`) will occur if the
  stack is empty.

### Declaration
    procedure Push(aData : pointer);
#### Description
  Pushes the data object `aData` onto the top of the stack.



TQueue (EZDSLQUE.PAS)
----------------------------------------------------------------------
A queue is a FIFO container (first in first out): the first object put
in the queue will be the first popped, the last object will be the
last to be popped. You can examine (peek at) the next data object to
be popped.  However you cannot navigate through the queue.


Interfaced methods
------------------

### Declaration
    constructor Create(DataOwner : boolean); override;
#### Description
  Creates the queue by calling the ancestor's `Create` after setting a
  node size of 8.  If `DataOwner` is true, the new queue will own its
  data objects.

### Declaration
    procedure Append(aData : pointer);
#### Description
  Adds the data object `aData` to the tail end of the queue.

### Declaration
    constructor Clone(Source : TAbstractContainer;
                    DataOwner : boolean; NewCompare : TCompareFunc);
                                                            override;
#### Description
  Creates a copy of the `Source` queue. If `DataOwner` is true, the data
  objects in the original queue will be duplicated for the cloned
  queue. If false, the data objects are copied. The data objects will
  be retrieved in the same order as the original queue.

### Declaration
    procedure Empty; override;
#### Description
  Repeatedly calls the `Pop` method until the queue is empty. If the
  queue is a data owner, `DisposeData` will be called to destroy the
  data objects as they are popped.

### Declaration
    function Examine : pointer;
#### Description
  Returns the data from the top of the queue without popping it.

  In DEBUG mode, an assertion error (`ascEmptyExamine`) will occur if
  the queue is empty.

### Declaration
    function Pop : pointer;
#### Description
  Pops the data object from the front of the queue and returns it.
  Even if the queue is a data owner, the data object is still returned
  (it will not be destroyed).

  In DEBUG mode, an assertion error (`ascEmptyPop`) will occur if the
  queue is empty.



TDeque
----------------------------------------------------------------------
A deque (sometimes pronounced DECK, sometimes DEQUEUE) is a queue that
allows objects to be added to, or removed from the front or back of
the queue. This particular implementation of a deque just allows queue
jumpers, ie data objects can also be pushed into the front of the
queue, giving it stack-like behavior (Flamig calls this variant a
Staque, see references). It is descended from the basic `TQueue` and
inherits `Pop` and `Append`.


Interfaced methods
------------------

### Declaration
    procedure Push(aData : pointer);
#### Description
  Pushes the data object `aData` to the front of the deque.



TPriorityQueue
----------------------------------------------------------------------
A priority queue is much like an ordinary queue, except that the
smallest data object in the queue will be popped first (rather than
the 'oldest'). Another name for a priority queue is a heap (not to be
confused with Delphi's heap where memory blocks are allocated and
freed).  As it imposes a sort order on the data objects, you must
override the `Compare` function.

If the Compare method returns values in the 'normal' sense (i.e.,
`Compare` returns a negative number if `Data1` < `Data2`, 0 if `Data1` =
`Data2`, and a positive number otherwise), then data objects will be
popped off smallest first, in other words, in increasing order.
However, if `Compare` returns values in the 'reverse' sense (i.e.,
returning negative if `Data1` > `Data2`, etc), then elements will be
popped off largest first, in other words, in decreasing order. Thus by
carefully selecting Compare, this object will provide the classic min-
heap and max-heap data structures.

Notice that the well-known Heap Sort algorithm uses a structure of
this type, and in fact this structure could be used to provide a
generic sort routine. It will be faster than using a skip list for
example (the data objects are not held internally in a fully sorted
manner, they are just sorted in a 'loose' sense).


Interfaced methods
------------------

### Declaration
    constructor Create(DataOwner : boolean);
#### Description
  Creates the priority queue by calling the ancestor's `Create` after
  setting a node size of 16.  If `DataOwner` is true, the new queue will
  own its data objects.

### Declaration
    procedure Append(aData : pointer);
#### Description
  Adds the data object aData to the queue. Because the queue is
  'partially' sorted internally the data object will be inserted into
  the correct position in the order.

### Declaration
    constructor Clone(Source : TAbstractContainer;
                    DataOwner : boolean; NewCompare : TCompareFunc);
                                                            override;
#### Description
  Creates a copy of the `Source` queue. If `DataOwner` is true, the data
  objects in the original queue will be duplicated for the cloned
  queue. If false, the data objects are copied.

### Declaration
    procedure Empty; override;
#### Description
  Repeatedly calls the `Pop` method until the queue is empty. If the
  queue is a data owner, `DisposeData` will be called to destroy the
  data objects as they are popped.

### Declaration
    function Examine : pointer;
#### Description
  Returns the data from the top of the queue without popping it.

  Because the data objects are sorted, you will retrieve the smallest
  data object with this method, providing that `Compare` returns values
  in the 'normal' sense. If, on the other hand, `Compare` is returning
  values in the 'reverse' sense, you'll get the largest data object
  instead.

  In DEBUG mode, an assertion error (`ascEmptyExamine`) will occur if
  the queue is empty.

### Declaration
    function Pop : pointer;
#### Description
  Pops the data object from the front of the queue and returns it.
  Even if the queue is a data owner, the data object is still returned
  (it will not be destroyed).

  If `Compare` works in the 'normal' sense (ie returns a negative value
  for 'less than') then the data object will be the smallest,
  otherwise the data object will be the largest.

  In DEBUG mode, an assertion error (`ascEmptyPop`) will occur if the
  queue is empty.

### Declaration
    function Replace(aData : pointer) : pointer;
#### Description
  Equivalent to an `Append` followed by a `Pop`, but faster since it uses
  an efficient algorithm. 

  Note that the same data object could be returned (it may be smaller
  (larger) than all the data objects in the queue).



TLinkList
----------------------------------------------------------------------
A linked list is a container which is a chain of data objects. From a
given position in the list you can move forwards or backwards to the
next or previous data object. You cannot directly jump to the Nth data
object, instead you have to walk the list from the beginning counting
as you go. This list implementation has an implied 'cursor' which
points to the current data object, you move this cursor along the list
by means of the list's methods (eg `Next` and `Prev`). You can examine the
data object that the cursor is pointing to, or insert a new one before
or after the cursor, or delete the data object at the cursor (unlink
it).

There are two special cursor positions associated with this list:
before the first data object (known as `BeforeFirst`) and after the last
data object (known as `AfterLast`). Even in an empty list these are
deemed different. In the diagram of a linked list below the three data
objects are `a`, `b` and `c`, and the internal cursor is pointing at `c`:

     BeforeFirst --> a --> b --> c --> AfterLast
                                 |
     Cursor ---------------------+

As a convenience to the programmer, this implementation of a linked
list allows the data objects to be maintained in a sorted order. To do
this there are some practical and some theoretical considerations. The
practical ones first: you must override `Compare` and you must use
`InsertSorted` to insert data objects into the list. If the list is
already sorted and you replace the `Compare` function, the list will be
reordered. The theoretical considerations are to do with the linear
sequential nature of a linked list: to find where to insert a data
object the list must start at the beginning of the list and walk the
list calling `Compare` for each data object it encounters until it finds
the place where the new data object can be inserted. A time-consuming
process as you can appreciate. You should only use sorted linked lists
if the list is built only once or if the number of data objects in the
list is likely to be small. 

(In actual fact, the linked list in EZDSL uses a modification of a
binary search to find a data object rather than the simplistic
sequential search just described. Nevertheless there is a great deal
of link following to be done.)

This particular implementation is a singly-linked list (each node has
just a single link to the 'next' node), however it uses a particular
algorithm that enables it to have the performance capabilities of a
doubly-linked list (see TDList) without the overhead of the extra
links.


Interfaced methods
------------------

### Declaration
    constructor Create(DataOwner : boolean);
#### Description
  Creates the object by calling the ancestor's `Create` after setting a
  node size of 8.  If `DataOwner` is true, the new list will own its
  data objects.


### Declaration
    constructor Clone(Source : TAbstractContainer;
                    DataOwner : boolean; NewCompare : TCompareFunc);
                                                            override;
#### Description
  Creates a copy of the `Source` list. If the original list was sorted
  then the cloned list is also sorted.

### Declaration
    procedure Delete;
#### Description
  Unlinks the current data object but does not dispose of it. The
  internal cursor is moved to the next data object in the list, or to
  the `AfterLast` position if there are no more objects after it.

  In DEBUG mode, an assertion error (`ascDeleteEdges`) will occur if the
  cursor is at `BeforeFirst` or `AfterLast`.

### Declaration
    procedure Empty; override;
#### Description
  Walks the list, calling `Erase` for every data object it finds.

### Declaration
    procedure Erase;
#### Description
  Works like `Delete`, but also disposes the data object by calling
  `DisposeData` if the list owns its data objects.

### Declaration
    function Examine : pointer;
#### Description
  Returns the data object the cursor is pointing to.

  In DEBUG mode, an assertion error (`ascExamineEdges`) will occur if
  the cursor is at `BeforeFirst` or `AfterLast`.

### Declaration
    procedure InsertAfter(aData : pointer);
#### Description
  Inserts the data object aData after the cursor for an unsorted list.
  For a sorted list you should use `InsertSorted` to insert new data
  objects.

  In DEBUG mode, an assertion error (`ascInsertEdges`) will occur if the
  cursor is at `AfterLast`.

  In DEBUG mode, an assertion error (`ascIsSortedList`) will occur if
  the list is sorted.

### Declaration
    procedure InsertBefore(aData : pointer);
#### Description
  Inserts the data object `aData` before the cursor for an unsorted
  list. For a sorted list you should use `InsertSorted` to insert new
  data objects.

  In DEBUG mode, an assertion error (`ascInsertEdges`) will occur if the
  cursor is at BeforeFirst.

  In DEBUG mode, an assertion error (`ascIsSortedList`) will occur if
  the list is sorted.

### Declaration
    procedure InsertSorted(aData : pointer);
#### Description
  Inserts the data object `aData` into a sorted list. For an unsorted
  list you should use `InsertAfter` or `InsertBefore` to insert new data
  objects.

  In DEBUG mode, an assertion error (`ascIsNotSortedList`) will occur if
  the list is not sorted.

### Declaration
    function IsAfterLast : boolean;
#### Description
  Returns true if the cursor is after all data objects in the list
  (beyond the end of the list).

### Declaration
    function IsBeforeFirst : boolean;
#### Description
  Returns true if the cursor is before all data objects in the list.

### Declaration
    function Iterate(Action : TIterator;
                     Backwards : boolean;
                     ExtraData : pointer) : pointer;
#### Description
  Walks the list from the start (if `Backwards` is False) or from the
  end (if `Backwards` is True) and calls `Action` for each data object
  found.  If `Action` returns False for a data object, then `Iterate`
  immediately returns with that data object; if `Action` always returns
  True then this method will return nil.  `ExtraData` is a pointer to
  any other data that you may want `Action` to use.

### Declaration
    procedure Join(List : TLinkList);
#### Description
  Adds all the data objects in `List` to the current list, placing them
  after the cursor. `List` is then destroyed. To add the data objects
  before all the current ones, call `SetBeforeFirst` first; to add them
  after all the current ones, call `SetAfterLast` and `Prev` first. 

  Note that if the destination list is sorted, the nodes from `List` are
  added in sorted order and it doesn't matter where the cursor is
  positioned.

  In DEBUG mode, an assertion error (`ascCannotJoinHere`) will occur if
  the cursor is at `AfterLast`.  

  Please note that both lists concerned must either be data owners or
  not. You cannot `Join` a list that is a data owner to one that is not.
  In DEBUG mode, an assertion error (`ascCannotJoinData`) will occur if
  you try.

### Declaration
    procedure Next;
#### Description
  Moves the cursor to the next data object in the list. 

  In DEBUG mode, an assertion error (`ascAlreadyAtEnd`) will occur if
  the cursor is at `AfterLast`.

### Declaration
    procedure Prev;
#### Description
  Moves the cursor to the previous data object in the list. 

  In DEBUG mode, an assertion error (`ascAlreadyAtStart`) will occur if
  the cursor is at `BeforeFirst`.

### Declaration
    function Replace(aData : pointer) : pointer;
#### Description
  Replaces the data object at the cursor with `aData` and returns the
  replaced data object. Note that for sorted lists, the cursor might
  be moved and will point to the `aData` data object.

  In DEBUG mode an assertion error will occur (`ascReplaceEdges`) if the
  cursor is currently at `BeforeFirst` or `AfterLast`.

### Declaration
    function Search(aData : pointer) : boolean;
#### Description
  Returns true if the data object `aData` is found in the list (`Compare`
  must return 0 between it and one of the data objects in the list)
  and the cursor is left pointing at the found data object.

  If the data object was not found it returns false. If the list is
  sorted, it leaves the cursor just past where the data object could
  be inserted. If the list is not sorted, the cursor is left in the
  `AfterLast` position.

### Declaration
    procedure SetAfterLast;
#### Description
  Moves the cursor after all the data objects in the list. Calling
  `Prev` from this position gives you the last object on the list.

### Declaration
    procedure SetBeforeFirst;
#### Description
  Moves the cursor before all the data objects in the list. Calling
  `Next` from this position gives you the first object on the list.

### Declaration
    function Split : TLinkList;
#### Description
  Splits the linked list into two at the cursor by creating a new
  list, and moving all the data objects from the cursor onwards to the
  new list.

  In DEBUG mode, an assertion error (`ascSplitEdges`) will occur if the
  cursor is at `BeforeFirst` or `AfterLast`. In other words, at least one
  data object must be moved; you cannot call `Split` to create an empty
  list.



TDList
----------------------------------------------------------------------
This is a linked list of data objects. Compared with `TLinkList`, this
implementation uses external 'cursors' which point to the various data
objects, and you move these cursors along the list by means of the
list's methods. You can examine the data object that a cursor is
pointing to, or insert a new one before or after a given cursor, or
delete the data object at a cursor (unlink it). Again, there are two
special cursor positions: before the first data object (`BeforeFirst`)
and after the last data object (`AfterLast`). Even in an empty list
these are different.

The difference between this object and `TLinkList` is in the use of
external cursors, and also in the internal implementation. This object
uses nodes with two links (hence 'doubly linked') rather than one.

Notice that it is up to you to make sure that your external cursors
are valid. The following code is deemed a programming error:

    begin
      {...}
      with MyDList do
        begin
          Cursor := Next(SetBeforeFirst);
          NextCursor := Delete(Cursor);
          if (Next(Cursor) = NextCursor) then   {<=== CRASH}
            {...}

Here, the value of `Cursor` is undefined after the call to `Delete`. This
is much the same as using pointers: you may have several copies of a
pointer to a heap memory block, but as soon as you free that memory
block all those pointer variables are invalid.

As a convenience to the programmer, this implementation of a doubly-
linked list allows the data objects to be maintained in a sorted
order, much as for `TLinkList`. Please see the discussion of sorted
lists in the `TLinkList` section.


Interfaced methods
------------------

### Declaration
    constructor Create(DataOwner : boolean);
#### Description
  Creates the object by calling the ancestor's `Create` after setting a
  node size of 12.  If `DataOwner` is true, the new list will own its
  data objects.

### Declaration
    constructor Clone(Source : TAbstractContainer;
                    DataOwner : boolean; NewCompare : TCompareFunc);
                                                            override;
#### Description
  Creates a copy of the Source list. If the original list was sorted
  then the cloned list is also sorted.

### Declaration
    function Delete(Cursor : TListCursor) : TListCursor;
#### Description
  Unlinks the current data object but does not dispose of it. The
  internal cursor is moved to the next data object in the list, or to
  the `AfterLast` position if there are no more objects after it.

  Please note that the parameter `Cursor` is invalid after this routine
  is called.

  In DEBUG mode, an assertion error (`ascDeleteEdges`) will occur if the
  cursor is at `BeforeFirst` or `AfterLast`.

### Declaration
    procedure Empty; override;
#### Description
  Walks the list, calling `Erase` for every data object it finds.

### Declaration
    function Erase(Cursor : TListCursor) : TListCursor;
#### Description
  Works like `Delete`, but also disposes the data object by calling
  `DisposeData`, providing the list owns its data objects.

### Declaration
    function Examine(Cursor : TListCursor) : pointer;
#### Description
  Returns the data object the cursor is pointing to.

  In DEBUG mode, an assertion error (`ascExamineEdges`) will occur if
  the cursor is at `BeforeFirst` or `AfterLast`.

#### Description
    procedure InsertAfter(Cursor : TListCursor; aData : pointer);
#### Description
  Inserts the data object `aData` after the cursor for an unsorted list.
  For a sorted list you should use `InsertSorted` to insert new data
  objects.

  In DEBUG mode, an assertion error (`ascInsertEdges`) will occur if the
  cursor is at `AfterLast`.

  In DEBUG mode, an assertion error (`ascIsSortedList`) will occur if
  the list is sorted.

### Declaration
    procedure InsertBefore(Cursor : TListCursor; aData : pointer);
#### Description
  Inserts the data object `aData` before the cursor for an unsorted
  list. For a sorted list you should use `InsertSorted` to insert new
  data objects.

  In DEBUG mode, an assertion error (`ascInsertEdges`) will occur if the
  cursor is at `BeforeFirst`.

  In DEBUG mode, an assertion error (`ascIsSortedList`) will occur if
  the list is sorted.

### Declaration
    procedure InsertSorted(aData : pointer);
#### Description
  Inserts the data object `aData` into a sorted list. For an unsorted
  list you should use `InsertAfter` or `InsertBefore` to insert new data
  objects.

  In DEBUG mode, an assertion error (`ascIsNotSortedList`) will occur if
  the list is not sorted.

### Declaration
    function IsAfterLast(Cursor : TListCursor) : boolean;
#### Description
  Returns true if the cursor is after all data objects in the list
  (beyond the end of the list).

### Declaration
    function IsBeforeFirst(Cursor : TListCursor) : boolean;
#### Description
  Returns true if the cursor is before all data objects in the list.

### Declaration
    function Iterate(Action : TIteratorFunc;
                     Backwards : boolean;
                     ExtraData : pointer) : TListCursor;
#### Description
  Walks the list from the start (if `Backwards` is False) or from the
  end (if `Backwards` is True) and calls `Action` for each data object
  found.  If `Action` returns False for a data object, then `Iterate`
  immediately returns with that data object; if `Action` always returns
  True then this method will return nil.  `ExtraData` is a pointer to
  any other data that you may want `Action` to use.

### Declaration
    procedure Join(Cursor : TListCursor; List : TDList);
#### Description
  Adds all the data objects in `List` to the current list, placing them
  after the cursor. `List` is then destroyed. To add the data objects
  before all the current ones, call `SetBeforeFirst` first; to add them
  after all the current ones, call `SetAfterLast` and `Prev` first. 

  Note that if the destination list is sorted, the nodes from `List` are
  added in sorted order and it doesn't matter where the cursor is
  positioned.

  In DEBUG mode, an assertion error (`ascCannotJoinHere`) will occur if
  the cursor is at `AfterLast`.  

  Please note that both lists concerned must either be data owners or
  not. You cannot `Join` a list that is a data owner to one that is not.
  In DEBUG mode, an assertion error (`ascCannotJoinData`) will occur if
  you try.

### Declaration
    function Next(Cursor : TListCursor) : TListCursor;
#### Description
  Moves the cursor to the next data object in the list. 

  In DEBUG mode, an assertion error (`ascAlreadyAtEnd`) will occur if
  the cursor is at `AfterLast`.

### Declaration
    function Prev(Cursor : TListCursor) : TListCursor;
#### Description
  Moves the cursor to the previous data object in the list. 

  In DEBUG mode, an assertion error (`ascAlreadyAtStart`) will occur if
  the cursor is at `BeforeFirst`.

### Declaration
    function Replace(Cursor : TListCursor; aData : pointer) :  pointer;
#### Description
  Replaces the data object at the cursor with `aData` and returns the
  replaced data object. Note that for sorted lists, the cursor might
  be moved and will point to the `aData` data object.

  In DEBUG mode an assertion error will occur (`ascReplaceEdges`) if the
  cursor is currently at `BeforeFirst` or `AfterLast`.

### Declaration
    function Search(var Cursor : TListCursor;
                        aData  : pointer) : boolean;
#### Description
  Returns true if the data object `aData` is found in the list (`Compare`
  must return 0 between it and one of the data objects in the list),
  and returns the cursor. 

  If the data object was not found in a sorted list, `Search` returns
  the cursor just past where the data object should be inserted. 

  If the data object was not found in a non-sorted list, the cursor
  returned is the `AfterLast` position.

### Declaration
    function SetAfterLast : TListCursor;
#### Description
  Returns the cursor that is after all the data objects in the list.
  Calling `Prev` with this cursor gives you the last object on the list.

### Declaration
    function SetBeforeFirst : TListCursor;
#### Description
  Returns the cursor that is before all the data objects in the list.
  Calling `Next` with this cursor gives you the first object on the
  list.

### Declaration
    function Split(Cursor : TListCursor) : TDList;
#### Description
  Splits the linked list into two at the cursor by creating a new
  list, and moving all the data objects from the cursor onwards to the
  new list.

  In DEBUG mode, an assertion error (`ascSplitEdges`) will occur if the
  cursor is at `BeforeFirst` or `AfterLast`. In other words, at least one
  data object must be moved; you cannot call `Split` to create an empty
  list.



TSkipList
----------------------------------------------------------------------
A skip list is a special type of linked list of data objects. The list
is sorted and therefore requires the `Compare` method to be overridden. 
Compared with `TLinkList` and `TDList`, this implementation uses nodes of
varying sizes. The nodes have between 1 and 16 (`skMaxLevels`) of
forward pointers, the higher ones skipping over nodes with less
forward pointers. This means much faster search times, but slightly
slower list update times (i.e., insert and delete). It can cope with
searching long lists without too much degradation. Compared with a
red-black binary search tree, this type of data structure will consume
more memory, will have equivalent insert times and delete times, and
will have comparable (amortized) search times. 

Like `TLinkList` and `TDList`, the skip list has two special positions:
before all the data objects and after all the data objects.

To grasp how it works, start with a classic doubly linked list where
each node has a pointer to the next and previous nodes. Now imagine
that about one in four of these nodes has a pointer to the node 4
nodes ahead as well. Furthermore suppose that about one in sixteen of
the nodes has a pointer to the node 16 nodes ahead. And so on. As you
can see, you can navigate through the list pretty quickly, jumping
over lesser nodes and 'homing in' on the node you want. For example,
in the diagram below, to get to `g` quickly from `BeforeFirst`, you can
jump to `d` straightaway, followed by a smaller jump to `f` followed by a
small jump to `g`.

                -------------------------------------------->

                -------------------->   -------------------->
                -------->   -------->   -------->   -------->
    BeforeFirst --> a --> b --> c --> d --> e --> f --> g --> AfterLast

The algorithm to determine how large each node is, and hence how many
forward pointers its has is statistical in nature. About 1 in 4 nodes
is created with two forward pointers (every node has at least one);
about 1 in 16 is created with 3, 1 in 64 with 4, and so on. A random
number generator is used to determine the number of forward pointers
and maintain these proportions.


Interfaced methods
------------------

### Declaration
    constructor Create(DataOwner : boolean);
#### Description
  Creates the object by calling the ancestor's `Create` after setting a
  node size of 0: the skip list will allocate its own nodes since they
  are of varying sizes. A random number generator object is created to
  provide a sequence of random numbers for the insert algorithm. If
  `DataOwner` is true, the new list will own its data objects.

### Declaration
    constructor Clone(Source : TAbstractContainer;
                      DataOwner : boolean; NewCompare : TCompareFunc);
                                                              override;
#### Description
  Creates a copy of the `Source` skip list.

### Declaration
    destructor TSkipList.Destroy; override;
#### Description
  Destroys the internal random number generator object and then allows
  the ancestor to destroy itself.

### Declaration
    function Delete(Cursor : TListCursor) : TListCursor;
#### Description
  Unlinks the current data object but does not dispose of it. The
  internal cursor is moved to the next data object in the list, or to
  the `AfterLast` position if there are no more objects after it.

  In DEBUG mode, an assertion error (`ascDeleteEdges`) will occur if the
  cursor is at `BeforeFirst` or `AfterLast`.

### Declaration
    procedure Empty; override;
#### Description
  Walks the list, calling `Erase` for every data object it finds.

### Declaration
    function Erase(Cursor : TListCursor) : TListCursor;
#### Description
  Works like `Delete`, but also disposes the data object by calling
  `DisposeData` if the list owns its data objects.

### Declaration
    function Examine(Cursor : TListCursor) : pointer;
#### Description
  Returns the data object the cursor is pointing to.

  In DEBUG mode, an assertion error (`ascExamineEdges`) will occur if
  the cursor is at `BeforeFirst` or `AfterLast`.

### Declaration
    procedure Insert(var Cursor : TListCursor; aData : pointer);
#### Description
  Inserts the data object `aData` in the correct place in the list's
  sort sequence. Returns the cursor of the newly inserted data object.

  Requires `Compare` to be overridden.

### Declaration
    function IsAfterLast(Cursor : TListCursor) : boolean;
#### Description
  Returns true if the cursor is after all data objects in the list
  (beyond the end of the list).

### Declaration
    function IsBeforeFirst(Cursor : TListCursor) : boolean;
#### Description
  Returns true if the cursor is before all data objects in the list.

### Declaration
    function Iterate(Action : TIteratorFunc;
                     Backwards : boolean;
                     ExtraData : pointer) : TListCursor;
#### Description
  Walks the list from the start (if `Backwards` is False) or from the
  end (if `Backwards` is True) and calls `Action` for each data object
  found.  If `Action` returns False for a data object, then `Iterate`
  immediately returns with that data object; if `Action` always returns
  True then this method will return nil.  `ExtraData` is a pointer to
  any other data that you may want `Action` to use.

### Declaration
    procedure Join(List : TSkipList);
#### Description
  Adds all the data objects in `List` to the current list in sorted
  order.

  Please note that both lists concerned must either be data owners or
  not. You cannot Join a list that is a data owner to one that is not.
  In DEBUG mode, an assertion error (`ascCannotJoinData`) will occur if
  you try.

### Declaration
    function Next(Cursor : TListCursor) : TListCursor;
#### Description
  Moves the cursor to the next data object in the list. 

  In DEBUG mode, an assertion error (`ascAlreadyAtEnd`) will occur if
  the cursor is at `AfterLast`.

### Declaration
    function Prev(Cursor : TListCursor) : TListCursor;
#### Description
  Moves the cursor to the previous data object in the list. 

  In DEBUG mode, an assertion error (`ascAlreadyAtStart`) will occur if
  the cursor is at `BeforeFirst`.

### Declaration
    function Replace(Cursor : TListCursor; aData : pointer) : pointer;
#### Description
  Replaces the data object at the cursor with `aData` and returns the
  replaced data object. Note that for sorted lists, the cursor might
  be moved and will point to the `aData` data object.

  In DEBUG mode an assertion error will occur (`ascReplaceEdges`) if the
  cursor is currently at `BeforeFirst` or `AfterLast`.

### Declaration
    function Search(var Cursor : TListCursor; aData : pointer)
                                                            : boolean;
#### Description
  Returns true if the data object `aData` is found in the list (`Compare`
  must return 0 between it and one of the data objects in the list)
  and the cursor is left pointing at the found data object.

  If the data object was not found it returns false, and it leaves the
  cursor just past where the data object could be inserted.

### Declaration
    function SetAfterLast : TListCursor;
#### Description
  Returns the cursor that is after all the data objects in the list.
  Calling `Prev` with this cursor gives you the last object on the list.

### Declaration
    function SetBeforeFirst : TListCursor;
#### Description
  Returns the cursor that is before all the data objects in the list.
  Calling `Next` with this cursor gives you the first object on the
  list.

### Declaration
    function Split(Cursor : TListCursor) : TSkipList;
#### Description
  Splits the skip list into two at the cursor by creating a new list,
  and moving all the data objects from the cursor onwards to the new
  list. The new list uses the same `Compare` function, and hence has the
  same sorted order, as the original list.

  In DEBUG mode, an assertion error (`ascSplitEdges`) will occur if the
  cursor is at `BeforeFirst` or `AfterLast`. In other words, at least one
  data object must be moved; you cannot call Split to create an empty
  list.



TBinTree
----------------------------------------------------------------------
A binary tree is a data structure that can best be described in a
recursive manner: a tree is either empty or consists of a node with
links to two other trees. Classically a binary tree is drawn:

                         Root
                        /    \
                       a      b
                      / \    / \
                     c   d  e   f

The node at the top of the tree is called the root node. The
implementation of a binary tree in EZDSL uses the concepts of
internal and external nodes. An internal node of the tree always has
two children, an external node (also known as a leaf) has no children.
An external node carries no data object, only internal nodes can be
associated with data objects; if you like, external nodes are like
earth connections in electrical diagrams (in this implementation
external nodes are nil).  In the literature internal nodes tend to be
drawn with small circles and external nodes with small squares.  In
the example above Root, `a` and `b` are all internal nodes; `c`, `d`, `e`, and `f`
are external nodes.  Internal and external nodes are important when
you are inserting or deleting data objects.

Rule 1: you can only insert a data object at an external node. What
happens is that a new internal node is created for the data object (it
is created with two external children), and is placed at the position
occupied by the external node. Think of an internal node having no
'room' for a new inserted data object. A sorted tree (for example
`TBinSearchTree` below) has enough information to be able to restructure
the tree (maintaining its essential sorted property) to violate this
rule if it needs to (but in fact it doesn't).

For example inserting `NewObject` at `Leaf` below:

         Parent                       Parent
        /      \         ====>       /      \
     Subtree    Leaf              Subtree    NewObject
       |                            |       /         \
      ...                          ...    Leaf       Leaf

Rule 2: you can only delete a node that's (a) internal and (b) has at
least one external child. When you delete the node the external
child(ren) are automatically deleted as well. The reason for this rule
is to allow the tree to remain connected after the deletion. Again the
sorted trees that descend from this object class will know how to
restructure the tree to allow deletion of an internal node with two
internal children. There are two cases to consider.  First deleting a
node with two external children:

         Parent                       Parent
        /      \                     /      \
     Subtree    Node     ====>    Subtree   Leaf
       |        /  \                |
      ...     Leaf Leaf            ...

And second, deleting a node with a single external child (and hence a
single internal child):

         Parent                           Parent
        /      \                         /      \
    Subtree1    Node        ====>    Subtree1    Subtree2
      |        /    \                  |            |
     ...    Leaf   Subtree2           ...          ...
                      |
                     ...

Generally you should only be concerned with this if you are going to
be creating an unsorted binary tree; to reiterate, the sorted variants
will restructure the tree using the sorted property to allow insertion
and deletion at any point.

Trees can be traversed in many different ways; the methods supported
by `TBinTree` are pre order, in order, post order and level order
traversal, from left to right and vice versa. The definitions of the
first three of these (from left to right), like that of a binary tree,
are recursive:

- `PreOrder`:  visit the root node, traverse the left sub-tree, traverse
            the right sub-tree. In the first diagram above, nodes
            would be visited in the order `Root`, `a`, `c`, `d`, `b`, `e`, `f`.

- `InOrder`:   traverse the left sub-tree, visit the root node, traverse
            the right sub-tree. In the first diagram above, nodes
            would be visited in the order `c`, `a`, `d`, `Root`, `e`, `b`, `f`.

- `PostOrder`: traverse the left sub-tree, traverse the right sub-tree,
            visit the root node. In the first diagram above, nodes
            would be visited in the order `c`, `d`, `a`, `e`, `f`, `b`, `Root`.

The last (`LevelOrder`) means visiting the root node first, then the
root node of its left subtree, then the root node of its right
subtree, then the root node of its left subtree's left subtree, and so
on, visiting all of the nodes from top to bottom from left to right at
each level. In the first diagram above, nodes would be visited in the
order `Root`, `a`, `b`, `c`, `d`, `e`, `f`.

When traversing the tree from right to left the definitions are the
same, except that the visiting order is reversed: you will be visiting
the right subtree before the left subtree.


Properties
----------

### Declaration
    property TraversalType : TTraversalType
#### Default
    ttInOrder
#### Description
  The traversal type for the `Iterate` method. 

  Note that you shouldn't alter this property in the middle of a call
  to the `Iterate` method (in the `Action` function for example).

### Declaration
    property UseRecursion : boolean
#### Default
    True
#### Description
  Defines whether to use recursion (true) or to unwind it with an
  explicit stack (false) when iterating through the tree.

  The classic way to traverse a binary tree is to use recursion: use a
  routine that calls itself. A pseudocode example is visiting a tree
  in `InOrder` traversal order:

     procedure VisitSubtree(Root : Node);
     begin
       VisitSubtree(Node^.LeftChild);
       ..do something with Node..
       VisitSubtree(Node^.RightChild);
     end;

  As you can see, it's compact, explicit and runs very well. This is
  fine in a lot of cases, however when the tree is very large you may
  use up a lot of your process' stack space. In 32-bit programming,
  this isn't too dire since the stack is so large (default 1MB), but
  in 16-bit programming you can easily get a stack overflow error
  where you literally run out of stack space. It's in the case where
  the tree is very large that you have the opportunity to 'unwind' the
  recursion with an explicit external stack on the heap. Typically,
  you won't have problems with stack overflow on the heap.


Interfaced methods
------------------

### Declaration
    constructor Create(DataOwner : boolean);
#### Description
  Creates the tree by calling the ancestor's `Create` after setting a
  node size of 16. The initial traversal order for the `Iterate` method
  is set to `InOrder`. If `DataOwner` is true, the new tree will own its
  data objects.

### Declaration
    constructor Clone(Source : TAbstractContainer;
                    DataOwner : boolean; NewCompare : TCompareFunc);
                                                            override;
#### Description
  Creates a copy of the `Source` binary tree.

### Declaration
    function Delete(Cursor : TTreeCursor) : TTreeCursor; virtual;
#### Description
  Unlinks the passed cursor from the tree but does not dispose of its
  data object. The cursor of the data object replacing it in the tree
  is returned. 

  Please note that the parameter `Cursor` is invalid after this routine
  is called.

  An exception (`escDelInvalidHere`) will occur if `Cursor` does not have
  at least one external child, or if `Cursor` is an external node.

### Declaration
    procedure Empty; override;
#### Description
  Destroys all nodes and removes all data objects in the tree (data
  objects are destroyed if the tree is a data owner). 

  Does a post order traversal of the tree calling `Erase` for all nodes.

### Declaration
    function Erase(Cursor : TTreeCursor) : TTreeCursor;
#### Description
  Works like `Delete` except that the data object pointed to by `Cursor`
  is disposed of as well, providing that the tree owns its data
  objects.

### Declaration
    function Examine(Cursor : TTreeCursor) : pointer;
#### Description
  Returns the data object at `Cursor`. 

  In DEBUG mode, an assertion error (`ascEmptyExamine`) will occur if
  the tree is empty.

  In DEBUG mode, an assertion error (`ascExamineLeaf`) will occur if
  `Cursor` is an external node (which have no data objects associated
  with them).

### Declaration
    procedure Insert(var Cursor : TTreeCursor; aData : pointer);
                                                              virtual;
#### Description
  Inserts the data object `aData` into the tree at `Cursor`, where `Cursor`
  is assumed to be an external node. 

  An exception (`escInsInvalidHere`) will be raised if the node is not a
  leaf (an external node).

### Declaration
    function IsLeaf(Cursor : TTreeCursor) : boolean;
#### Description
  Returns true if `Cursor` is pointing at an external node (a leaf). 

  Note that although this implementation of binary trees uses nil for
  external nodes, this does NOT mean that a cursor that's pointing to
  an external node is itself nil or zero.

### Declaration
    function IsRoot(Cursor : TTreeCursor) : boolean;
#### Description
  Returns true if `Cursor` is pointing at the data object at the tree's
  root.

### Declaration
    function Iterate(Action : TIteratorFunc;
                     Backwards : boolean;
                     ExtraData : pointer) : TListCursor;
#### Description
  Walks the tree from the root data object in the traversal order
  defined by `TraversalType`. If `Backwards` is False the traversal goes
  from left to right, if True from right to left. `Iterate` calls `Action`
  for each data object found.  If `Action` returns False, then this
  method immediately returns with that data object's cursor; if `Action`
  always returns True then this method will return 0. `ExtraData` is a
  pointer to any other data that you may want `Action` to use.

### Declaration
    procedure Join(Cursor : TTreeCursor; Tree : TBinTree); virtual;
#### Description
  Moves all the data objects in `Tree` onto the tree at the position
  pointed to by `Cursor`, where `Cursor` is assumed to be an external
  node. Tree is then disposed of. 

  An exception (`escInsInvalidHere`) will be raised if `Cursor` is not an
  external node.

### Declaration
    function Left(Cursor : TTreeCursor) : TTreeCursor;
#### Description
  Returns the cursor of the left child of `Cursor`. 

  An exception (`escCannotMoveHere`) will occur if `Cursor` is at an
  external node (which has no children).

### Declaration
    function Parent(Cursor : TTreeCursor) : TTreeCursor;
#### Description
  Returns the cursor of the parent of `Cursor`. 

  An exception (`escCannotMoveHere`) will occur if `Cursor` is at the
  root.

### Declaration
    function Replace(Cursor : TTreeCursor; aData : pointer) : pointer;
#### Description
  Replaces the data object at the cursor with `aData` and returns the
  replaced data object. 

  In DEBUG mode, an assertion error (`ascExamineLeaf`) will occur if
  `Cursor` is an external node.

### Declaration
    function Right(Cursor : TTreeCursor) : TTreeCursor;
#### Description
  Returns the cursor of the right child of `Cursor`. 

  An exception (`escCannotMoveHere`) will occur if `Cursor` is at an
  external node (which has no children).

### Declaration
    function Root : TTreeCursor;
#### Description
  Returns the cursor of the root data object.

### Declaration
    function Search(var Cursor : TTreeCursor;
                        aData  : pointer) : boolean; virtual;
#### Description
  Returns true if the data object `aData` is found in the tree (the
  tree's `Compare` function must return 0 between it and an object in
  the list), and returns the cursor. If `aData` was not found then
  `Cursor` is undefined.



TBinSearchTree
----------------------------------------------------------------------
A binary search tree is a sorted binary tree where for any given data
object, all data objects in its left subtree are less than it, and all
data objects in the right subtree are greater than it. This ordering
relies on the `Compare` function to be overridden.

Binary search trees of this simple type suffer from the well-known
problem that they can degenerate into sorted linked lists. There is no
balancing mechanism for dealing with such degenerate cases as
inserting data objects in sorted order or in other pathological
orders.

For example insert the letters `abcd` into a binary search tree using
the normal ASCII collating sequence and you'd get the tree:

                     a
                      \
                       b
                        \
                         c
                          \
                           d

and if you inserted the letters `aebdc` into a binary search tree you'd
get:

                     a
                      \
                       e
                      /
                     b
                      \
                       d
                      /
                     c

For a balanced tree see the red-black tree `TrbSearchTree`. However if
you are fairly sure that either (a) the number of data objects is
likely to be small and/or (b) they will be inserted in a non-sorted
fashion, then an ordinary binary search tree will be more efficient.


Interfaced methods
------------------

### Declaration
    constructor Create(DataOwner : boolean);
#### Description
  Creates the tree by calling the ancestor's `Create`. The container is
  forced to sorted.

### Declaration
    constructor Clone(Source : TAbstractContainer;
                      DataOwner : boolean; NewCompare : TCompareFunc);
                                                             override;
#### Description
  Creates a copy of the `Source` binary search tree.

### Declaration
    function Delete(Cursor : TTreeCursor) : TTreeCursor;  virtual;
#### Description
  Unlinks the passed cursor from the tree but does not dispose of its
  data object.  The cursor of the data object replacing it in the tree
  is returned. 

  Please note that the parameter `Cursor` is invalid after this routine
  is called.

  NOTE: you can delete any internal node in a binary search tree (the
  tree will be rearranged), but `Delete` will still cause an exception
  if you try and delete an external node.

### Declaration
    procedure Insert(var Cursor : TTreeCursor; aData : pointer);
                                                            override;
#### Description
  Searches for the data object with the `Search` method. If not found,
  `Search` will return an external cursor, and the data object is
  inserted there.  If found, an exception (`edsInsertDup`) will be
  raised. 

  The cursor of the newly inserted data object is returned in `Cursor`.

### Declaration
    procedure Join(Cursor : TTreeCursor; Tree : TBinTree); override;
#### Description
  Moves all the data objects in `Tree` into Self. Because of the sorted
  nature of binary search trees, the new data objects cannot be
  inserted en masse at `Cursor`: the tree must maintain its sequence.
  Hence `Cursor` is not used and is ignored (but must be defined as `Join`
  is a virtual method).

### Declaration
    function Replace(Cursor : TTreeCursor;
                     aData : pointer) : pointer;
#### Description
  Replaces the data object of `Cursor`, and returns the replaced object.
  Note that this is just shorthand for `Delete` followed by `Insert`.

### Declaration
    function Search (var Cursor : TTreeCursor;
                         aData : pointer) : boolean; virtual;
#### Description
  Searches for a data object `aData` by traversing the tree (in 'sorted'
  order), and calling the `Compare` method for each visited data object
  and the given data object aData. If `Compare` returns 0 (equal),
  `Search` will return the cursor of the current data object, if `Compare`
  never returns 0, `Search` returns the cursor of the external node
  where it ended up (this would be where the data object could be
  inserted).



TrbSearchTree
----------------------------------------------------------------------
A red-black tree is a binary search tree with inbuilt tree balancing
algorithms during `Insert` and `Delete`. This ensures that the tree does
not degenerate into a sorted linked list, maintaining its excellent
search times. Balancing in this sense means that the internal
algorithms try to ensure that the path from every leaf to the root is
'about' the same, it does not ensure that they are all equal (or at
most one apart).

The tree is called red-black because certain nodes in the tree are
labelled Black and the others are labelled Red such that

 (1) all external nodes (or leaves) are Black,

 (2) every Red data object (that is not at the root) has a Black
     parent,

 (3) each path from leaf to root has the same number of Black data
     objects.

This set of rules ensures that the tree is (quite) balanced. To see
that this is so I would recommend you read [Sedgewick] or [Wood].


Interfaced methods
------------------

### Declaration
    function Delete(Cursor : TTreeCursor) : TTreeCursor; override;
#### Description
  Deletes the data object at `Cursor` from the tree, and returns the
  cursor of the data object that replaced the deleted one. Rebalances
  the tree if necessary.

### Declaration
    procedure Insert(var Cursor : TTreeCursor;
                         aData : pointer); override;
#### Description
  Searches for the data object with `Search`. If found an exception
  will be raised with code `edsInsertDup`.  Otherwise, the data object
  is inserted at the cursor returned by `Search`, and the tree is
  re-balanced if necessary.  The cursor of the newly inserted data
  object is returned in `Cursor`.



THashTable
----------------------------------------------------------------------
A hash table is a container which stores strings (known as keys) and
data objects, one data object per string. The key is used to identify
the data object. This is different from the other 'sorted' containers
in EZDSL where, if the data objects were sorted, the keys to sort on
were part of the data object itself. The hash table uses separate
string keys to identify each data object. In fact, the `Compare`
function is not used in the hash table. Sometimes hash tables are
known as string dictionaries.

The container is characterized by using a fast retrieval mechanism to
extract individual strings (and their associated data objects) at the
expense of keeping them in a recognizable sorted order.

The container works by using a hash function. The process goes like
this: the string is 'hashed' using the hash function to produce an
integer value. This value is used as an index into a simple array. As
you can see, getting to a given string is very fast since no searching
has to be done. 

In reality, the above is a slight simplification since two or more
strings may hash to the same value and hence the container has to deal
with these collisions. The hash table implemented by EZDSL uses linear
probing to resolve collisions, and, to ensure that the linear chains
do not grow too large, it uses automatic table growing and shrinking.
This algorithm ensures that the average search for a string doesn't go
above 2 accesses.

One feature of EZDSL's hash table is the ability to use strings in a
case-sensitive or case-insensitive manner. You can set this property
of the hash table at any time: the hash table will reorganize itself
to accommodate the change. Obviously, though, it makes sense to fix
the case-sensitivity at object creation time.

Another property of the hash table algorithm is that it works better
if the array sizes are prime. EZDSL enforces this attribute as well.

EZDSL supplies three hash functions for your use. They are all
standard routines, cast in the Delphi mold. In general use, the
default one (the fastest) will prove to be more than adequate.
Nevertheless, you can change a hash table's hash function at any time,
and the table will reorganize itself to accommodate the change.

To reiterate, the hash table is great for finding a data object via
its key quickly. It does not store its keys in alphabetic sorted
order.


Properties
----------

### Declaration
    property HashFunction : THashFunction

    THashFunction = function (const S : string) : longint;
#### Default 
    HashBKDR
#### Description
  The hash function for the hash table. Keys of data objects in the
  table will be hashed by this function to determine their index into
  the hash table.

  If you set `HashFunction`, then the hash table will reorganize its
  data objects and keys to use the new function. This requires a
  separate internal array to be allocated, all the keys and data
  objects transferred to it and the old array disposed of. Naturally
  this takes time. In general, though, you wouldn't change a hash
  table's hash function after creating the table.

### Declaration
    property IgnoreCase : boolean
#### Default
    false
#### Description
  Defines whether the hash table should use keys with case-sensitivity
  or without. If false, the hash table will use the string directly in
  comparisons and with the hash function. If true, the hash table will
  use a case-insensitive comparison, and the string will be uppercased
  before the hash function is used.

  The Windows locale is used both to compare and to uppercase strings.
  You cannot change this behavior.

  If you set `IgnoreCase`, then the hash table will reorganize its data
  objects and keys to use the new value. This requires a separate
  internal array to be allocated, all the keys and data objects
  transferred to it and the old array disposed of. Naturally this
  takes time. In general, though, you wouldn't change a hash table's
  `IgnoreCase` property after creating the table.

### Declaration
    property TableSize : integer
#### Default
    53
#### Description
  The current number of slots in the table. This number is prime.

  The table is grown when it reaches 2/3 full (i.e., `Count` is 2/3 of
  `TableSize`) by approximately doubling its size. The table is shrunk
  when is reaches 1/6 full (i.e., `Count` is 1/6 of `TableSize`) by
  approximately halving its size.

  If you set this property, the container will force the number of
  slots to the nearest prime. So if you try and set the table size to
  100, say, the set method for the property will force the size to
  101, the nearest prime. This improves the efficiency of the hash
  table. If you set `TableSize`, then the hash table will reorganize its
  data objects and keys to use the new value. This requires a separate
  internal array to be allocated, all the keys and data objects
  transferred to it and the old array disposed of. 

  HINT: if you know that you'll be adding 1000 keys and data objects
  to a hash table, it will be more efficient to set the table size
  right at the start, rather than letting the table grow
  incrementally. Since `Count` cannot be greater than 2/3 `TableSize`, you
  need to set `TableSize` to 1500 when you create the hash table. Since
  there won't be any data objects in the table, this will be very
  fast, and you've ensured that you won't need to grow the table.


Interfaced methods
------------------

### Declaration
    constructor Create(DataOwner  : boolean); override;
#### Description
  Creates a new instance of a hash table. Calls the ancestor, passing
  a NodeSize of 0 -- the hash table does not use nodes. Sets the
  `HashFunction` property to the default `HashBKDR` routine. Sets the
  `IgnoreCase` property to false. Sets the `TableSize` property to 53, and
  allocates an array of that many slots.

### Declaration
    destructor Destroy; override;
#### Description
  Destroys the hash table by calling `Empty` and then disposing of the
  current array of slots.

### Declaration
    constructor Clone(Source : TAbstractContainer;
                      DataOwner : boolean; 
                      NewCompare : TCompareFunc); override;
#### Description
  Creates a clone of an existing hash table. The newly created hash
  table has the same values for the `HashFunction`, `IgnoreCase` and
  `TableSize` properties as the old. 

  NOTE: the `Compare` function is not used in hash tables so always pass
  nil for `NewCompare`.

### Declaration
    procedure Delete(const aKey : string);
#### Description
  Deletes the data object associated with the string `aKey`. The data
  object itself is not destroyed by this method, but the slot for its
  key is marked as deleted. 

  If the number of data objects in the hash table falls to below 1/6
  of `TableSize` as a result of calling this method, the table is shrunk
  to roughly half its size and the remaining data objects reorganized.

  If the key is not present, an exception (`escKeyNotFound`) is raised.

### Declaration
    procedure Empty; override;
#### Description
  Empties the hash table and marks all slots as unused. If the hash
  table is a data owner it will dispose of all the data objects as
  well. The hash table is not shrunk; after this method is called the
  `TableSize` property will have the same value as before.

### Declaration
    procedure Erase(const aKey : string);
#### Description
  Calls `Delete` to remove the string `aKey` from the hash table, and, if
  the container is a data owner, disposes of the data object.

  If the key is not present, an exception (`escKeyNotFound`) is raised.

### Declaration
    function Examine(const aKey : string) : pointer;
#### Description
  Returns the data object for the given string `aKey`. 

  If the key is not present, an exception (`escKeyNotFound`) is raised.

### Declaration
    procedure Insert(const aKey : string; aData : pointer);
#### Description
  Inserts a data object into the tree using the string `aKey`.

  If the key is already present in the hash table, an exception
  (`escInsertDup`) is raised.

  If the number of data objects in the hash table rises to above 2/3
  of `TableSize` as a result of calling this method, the table is grown
  to roughly twice its size and the data objects reorganized.

### Declaration
    function Iterate(Action : TIterator; Backwards : boolean;
                     ExtraData : pointer) : pointer;
#### Description
  Walks the hash table from the first slot (if `Backwards` is False) or
  from the last (if `Backwards` is True) and calls `Action` for each data
  object found.  If `Action` returns False for a data object, then
  `Iterate` immediately returns with that data object; if `Action` always
  returns True then this method will return nil.  `ExtraData` is a
  pointer to any other data that you may want `Action` to use.

  It is important to realize that the order in which you'll see the
  data objects is essentially random. Also you cannot 'see' the keys
  with this method.

### Declaration
    procedure THashTable.Join(HashTable : THashTable);
#### Description
  Joins a hash table to the current one. All of the data objects and
  keys in the `HashTable` container are moved over to the current one,
  and `HashTable` is freed afterwards.

### Declaration
    function THashTable.Search(const aKey : string; 
                               var aData : pointer) : boolean;
#### Description
  Searches for the string `aKey` in the hash table. If found, the method
  returns true, and the data object is returned in `aData`, If not
  found, the method returns false, and `aData` is set to nil.

  This is a better method to use than `Examine` in certain cases since
  it does not raise an exception if the key is not found.



TBooleanArray
----------------------------------------------------------------------
The `TBooleanArray` class encapsulates an array of boolean values.
Sometimes in the literature it is known as a bitset or a bit array, or
a bitmap. The main difference between the `TBooleanArray` and a simple
array [] of boolean is that the `TBooleanArray` manages to pack 8
boolean values per byte, one for each bit.

The class has the usual complement of methods and properties. You can
read, set, and toggle individual values; you can set all the boolean
values to true or false in one go (and toggle them all in one go, if
required). You can perform logical operations (ie, AND, OR or XOR) on
one boolean array using another. 

There is a set of iteration type methods as well. You can efficiently
locate the first true boolean (as well as the last, and the same goes
for false values). You can locate the next or prior, true or false
boolean value from a given boolean. Finally, you can iterate through
all the true (or false) values, either forwards or backwards.

There is also a method whereby you can switch the array the class is
working on to some exterior bitset, for example, a longint or a Delphi
set.


Properties
----------

### Declaration
    property Capacity : longint
#### Description
  The total number of booleans in the array.

  Reading the property returns the current number of boolean values
  being tracked in the array. 

  Writing to the property changes the number of values. In doing so,
  the size of the memory block being used to store all the boolean
  values may change: another one will be allocated on the heap, the
  current set of boolean values will be copied over - at least as many
  as will fit, and if the block has grown, the new boolean values are
  set to false - and then the old memory block is freed. If the
  current memory block is an external one (i.e., `SwitchArrays` was
  used), it is not freed of course.

### Declaration
    property Count : longint
#### Description
  READ ONLY. The number of true boolean values. 

  The number of false boolean values can be calculated by subtracting
  `Count` from `Capacity`.

### Declaration
    property Flag[aInx : longint] : boolean 
#### Description
  The array of boolean values.

  This property can be read or written to. The booleans in the array
  have indexes that start at zero; the final boolean has index
  `Capacity`-1. If `aInx` is out of range (i.e., is less than zero or
  greater than or equal to `Capacity`) an `EEZContainerError` exception is
  raised with code `escBadBooleanInx`. This array property is the
  default one for the class, so in general you can specify code like
  this:

     var
       MyBoolArray : TBooleanArray;
     ...
     MyBoolArray[42] := true;


Interfaced methods
------------------

### Declaration
    constructor Create(aCapacity : longint);
Descripition
  Create a new boolean array instance.

  `aCapacity` is the number of boolean values in the array. It can be
  zero, but be aware that in order to use the instance you must either
  switch to an external memory block (`SwitchArrays`) or set the
  `Capacity` property.

### Declaration
    destructor Destroy; override;
#### Description
  Destroy the boolean array.

  If the memory block being used as the repository for all the boolean
  values is internal (in other words you have not switched to an
  external memory block with `SwitchArrays`) then it will be freed.

### Declaration
    procedure AndArray(aArray : TBooleanArray);
#### Description
  ANDs another boolean array into this one.

  The other boolean array aArray cannot be nil, and must have the same
  `Capacity` as the current one. In DEBUG mode, an assertion error
  (`ascNilArray`) will occur for the former condition, (`ascNotSameSize`)
  for the latter.

### Declaration
    function FirstFalse : longint;
#### Description
  Returns the index of the first false boolean in the array. If there
  is none, -1 is returned instead.

### Declaration
    function FirstTrue : longint;
#### Description
  Returns the index of the first true boolean in the array. If there
  is none, -1 is returned instead.

### Declaration
    function Iterate(aAction    : TBooleanArrayIterator;
                     aValue     : boolean;
                     aBackwards : boolean;
                     aExtraData : pointer) : longint;
#### Description
  Iterates through the boolean values in the array.

  The iteration visits all true booleans only if `aValue` is true, or
  all false ones if `aValue` is false. The iteration is done backwards,
  starting from the end, if `aBackwards` is true. For each boolean that
  is equal to `aValue`, the routine `aAction` is called. This routine is
  of the form

     TBooleanArrayIterator = function(C : TBooleanArray;
                                      aIndex : longint;
                                      ExtraData : pointer) : boolean;

  and will be called with `C` equal to the boolean array instance,
  `aIndex` the element number of the boolean that triggered the call,
  and `ExtraData` is the aExtraData pointer passed to the `Iterate`
  function in the first place. The action routine returns true to
  continue the iteration, and false to stop it immediately. If the
  latter occurs, `Iterate` returns the index of the boolean for which
  `aAction` returned true, otherwise `Iterate` returns -1.

### Declaration
    function LastFalse : longint;
#### Description
  Returns the index of the last false boolean in the array. If there
  is none, -1 is returned instead.

### Declaration
    function LastTrue : longint;
#### Description
  Returns the index of the last true boolean in the array. If there is
  none, -1 is returned instead.

### Declaration
    function NextFalse(aFromInx : longint) : longint;
#### Description
  Returns the index of the next false boolean in the array, counting
  from the boolean just after the given index (in other words, the
  first boolean to be checked is at index `aFromInx`+1). If there is
  none, -1 is returned instead.

### Declaration
    function NextTrue(aFromInx : longint) : longint;
#### Description
  Returns the index of the next true boolean in the array, counting
  from the boolean just after the given index (in other words, the
  first boolean to be checked is at index `aFromInx`+1). If there is
  none, -1 is returned instead.

### Declaration
    procedure OrArray(aArray : TBooleanArray);
#### Description
  ORs another boolean array into this one

  The other boolean array `aArray` cannot be nil, and must have the same
  `Capacity` as the current one. In DEBUG mode, an assertion error
  (`ascNilArray`) will occur for the former condition, (`ascNotSameSize`)
  for the latter.

### Declaration
    function PrevFalse(aFromInx : longint) : longint;
#### Description
  Returns the index of the previous false boolean in the array,
  counting from the boolean just before the given index (in other
  words, the first boolean to be checked is at index `aFromInx`-1). If
  there is none, -1 is returned instead.

### Declaration
    function PrevTrue(aFromInx : longint) : longint;
#### Description
  Returns the index of the previous true boolean in the array,
  counting from the boolean just before the given index (in other
  words, the first boolean to be checked is at index `aFromInx`-1). If
  there is none, -1 is returned instead.

### Declaration
    procedure SetAllFalse;
#### Description
  Sets all booleans in the array to false.

### Declaration
    procedure SetAllTrue;
#### Description
  Sets all booleans in the array to true.

### Declaration
    procedure SwitchArrays(aNewArray   : PByteArray;
                           aCapacity   : longint);
#### Description
  Force the boolean array to use another memory block for its boolean
  values.

  `aCapacity` is the number of boolean values (or bits) in the new
  memory block. 

  Whereas the boolean array class will dispose of its own internal
  array memory block when required, if you assign an external memory
  block using this routine be aware that you are responsible for
  deallocating the external memory block (if required). The boolean
  array class never disposes of an external memory block.

  If you have switched arrays to an external memory block for a
  boolean array instance and you wish to switch back to an internal
  one, just set the `Capacity` property to something new.

  For example, to use a longint variable as the new memory block, do
  this:

     var
       MyBoolArray : TBooleanArray;
       MyLongint   : longint;
     ... 
     MyBoolArray.SwitchArrays(@MyLongint, 32);
     MyBoolArray.SetAllFalse; {clear all booleans}
     MyBoolArray[4] := true;  {set the 5th one to true}

### Declaration
    function Toggle(aInx : longint) : boolean;
#### Description
  Toggles the given boolean from false to true or vice versa. This is
  equivalent to the following code:

     Flag[aInx] := not Flag[aInx];

  If the index passed to the method is out of range (i.e., is less
  than zero, or is equal to or greater than `Capacity`) then an
  `EEZContainerError` exception is raised with code `escBadBooleanInx`.

### Declaration
    procedure ToggleAll;
#### Description
  Toggles all booleans in the array from false to true or vice versa.

### Declaration
    procedure XorArray(aArray : TBooleanArray);
#### Description
  XORs another boolean array into this one.

  The other boolean array `aArray` cannot be nil, and must have the same
  `Capacity` as the current one. In DEBUG mode, an assertion error
  (`ascNilArray`) will occur for the former condition, (`ascNotSameSize`)
  for the latter.



Random Number Generator
----------------------------------------------------------------------
EZDSL comes with a random number generator that is used by `TSkipList`.
Although not a container for anything (unless you view it as a
container of random numbers), and hence not really a part of EZDSL, it
might be useful in your own programming projects.

The generator differs from the standard Delphi one in two respects: 

- It is implemented as a class (`TEZRandomGenerator`); therefore you can
  have several independent random number generators in your
  applications.

- uses an algorithm that has a much longer cycle: 2^23 times
  longer. (Delphi's is 2^32, this one is 2^55.)

Furthermore, it runs nearly as quickly as the standard Delphi one.


Interfaced methods
------------------

### Declaration
    constructor Create;
#### Description
  Create a new generator. The generator is seeded from the system
  clock by calling `SetSeed(0)`.

### Declaration
    destructor Destroy; override;
#### Description
  Destroy the generator, internal tables are freed.

### Declaration
    procedure AcquireAccess;
#### Description
  Lock the generator in a multithreaded process. 

  If you are going to use a random number generator object across two
  or more threads, you must acquire exclusive access to the random
  number generator by calling this method before calling one of the
  random number methods. If you do not, severe memory corruption or
  access violations could result. You release exclusive access by
  calling `ReleaseAccess`.

  Generally if you are using a single generator across multiple
  threads, you'd call it like this:

     MyGen.AcquireAccess;
     try
       RandomNumber := MyGen.Random;
     finally
       MyGen.ReleaseAccess;
     end;

  However, my advice is don't: do not share a generator across
  threads. Two main reasons: it's slower, and repeatability of the
  random numbers would be impossible.

### Declaration
    function Random : double;
#### Description
  Return a random number in the range: 0.0 <= R < 1.0.

### Declaration
    function RandomByte : byte;
#### Description
  Return a random byte value in the range: 0 <= R < 256.

### Declaration
    function RandomWord : word;
#### Description
  Return a random word value in the range: 0 <= R < 65536.

### Declaration
    function RandomLong : longint;
#### Description
  Return a random longint value in the range: 0 <= R < 2,147,483,648.

### Declaration
    function RandomDWord : DWORD;
#### Description
  Return a random dword value in the range: 0 <= R < 4,294,967,296.

### Declaration
    function RandomIntLimit(aUpperLimit : integer) : integer;
#### Description
  Return a random integer in the range: 0 <= R < `aUpperLimit`.

  NOTE: no check is made to see whether `aUpperLimit` > 0.

### Declaration
    function RandomIntRange(aLowerLimit, 
                            aUpperLimit : integer) : integer;
#### Description
  Return a random integer in the range: `aLowerLimit` <= R <
  `aUpperLimit`.

  NOTE: no check is made to see whether `aUpperLimit` > `aLowerLimit`.

### Declaration
    function RandomFloatLimit(aUpperLimit : double) : double;
#### Description
  Return a random double in the range: 0.0 <= R < `aUpperLimit`.

  NOTE: no check is made to see whether `aUpperLimit` > 0.

### Declaration
    function RandomFloatRange(aLowerLimit, 
                              aUpperLimit : double) : double;
#### Description
  Return a random double in the range: `aLowerLimit` <= R < `aUpperLimit`.

  NOTE: no check is made to see whether `aUpperLimit` > `aLowerLimit`.

### Declaration
    procedure ReleaseAccess;
#### Description
  Unlock the generator in a multithreaded process, the lock having
  been acquired by `AcquireAccess`.

### Declaration
    procedure SetSeed(const aSeed : longint);
#### Description
  Reseed the generator from a known value.

  If `aSeed` is zero, the generator reseeds from the system clock. The
  `Create` constructor includes a call to `SetSeed` that does this.



Licensing
---------

Copyright (c) 1993-2015, Julian M Bucknall
All rights reserved.

Redistribution and use in source and binary forms, with or without 
modification, are permitted provided that the following conditions are 
met:

1. Redistributions of source code must retain the above copyright 
notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright 
notice, this list of conditions and the following disclaimer in the 
documentation and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its 
contributors may be used to endorse or promote products derived from 
this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT 
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR 
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT 
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, 
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT 
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, 
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY 
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE 
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

