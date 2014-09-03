
SystemK Design Doc
==================

# Introduction

SystemK is a userspace process running with PID 0. It is the first process
invocated by the kernel in userspace mode and its duty during its life-time
is an infinite main-loop that spawns new userspace processes and manages their
respective life-time.

# Abstract Design

Userspace process daemons are prone to failure in a great number of ways. To
protect SystemK's critical data structures we model its behaviour and internal
mechanism in a pure way and wrap this up by lifting its operational semantics
into an IO () monad via IO () action primitives. These IO () action primitives
serve as the SystemK API.

A high-level view looks something like this:

'''

+---------------------------------+
|  IO Monad                       |
|  +---------------------------+  |
|  | KMonad = ReaderT + StateT |  |<------*
|  |   +-----------------+     |  |        \
|  |   | pure functional |     |  | ------>= Evil daemons
|  |   | data structures |     |  |
|  |   +-----------------+     |  |
|  +---------------------------+  |
+---------------------------------+

'''

# K Repository

At the pure core of SystemK is the service configuration repository,
which is cached in memory and stored on-disk in the form of JSON files.
The repository provides a persistent way to toggle services, a consistent
view of service states, and a unified interface to atomically manipulate
service configuration properties. The repository is ACID compliant and so
guarantees repository transactions are processed reliably and known good
configurations are always accessible. Configuration repository transactions
are mediated by the k.configd service via inter-process communication.

# K Restarter

Upon invocation, the SystemK main loop spawns a restart daemon thread
called k.startd. The k.startd daemon queries the K repository to define
the dependency chain of system services and invocate them in the
appropriate order. Each chain is started asynchronously with respect to
each entry-point service.

In addition to spawning services, the k.startd service keeps track of
state of services failures and dependency events. The k.startd service
uses inter-process communication to interact with k.configd as to become stateful.
It should be remarked here that the k.startd service itself is stateless,
represented as the IO Monad. This allows SystemK to recover from service or
service dependency failures, even critical failure in k.startd itself shall
not bring down the system, however manual intervention would be required to restart
it.


NOTES:

http://www.cns.nyu.edu/~fan/sun-docs/talks/practical-solaris10-security.pdf

http://home.mit.bme.hu/~meszaros/edu/oprendszerek/segedlet/unix/5_solaris/Service%20Management%20Facility%20%28SMF%29%20in%20the%20Solaris%2010%20OS.pdf

http://acid-state.seize.it/

http://acid-state.seize.it/safecopy

https://www.usenix.org/legacy/event/lisa05/tech/full_papers/adams/adams.pdf
