# DNS Server Tests

**What is this?**

This is a set of test cases to look at if you're building a DNS recursive
resolver. It includes weird queries, weird answers, and other things your server
might want to handle.

**How is it organized?**

Each test is in its own folder that contains a couple of files:

* `README.md` will tell you how to run the test. It doesn't give too many details
  on what the test is doing (on purpose)!
* `whats_happening.md` has everything. What's going on in the test, what the
  expected behavior should be. My intent is that you check it *after* you think
  you've got a solution for the behavior in the test, but do whatever helps you
  learn the best!
* `run_test.sh`, where it exists, is a script to run the test by sending a
  packet against a DNS server.
* `packet`, where it exists, is the packet contents itself. DNS packets aren't
  printable binary, so you'll probably want to open it in a hex editor or
  examine it with a tool like Wireshark!

The test folders are just numbered in the order I wrote them, which is somewhat
arbitrary but tries to be in increasing "trickyness." Tests 1-5 are
straightforward and don't do anything too tricky with DNS. Tests after that are
a bit more sinister.

**Will it work in my DNS server written in Rust/Haskell/Clojure/Shakespeare?**

These tests all treat the DNS server as a black box, so they should be fully
language agnostic.

**How do I know if the test passed?**

On a first pass, you might not! Some of the fun of debugging these is breaking
down what's happening and determining what behavior *should* occur. The README
will explain if the test has a somewhat obscure pass condition or if there's a
particular part of your server to pay attention to when running the test. For
others, failures will be pretty clear (e.g. not return a reply or crash the
whole server).

You can always look in the `whats_happening` file if you're unsure!

**Can I use these against a production server?**

Yes! If you don't control that server, you should probably check with the
administrator of it first, though. Some of these tests have the possibility of
crashing servers.

**What work is still todo on this?**

I'm going to organize the tests better than just `test1`, `test2`, and so on. I
also want to add support to the test run script for showing the response
received (and maybe even validating it) at some point.

And there's a lot more things I want to test.
