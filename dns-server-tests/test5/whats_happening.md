This query contains an EDNS OPT record. Most modern DNS tools do this by
*default*, and if you've been using `dig` or similar to test, you may have
already run into it.

*What response is expected?*

Just like in test 1, you want a well formed NOERROR reply with an A record or A
records for google.com.

Technically, a FORMERR is a *valid* response here (and in compliance with older
specs). A DNS client should retry a request without EDNS when it receives a
FORMERR from a server after transmitting an OPT record (and tools like `dig`
will alert you that this might be the case).

*What might go wrong?*

If your DNS server is trying to parse all the records it gets, you might error
on the OPT record being malformed. This is because OPT breaks the standard
assumption that the second field following the name is a CLASS. If you're
breaking CLASS out into an enum, you're probably checking that the value is 1
(INternet) or perhaps 3 (CHaos) and erroring on other class values.

OPT records use the CLASS field to store a UDP payload size instead. It's up to
you how you want to handle that special case!
