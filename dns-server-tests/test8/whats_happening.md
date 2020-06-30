This request has a CNAME, and the domain pointed to ultimately returns an
NXDOMAIN.

*What response is expected?*

I'm not totally sure! There's a couple of possibilities, though:

* You could just consider this a configuration error and return SERVFAIL or
  similar. This might be legal in theory, but it seems wrong in practice, since
  this is totally a *possible* configuration in the wild, even if it's not
  supposed to be.
* You can return an NXDOMAIN *and* put the CNAME record in the answers and the
  SOA for the domain looked up. This is what the mainstream resolvers I checked
  seem to do.

*What might go wrong?*

Technically a CNAME is a domain redirect and an NXDOMAIN is an error stating
that the domain queried for does not exist. In this case, the domain queried for
*does* exist, but the domain that its CNAME points to *does not*.

This begins to highlight some of the issues that happen with NXDOMAIN being in
the RCODE header instead of an answer for a specific question. This is part of
why most servers reject queries containing multiple questions (if one or more
doesn't exist, how would you specify which ones gave an NXDOMAIN).
