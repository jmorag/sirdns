This time we're querying for a domain that doesn't exist at all!

*What response is expected?*

You should get a well formed response from your server that contains an NXDOMAIN
(no such domain exists) error code (RCODE 3). The answers section should be
empty and the authorities (nameservers) section should contain an SOA record for
the authoritative nameserver which controls the zone the domain *would* be in
(and thus can authoritatively say the domain doesn't exist).

*What might go wrong?*

A couple of things are easy mistakes here. First, treating any non-zero RCODE
from an authority as an error won't work; NXDOMAIN is a form of error, but our
recursive server needs to return the error, not fail out on it. Second, we need
to attach the SOA for the authority, since the authority is the one who actually
says that the domain definitively doesn't exist.
