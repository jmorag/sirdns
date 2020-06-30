Unlike in the first test, here we're querying for A records on a domain with a
CNAME record (www.dylnuge.com).

*What response is expected?*

You should get a well formed response from your server that contains a CNAME
record for www.dylnuge.com and an A record for the domain it redirects to
(dylnuge.netlify.com).

CNAMEs are effectively DNS redirects; when they exist on a domain name they're
the only record that exists for that name and they tell you to go look somewhere
else for the result. A recursive resolver should see a CNAME, make the lookup
again for the new name, and build a response that contains the A record
eventually found and any CNAMEs encountered along the way.

*What might go wrong?*

The most common issues here are just returning the CNAME without doing a
subsequent A record lookup, returning the A record response but not the CNAME
that redirected there in the packet sent back, and not doing label decompression
properly on the CNAME response. Add other issues you encounter to this list!
