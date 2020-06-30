This first test is pretty straightforward! We're sending out a query for A
records for google.com.

*What response is expected?*

You should get a well formed response from your server that contains an A record
for google.com. The exact IPv4 address will differ.

*What might go wrong?*

In order to properly respond here, you'll probably need to read compressed
labels. Specifically, the response answer likely contains a label pointer to the
"google.com" in the query, instead of a fully qualified domain name.

Beyond that, just about anything, since this is a pretty basic case.

*How can I tell what response I'm getting?*

It's dumped to `response`, but you'll need to manually deconstruct that packet
to understand it. It's probably a lot easier for now to just do this query with
dig:

`dig @127.0.0.1 -p 5300 google.com A +noedns`

Change `127.0.0.1` and `5300` to be the host and port for your DNS server.
