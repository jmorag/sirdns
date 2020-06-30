This query is for MX records on gmail.com.

*What response is expected?*

You'll get five MX records from gmail.com. Each record should contain a priority
(`10`, `20`, `30`, `40`, and `50`) and an MX server hostname.

*What might go wrong?*

MX records are a little weird; like CNAMEs, they contain a label that needs to
be decompressed, but unlike CNAMEs, it's not the *only* thing they contain. If
your response looks weirdly formed, it's probably because you're not explicitly
handling label pointers for MX records.

This is a good point to look at *all* the different RRTYPEs you might have to
handle. There's a *lot* of them, but here's a list of the ones that occur pretty
commonly and should probably be handled by your server:

* A — IPv4 addresses. We've tested these.
* AAAA — IPv6 addresses. They're larger than A records, but otherwise similar.
* CNAME — DNS redirections. We've already tested these as well.
* MX — Mail eXchange records, used for email servers. That's this test!
* TXT — Text records containing arbitrary data. They're used for tons of things,
  but you can just pass them back as-is with no real processing.
* NS — Nameserver records. Another one with labels in the record data!
* PTR — Pointer records. These are used for reverse DNS lookup and basically
  only exist on in-addr domains. They contain labels.
* SOA — Start of Authority records. These are used to define a zone, which is
  the set of domains an authority is the authority for. They contain labels.

In addition, the DNSSEC `RRSIG` and EDNS `OPT` record will likely come up. We'll
look at EDNS in the next test!
