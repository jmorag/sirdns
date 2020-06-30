This packet is malicious or really malformed, and has a label pointer that
points to itself.

*What response is expected?*

The server should return a FORMERR. The server should not crash.

*What might go wrong?*

If you're following label pointers whereever they go, you'll follow a label
pointer here which points back to itself. Your unroll function will keep
following the label pointer until the stack overflows.

There are other ways to construct an infinite loop, such as having two pointers
which point to each other, or a pointer at the end of a list of valid labels
pointing back to the start. Each of these should be handled by your server. The
case here is particularly rough because even a server which will error if the
FQDN grows beyond the 255 character limit won't fail out on it (since the FQDN
length is never growing; the labels are all empty pointers).

There is an effective limit of 127 labels on a domain name (because there is the
255 character limit and only the root label can be empty). You can track the
number of labels you're reading on your stack and safely return a FORMERR if the
depth exceeds 127.
