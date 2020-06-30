This packet has an error in it! The label length is incorrect, and the server
will run into an issue processing it.

*What response is expected?*

The server should return a FORMERR.

*What might go wrong?*

If the server doesn't handle cases where a packet ends while still trying to
read a label or a label is otherwise unreadable because the length values are
corrupted or maliciously constructed, the server might crash without returning
an error.
