# pt-helper

A PT (Private Tracker) client.

## Scheduled features

* Login a PT site with cookies.
* Monitor certain search pages (for example, free download/2x upload torrents).
* Add new torrents to the task queue of a remote Transmission server.
* Manage a disk quota, purge old/unpopular torrents.

## Deployment

1. Install [`stack`](http://docs.haskellstack.org/en/stable/README/).
2. Run `stack setup && stack install`. This will setup a Haskell build environment, build the `pt-helper` binary and copy it to the local bin path.
3. Run `pt-helper --help` for a brief documentation.

Special note for Chinese users: `stack` accesses `s3.amazonaws.com` to fetch build plans, and for obvious reasons, this means you may experience Internet connectivity issues. Prepare a GFW-bypassing proxy in that case.
