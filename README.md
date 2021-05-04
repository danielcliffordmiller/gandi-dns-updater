gandi-dns-updater
=================

This is a simple script to call gandi's DNS api and update my dns entry if necessary.

It currently runs via roswell:
```
./dns-updater.ros
```
and uses `bot.whatismyipaddress.com` for figuring our your public facing ip.

It also includes systemd files for running on a timer (currently checking the ip-address every hour).
