#!/bin/bash

#Exercício 0
cat /etc/passwd
#root:x:0:0:root:/root:/bin/bash
#daemon:x:1:1:daemon:/usr/sbin:/usr/sbin/nologin
#bin:x:2:2:bin:/bin:/usr/sbin/nologin
#sys:x:3:3:sys:/dev:/usr/sbin/nologin
#sync:x:4:65534:sync:/bin:/bin/sync
#games:x:5:60:games:/usr/games:/usr/sbin/nologin
#man:x:6:12:man:/var/cache/man:/usr/sbin/nologin
#lp:x:7:7:lp:/var/spool/lpd:/usr/sbin/nologin
#mail:x:8:8:mail:/var/mail:/usr/sbin/nologin
#news:x:9:9:news:/var/spool/news:/usr/sbin/nologin
#uucp:x:10:10:uucp:/var/spool/uucp:/usr/sbin/nologin
#proxy:x:13:13:proxy:/bin:/usr/sbin/nologin
#www-data:x:33:33:www-data:/var/www:/usr/sbin/nologin
#backup:x:34:34:backup:/var/backups:/usr/sbin/nologin
#list:x:38:38:Mailing List Manager:/var/list:/usr/sbin/nologin
#irc:x:39:39:ircd:/run/ircd:/usr/sbin/nologin
#_apt:x:42:65534::/nonexistent:/usr/sbin/nologin
#nobody:x:65534:65534:nobody:/nonexistent:/usr/sbin/nologin
#systemd-network:x:998:998:systemd Network Management:/:/usr/sbin/nologin
#systemd-timesync:x:996:996:systemd Time Synchronization:/:/usr/sbin/nologin
#dhcpcd:x:100:65534:DHCP Client Daemon,,,:/usr/lib/dhcpcd:/bin/false
#messagebus:x:101:101::/nonexistent:/usr/sbin/nologin
#syslog:x:102:102::/nonexistent:/usr/sbin/nologin
#systemd-resolve:x:991:991:systemd Resolver:/:/usr/sbin/nologin
#ubuntu:x:1000:1000::/home/ubuntu:/bin/bash
#sshd:x:103:65534::/run/sshd:/usr/sbin/nologin
#goncalo:x:1001:1001:,,,:/home/goncalo:/bin/bash

cat /etc/group
#root:x:0:
#daemon:x:1:
#bin:x:2:
#sys:x:3:
#adm:x:4:syslog
#tty:x:5:
#disk:x:6:
#lp:x:7:
#mail:x:8:
#news:x:9:
#uucp:x:10:
#man:x:12:
#proxy:x:13:
#kmem:x:15:
#dialout:x:20:
#fax:x:21:
#voice:x:22:
#cdrom:x:24:
#floppy:x:25:
#tape:x:26:
#sudo:x:27:ubuntu,goncalo
#audio:x:29:
#dip:x:30:
#www-data:x:33:
#backup:x:34:
#operator:x:37:
#list:x:38:
#irc:x:39:
#src:x:40:
#shadow:x:42:
#utmp:x:43:
#video:x:44:
#sasl:x:45:
#plugdev:x:46:
#staff:x:50:
#games:x:60:
#users:x:100:goncalo
#nogroup:x:65534:
#systemd-journal:x:999:
#systemd-network:x:998:
#crontab:x:997:
#systemd-timesync:x:996:
#input:x:995:
#sgx:x:994:
#kvm:x:993:
#render:x:992:
#messagebus:x:101:
#syslog:x:102:
#systemd-resolve:x:991:
#ubuntu:x:1000:
#_ssh:x:103:
#goncalo:x:1001:

#Exercício 1
sudo adduser anibal
sudo adduser antonio
sudo adduser amilcar

#Exercício 2
sudo groupadd grupo-ssi
sudo groupadd par-ssi

sudo usermod -aG grupo-ssi anibal
sudo usermod -aG grupo-ssi antonio
sudo usermod -aG grupo-ssi amilcar

sudo usermod -aG par-ssi anibal
sudo usermod -aG par-ssi antonio

# Exercício 3
#Mudanças no ficheiro /etc/paswd (novos utilizadores)
#anibal:x:1004:1004:,,,:/home/anibal:/bin/bash
#antonio:x:1005:1005:,,,:/home/antonio:/bin/bash
#amilcar:x:1006:1006:,,,:/home/amilcar:/bin/bash

#Mudanças no ficheiro /etc/group (novos grupos )
#grupo-ssi:x:1002:anibal,antonio,amilcar
#par-ssi:x:1003:anibal,antonio
#anibal:x:1004:
#antonio:x:1005:
#amilcar:x:1006:

#Exercício 4
sudo chown anibal braga.txt

#Exercício 5
cat braga.txt 
#cat: braga.txt: Permission denied
#Não tem premissão para ler o ficheiro

#Exercício 6
su anibal

#Exercício 7
id
#uid=1004(anibal) gid=1004(anibal) groups=1004(anibal),100(users),1002(grupo-ssi),1003(par-ssi)
groups
#anibal users grupo-ssi par-ssi

#Pelo comando `id` verificamos que o user com o UID 1004 se encontra em 4 grupos (anibal, users, grupo-ssi e par-ssi)
#Pelo comando `groups` confirmamos o mesmo 

#Exercício 8
cat braga.txt 
#Texto sobre Braga

#Exercício 9
cd dir2
#bash: cd: dir2: Permission denied
