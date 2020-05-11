---
title: ''
author: "Bastien Trächsel"
date: "4/22/2020"
output:
  pdf_document: default
  html_document: default
---

### Virtual machine/server configuration

## Base configuration:
I would advise switching to your favorite ubuntu source for updates.

### Install SSH server (skip if already be installed)

SSH allows controlling your server.
You need ssh-server to control it remotely (port 22).

Open a terminal on the Ubuntu virtual machine and enter:
```
sudo apt update
sudo apt install openssh-server
sudo systemctl status ssh
sudo ufw allow ssh 
```

### fail2ban (skip if already be installed)

Fail2ban allows to block attacks on the ssh port.
It bans IP addresses after a desired number of attempts.

This will allow you to edit the configuration file and add:
```
sudo apt install fail2ban
sudo nano /etc/fail2ban/jail.local #to edit the configuration file

[sshd]
enabled = true
port = 22
filter = sshd
logpath = /var/log/auth.log
maxretry = 10
findtime = 120
bantime = 1200
```

```
sudo systemctl restart fail2ban
sudo fail2ban-client status sshd
```

Now you should be able to connect via putty to your machine but you need to know your IP (from the internet settings from your within your virtual machine or from your router's client list).

### Enabling the firewall

Allowing incoming ssh
```
sudo ufw allow ssh
sudo ufw enable
```

We will need to open the ports that we will use.

```
sudo ufw allow 80/tcp
sudo ufw allow 80/udp
sudo ufw allow 443/tcp
sudo ufw allow 443/udp
```
or we could open ranges & remove rules:

```
sudo ufw allow 3838:9000/tcp
sudo ufw delete allow 3838:9000/tcp
```
### Allowing root control via Winscp (optional but useful if connecting to sftp via Winscp)

First we need to allow no password sudo.

```
sudo visudo
```
and add the following line at the end (for pick):
```
pick  ALL=(ALL) NOPASSWD:ALL
```


In the advanced tab under SFTP:
```
sudo /usr/lib/openssh/sftp-server
```
Now, we can transfer files as SU.


## Installing R, Shiny

### Installing R (example for 20.04)

To install R, you need to add a repository and the corresponding key to authenticate the source.
The last line installs two dependencies for the R packages.
```
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
sudo add-apt-repository 'deb https://stat.ethz.ch/CRAN/bin/linux/ubuntu/ focal-cran40/'
sudo apt update
sudo apt install r-base
sudo apt-get install libssl-dev libcurl4-openssl-dev
sudo apt install libudunits2-dev
sudo apt install libxml2-dev
sudo apt install libgdal-dev
```

### Installing Shiny

First, we install the shiny package within R.
```
sudo SU - \
-c "R -e \"install.packages('shiny', repos = 'https://stat.ethz.ch/CRAN/')\""
```
 
Then, we install gdebi-core which is required for shiny and we download/install the last version of shiny-server from rstudio.
```
sudo apt-get install gdebi-core
wget https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.13.944-amd64.deb
sudo gdebi shiny-server-1.5.13.944-amd64.deb
```
It should be installed.

To add app, add its folder inside 
```
/srv/shiny-server/
```

It needs to have an app.R or interface&server part or a markdown.RMD.

To host Rmarkdown documents, you need to install the rmarkdown package within R.

To access your app you can type (if firewall allows it):
```
127.0.0.1:3838
```
Replacing 127.0.0.1 by your server's IP.
You will see an index of your folder/apps

## Securing your shiny-server connection using a proxy.

You need a domain / sub.domain name and you need it to redirect to your server.

The goal of this part is to allow you to encrypt the connection between the users and your shiny-apps.

We will use haproxy to take care of the SSL part and redirect the traffic to our shiny-server.
This basic task can also be done via apache. 

### Getting an SSL certificate

This step will enable you to get a certificate.
A third party will identify our server as being the one corresponding to the domain.
A free option is to use a Letsencrypt certificate.

## Getting the certificate
In this step, we need to run a program from the certificate authority that will put on the port 80 a webpage containing a string. 

The first step is to stop any services that use the port 80.

Then, we need to install&run the tool from our certificate provider.

```
sudo apt-get update
sudo apt-get install software-properties-common
sudo add-apt-repository universe
sudo add-apt-repository ppa:certbot/certbot
sudo apt-get update
sudo apt-get install certbot
sudo certbot certonly --standalone
```
Follow the steps and copy the locations of your new certificate and key.


### The simple but less versatile option: use apache as a https redirect

```
sudo apt-get install apache2

sudo a2enmod
	
sudo ssl proxy proxy_ajp proxy_http rewrite deflate headers proxy_balancer proxy_connect proxy_html
 
sudo nano /etc/apache2/sites-enabled/000-default.conf

sudo sudo service apache2 restart
	
sudo service apache2 restart

```

Edit the conf file and replace pick.internet-box.ch from


```
<VirtualHost *:*>
 
 SSLEngine on
 
 SSLCertificateFile /etc/letsencrypt/live/pick.internet-box.ch/fullchain.pem
 
 SSLCertificateKeyFile /etc/letsencrypt/live/pick.internet-box.ch/privkey.pem
 
 ProxyPreserveHost On
 
 ProxyPass / http://0.0.0.0:3838/
 
 ProxyPassReverse / http://0.0.0.0:3838/
 
 ServerName localhost
 
</VirtualHost>
```

It will redirect the https to your shiny app.    

You can close your firewall for the port 3838 as your app will now be accessible at:
```
https://subdomain.domain.com
```
https is using the port 443

### Installing haproxy: the simple and versatile option

This part will show an example configuration for the subdomain:
stat-cmb.ddns.net

First, add a repository and install haproxy. (not needed for 20.04)
Then, we put the key and certificate in one file used by haproxy.
```
sudo add-apt-repository ppa:vbernat/haproxy-2.0
sudo apt install haproxy
haproxy -v
sudo mkdir -p /etc/ssl/stat-cmb.ddns.net

sudo cat /etc/letsencrypt/live/stat-cmb.ddns.net/fullchain.pem \
    /etc/letsencrypt/live/stat-cmb.ddns.net/privkey.pem \
    | sudo tee /etc/ssl/stat-cmb.ddns.net/stat-cmb.ddns.net.pem
```
We edit the config file


```
sudo nano /etc/haproxy/haproxy.cfg
```

Haproxy works by collecting requests (front end) and then forwarding them to back ends.
It's the perfect tool for load balancing and it has a lot of options.


In this config file we proxy any requests on /COVID to /COVID19_1..11 to allow concurrent computations by the shiny app (one app(folder) can compute only 1 thing at a time even if this task can be //). We could also proxy requests to other servers.

We also redirect http requests to https.

Requests on stat-cmb.ddns.net are redirected to our page on the unisante website.


```
		
global
        log /dev/log    local0
        log /dev/log    local1 notice
        chroot /var/lib/haproxy
        stats socket /run/haproxy/admin.sock mode 660 level admin expose-fd listeners
        stats timeout 30s
        user haproxy
        group haproxy
        daemon

        # Default SSL material locations
        ca-base /etc/ssl/certs
        crt-base /etc/ssl/private

        # See: https://ssl-config.mozilla.org/#server=haproxy&server-version=2.0.3&config=intermediate
        ssl-default-bind-ciphers ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES128-GCM-SHA256:ECDHE-ECDSA-AES256-GCM-SHA384:ECDHE-RSA-AES256-GCM-SHA384:ECDHE-ECDSA-CHACHA20-POLY1305:ECDHE-RSA-CHACHA20-POLY1305:DHE-RSA-AES128-GCM-SHA256:DHE-RSA-AES256-GCM-SHA384
        ssl-default-bind-ciphersuites TLS_AES_128_GCM_SHA256:TLS_AES_256_GCM_SHA384:TLS_CHACHA20_POLY1305_SHA256
        ssl-default-bind-options ssl-min-ver TLSv1.2 no-tls-tickets
        tune.ssl.default-dh-param 2048

defaults
        log     global
        mode    http
        option  httplog
        option  dontlognull
        timeout connect 5000
        timeout client  50000
        timeout server  50000
        errorfile 400 /etc/haproxy/errors/400.http
        errorfile 403 /etc/haproxy/errors/403.http
        errorfile 408 /etc/haproxy/errors/408.http
        errorfile 500 /etc/haproxy/errors/500.http
        errorfile 502 /etc/haproxy/errors/502.http
        errorfile 503 /etc/haproxy/errors/503.http
        errorfile 504 /etc/haproxy/errors/504.http


frontend http-in
   bind *:80
   mode http
   http-request redirect scheme https

frontend stat-cmb.ddns.net
    bind *:443 ssl crt /etc/ssl/stat-cmb.ddns.net/stat-cmb.ddns.net.pem
    use_backend shiny if { path /COVID }
	use_backend cmb if { path / }
    default_backend shiny_out

backend cmb
redirect location https://www.unisante.ch/fr/formation-recherche/ressources-pour-recherche/soutiens-methodologiques/biostatistique
server unisante www.unisante.ch

backend shiny
        balance leastconn
        server COVID1 0.0.0.0:59910
        server COVID2 0.0.0.0:59592
        server COVID3 0.0.0.0:59593
        server COVID4 0.0.0.0:59594
        server COVID5 0.0.0.0:59595
        server COVID6 0.0.0.0:59596
        server COVID7 0.0.0.0:59597
        server COVID8 0.0.0.0:59598
        server COVID9 0.0.0.0:59599
        server COVID10 0.0.0.0:55910
        server COVID11 0.0.0.0:55911


frontend COVID1
	bind *:59910
	http-request redirect location /COVID19_1
	default_backend shiny_out
	
	frontend COVID2
	bind *:59592
	http-request redirect location /COVID19_2
	default_backend shiny_out
	
	frontend COVID3
	bind *:59593
	http-request redirect location /COVID19_3
	default_backend shiny_out
	
	frontend COVID4
	bind *:59594
	http-request redirect location /COVID19_4
	default_backend shiny_out
	
	frontend COVID5
	bind *:59595
	http-request redirect location /COVID19_5
	default_backend shiny_out
	
	frontend COVID6
	bind *:59596
	http-request redirect location /COVID19_6
	default_backend shiny_out
	
	frontend COVID7
	bind *:59597
	http-request redirect location /COVID19_7
	default_backend shiny_out
	
	frontend COVID8
	bind *:59598
	http-request redirect location /COVID19_8
	default_backend shiny_out
	
	frontend COVID9
	bind *:59599
	http-request redirect location /COVID19_9
	default_backend shiny_out
	
	frontend COVID10
	bind *:55910
	http-request redirect location /COVID19_10
	default_backend shiny_out
	
	frontend COVID11
	bind *:55911
	http-request redirect location /COVID19_11
	default_backend shiny_out
	
backend shiny_out
        balance roundrobin
        server COVID1 0.0.0.0:3838
        server COVID2 0.0.0.0:3838

```




Check config file and restart haproxy
And allow automatic startup when rebooting (editing the haproxy.service config file)
```
haproxy -c -f /etc/haproxy/haproxy.cfg
	
sudo systemctl restart haproxy

sudo systemctl enable haproxy.service
nano /lib/systemd/system/haproxy.service

[Unit]
After=network-online.target
```

### Specific useful commands

Pulling from github
```
rm -r /srv/shiny-server/COVID19
git clone https://github.com/kilou/COVID19.git /srv/shiny-server/COVID19
rm -r /srv/shiny-server/COVID19_1
rm -r /srv/shiny-server/COVID19_2
rm -r /srv/shiny-server/COVID19_3
rm -r /srv/shiny-server/COVID19_4
rm -r /srv/shiny-server/COVID19_5
rm -r /srv/shiny-server/COVID19_6
rm -r /srv/shiny-server/COVID19_7
rm -r /srv/shiny-server/COVID19_8
rm -r /srv/shiny-server/COVID19_9
rm -r /srv/shiny-server/COVID19_10
rm -r /srv/shiny-server/COVID19_11
cp -R --remove-destination /srv/shiny-server/COVID19 /srv/shiny-server/COVID19_1
cp -R --remove-destination /srv/shiny-server/COVID19 /srv/shiny-server/COVID19_2
cp -R --remove-destination /srv/shiny-server/COVID19 /srv/shiny-server/COVID19_3
cp -R --remove-destination /srv/shiny-server/COVID19 /srv/shiny-server/COVID19_4
cp -R --remove-destination /srv/shiny-server/COVID19 /srv/shiny-server/COVID19_5
cp -R --remove-destination /srv/shiny-server/COVID19 /srv/shiny-server/COVID19_6
cp -R --remove-destination /srv/shiny-server/COVID19 /srv/shiny-server/COVID19_7
cp -R --remove-destination /srv/shiny-server/COVID19 /srv/shiny-server/COVID19_8
cp -R --remove-destination /srv/shiny-server/COVID19 /srv/shiny-server/COVID19_9
cp -R --remove-destination /srv/shiny-server/COVID19 /srv/shiny-server/COVID19_10
cp -R --remove-destination /srv/shiny-server/COVID19 /srv/shiny-server/COVID19_11
```

