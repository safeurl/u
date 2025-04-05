<p align="center">
<img src="./static/ok.png" alt="okurl image" width="128" height="128" />
</p>

---
<p align="center">
<h3>
<a href="https://www.okurl.in">okurl.in</a>
</h3>
</p>

![MIT License badge](https://img.shields.io/github/license/safeurl/u) ![Github Issues badge](https://img.shields.io/github/issues/safeurl/u)

You can learn more about the service [here](https://www.okurl.in/about.html).


## local development

``` shell
export PORT=3000
export CAPTCHA_SECRET=XXX
export REDIS_URL=redis://127.0.0.1:6379

stack build && stack exec safeurl
```

## useful commands

[Setup testing captcha keys](https://developers.cloudflare.com/turnstile/troubleshooting/testing/)

[acme.sh deploy BuyPass.com CA](https://github.com/acmesh-official/acme.sh/wiki/BuyPass.com-CA)

``` shell

# issue cert

acme.sh --server https://api.buypass.com/acme/directory --issue -d okurl.in -d www.okurl.in -w /home/ravi/wwwroot/okurl.in --days 170 --nginx

# install cert
acme.sh --install-cert -d okurl.in -d www.okurl.in --cert-file /etc/nginx/certs/okurl/cert --key-file /etc/nginx/certs/okurl/key --fullchain-file /etc/nginx/certs/okurl/fullchain --reloadcmd "systemctl reload nginx.service"
```
