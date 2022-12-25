<p align="center">
<img src="./static/ok.png" alt="okurl image" width="128" height="128" />
</p>

---
<p align="center">
<h3>
<a href="https://www.okurl.in">okurl.in</a>
</h3>
</p>

![Better Uptime Badge](https://betteruptime.com/status-badges/v1/monitor/5gtp.svg)

![MIT License badge](https://img.shields.io/github/license/safeurl/u) ![Github Issues badge](https://img.shields.io/github/issues/safeurl/u)

You can learn more about the service [here](https://www.okurl.in/about.html).

This service is deployed on [Heroku](http://heroku.com) using [heroku-buildpack-stack](https://github.com/mfine/heroku-buildpack-stack)

``` shell
heroku create safe-url \
  -b https://github.com/mfine/heroku-buildpack-stack
```

## local development

``` shell
export PORT=3000
export HCAPTCHA_SECRET=XXX
export REDIS_URL=redis://127.0.0.1:6379

stack build && stack exec safeurl
```

## useful heroku commands

``` shell
heroku login

# see logs
heroku logs --tail

# see dyno status and scale
heroku ps

# open terminal
heroku run bash

# see list of env variables
heroku config
heroku config:set MYSECRET=XXX

# heroku postgresql
heroku pg
heroku pg:psql

# see list of addons and state
heroku addons
heroku addons:docs <addon>

# open addon dashboard
heroku addons:open papertrail
heroku addons:open heroku-redis

# domain and cert info
heroku domains
heroku certs

# display the releases for an app
heroku releases

# rollback a release
heroku rollback <release_num>
```
