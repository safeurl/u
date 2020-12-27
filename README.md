# https://u.r4v1.net/

[![Better Uptime Badge](https://betteruptime.com/status-badges/v1/monitor/5gtp.svg)](https://betteruptime.com/?utm_source=status_badge)

You can learn more about the service [here](https://u.r4v1.net/about.html).

This service is deployed on Heroku using [heroku-buildpack-stack](https://github.com/mfine/heroku-buildpack-stack)

``` shell
heroku create safe-url \
  -b https://github.com/mfine/heroku-buildpack-stack
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
