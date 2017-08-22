### rules (tenative)

- px and absolute distances are simple and therefore god.  do not use
  em, rem, % without a good reason.



### to work with the styleguide

install http://sass-lang.com/:

```
[as root] apt-get install ruby-dev
[as root] gem install sass
```

alternatively, if you don't want to install the ruby gem globally:

```
[as user] gem install --user-install sass  (follow the warning to update $PATH)
```

now start the scss compile loop:

```
cd pkg/frontend/scss-new && make
```

and direct your browser to the index page:

```
chromium pkg/frontend/styleguide/html/index.html
```
