#### Running the service

In order to run the service locally, both the backend and the web
server need to be running.

#### The backend

The backend can be started as it follows:

    cd pkg/backend/
    stack build --fast && rm -rf .backend-data && stack exec -- refine

the server is running on the port 3000

#### The static web server on the built assets

There are two ways of building and running the frontend: production
mode and dev mode.

Dev mode builds *much* faster, prod mode runs much faster. In
particular, if you're in dev mode, firefox will choke on the 8MB
javascript blob and you will have to use chrome/chromium.

In dev mode, you run webpack to serve everything you need with
reload-on-file-change. This works better in vanilla-js-react projects
where the changed code can be injected into the running app most of
the time. In our case, at least we get this feature for the code under
pkg/frontend/{jsbits/,scss/,images/}.

the way you run in dev mode is as follows:

```
cd pkg/frontend
stack build --fast #seems to be necessary
make all
npm start
```

You also need to have the backend running on port 3000 as written
above. Now you can point your (fastest) browser to
localhost:9090. Registration and login works also on your local
instance, have fun playing with Refine!
