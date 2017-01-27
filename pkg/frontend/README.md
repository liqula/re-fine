# Re:Fine Frontend

This application is based on react-flux.

## Building

### For the first time

1. Install Node.js >= 6.x.x
1. cd pkg/frontend
1. npm install

### Each time

1. stack build
1. make

## Running the tests

1. stack test

## Running the App with Webpack's "watch mode":

1. If any dependencies in package.json were changed: npm install
1. npm start
1. Wait until you see something like "Listening at localhost:9090 proxying to http://localhost:8080"
1. Point your browser at http://localhost:9090
1. Wait until `webpack: bundle is now VALID.` appears (in the shell in which you ran "npm start")
1. Now you can see the application in the browser

The browser window will be refreshed whenever the local JavaScript changes.

## Creating Static Assets to be delivered by the Server

1. If any dependencies in package.json were changed: npm install
1. npm run build
1. Deliver `js-build/` from the backend.