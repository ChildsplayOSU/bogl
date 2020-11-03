# Installing BoGL with Spiel

### New Tutorial Location
These tutorials are out of date, and will be removed. The new location for all up to-date BoGL information will is [https://bogl.engr.oregonstate.edu/tutorials/](https://bogl.engr.oregonstate.edu/tutorials/).

Spiel, our application stack for the BoGL teaching language, is primarily designed to be used from a web interface, but is also capable of being worked on locally. We would recommend this only for developers who are familiar with the command prompt, a unix shell, and potentially a little bit of Haskell.

## Installation Video

You can watch [this video](https://media.oregonstate.edu/media/0_hxrt5f8y) for a quick tutorial on installing and running Spiel.

## Installation from Existing Package

We have a preconfigured macOS package suitable for testing purposes, you can download [our beta release here](https://github.com/The-Code-In-Sheep-s-Clothing/Spiel-Front/releases/download/beta3/spiel-front-0.1.0.dmg). Note, because it is unsigned you will need to open it via Cmd + Right Click, and select open from the options. If you don't macOS will complain about it not being a legitimate mac app.

A linux package is available, but not yet uploaded.

For Windows you will have to build from source.

In order to build the backend, please utilize the appropriate release script for the backend. For each platform it is as follows:

- macOS: release_tools/mac/release.sh
- linux: release_tools/linux/release.sh
- windows: release_tools/windows/release.sh

Notice that each of these locations contains the last pre-built **spielserver** binary for that platform if you wish to use that instead. Spiel was the codename of the development project that ultimately produced BoGL, and so it sticks around in some of the names still (in case you were wondering).

## Installation from Source
1. Install [Stack](https://docs.haskellstack.org/en/stable/README/), which is required for building the language and backend itself.
2. Install [npm](https://docs.npmjs.com/downloading-and-installing-node-js-and-npm) if you don't already have it.
3. Ensure that you are running a recent version of node by checking `node --version` against the [current node releases](https://nodejs.org/en/).
3. [Clone the Spiel Front project](https://github.com/The-Code-In-Sheep-s-Clothing/Spiel-Front/). This is the frontend for our language, and will automatically setup the backend (the langauge itself) for you. Without the frontend you will not have an interface by which to interact with the language.
4. From this project root, run `npm run setup` once. This will clone, build, and install the backend for you.
5. To start the front-end and back-end servers, you can run `npm run startProduction`. You should have a browser window open up, and you can select the Editor tab to start coding.
6. From here you can refer back to [our tutorials to get started](/Tutorials/GettingStarted).

## Learn More

You can learn more in the [Create React App documentation](https://facebook.github.io/create-react-app/docs/getting-started).

To learn React, check out the [React documentation](https://reactjs.org/).
