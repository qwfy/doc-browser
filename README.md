# Doc Browser

An API documentation browser written in Haskell and QML.


## Screenshot

![Main Interface](asset/interface-annotated.png)


## Credits

This application is written by incomplete@aixon.co.

Many thanks to [Thibaut Courouble](https://github.com/Thibaut) and [other contributors](https://github.com/Thibaut/devdocs/graphs/contributors) of [DevDocs](https://github.com/Thibaut/devdocs), without their work, this application wouldn't be possible:

- This application ships with icons collected by DevDocs.

- This application uses docsets, along with the corresponding style sheets, produced by DevDocs.

The Hoogle support receives [helps](https://github.com/qwfy/doc-browser/issues/2) from [Neil Mitchell](https://github.com/ndmitchell), the author of Hoogle. This application also ships with a little code (modified) from the [Hoogle](https://github.com/ndmitchell/hoogle) project, the modified code is licensed under the BSD license.

For Hoogle support, this application guides user to download documentation archive from [Stackage](https://www.stackage.org).


## Implemented Features

- Native desktop application

- Works offline

- Near real-time fuzzy search

- Easy-to-type shortcuts

- Hoogle integration


## Planned (in no particular order)

- Persistent tabs across application restarts

- Docsets management

- DBus interface

- Configurable

- Display version number


## Current Status

It's in early stage, the main interface is in shape and usable, but other aspects, like installing docsets and configuration, lack polishing. That being said, I use this application every day.


## Installation

Currently, this application can only be installed from source, and only tested on Linux, and the installation process is pretty rough. This will be improved in future versions.

1. Install the font [Input Mono](http://input.fontbureau.com/), it is free for personal use. (In a later version you can specify the font you want to use)

2. This application uses [Qt 5](http://qt-project.org/), make sure you have it installed. You also need [Qt WebEngine](https://wiki.qt.io/QtWebEngine), install it if it doesn't come with your Qt installation, on Arch Linux, this is provided by the [extra/qt5-webengine](https://www.archlinux.org/packages/extra/x86_64/qt5-webengine/) package.

3. Install the Haskell tool [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/).

4. The build process depends on an executable called `c2hs`, you can install it using your package manager, or use the instruction below.

5. You may also need an executable called `happy`, if the build process reports that this is missing, install it in a similar manner with c2hs.

Finally, run these commands in the shell to build and install the application:

``` bash
git clone 'https://github.com/qwfy/doc-browser.git'
cd doc-browser

# optionally, install `c2hs` if not already installed:
# stack install c2hs

stack install

# optionally, install `happy` if the above command complains the lack of `happy`:
# and run `stack install` again after the installation
# stack install happy

echo "binary installed to $(stack path --local-bin)"
```

Note, due to a restriction of stack, you shouldn't delete the `.stack-work` directory inside the source code directory you just cloned after the build, for the installed binary still need to access files in it. If you really don't want to depends on this `.stack-wrok` directory, you can copy the `ui` directory of this repository to somewhere, say `/foo/ui`, and then start this application with `doc_browser_datadir=/foo doc-browser` instead of the usual `doc-browser`. This annoying situation will be handled when this application gets a packaging system for various operating systems.

If you have trouble building this application, you can:

- Is it a dependency problem?
- Does [this page](http://www.gekkou.co.uk/software/hsqml/) help? Especially the _Requirements_ and _Installation_ section.
- Open an issue.


### Install DevDocs' docset
To install DevDocs' docset, invoke:

```bash
doc-browser --install-devdocs DOC1 DOC2
# e.g. doc-browser --install-devdocs python haskell
```

This will download docsets from devdocs.io, and unpack them to `XDG_CONFIG/doc-browser/DevDocs`.

### Enable Hoogle Support

To support Hoogle, this application creates a Hoogle database from a documentation archive, which can be done by these steps:

1. Find out which documentation archive you want to use. The Hoogle support is tested on archives provided by [Stackage](https://www.stackage.org), like [this one](https://s3.amazonaws.com/haddock.stackage.org/lts-10.8/bundle.tar.xz), which can be found at: [https://www.stackage.org/lts-10.8](https://www.stackage.org/lts-10.8). You can use other archives as well, as long as the unpacked archive can be read by `hoogle generate --local=dir_to_unpacked_archive`, but this is untested.

2. Invoke the following installation command, this will unpack the archive to `XDG_CONFIG/doc-browser/Hoogle/NAME`, and creates a Hoogle database `XDG_CONFIG/doc-browser/Hoogle/NAME.hoo` for it, (doc-browser's Hoogle doesn't interfere with your system Hoogle in any way):

```bash
doc-browser --install-hoogle URL NAME
# e.g. doc-browser --install-hoogle 'https://s3.amazonaws.com/haddock.stackage.org/lts-10.8/bundle.tar.xz' lts-10.8
#
# URL is where to read the archive. It can also be a local file, which I suggest you to use if you have a bad network connection, since the download function included in this program is pretty savage at this stage.
#
# NAME shouldn't contain "/".
#
# See `doc-browser --help` for more
#
# Don't panic if you see a lot of lines like this at the begining and ending of the generation (for the above lts-10.8, there are 43 of these):
#
# temporarily rename /home/user/.config/doc-browser/Hoogle/lts-10.8/Decimal-0.4.2/LICENSE.txt to /home/user/.config/doc-browser/Hoogle/lts-10.8/Decimal-0.4.2/LICENSE.txt.__co.aixon.docbrowser-tempfile__
# move /home/user/.config/doc-browser/Hoogle/lts-10.8/Decimal-0.4.2/LICENSE.txt.__co.aixon.docbrowser-tempfile__ back to /home/user/.config/doc-browser/Hoogle/lts-10.8/Decimal-0.4.2/LICENSE.txt
#
# These are necessary to work around an Hoogle issue.
```

3. Prefix or suffix a search with "/hh" to query Hoogle, like this: `/hh[a]->Int->[a]`.

### Start the Application

```bash
doc-browser
```


## GUI

- When the application starts, you will see a blank screen, you can start typing to search.

- Press "Enter" to accept query string.

- Press one of "ASDFWERTC", or "G" + one of "ASDFWERTC", or "V" + one of "ASDFWERTC" to open a match.

- "j" to select next match, and "k" to select the previous one, and "Enter" to open.

- Press one of "1234567890" to go to the corresponding tab.

- "Alt+h" to go to the previous tab, "Alt+l" to go to the next tab.

- "Ctrl+w" to close the current tab.

- Press "/" to input query string.

- Prefix or suffix a search string with "/py", (eg. "/pyabspath", "abspath/py"), to limit the search to Python, more abbreviations are available, see file `src/Search.hs`, binding `shortcuts`.

- If there are less than 10 tabs, match will be opened in a new tab. If there are 10 tabs open, match will be opened at the current tab.


## FAQ

Q: Why does this application display at most 27 matches?

A: If your desired match is not in the top 27 matches, then there is probability something wrong with the search algorithm.


Q: Why does this application display at most 10 tabs?

A: If too many tabs are displayed, the tab title would be hard to see on many monitors. Instead of wanting more tabs, try open another instance of this application. (There is a restriction if you want to use multiple instances, namely, you should not close the first started one, for the documentation is served via a web server running in the first instance. This restriction will be removed in future versions). The number of maximum tabs will be configurable in future versions, so you can benefit from a large monitor.
