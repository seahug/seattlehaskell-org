# `seattlehaskell.org` web site

**This version of the SeaHUG web site has now been decommissioned!**

This is the [Yesod][yesod] source code for [seattlehaskell.org][seahug].

Source code for the Hakyll portion of the web site is [also
available][seahug-hakyll].

## Building and deploying

### Build

```bash
$ stack build
```

### Install runtime prerequisites

```bash
$ stack install yesod-bin cabal-install
```

### Run development server

```bash
$ script/devel
```

### Create Keter bundle

```bash
$ script/keter
```

### Helper scripts

Templates for helper scripts to run a local development server and to generate
an Apache2 configuration file, `devel.template` and `genconf.template`
respectively, are provided. For example, to generate a local Apache2
configuration file:

```bash
$ cp devel.template devel
$ chmod +x devel
$ # Edit devel script
$ ./devel
```

## Licence

Released under MIT License

[seahug]: http://seattlehaskell.org/
[seahug-hakyll]: https://github.com/seahug/seattlehaskell-org-static
[yesod]: http://www.yesodweb.com/
