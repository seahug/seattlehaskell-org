# SeaHUG web site

This is the source code for the web site of the [Seattle Area Haskell Users'
Group][1].

# Building and deploying

## Build

```bash
$ script/build
```

## Run development server

```bash
$ script/devel
```

## Create Keter bundle

```bash
$ script/keter
```

## Helper scripts

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

# Licence

Released under MIT License

[1]: http://seattlehaskell.org/

