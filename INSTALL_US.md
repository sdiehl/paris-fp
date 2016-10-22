Install Directions
==================

Linux
-----

**Stack**

On most Unix variants stack can be installed via the following shell script:

```bash
$ wget -qO- https://get.haskellstack.org/ > installer
$ sha256sum installer
$ bash installer
```

Stack Checksum: ``a651e631e2ce9b69259badac3d88a3b7278b0a5f626854cc62f2f0f13a32e4ba``

**LLVM**

The LLVM compiler toolchain can be installed via system package manager. On
Debian variants:

```bash
$ sudo apt-get install llvm3.5 llvm3.5-dev
```

Verify the install:

```bash
$ llvm-config --ldflags
-L/usr/lib/llvm-3.8/lib  -lpthread -lffi -ltinfo -ldl -lm
```

**Z3**

The Z3 theorem prover can be installed via system package manager. On Debian
variants:

```bash
$ sudo apt-get install z3 libz3-dev
```

Verify the install:

```bash
$ z3 -version
Z3 version 4.4.1
```

Mac OSX
-------

**Stack**

There are several ways to install Stack on OSX:

[Graphical Installer](https://www.stackage.org/stack/osx-x86_64)

or

```bash
$ brew install haskell-stack
```

**LLVM**

The LLVM compiler toolchain can be installed via brew formula:

```bash
$ brew install libffi llvm35
```

Verify the install:

```bash
$ llvm-config --ldflags
-L/usr/local/Cellar/llvm/3.9.0/lib -Wl,-search_paths_first -Wl,-headerpad_max_install_names
```

**Z3**

The Z3 theorem prover can be installed via brew formula:

```bash
$ brew install z3
```

Verify the install:

```bash
$ z3 -version
Z3 version 4.4.1
```

Haskell
-------

To build the project:

```bash
$ git clone https://github.com/sdiehl/paris-fp.git
$ cd paris-fp
$ stack setup
$ stack build
```
