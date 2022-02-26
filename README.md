# Cachet

## Syntax highlighting

A syntax highlighting plugin for Vim/Neovim can be found in the `contrib/vim/` directory. It is
updated as the language evolves.

## Testing scripts

### Prerequisites

1. Clone this repository with the `-r` (recursive) flag, or run `git submodule init` / `git
   submodule update` to initialize submodules after cloning.
2. Make sure the `sponge` command from [moreutils](https://joeyh.name/code/moreutils/) is
   available in your `PATH`.
3. Install the [`dotnet` CLI](https://docs.microsoft.com/en-us/dotnet/core/tools/).
4. Build Boogie with `./scripts/build-boogie.sh`.
5. Build Corral with `./scripts/build-corral.sh`.

### Running the Cachet compiler

A helper script is available to compile Cachet source files from the `notes/` directory.

```
./scripts/compile.sh <name>
```

will compile `notes/<name>.cachet`, producing `out/<name>.h`, `out/<name>.inc`, and
`out/<name>.bpl`.

### Verifying the output

After compiling, samples can be verified with `./scripts/verify.sh`.

```
./scripts/verify.sh <name>
```

will run the Corral verifier on `out/<name>.bpl`. Note we're using a small
[fork](https://github.com/PLSysSec/corral/tree/cachet) of Corral, vendored in this repository under
`vendor/corral`.
