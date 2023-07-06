# projects

Some of my projects with `Nix` flakes

## Prerequisites

- [Nix prerequisites](https://github.com/deemp/flakes/blob/main/README/NixPrerequisites.md)
- [Conventions](https://github.com/deemp/flakes/blob/main/README/Conventions.md)
- [Troubleshooting](https://github.com/deemp/flakes/blob/main/README/Troubleshooting.md)

## Projects

### Haskell

- [optics-by-example](./haskell/optics-by-example) - notes and code for the [Optics by Example](https://leanpub.com/optics-by-example) book
- [developers-roadmap](./haskell/developers-roadmap) - notes on `Haskell` based on the [backend developers roadmap](https://github.com/fullstack-development/developers-roadmap)
- [simplex-cheat](./haskell/simplex-cheat#readme) - an extremely simplified version of [simplex-chat](https://github.com/simplex-chat/simplex-chat) with bots talking to each other via a `servant` server
- [ts-serializable-test](./haskell/ts-serializable-test#readme) - a generator of test cases for the [ts-serializable](https://github.com/LabEG/Serializable) library.
    > Small library for deserialization and serialization for javascript and typescript
- [try-esqueleto](./haskell/try-esqueleto#readme) - a demo Haskell app that connects to a `PostgreSQL` database
- [sockets-and-pipes-notes](./haskell/sockets-and-pipes-notes) - (Unfinished) notes and code for the [Sockets and Pipes](https://leanpub.com/sockets-and-pipes) book
- [manager](./haskell/manager#readme) - (Unfinished) a `Haskell` CLI app for managing `stack` projects with multiple `Main.hs` modules

### Prolog

- [maze](./prolog/maze) - find a shortest path between to cells of a grid while avoiding particular cells.

### Blockchain

- [blockchain](./blockchain) - exercises for a `Blockchain` course

### Scala

- [scala](./scala) - notes on the Stepik Scala courses

### V language

- [vforces](./vlang/vforces/) - Solutions to `Codeforces` problems in [V](https://vlang.io/).

## Docs

```console
nix run .#genDocs
```

## References

- [Find non-UTF8 characters](https://stackoverflow.com/a/68205939)
