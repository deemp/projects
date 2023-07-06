# Intro2AI [S21] course assignment

## Description

The goal is to find an optimal path from a start to a home and avoid infected cells.

* Full problem [statement](./Statement.pdf).

* Here's an example run.

    ![run](run.png)

* I used the cartesian coordinates

* Meaning of letters
  * `h` - home
  * `D` - doctor
  * `s` - start
  * `I` - infected cells
  * `C` - Covid

## Run

* [Install](https://wwu-pi.github.io/tutorials/lectures/lsp/010_install_swi_prolog.html) SWI-Prolog.
  * Alternatively, [install Nix](https://github.com/deemp/flakes/blob/main/README/InstallNix.md) and use a devshell from this flake.

    ```console
    nix develop
    ```

* Run an example in [main.pl](./main.pl).

  ```sh
  swipl main.pl
  ```

* Change the search method and grid size.

  ```sh
  random_run(dfs, 10, 10).
  ```

## Examples

### Commands

Defined in [game.pl](./game.pl).

* `run(Method, Dimensions, Agents)` - run game for given method, dimensions, and agents
* `random_run` - run game for given method, dimensions, and randomly generated agents.
* `Agents = [Doc, Mask, Covid1, Covid2, Home]`

### "Impossible" maps

Maps where there's no way from `Start` to `Home`.

* Map 1

    ```console
    run(bfs,[10,10],[[5, 5], [2, 9], [2, 3], [4, 1], [1, 2]]).
    ..........
    .M........
    ..........
    ..........
    ..........
    ....D.....
    III.......
    ICI.......
    hIIII.....
    s.ICI.....
    ```

* Map 2

    ```console
    run(bfs,[10,10],[[5, 5], [2, 9], [1, 3], [3, 2], [1, 2]]).
    ..........
    .M........
    ..........
    ..........
    ..........
    ....D.....
    II........
    CIII......
    hICI......
    sIII......
    ```

* Map 3

    ```console
    run(bfs,[10,10],[[9, 10], [9, 10], [10, 8], [7, 10], [10, 10]]).
    .....ICIDh
    .....IIIII
    ........IC
    ........II
    ..........
    ..........
    ..........
    ..........
    ..........
    s.........
    ```
