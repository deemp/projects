# Vforces

Hi! This repo contains a number of solutions to Codeforces problems written in V for demonstrational purposes. They're in `solutions` folder. Feel free to PR your solutions!

## Simple setup

1. Install **V** ([link](https://github.com/vlang/v#installing-v---from-source-preferred-method))
1. Symlink the compiler ([link](https://github.com/vlang/v#symlinking))
1. Open terminal with `Ctrl` + `Alt` + `T`
1. Clone this repository

    ```console
    git clone https://github.com/br4ch1st0chr0n3/vforces
    ```

1. Change current directory to `vforces`

    ```console
    cd vforces
    ```

1. Now, run. This will re-run your code on changes

    ```console
    v watch -s -c run ./solutions/1661D/_test.v 0
    ```

    Where `0` is the test number. Add more tests and list ones that you want to run, e.g. `0 1 2`
1. Tests should be called

* `test.in` - test input
* `test.ans` - correct answer

1. Try to edit `solutions/1661D/sol.v` in your favorite editor. Tests will reload in the terminal
1. Please note, this is just a demo setup, not an optimal way to test your solutions!

## Setup

I use VS Code with an awesome **acmX** [extension](https://marketplace.visualstudio.com/items?itemName=marx24.acmx) on Ubuntu 20.04. Here's their [GitHub](https://github.com/mfornet/acmx)

1. Install **V** ([link](https://github.com/vlang/v#installing-v---from-source-preferred-method))
1. Symlink the compiler ([link](https://github.com/vlang/v#symlinking))
1. Get Competitive Companion for [Chrome](https://chrome.google.com/webstore/detail/competitive-companion/cjnmckjndlpiamhfimnnjmnckgghkjbl) or [Firefox](https://addons.mozilla.org/ru/firefox/addon/competitive-companion/).
1. In browser, go to
    * (*Chrome*)
        * Extensions -> Competitive Companion -> Options
        * In `Custom ports`, type `10042`
    * (*Firefox*)
        * Add-ons and themes -> Competitive Companion -> Preferences
        * In `Custom ports`, type `10042`
1. Now, it will send the parsed problems to the port `10042`
1. Install VS Code ([link](https://code.visualstudio.com/))
1. Open terminal with `Ctrl` + `Alt` + `T`
1. Clone this repository

    ```console
    git clone https://github.com/br4ch1st0chr0n3/vforces
    ```

1. Open it in VS Code

    ```console
    code vforces
    ```

1. Go to Extensions (looks like blocks in the sidebar)
1. Type `V`, press, on it, select one with 11K+ installs, click `Install`
1. Type `acmX`, press on it, click `Install`
1. While on this extension page, click on the `gear icon` -> `Extension settings`
1. Scroll down until `Acmx › Template: Solution Path`
1. Paste `$HOME/.acmx/templates/sol.v` there. This will be the path to our template solution
1. Now, open terminal via `Ctrl` + `Shift` + `` ` ``
1. Copy the template `sol.v` from this repository into `~/.acmx/templates/sol.v`

    ```console
    mkdir -p ~/.acmx/templates/ && cp sol.v ~/.acmx/templates/sol.v
    ```

    * Of course, you can edit this template as you wish
1. Let's add the configuration file for `V`:

    ```console
    cp vlang.json ~/.acmx/languages/
    ```

    * Next time, if you want to edit it, open Command Palette with `Ctrl` + `Shift` + `P`, then type `acmx edit`, then choose `vlang`
    * IDK why but they need all commands to be split into separate strings.
1. Time to run our solutions! Open a Codeforces problem, say [1661D](https://codeforces.com/problemset/problem/1661/D)
1. Parse problem with Competitive Companion extension
1. VS Code opens with full setup!
1. Open `sol.v` there
1. Copy code from this repository, from `solutions/1661D/sol.v` into this newly opened `sol.v`
1. While in `sol.v`, open Command Palette with `Ctrl` + `Shift` + `P`, type `ACMX: Run`
1. Tests passed!

## binary-heap-benchmark

This is a benchmark for [@MikeMirzayanov](https://github.com/MikeMirzayanov)'s [binary-heap-benchmark](https://github.com/MikeMirzayanov/binary-heap-benchmark)

* See [simple setup](README.md#simple-setup) and then run to auto-reload the solution on code changes

    ```console
    v watch -s -c -prod run solutions/binary-heap-benchmark/sol.v
    ```

* Running 10 times on an array with 1e7 elements shows `Done on average in 738.40 ms`
* The solution is [here](./solutions/binary-heap-benchmark/sol.v)
* Compile the program to check its syntax

    ```console
    v solutions/binary-heap-benchmark/sol.v
    ```

## Troubleshooting

* See acmX [documentation](https://github.com/mfornet/acmx#documentation)
* For now, V doesn't have [scanf](https://github.com/vlang/v/discussions/14086), but you can create your workarounds, considering the specifics of Codeforces.
