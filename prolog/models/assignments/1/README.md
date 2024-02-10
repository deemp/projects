# Home Assignment 1

[Assignment.pdf](./Assignment.pdf)

## Theory

[Solution.pdf](./Solution.pdf)

## Prolog

- [a](src/a.pl)

    ```sh
    swipl -s src/a.pl
    ```

    ```prolog
    main(3).
    ```

    ```console
    State 0:
    |21
    |
    |

    State 1:
    |2
    |1
    |

    State 2:
    |
    |1
    |2

    State 3:
    |
    |
    |21

    true
    ```

- [b](src/b.pl)

    ```sh
    swipl -s src/b.pl
    ```

    ```prolog
    main([3,4,4,5], [3,4,2], X).
    ```

    ```prolog
    X = [3, 4].
    ```

- [c](src/c.pl)

    ```sh
    swipl -s src/c.pl
    ```

    ```prolog
    main([3,4,4,5]).
    ```

    ```prolog
    false
    ```

    ```prolog
    main([3,4,4,3]).
    ```

    ```prolog
    true
    ```
