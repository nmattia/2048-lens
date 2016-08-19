# 2048 - Exercice de style

This is a small game mimicking [2048](https://gabrielecirulli.github.io/2048/).
There are three big differences:

 * You decide where the numbers appear
 * It is terminal based
 * There are no points

It is not particularly fun, nor useful. This was an exercise/proof of concept.

## Building

Using stack, run `stack build`.

## Playing

To start the game, run `stack exec 2048-lens` (or use
[giak](http://hackage.haskell.org/package/giak)). The board looks like this:

```
X X X X

X X X X

X X X X

X X X X
```

In order to insert numbers, enter the following:

```
[row] [col] [value]
```

and hit `<return>`. `[row]` and `[col]` can be one of the following: `x`, `y`,
`z`, `w`. `[value]` must be an integer, like `256`. For instance:

`x w 2`

will transform the board as follows:

```
X X X 2

X X X X

X X X X

X X X X
```

Then, enter either `h`, `j`, `k` or `l` (followed by `<return>`) to move the
board around like in the original game.

## But why though?

I though it would be nice to define the update function for a single row, and
be able to apply it anywhere, in any direction, in the game matrix. The
[linear](https://hackage.haskell.org/package/linear) package allows you to do
just that.
