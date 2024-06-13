# Elm Iddict

Sometimes, you want to store items efficiently in a dictionary, but you don't
know (or don't care) about what key to use, and you prefer to store the values
more efficiently than in a list.

The **Iddict** module in Elm lets you store values in a dictionary without
including a key. Then, you can refer to the item using its integer key. The
Iddict uses a counter, so keys will never collide - even after removing items.

## How to install

In your terminal, type

```sh
elm install noordstar/elm-iddict
```

And the library will be installed shortly!
