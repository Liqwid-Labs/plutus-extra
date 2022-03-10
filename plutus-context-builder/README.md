# `plutus-context-builder`

## What is this?

A utility library for creating `ScriptContext`s with `Spending` or `Minting` 
`ScriptPurpose`. We provide a builder abstraction for assembling 
`ScriptContext`s; we use this library in `tasty-plutus`, but provide this 
functionality for use in other places as well.

## What can this do?

We currently support the following:

* Incremental construction of `ScriptContext`s, using a range of helper
  functionality.
* The ability to name, and modify, arbitrary sub-contexts if you wish.
* Low-level 'hooks' allowing definition of precise `ScriptContext` behaviour if
  required.
* Construction of `ScriptContext`s for both minting and spending.

## How do I use this?

To create a `ScriptContext`, use functions such as `outToPubKey`,
`inFromValidator`, `mintedValue`, and so on. These functions create anonymous
`ContextBuilder` parts, which describe various parts of the transaction from a
script point of view. If you wish, you can use `named` to name anonymous
`ContextBuilder`s, which can then be combined with other named
`ContextBuilder`s. We provide a monoidal API for both anonymous and named
`ContextBuilder`s to assist in this.

Once you have a finished `ContextBuilder`, you can use it to build a
`ScriptContext` using either `spendingScriptContext` or `mintingScriptContext`.

### What does 'script point of view' mean?

A `ScriptContext` is always passed to one specific _script_ (which can be a
_validator_ or a _minting policy_ in our case), for one specific _check_. 

For a validator, any transaction input can be classified as:

1. Input from the validator address, which is validated in the check;
1. Input from the validator address, which is **not** validated in the
   check;
1. Input from the address of any other script;
1. Input from the address of a wallet.

We can do the same with its transaction outputs:

1. Output to the validator address;
1. Output to the address of any other script;
1. Output to the address of a wallet.

Let's examine this classification, and its implications for the library, in more
detail. Inputs of type 3 and 4, as well as outputs of type 2 and 3, correspond
to the `SideUTXO` data type, and can be constructed by using `ToPubKey`,
`FromPubKey`, `ToOtherScript` and `FromOtherScript`, as well as helpers
operating on these. Inputs of type 2, as well as outputs of type 1, correspond
to the `ValidatorUTXO` data type, and can be built using the helpers
`inFromValidator` and `outFromValidator. Lastly, inputs of type 1 correspond to
the `TestUTXO` data type. Unlike the others, this is not part of the
`ContextBuilder` abstraction, and needs to be provided directly to one of the
functions that _consume_ a `ContextBuilder` (namely, `spendingScriptContext`).

For minting policies, we can also classify inputs and outputs in a similar
way:

1. Those which only contain tokens controlled by the minting policy;
1. Those which only contain tokens _not_ controlled by the minting policy;
1. Those which contain both.

Type 1 inputs and outputs are represented with helper functions using the
`Tokens` type. Type 2 inputs and outputs are represented with helper functions
_not_ using the `Tokens` type. Type 3 inputs and outputs require use of both.
Note that `TestUTXO` does not make sense for a `ScriptContext` designed for a
minting policy.

## What can I do with this?

The code is licensed under Apache 2.0; check the LICENSE.md for details.
