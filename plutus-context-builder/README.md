# `plutus-context-builder`

## What is this?

A utility library for creating `ScriptContext` with `Spending` or `Minting` `ScriptPurpose`. This provides a builder abstraction for assembling ScriptContext, for use in `tasty-plutus`, or somewhere else.

## What can this do?

To create `ScriptContext`, you can use functions like `outToPubKey`, `inFromValidator`, `mintedValue` etc. These functions create named `ContextBuilder` parts, that describe various parts of the transaction from a script point of view.

Once you have a `ContextBuilder`, you can
* modify parts of it by accessing them by name 
* build `ScriptContext` using `spendingScriptContext` and `mintingScriptContext` functions

## What does 'script point of view' mean?

`ScriptContext` is always passed to one specific _script_ (_validator_ or _mintingpolicy_) for one specific _check_.

Thus any transaction input can be classified as:
1. input from the _validator_ address, validated in the _check_
2. input from the _validator_ address, not validated in the _check_
3. input from the address of any other script
4. input from the address of a wallet

The same with a transaction output. It can be classified as:
1. output to the _validator_ address
2. output to the address of any other script
3. output to the address of a wallet

Let's look at this classification in a little more detail.

Inputs `3` and `4`, as well as outputs `2` and `3` correspond to `SideUTXO`.
They can be represented by functions like `...ToPubKey...`, `...FromPubKey...`, `...ToOtherScript...`, `...FromOtherScript...`.

Inputs `2` and outputs `1` correspond to `ValidatorUTXO` and can be represented by functions `inFromValidator` and `outFromValidator`.

Input `1` corresponds to `TestUTXO`. It is not a part of the `ContextBuilder`. You have to provide a `TestUTXO` directly to one of the consumer functions (`spendingScriptContext` or `spendingScriptContextDef`).
__Note__ `TestUTXO` doesn't make sense in `ScriptContext` with `Minting` purpose.

If you create a `Minting` context for the _mintingpolicy_, you must also classify inputs and outputs as:
1. Which only contain tokens controlled by the _mintingpolicy_.
   Those inputs and outputs have to be represented by functions like `...Tokens...`.
2. Which don't contain tokens controlled by the _mintingpolicy_.
   Those inputs and outputs have to be represented by functions without `...Tokens...`.
3. Which contain tokes controlled as well as not controlled by the _mintingpolicy_.
   Those inputs and outputs have to be represented as a combination of `1` and `2`