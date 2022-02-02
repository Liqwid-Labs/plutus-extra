# `plutus-context-builder`

## What is this?

A utility library for cteating `ScriptContext` with `Spending` or `Minting` `ScriptPurpose`. That `ScriptContext` can be used with `tasty-plutus` or by itself.

## What can this do?

To create `ScriptContext`, you can use basic blocks like `outToPubKey`, `inFromValidator`, `mintedValue` etc. These blocks create named `ContextBuilder` parts, that describe various parts of the transaction from a script point of view.

Once you have a `ContextBuilder`, you can
* modify parts of it by accessing them by name 
* build `ScriptContext` with `spendingScriptContext` and `mintingScriptContext` functions