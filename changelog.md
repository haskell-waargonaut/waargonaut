# Revision history for waargonaut

## 0.8.0.1  -- 2019-09-22

* Support GHC 8.8.1

* Support generics-sop 0.5

## 0.8.0.0  -- 2019-09-04

* Add `onObj'` which is just `onObj` but specialised to `Identity`.

* Add `gObjEncoder` for deriving `ObjEncoder` structures for record types only. Using the
  `IsRecord` constraint from `record-sop` package. This makes it easier to leverage the
  Contravariant functionality of the `ObjEncoder` without losing the benefits of deriving
  more trivial encoders.

* Added the `FieldNameAsKey` option to the newtype options for generic derived enc/decoders.

* Fixes #69 by removing duplicate call to `_optionsFieldName` function. Added regression test.

* Improved the handling of newtype options for generic deriving to give a bit more
  flexibility and avoid strangeness with respect to some combinations of options.

* Change the building of escaped whitespace chars to actually use the
  `escapedWhitespaceChar` function, instead of incorrectly generating an unescaped
  character.

* Add haddock to gObjEncoder function

* Correctly bump version to 0.8.0.0 as this is a breaking change because of new
  constructors on an exported sum type.

* Remove some commented out code.

* Add a better failure message to "impossible" error case.

* Regenerate nix after cabal file changes

## 0.6.1.0  -- 2019-02-27

* Add `passKeysToValues` decoder for decoding JSON objects where the key should
  be part of the value.

## 0.6.0.0  -- 2019-02-19

#### Fixes

* Handling of HeXDigit4 values was not correct. The bug was partly due to the
  choice of optic, instead of producing a (type/failure) error when working with
  mixed-case hex values, it seems to be zero'ing them out.
* Added regression tests

#### Rework

* Redesigned ParseFn to handle:
  * Data.String.String
  * Data.Text.Text
  * Data.ByteString.ByteString
* Updated documentation for ParseFn to match changes
* Updated documentation for default parsing functions
* Generalised the Builder process to handle Text and ByteString
  * Created a record type to hold the required functions for builders
  * Created submodules to house the generalised builders (see Waargonaut.Encode.Builder and friends)
  * Added test to ensure both builders produce identical output
* Updated documentation for Encode process to match changes
* Added deprecation notice to `Waargonaut.Decode.Traversal`

#### Cleanup

* Factored out components into more submodules:
  * UnescapedJChar
  * EscapedJChar
  * HexDigit4
  * Elem
  * Elems
  * JAssoc
  * Decode.Runners
* Updated documentation if required for module changes.
* Deleted commented out code
* Changed all file textual encoding/decoding tests to Test.Tasty.Golden.

#### New hotness

* Added a few prisms to allow for similar behaviour to the lens-aeson package.
* Added property tests for these new prisms to check they comply with the prism law.

## 0.5.2.1  -- 2019-01-08

* Upgraded the nix overrides to use the overlay technique.
* Lowered the bound on tagged to 0.8.5 which allowed it to be removed from the list of overridden packages.

## 0.5.2.0  -- 2019-01-03

* Add `Waargonaut.Prettier` module that contains a traversal to modify a `Json` structure to add indentation and newlines.

## 0.5.1.0  -- 2019-01-02

* Fix order of `either` decoder to match documentation, `Right` decoder was not being attempted first.
* Expose functionality to check the 'type' of the JSON at the current cursor position.
* Update list decoder to check that we're at an array before attempting to decode. It will now fail when attempting to decode a list and something of type list is not found. Previously it would return an empty list when a number was present.

## 0.5.0.0  -- 2018-12-18

* Changed internal builder from `ByteString` to `Text` builder.
* Fixed bug for going from `JString` <-> `Text`, was breaking round-trip.
* Removed instances of `AsJString` for `Text` and `ByteString`, replaced with more correct `Prism` and some better functions.
* Added regression tests for round tripping text and bytestring (char8).

## 0.4.2.0  -- 2018-11-29

* Improved pretty printing of CursorHistory by condensing multiple numeric movements and removing the single movements following searching for keys.
* Add `fromKeyOptional` and `atKeyOptional` that make it easier to handle optional keys on objects.
* Add `prismDOrFail'` function to allow the user to construct an error from the value that was decoded.

## 0.4.1.0  -- 2018-11-20

* Add `oneOf` decoder and tests

## 0.4.0.0  -- 2018-11-19

* Redesign & rebuild of `Encoder` internals to allow for greater control and flexibility
* Factor our law tests into their own module (a recheck of these tests is needed)
* Fixed bug in `list` and `nonempty` decoders
* Fixed bug in `foldCursor` function
* Fixed bug in `Cons` instance for `CommaSep`
* Fixed bug in documentation for `atKey`
* Added `_MapLikeObj` `Prism`
* Added some optics into object / maplikeobj keys
* Fixed bug in `maybeOrNull` decoder to be more strict in what it accepts
* Rewrote `either` decoder in terms of the alternative instance to allow for better errors

## 0.3.0.0  -- 2018-11-14

* Change to use the `natural` package for `Natural` numbers.

## 0.2.1.0  -- 2018-11-13

* Add `MonadError` and `Alt` instance for `Decoder`
* Add property tests for the typeclass laws for `Encoder` and `Decoder`
* Removed need for `MonadError` constraint on `prismDOrFail`

## 0.2.0.2  -- 2018-11-12

* Fix `Applicative` instance for `Decoder`.

## 0.2.0.1  -- 2018-11-07

* Update `moveToKey` to record a successful movement to a key, before continuing

## 0.2.0.0  -- 2018-11-06

* Provide more precise errors from Decoder for missing or invalid keys
* Removed a parameter from `KeyDecodeFailed` error constructor
* Fix issue where printing the zipper movements had left and right movement arrows swapped.

## 0.1.0.0  -- 2018-11-01

* First version. Released on an unsuspecting world.
