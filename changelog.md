# Revision history for waargonaut

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
