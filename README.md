# snaker-hsk

- Create the application:
> stack new snaker-hsk

- Install the `websockets` package:
> stack install websockets

- Add `websockets` to the `build-depends` section of the cabal file, for `Network.WebSockets`
- Add `containers` to the `build-depends` section of the cabal file, for `Data.Map`
- Add `text` to the `build-depends` section of the cabal file, for `Data.Text`
