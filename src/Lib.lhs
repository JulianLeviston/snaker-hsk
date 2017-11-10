Snaker Server Library
=====================

This is the Haskell websockets server library for our Snaker game.

This is a 'literate haskell' file where comments and code are reversed. That is, normal writing is considered as comments by the compiler, and Haskell code needs to be indicated by using the ">" character.

Firstly, Haskell code may begin by indicating one or more `language pragma`, which are directives that tell the compiler to behave differently in some way. We're going to use one to let us write text type data as literal strings:

> {-# LANGUAGE OverloadedStrings #-}

Next up, we have the definition of our module (which is itself a bundle of definitions) and we name the definitions that it exports to the "outside world":

> module Lib
>     ( serverApp
>     )
>     where

Then, we'll make some imports.

We're using Jasper van der Jeugt's websockets server library to provide the websockets functionality:

> import qualified Network.WebSockets as WS

importing it qualified like this means we have to refer to its functions by prefixing them with "WS"

We'll also need a bunch of other imports, such as:

a module including a Map data type and functions for using it:

> import qualified Data.Map.Strict as DM

a module for dealing with text;

> import qualified Data.Text as T
> import qualified Data.Text.IO as T

Here we're overloading our qualified import T with the additional IO related functions from Data.Text.IO as well

and, a module that helps us with actions like IO actions:

> import Control.Monad (forM_, forever)

then a module that lets us work with a type of concurrency primitive:

> import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar, forkIO, threadDelay)

Now, we're going to set up some definitions for our data model.

Haskell files of code almost always each have:
- some language pragmas
- a module declaration, including what's being exported
- some imports
- a bunch of definitions

Things to note are:
* no definitions can do input or output, they can only describe it
* because of that, there is no sense of "linear compute time", when writing programs, except maybe in *do* blocks, and when functions or values rely on other functions or values being computed first for evaluation
* because of *that*, you can have forward or backward declarations just fine
* functions are definitions of relationships between values, not like in other languages where they are executed and can do practically anything
* most of what seems like special symbols are just operators, which are simply functions of two values, with a special syntax
* almost everything uses immutable data, it's lazily computed, functions are curried, and almost everything is an expression which means it can be evaluated
* all values, expressions and functions have a typed, and the compiler type checks them while compiling, which means it won't compile our code if we've put an expression or function of the wrong type in the wrong place. It doesn't remove all errors, but it does get rid of most of the common ones.

So...

We'll make a new name for Clients; this is a type alias, that is, we're giving a new name 'Clients' to a Map where the keys are ClientID and the values are Client; we'll define what those mean later:

> type Clients = DM.Map ClientID Client

Note that we're not defining a type here, just a new name for a type. That means we can now just write `Clients` and we'll get the same type as Map ClientID Client, but it doesn't mean it's a type error to use one instead of the other.

> type Apples = [Apple]

We'll also set up a type that describes the entire server state. This will be a list of Apples and the Clients

> data ServerState = ServerStateConstructor Clients Apples

Next we'll define a Client data type as a data structure consisting of a Connection, a Name, a Colour, and a Snake:

> data Client = ClientConstructor ClientID WS.Connection T.Text Colour Snake

This *does* create a new type, called `Client`. It also creates a function named `ClientConstructor` (very weird, but true) that can be used to create `Client` typed values, as well as be used to pattern match on them.

Now, we'll set up some functions for extracting fields from values of this type, and also for applying a function to create new updated versions of them:

> getClientID :: Client -> ClientID
> getClientID (ClientConstructor clientId _ _ _ _) = clientId

> alterClientID :: (ClientID -> ClientID) -> Client -> Client
> alterClientID f (ClientConstructor cid conn n col s) =
>   ClientConstructor (f cid) conn n col s

> getConnection :: Client -> WS.Connection
> getConnection (ClientConstructor _ conn _ _ _) = conn

> alterConnection :: (WS.Connection -> WS.Connection) -> Client -> Client
> alterConnection f (ClientConstructor cid conn n col s) =
>   ClientConstructor cid (f conn) n col s

> getName :: Client -> T.Text
> getName (ClientConstructor _ _ name _ _) = name

> alterName :: (T.Text -> T.Text) -> Client -> Client
> alterName f (ClientConstructor cid conn n col s) =
>   ClientConstructor cid conn (f n) col s

> getColour :: Client -> Colour
> getColour (ClientConstructor _ _ _ colour _) = colour

> alterColour :: (Colour -> Colour) -> Client -> Client
> alterColour f (ClientConstructor cid conn n col s) =
>   ClientConstructor cid conn n (f col) s

> getSnake :: Client -> Snake
> getSnake (ClientConstructor _ _ _ _ snake) = snake

> alterSnake :: (Snake -> Snake) -> Client -> Client
> alterSnake f (ClientConstructor cid conn n col s) =
>   ClientConstructor cid conn n col (f s)

These are pattern matching functions that simply extract the matched value out, and then give it back or give back an altered version of the Client.

The "::" bit is how we indicate the type of a function or value, and the _ bit means we're going to ignore that piece of the value, and match any value in its place but not use it at all in our function definitions.

There are easier and more concise ways to set this type up, but we'll use these for now, because they're helpful to learn with (some of them are *Record Types* and *Lenses*)

Now we'll define ClientID, Colour and Snake:

> type ClientID = Int
> type Colour = (Int, Int, Int)
> type Point = (Int, Int)
> type Snake = [Point]
> type Apple = (Point, Int) -- An apple has a point and a time to live

So we'd like a client to be able to connect to the server, giving us its name, and we'd like to send back the map of all the snakes. We'd also like the client to be able to send us control information which will inform our world-stepper what to broadcast to all the connected clients when the world steps its state forward.

Let's set up some initial state and some helper functions for managing the ServerState's content and interacting with clients:

Set up the initial server state:

> initialServerState :: ServerState
> initialServerState = ServerStateConstructor DM.empty []

Check if a client exists based on its ClientID:

> clientExists :: Client -> ServerState -> Bool
> clientExists client (ServerStateConstructor clients _) =
>   DM.member (getClientID client) clients

This function works by using two other functions: DM.member and getClientID:

1. DM.member: you can think of the type of DM.member as `k -> Map k a -> Bool` where `k` is a type-variable representing the type of the keys, and `a` is a type variable representing the values (though, the type of the function is actually Ord k => k -> Map k a -> Bool, which is the same, but constrains the k values such that they have to be only ORDerable types, that is, types that have a definition of some functions required to allow them to do ordering functions, which Data.Map needs to do its work)

2. getClientID, whose type is Client -> ClientID we defined ourselves above.

When we "specialise" the type of member to our DM.Map ClientID Client type, it becomes DM.member :: ClientID -> DM.Map ClientID Client -> Bool which means it'll happily fit together with our getClientID function applied to the passed in client.


Next we want to be able to add a client. Data.Map provides the insert function as follows: insert :: Ord k => k -> a -> Map k a -> Map k a

So, we'll use that to make a function to add a client:

> addClient :: Client -> ServerState -> ServerState
> addClient client (ServerStateConstructor clients apples) =
>     ServerStateConstructor newClients apples
>   where newClients = DM.insert (getClientID client) client clients

The where clause introduces a set of definitions that can be used in the expressions above.

Removing a client, using Data.Map's delete :: Ord k => k -> Map k a -> Map k a

> removeClient :: Client -> ServerState -> ServerState
> removeClient client (ServerStateConstructor clients apples) =
>     ServerStateConstructor newClients apples
>   where newClients = DM.delete (getClientID client) clients

Get a list of clients from the server state, we use the Data.Map elems function, which gets a list of the values from a Map. Providing these kinds of interface functions allow us to refactor much more easily later on, or change the internal design if we would like to, because they decouple the dependencies:

> getClients :: ServerState -> [Client]
> getClients (ServerStateConstructor clients _) =
>   DM.elems clients

Now we want a way to send a message to one client:

> sendTextDataToClient :: T.Text -> Client -> IO ()
> sendTextDataToClient message client =
>   let connection = getConnection client
>   in WS.sendTextData connection message

Then we want a way to use that function to send a message to _all_ of our clients:

> broadcast :: T.Text -> ServerState -> IO ()
> broadcast message (ServerStateConstructor clients _) = do
>   T.putStrLn message
>   let
>     sendMessageToClient :: Client -> IO ()
>     sendMessageToClient = sendTextDataToClient message
>   forM_ clients sendMessageToClient

The forM_ function takes a list of items as its first argument, and a function that can produce an IO () action as its second argument, then it evaluates to an action that uses each item of the list and passes it to its function argument, threading the resultant effect along as it does so, building up one big composed action as a result.

In this case, the effect we're describing is an IO () action that sends the message to the client. So we're sending that same message across all of our clients, which we're getting out of the `serverState` by using the getClients function we defined earlier.

Writing Haskell is like wiring together a state machine rather than instructing a machine what to do. You use functions, values and composition to wire it together, then you compile it and the Haskell compiler builds the program that contains the computer's instructions for you.


Ok, so next, serverApp is an IO action, which we build using a do block to create an initial server state inside a concurrency primitive, and then pass that to the websocketHandler function.

The <- syntax in the do block represents pulling a variable that represents a value out from the IO context so that we can then wire it into another expression. In this case, we're passing it to the websocketHandler function which we're then calling runServer on to start the server, bound to a particular IP address and port.

At this point, we need to fork another thread to run the game state stepper itself.

We should also mention that the ($) function is the low precedence function application operator. It takes the function to the left hand side of it, and applies it to the expression to the right hand side of it as a value. We use it to avoid having to wrap expressions to the right in parenthesis.

> serverApp :: IO ()
> serverApp = do
>   serverStateMVar <- newMVar initialServerState
>   forkIO $ gameWorldRunner serverStateMVar
>   WS.runServer "127.0.0.1" 3033 $ websocketHandler serverStateMVar

Next we'll define the gameWorldRunner, which is effectively where all of the game logic will reside.

> gameWorldRunner :: MVar ServerState -> IO ()
> gameWorldRunner serverStateMVar = do
>   putStrLn "Hey from the gameWorldRunner"
>   return ()

Next we'll define our websocketHandler function whose job it is to respond to websocket requests:

> websocketHandler :: MVar ServerState -> WS.ServerApp
> websocketHandler serverStateMVar pendingConnection = do
>   modifyMVar_ serverStateMVar (fmap return addApple)
>   state <- readMVar serverStateMVar
>   putStr "Hey from the websocketHandler"
>   putStrLn $ " - there are " ++ show (getApplesCount state) ++ " apples."
>   threadDelay $ (10^6) * 3 -- three seconds
>   modifyMVar_ serverStateMVar (fmap return removeApple)
>   stateAfter <- readMVar serverStateMVar
>   putStr "Goodbye from the websocketHandler"
>   putStrLn $ " - there are now " ++ show (getApplesCount stateAfter) ++ " apples."
>   return ()

The `WS.ServerApp` type is a synonym for `WS.PendingConnection -> IO ()`

> addApple :: ServerState -> ServerState
> addApple (ServerStateConstructor clients apples) =
>   ServerStateConstructor clients $ ((0,0), 0) : apples

> removeApple :: ServerState -> ServerState
> removeApple ss @ (ServerStateConstructor clients []) = ss
> removeApple (ServerStateConstructor clients (apple : apples)) =
>   ServerStateConstructor clients apples

> getApplesCount :: ServerState -> Int
> getApplesCount (ServerStateConstructor _ apples) = length apples

