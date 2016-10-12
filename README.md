# pong
A Ping Pong game - educational project

Well, it works, but is not suitable as a game, because the ball position is deterministic and is always predicted by the computer player.
Btw, did you know that in the original Atari Ping Pong there was a dead zone at the top raw, caused by a bag in TV? It was intentionally made unreachable for a player's paddle, so any player, no matter how skilled, could lose. 

How to build & run:

`cabal install random && cabal configure && cabal build && dist/build/Pong/Pong`
