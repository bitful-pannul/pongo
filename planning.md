##  purely peer to peer org-comms

PONGO:
  Peers Only Naively Gossipping Online

set of ships in a channel is determined by (set ship) in org stored on-chain

so, everyone always has an up-to-date representation of who to send messages to

therefore, you can directly poke each channel member with your messages --
you can also receive all messages directly from their sender.

(if you have a comms issue with a single ship in the group, you should be able to
ask another peer to forward them to you?)

ordering is not well-regulated by default -- messages can reach participants
in different orders, resulting in confusion

vector clock-esque solution:

existing messages:
[1 2 3 4 5 6]

user A posts message 7 by including the ID of message they last saw (6)
user B posts message 7 in the same way, having not seen user A's message yet





##  decentralized wager system

PONGO:
  Putting Our Natural Graph On-chain

one ship creates a "wager" and requests all ships with a certain tag in their PONGO network to respond with an "outcome"

```hoon
+$  wager
  :-  desc=@t
  $%  [%value @]
      [%choice (list wager)]
      [%complex *]
  ==
::
+$  outcome
  $%  [%value @]
      [%choice @]  ::  index in %choice list
      [%complex *]
  ==
```

when a ship is requested to respond, it is given a push notification

the UI allows user to see the source of the wager and the description+structure of the wager

the user indicates an outcome and signs it as a ring signature, with set of ships being the tagged group in PONGO, then gossips ring signature to group -- signers build on gossiped ring



