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



