/-  sg=social-graph, uqbar=zig-uqbar,
    wallet=zig-wallet, indexer=zig-indexer
/+  smart=zig-sys-smart
/=  con  /con/lib/orgs
|%
::  set this to the deployed orgs contract
++  orgs-contract-id  ^-  id:smart
  0xb0f4.f607.41cf.4fac.3998.c923.cd1b.7132.f1b5.0d08.ee80.f00c.b23b.22b1.8b3c.c9c2
::
++  orgs-contract-town  ^-  id:smart
  0x0
::
+$  org-id  id:smart  ::  ID of an *item* containing org data
::
+$  orgs-receipt
  [=org-id =hash:smart =sequencer-receipt:uqbar]
::
+$  orgs-action  (pair address:smart action:con)
--
