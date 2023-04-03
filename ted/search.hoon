/-  spider, pongo
/+  *strandio, nectar, pongo
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
=/  search  !<(search:pongo arg)
^-  form:m
;<  =path   bind:m  take-watch
::
::  TODO: turn this into a loop that progressively searches deeper into history
::
;<  our=@p   bind:m  get-our
;<  now=@da  bind:m  get-time
;<  ~  bind:m
  %-  send-raw-card
  :^  %give  %fact  ~[path]
  :-  %update
  !>  ^-  pongo-update:pongo
  [%search-result (do-search:pongo search our now)]
::
;<  ~  bind:m
  (send-raw-card [%give %kick ~[path] ~])
::
(pure:m !>(~))