::  lib wrapper to access usernames
::  
/-  *nimi
|% 
  ++  enjs-update
    =,  enjs:format
    |=  up=update
    ^-  json
    %+  frond  -.up
    ?-    -.up
        %ships
      %-  pairs
      %+  turn  ships.up
      |=  [s=@p p=(unit profile)]
      :-  `@tas`(scot %p s)
      %-  pairs
      :~  ::  ['ship' s+(scot %p ship)]
          ['name' ?~(p [%s ''] s+(scot %tas name.u.p))]
          ['uri' ?~(p [%s ''] s+(scot %tas uri.u.p))]
          ['item' ?~(p [%s ''] s+(scot %ux item.u.p))]
          ['address' ?~(p [%s ''] s+(scot %ux address.u.p))]
          ::  could do sig too.
      ==
        %ship
      %-  pairs
      :~  [%ship s+(scot %p ship.up)]
          [%name s+(scot %tas name.profile.up)]
          [%uri s+(scot %tas uri.profile.up)]
          [%item s+(scot %ux item.profile.up)]
          [%address s+(scot %ux address.profile.up)]
          ::  add others?
      ==
        %user
      %-  pairs
      :~  [%address s+(scot %ux address.up)]
          [%item s+(scot %ux item.up)]
          [%ship ?~(ship.up [%s ''] s+(scot %p u.ship.up))]
          [%name ?~(name.up [%s ''] s+(scot %tas u.name.up))]
          [%uri ?~(uri.up [%s ''] s+(scot %tas u.uri.up))]
      ==
        %no-user    :: fix ~
      (pairs ~[['s' %s '']])
    ==
  ++  dejs-action
    =,  dejs:format
    |=  jon=json
    ^-  action
    =<  (decode jon)
    |%
    ++  decode
      %-  of
      :~  
        [%whodis (se %p)]                     
        [%disme dejs-disme]    
        [%mint dejs-mint]
        [%set-profile dejs-setprofile]
        [%sign-ship (se %ux)]
        [%find-ships (ar (se %p))]
        [%tell-ships (ar (se %p))]
      ==
    ++  dejs-mint
      %-  ot
      :~  [%name so]
          [%uri so]
          [%nft (se %ux)]
          [%address (se %ux)]
          [%ship bo]
      ==
    ++  dejs-setprofile
      %-  ot 
      :~  [%item (se %ux)]
          [%address (se %ux)]
      ==

    ++  dejs-disme
      %-  ot
      :~  [%item (se %ux)]
          [%address (se %ux)]
          [%sig dejs-sig]
      ==
    ++  dejs-sig
      ^-  $-(json [v=@ r=@ s=@])
      %-  ot
      :~  [%v (se %ud)]
          [%r (se %ud)]
          [%s (se %ud)]
    ==
  --
--