/-  pongo-pings
/+  *pongo, sig, io=agentio, nectar, *sss
::  keep things here that let us go from pongo 1.0 to 1.1
|%
+$  pending-ping
  $%  [%edit src=@p edit=@t]
      [%react src=@p =reaction]
  ==
+$  old-message-type
  $:  id=message-id
      author=@p
      signature=[%b p=ship-sig]
      timestamp=@da
      kind=old-message-kind
      content=@t
      edited=?
      reference=(unit message-id)
      reactions=[%m p=(map @p reaction)]
      mentions=[%s p=(set @p)]
      ~
  ==
+$  old-message-kind
  $?  %text  %code
      ::  in these kinds, message content is a `@t`(scot %p @p)
      %member-add     ::  in %open, anyone can send this, otherwise only leaders
      %member-remove  ::  in %open, only member leaving can send
      %change-name
      %leader-add     ::  only for %many-leader
      %leader-remove  ::  only for %many-leader
      %change-router  ::  TBD
      ::  special bespoke message types
      %poll         ::  content is "question being asked \n
                    ::  first response \n second response \n ..."
      %send-tokens  ::  content is a (scot %ux transaction-hash) (TODO)
      %app-link     ::  content is @t link (everything after ship URL)
  ==
::
+$  state-1
  $:  %1  ::  "deep state" (TODO get rid of) (done!)
      =database:nectar
      tagged=(map ?(@t path) conversation-id)  ::  conversations linked to %posse
      ::  "configuration state"
      =notif-settings
      invites=(map conversation-id [from=@p =conversation])
      invites-sent=(jug conversation-id @p)
      ::  "ephemeral state"
      undelivered=(map @uvH [old-message-type fe-id=@t want=(set @p)])
      pending-pings=(jar [conversation-id message-id] pending-ping)
  ==
::
++  transition
  =/  ping-sub  (mk-subs pongo-pings ,[%ping @ ~])
  =/  ping-pub  (mk-pubs pongo-pings ,[%ping @ ~])
  |_  =bowl:gall
  ++  part-one
    |=  old=state-1
    ::  what do we have to do here?
    ::  1. move conversation table into %nectar
    ::     (destroy the %last-active record)
    ::  2. move all messages tables into %nectar
    ::     (do NOT move those for *deleted* convos)
    ::     (make all these tables private, as well)
    ::  3. delete all other old state, except for notif-settings
    ::     (produce a state-2)
    ::  4. for all convos that *we route*, create a publication
    ::     (produce a du-ping)
    ::  5. for all other convos, subscribe to router SSS
    ::     (produce a da-ping)
    ::
    ::  then we're good?!
    =/  conversations-table=table:nectar
      =+  (~(got by database.old) [%pongo %conversations])
      -(records (~(del by records.-) ~[%last-active]))
    =/  convos=(map @ux conversation)
      %-  ~(gas by *(map @ux conversation))
      %+  turn
        (~(get-rows tab:nectar conversations-table) ~[%id])
      |=  =row:nectar
      =-  [id.- -]
      !<(conversation [-:!>(*conversation) row])
    ::
    =/  messages-tables=(list [@ux table:nectar])
      %+  murn  ~(val by convos)
      |=  convo=conversation
      ?:  deleted.convo  ~
      :+  ~
        id.convo
      %+  ~(gut by database.old)  [%pongo id.convo]
      :^    (make-schema:nectar messages-schema)
          primary-key=~[%id]
        (make-indices:nectar messages-indices)
      ~
    ::
    =/  nectar-cards=(list card:agent:gall)
    %+  welp
      :-  %+  ~(poke pass:io /make-table)
            [our.bowl %nectar]
          :-  %nectar-query
          !>  ^-  query-poke:nectar
          [%pongo %add-table %conversations conversations-table]
      %+  turn  messages-tables
      |=  [id=@ux mt=table:nectar]
      %+  ~(poke pass:io /make-table)
        [our.bowl %nectar]
      :-  %nectar-query
      !>  ^-  query-poke:nectar
      [%pongo %add-table id mt]
    :-  %+  ~(poke pass:io /make-private)
          [our.bowl %nectar]
        :-  %nectar-set-perms
        !>  ^-  set-perms:nectar
        [%pongo %conversations]^[%private ~]
    %+  turn  messages-tables
    |=  [id=@ux mt=table:nectar]
    %+  ~(poke pass:io /make-private)
      [our.bowl %nectar]
    :-  %nectar-set-perms
    !>  ^-  set-perms:nectar
    [%pongo id]^[%private ~]
    ::
    =/  state-2
      [%2 notif-settings.old]
    ::
    =/  du  (du pongo-pings ,[%ping @ ~])
    ::
    =.  ping-pub
      =<  q
      %^  spin  messages-tables  ping-pub
      |=  [[id=@ux mt=table:nectar] =_ping-pub]
      ~&  >  ping-pub
      =/  convo=conversation  (~(got by convos) id)
      :-  [id mt]
      ?.  =(our.bowl router.convo)
        ping-pub
      =/  path  [%ping id ~]
      =.  ping-pub
        +:(give:(du ping-pub bowl -:!>(*result:du)) path *ping)
      =.  ping-pub
        (rule:(du ping-pub bowl -:!>(*result:du)) path [0 1])
      %+  perm:(du ping-pub bowl -:!>(*result:du))
        path^~
      |=((unit (set @p)) `members.p.meta.convo)
    ::
    [nectar-cards [state-2 ping-pub]]
  ::
  ++  part-two
    |=  =_ping-sub
    ^-  (quip card:agent:gall _ping-sub)
    =/  da  (da pongo-pings ,[%ping @ ~])
    ::  for every conversation we have, if we are not the router,
    ::  sub to the router!
    =/  convos=(list conversation)
      %+  turn
        %^  nectar-scry  %conversations
          [%select %conversations %n ~]
        [our now]:bowl
      |=  =row:nectar
      !<(conversation [-:!>(*conversation) row])
    ::
    =-  [(zing p.-) q.-]
    %^  spin  convos  ping-sub
    |=  [convo=conversation =_ping-sub]
    ^-  [(list card:agent:gall) _ping-sub]
    ?:  =(our.bowl router.convo)
      [~ ping-sub]
    %:  surf:(da ping-sub bowl -:!>(*result:da) -:!>(*from:da) -:!>(*fail:da))
        router.convo  %pongo  [%ping id.convo ~]
    ==
  --
--