/-  *pongo, uqbar=zig-uqbar, wallet=zig-wallet,
    nectar, table-updates
/+  verb, dbug, default-agent, io=agentio,
    *pongo, *sss, sig
|%
::
::  pongo is currently tuned to auto-accept invites to new conversations.
::  it's also set up to automatically allow ships to join an *open*
::  conversation if they have the ID.
::  these can be turned off with a few small changes.
::
::  if the conversation has this many members or less,
::  we'll track delivery to each recipient.
++  delivery-tracking-cutoff  5
::  arbitrary limits for some measure of performance guarantees
++  message-length-limit      1.024
::
::  %pongo agent state
::
+$  state-2
  $:  %2
      ::  "deep state" = ELIMINATED
      ::  "configuration state"
      =notif-settings
      invites=(map conversation-id [from=@p =conversation])
      invites-sent=(jug conversation-id @p)
      ::  "ephemeral state"
      pending-pings=(jar [conversation-id message-id] [src=@p =ping])
  ==
+$  card  card:agent:gall
--
::
^-  agent:gall
%+  verb  &
%-  agent:dbug
::  SSS declarations
=/  updates-sub  (mk-subs table-updates ,[%updates @ @ ~])
::
=|  state=state-2
=<  |_  =bowl:gall
    +*  this  .
        hc    ~(. +> bowl)
        def   ~(. (default-agent this %|) bowl)
        da-sub
      =/  da  (da table-updates ,[%updates @ @ ~])
      (da updates-sub bowl -:!>(*result:da) -:!>(*from:da) -:!>(*fail:da))
    ::
    ++  on-init
      :_  this(state [%2 ['' '' %low] ~ ~ ~])
      ::  produce our conversations table
      ::  this will fail if it already exists, and that's okay!
      ::  TODO create our stored procedures here!
      :_  ~
      %+  ~(poke pass:io /make-table)  [our.bowl %nectar]
      :-  %nectar-query
      !>  ^-  query-poke:nectar
      :^  %pongo  %add-table  %conversations
      ^-  table:nectar
      :^    (make-schema:nectar conversations-schema)
          primary-key=~[%id]
        (make-indices:nectar conversations-indices)
      ~
    ::
    ++  on-save  !>([state updates-sub])
    ::
    ++  on-load
      |=  =vase
      ^-  (quip card _this)
      ?:  =(%0 -.q.vase)  on-init
      ?:  =(%1 -.q.vase)  on-init
      =/  old  !<([=state-2 =_updates-sub] vase)
      `this(state state-2.old, updates-sub updates-sub.old)
    ::
    ++  on-poke
      |=  [=mark =vase]
      ^-  (quip card _this)
      ?:  ?=(%sss-table-updates mark)
        =^  cards  updates-sub
          (apply:da-sub !<(into:da-sub (fled vase)))
        [cards this]
      =^  cards  state
        ?+    mark  (on-poke:def mark vase)
            %ping
          (handle-ping:hc !<(ping vase))
            %entry
          (handle-entry:hc !<(entry vase))
            %pongo-action
          (handle-action:hc !<(action vase))
            %wallet-update
          (handle-wallet-update:hc !<(wallet-update:wallet vase))
            %uqbar-share-address
          (handle-address-share:hc !<(share-address:uqbar vase))
            %sss-on-rock
          =/  msg  !<(from:da-sub (fled vase))
          ?-    -.msg
              [%updates @ @ ~]
            ::  TODO this is where we produce updates for frontend
            ::  based on incoming messages/reactions/etc!!!
            `state
          ==
        ==
      [cards this]
    ::
    ++  on-peek   handle-scry:hc
    ::
    ++  on-watch
      |=  =path
      ^-  (quip card _this)
      ?>  =(src our):bowl
      ?+    path  ~|("watch to erroneous path" !!)
      ::  path for frontend to connect to and receive
      ::  all actively-flowing information. does not provide anything
      ::  upon watch, only as it happens.
        [%updates ~]  `this
      ::  path for frontend to receive search results.
      ::  subscribe before poking %search with matching uid
        [%search-results @ ~]  `this
      ::  path for token-send thread to receive info about a token send
        [%token-send-updates ~]  `this
      ==
    ::
    ++  on-agent
      |=  [=wire =sign:agent:gall]
      ^-  (quip card _this)
      ::  SSS wires
      ::
      ?:  ?=([~ %sss %on-rock @ @ @ %updates @ @ ~] wire)
        ?>  ?=(%poke-ack -.sign)
        ?~  p.sign  `this
        =.  updates-sub  (chit:da-sub |3:wire sign)
        `this
      ?:  ?=([~ %sss %scry-request @ @ @ %updates @ @ ~] wire)
        ?>  ?=(%poke-ack -.sign)
        ?~  p.sign  `this
        =^  cards  updates-sub  (tell:da-sub |3:wire sign)
        [cards this]
      ::  other wires
      ::
      ?+    -.wire  (on-agent:def wire sign)
          %send-tokens-thread
        ?+    -.sign  (on-agent:def wire sign)
            %fact
          ?+    p.cage.sign  (on-agent:def wire sign)
              %thread-fail
            =/  err  !<((pair term tang) q.cage.sign)
            %-  (slog leaf+"send-tokens thread failed: {(trip p.err)}" q.err)
            `this
          ==
        ==
      ::
          %thread
        ?+    -.sign  (on-agent:def wire sign)
            %fact
          ?+    p.cage.sign  (on-agent:def wire sign)
              %thread-fail
            =/  err  !<((pair term tang) q.cage.sign)
            %-  (slog leaf+"search thread failed: {(trip p.err)}" q.err)
            `this
              %update
            ::  forward updates along search results path
            =/  tid  -.+.+.wire
            =/  upd  !<(pongo-update q.cage.sign)
            :_  this
            (fact:io pongo-update+!>(upd) ~[/search-results/[tid]])^~
          ==
        ==
      ==
    ::
    ++  on-arvo
      |=  [=wire =sign-arvo]
      ^-  (quip card _this)
      ?+    wire  `this
          [~ %sss %behn @ @ @ %updates @ @ ~]
        [(behn:da-sub |3:wire) this]
      ==
    ++  on-leave  on-leave:def
    ++  on-fail   on-fail:def
    --
::
|_  bowl=bowl:gall
++  handle-ping
  |=  =ping
  ^-  (quip card _state)
  !!
::
++  handle-entry
  |=  =entry
  ^-  (quip card _state)
  ?:  ?=(%invite -.entry)
    ::  we've received an invite to a conversation
    =+  [src.bowl conversation.entry]
    :_  state(invites (~(put by invites.state) id.conversation.entry -))
    :~  ::  remove this to turn off auto-accept
        %+  ~(poke pass:io /accept-invite)  [our.bowl %pongo]
        pongo-action+!>(`action`[%accept-invite id.conversation.entry])
      ::
        (give-update [%invite conversation.entry])
    ==
  ?~  conv=(fetch-conversation conversation-id.entry)
    ::  we got pinged for a conversation we don't know about
    ::  be optimistic and request an invite!
    ?<  =(our src):bowl
    ?<  ?=(%invite-request -.entry)
    ~&  >>  "%pongo: trying to re-join missing convo..."
    :_  state  :_  ~
    %+  ~(poke pass:io /request-invite-for-missing-convo)
      [src.bowl %pongo]
    entry+!>(`^entry`[%invite-request conversation-id.entry])
  =*  convo  u.conv
  ?-    -.entry
      %accept-invite
    ::  an invite we sent has been accepted
    ::  create a message in conversation with kind %member-add
    ?>  (~(has ju invites-sent.state) id.convo src.bowl)
    :_  state(invites-sent (~(del ju invites-sent.state) id.convo src.bowl))
    =/  hash  (make-message-hash (scot %p src.bowl) [our now]:bowl)
    :_  ~
    %+  ~(poke pass:io /accept-invite)
      [router.convo %pongo]
    :-  %ping
    !>  ^-  ping
    :+  %message  id.convo
    :*  *message-id
        our.bowl
        signature=[%b (sign:sig our.bowl now.bowl hash)]
        now.bowl
        %member-add
        (scot %p src.bowl)
        [%.n ~ [%m ~] [%s ~] ~]
    ==
  ::
      %reject-invite
    ::  an invite we sent has been rejected
    ~&  >>  "%pongo: {<src.bowl>} rejected invite to conversation {<id.convo>}"
    `state(invites-sent (~(del ju invites-sent.state) id.convo src.bowl))
  ::
      %invite-request
    ::  someone wants to join one of our conversations
    ::  if we don't have ID, or convo is not FFA, reject
    ::  otherwise send them an invite! (can remove this)
    ?>  ?=(?(%open %dm) -.p.meta.convo)
    :_  state(invites-sent (~(put ju invites-sent.state) id.convo src.bowl))
    :_  ~
    %+  ~(poke pass:io /send-invite)
      [src.bowl %pongo]
    entry+!>(`^entry`[%invite convo])
  ==
::
++  apply-pending-pings
  |=  [=message cid=conversation-id]
  ^+  message
  =/  pending=(list [src=@p =ping])
    %-  flop  ::  want to apply newest last
    (~(get ju pending-pings.state) [cid id.message])
  |-
  ?~  pending  message
  =.  message
    ?+    -.ping.i.pending  message
        %edit
      ?.  =(src.i.pending author.message)  message
      message(edited %.y, content edit.ping.i.pending)
        %react
      =+  [src.i.pending reaction.ping.i.pending]
      message(p.reactions (~(put by p.reactions.message) -))
    ==
  $(pending t.pending)
::
::  +valid-message-contents: as a router node, verify that messages which
::  adjust the "leadership structure" of the groupchat are performed by
::  those with the privilege to do so.
::
++  valid-message-contents
  |=  [=message convo=conversation]
  ^-  ?
  ?.  (gth id.message last-message.convo)  %.n
  ?+    kind.message  %.y
      %member-remove
    ?:  =(author.message (slav %p content.message))  %.y
    ?-  -.p.meta.convo
      ?(%open %dm)  %.n
      %managed      (~(has in leaders.p.meta.convo) author.message)
    ==
  ::
      ?(%member-add %change-name)
    ?-  -.p.meta.convo
      ?(%open %dm)  %.y  ::  this is right, sadly
      %managed      (~(has in leaders.p.meta.convo) author.message)
    ==
  ::
      %leader-add
    ?-  -.p.meta.convo
      ?(%open %dm)  %.n
      %managed      (~(has in leaders.p.meta.convo) author.message)
    ==
  ::
      %leader-remove
    ?-  -.p.meta.convo
      ?(%open %dm)  %.n
        %managed
      ?&  (gte ~(wyt in leaders.p.meta.convo) 2)
          (~(has in leaders.p.meta.convo) author.message)
      ==
    ==
  ::
      %change-router
    !!  ::  TBD
  ==
::
++  handle-action
  |=  =action
  ^-  (quip card _state)
  ::
  ::  we receive actions from our own client app
  ::
  ?>  =(our src):bowl
  ?-    -.action
      %make-conversation
    ::  create a new conversation and possibly send invites.
    ::  conversation IDs are meant to be *globally* unique
    ::
    ::  if a conversation is a DM (1:1 convo) we assign unique ID based
    ::  on the two ship names. DMs cannot be duplicated this way.
    ::
    !!
    ::  =.  members.config.action
    ::    (~(put in members.config.action) our.bowl)
    ::  =/  member-count  ~(wyt in members.config.action)
    ::  ::  generate unique ID
    ::  =/  id
    ::    ?:  ?=(%dm -.config.action)
    ::      ::  enforce that we don't already have a DM of this nature
    ::      ::  and that DMs have exactly 2 members
    ::      ?.  =(member-count 2)
    ::        ~|("pongo: error: tried to make multiparty DM" !!)
    ::      `@ux`(sham (rap 3 ~(tap in members.config.action)))
    ::    ::  enforce group chats have at least 3 members
    ::    ?.  (gth member-count 2)
    ::      ~|("pongo: error: tried to make group with <3 members" !!)
    ::    `@ux`(sham (cat 3 our.bowl eny.bowl))
    ::  ::  TODO: fix this in a more permanent way?
    ::  =^  old-last-message  database.state
    ::    ?~  have=(fetch-conversation id)
    ::      [0 database.state]
    ::    ?.  deleted.u.have
    ::      ~|("pongo: error: duplicate conversation ID" !!)
    ::    ::  drop an old messages-table if replacing deleted convo
    ::    ::  BUT save the last-message index!
    ::    :-  last-message.u.have
    ::    (~(drop-table db:nec database.state) %pongo^id)
    ::  ::
    ::  =/  convo=conversation
    ::    :*  `@ux`id
    ::        name=(make-unique-name name.action)
    ::        last-active=now.bowl
    ::        last-message=old-last-message
    ::        last-read=0
    ::        router=our.bowl
    ::        [%b config.action(members members.config.action)]
    ::        deleted=%.n
    ::        muted=%.n
    ::        ~
    ::    ==
    ::  ::  add this conversation to our table and create a messages table for it
    ::  =.  database.state
    ::    (~(update-rows db:nec database.state) %pongo^%conversations ~[convo])
    ::  =.  database.state
    ::    %+  ~(add-table db:nec database.state)
    ::      %pongo^id.convo
    ::    :^    (make-schema:nec messages-schema)
    ::        primary-key=~[%id]
    ::      (make-indices:nec messages-indices)
    ::    ~
    ::  ::  poke all indicated members in metadata with invites
    ::  =/  mems  ~(tap in (~(del in members.config.action) our.bowl))
    ::  ~&  >>  "%pongo: made conversation id: {<id.convo>} and invited {<mems>}"
    ::  :-  %+  turn  mems
    ::      |=  to=@p
    ::      %+  ~(poke pass:io /send-invite)  [to %pongo]
    ::      ping+!>(`ping`[%invite convo(name name.action)])
    ::  %=    state
    ::      invites-sent
    ::    |-
    ::    ?~  mems  invites-sent.state
    ::    %=  $
    ::      mems  t.mems
    ::      invites-sent.state  (~(put ju invites-sent.state) id.convo i.mems)
    ::    ==
    ::  ==
  ::
      %leave-conversation
    ::  leave a conversation we're currently in
    ::  set it to deleted in our conversations table, tell nectar to
    ::  stop tracking the router's table, and poke chat telling it we left
    ?~  convo=(fetch-conversation conversation-id.action)
      ~|("%pongo: couldn't find that conversation id" !!)
    :_  state
    :~  %+  ~(poke pass:io /leave-convo)
          [our.bowl %nectar]
        :-  %nectar-query
        !>  ^-  query-poke:nectar
        :-  %pongo
        :^  %update  %conversations
          [%s %id %& %eq conversation-id.action]
        ~[[%deleted |=(v=value:nectar %.y)]]
    ::
        %+  ~(poke pass:io /leave-convo)
          [our.bowl %nectar]
        nectar-track+!>(`track:nectar`pongo+[%stop [router %pongo^id]:u.convo])
    ::
        %+  ~(poke pass:io /send-member-remove)
          [our.bowl %pongo]
        =-  pongo-action+!>(`^action`[%send-message -])
        ['' id.u.convo %member-remove (scot %p our.bowl) ~ ~]
    ==
  ::
      %send-message
    ::  create a message and send to a conversation we're in
    ?>  (lte (met 3 content.action) message-length-limit)
    =/  hash  (make-message-hash content.action [our now]:bowl)
    =/  =message
      :*  id=0  ::  router will make this
          author=our.bowl
          signature=[%b (sign:sig our.bowl now.bowl hash)]
          timestamp=now.bowl  ::  TODO ?
          message-kind.action
          content.action
          edited=%.n
          reference.action
          [%m ~]  [%s mentions.action]  ~
      ==
    ?~  convo=(fetch-conversation conversation-id.action)
      ~|("%pongo: couldn't find that conversation id" !!)
    ::  update last-read message in convo, since if we are sending a
    ::  message, we've definitely read all previous messages
    :_  state
    :~  (give-update [%sending id.u.convo identifier.action])
        %+  ~(poke pass:io /send-message)
          [router.u.convo %pongo]
        ping+!>(`ping`[%message conversation-id.action message])
    ::
        %+  ~(poke pass:io /read-convo)
          [our.bowl %nectar]
        :-  %nectar-query
        !>  ^-  query-poke:nectar
        :-  %pongo
        :+  %update-rows  %conversations
        ~[u.convo(last-read last-message.u.convo)]
    ==
  ::
      %send-message-edit
    !!
    ::  ::  edit a message we sent (must be of kind %text/%code)
    ::  ::  as opposed to *new* messages, which must be sequenced by router,
    ::  ::  we can poke edits out directly to all members
    ::  ?~  convo=(fetch-conversation conversation-id.action)
    ::    ~|("%pongo: couldn't find that conversation id" !!)
    ::  :_  state
    ::  %+  turn  ~(tap in members.p.meta.u.convo)
    ::  |=  to=@p
    ::  %+  ~(poke pass:io /send-edit)  [to %pongo]
    ::  ping+!>(`ping`[%edit [conversation-id on edit]:action])
  ::
      %send-reaction
    !!
    ::  ::  create a reaction and send to a conversation we're in
    ::  ::  as opposed to messages, which must be sequenced by router,
    ::  ::  we can poke reactions out directly to all members
    ::  ?~  convo=(fetch-conversation conversation-id.action)
    ::    ~|("%pongo: couldn't find that conversation id" !!)
    ::  :_  state
    ::  %+  turn  ~(tap in members.p.meta.u.convo)
    ::  |=  to=@p
    ::  %+  ~(poke pass:io /send-react)
    ::    [to %pongo]
    ::  ping+!>(`ping`[%react [conversation-id on reaction]:action])
  ::
      %send-tokens
    =/  tid  `@ta`(cat 3 'token-send_' (scot %uv (sham eny.bowl)))
    =/  ta-now  `@ta`(scot %da now.bowl)
    =/  start-args
      :^  ~  `tid  byk.bowl(r da+now.bowl)
      token-send+!>(`^action`action)
    :_  state  :_  ~
    %+  ~(poke pass:io /thread/[ta-now])
      [our.bowl %spider]
    spider-start+!>(start-args)
  ::
      %read-message
    ::  if read id is newer than current saved read id, replace in convo
    ::  send out a new badge notif for app to update unread count
    ?~  convo=(fetch-conversation conversation-id.action)
      ~|("%pongo: couldn't find that conversation id" !!)
    ?.  (gth message-id.action last-read.u.convo)  `state
    :_  state  :_  ~
    %+  ~(poke pass:io /read-convo)
      [our.bowl %nectar]
    :-  %nectar-query
    !>  ^-  query-poke:nectar
    :-  %pongo
    :+  %update-rows  %conversations
    ~[u.convo(last-read message-id.action)]
  ::
      %make-invite
    ::  create an invite and send to someone
    ?~  convo=(fetch-conversation conversation-id.action)
      ~|("%pongo: couldn't find that conversation id" !!)
    :_  state(invites-sent (~(put ju invites-sent.state) id.u.convo to.action))
    :_  ~
    %+  ~(poke pass:io /send-invite)
      [to.action %pongo]
    entry+!>(`entry`[%invite u.convo])
  ::
      %accept-invite
    ::  accept an invite we've been sent, join the conversation
    ::  add this convo to our conversations table and create
    ::  a messages table for it
    ::
    ::  conversation names must be locally unique, so if we
    ::  already have a conversation with this name, we append
    ::  a number to the end of the name.
    ::
    !!
    ::  =/  [from=@p convo=conversation]
    ::    (~(got by invites.state) conversation-id.action)
    ::  =.  members.p.meta.convo
    ::    (~(put in members.p.meta.convo) our.bowl)
    ::  =^  convo  database.state
    ::    ?~  hav=(fetch-conversation id.convo)
    ::      ::  we've never been in this conversation before
    ::      =.  name.convo  (make-unique-name name.convo)
    ::      =.  database.state
    ::        =+  %+  ~(insert-rows db:nec database.state)
    ::              %pongo^%conversations
    ::            ~[convo(last-active now.bowl, last-read 0)]
    ::        %+  ~(add-table db:nec -)
    ::          %pongo^id.convo
    ::        :^    (make-schema:nec messages-schema)
    ::            primary-key=~[%id]
    ::          (make-indices:nec messages-indices)
    ::        ~
    ::      [convo database.state]
    ::    ::  we've been here before, revive "deleted" convo
    ::    ?.  deleted.u.hav
    ::      ::  we've been here before and we never really left!
    ::      ::  if this is a DM, it means that they deleted and now want
    ::      ::  to start anew, but we never deleted the old DMs. treat
    ::      ::  it as them re-joining.
    ::      ?:  ?=(%dm -.p.meta.convo)
    ::        =.  members.p.meta.convo
    ::          (~(put in members.p.meta.convo) from)
    ::        :-  convo
    ::        %+  ~(update-rows db:nec database.state)
    ::        %pongo^%conversations  ~[convo]
    ::      [convo database.state]
    ::    :-  convo
    ::    ::  delete old messages table
    ::    =+  (~(drop-table db:nec database.state) %pongo^id.convo)
    ::    ::  update entry for this convo
    ::    =+  %+  ~(update-rows db:nec -)
    ::          %pongo^%conversations
    ::        ~[convo(last-active now.bowl, last-read 0)]
    ::    ::  add new messages table
    ::    %+  ~(add-table db:nec -)
    ::      %pongo^id.convo
    ::    :^    (make-schema:nec messages-schema)
    ::        primary-key=~[%id]
    ::      (make-indices:nec messages-indices)
    ::    ~
    ::  :_  state(invites (~(del by invites.state) id.convo))
    ::  :_  ~
    ::  %+  ~(poke pass:io /accept-invite)
    ::    [from %pongo]
    ::  ping+!>(`ping`[%accept-invite id.convo])
  ::
      %reject-invite
    ::  reject an invite we've been sent
    ?~  invite=(~(get by invites.state) conversation-id.action)  `state
    :_  state(invites (~(del by invites.state) conversation-id.action))
    :_  ~
    %+  ~(poke pass:io /accept-invite)
      [from.u.invite %pongo]
    entry+!>(`entry`[%reject-invite conversation-id.action])
  ::
      %make-invite-request
    ::  try to join a public conversation off id and existing member
    :_  state  :_  ~
    %+  ~(poke pass:io /request-invite)
      [to.action %pongo]
    entry+!>(`entry`[%invite-request conversation-id.action])
  ::
      %search
    ::  search in messages for a phrase. can filter by conversation
    ::  or author. to get results, first subscribe to /search-results
    ::  batch results in groupings of 1.000 messages in order of
    ::  recency to get fast initial returns
    =/  tid  `@ta`(cat 3 'search_' (scot %ux uid.action))
    =/  ta-now  `@ta`(scot %da now.bowl)
    =/  start-args
      :^  ~  `tid  byk.bowl(r da+now.bowl)
      search+!>(`search`+.+.action)
    :_  state
    :~  %+  ~(poke pass:io /thread/[ta-now])
          [our.bowl %spider]
        spider-start+!>(start-args)
        %+  ~(watch pass:io /thread/updates/(scot %ux uid.action))
          [our.bowl %spider]
        /thread/[tid]/updates
    ==
  ::
      %cancel-search
    =/  tid  `@ta`(cat 3 'search_' (scot %ux uid.action))
    =/  ta-now  `@ta`(scot %da now.bowl)
    :_  state  :_  ~
    %+  ~(poke pass:io /thread-stop/[ta-now])
      [our.bowl %spider]
    spider-stop+!>([tid %.y])
  ::
      %set-notifications
    `state(notif-settings notif-settings.action)
  ::
      %set-notif-token
    :-  ~
    %=  state
      expo-token.notif-settings  expo-token.action
      ship-url.notif-settings    ship-url.action
    ==
  ::
      %set-notif-level
    `state(level.notif-settings level.action)
  ::
      ?(%mute-conversation %unmute-conversation)
    :_  state  :_  ~
    %+  ~(poke pass:io /mute-unmute-conversation)
      [our.bowl %nectar]
    :-  %nectar-query
    !>  ^-  query-poke:nectar
    :-  %pongo
    :^  %update  %conversations
      [%s %id %& %eq conversation-id.action]
    ~[[%muted |=(v=value:nectar ?:(=(%mute-conversation -.action) %.y %.n))]]
  ==
::
++  handle-wallet-update
  |=  upd=wallet-update:wallet
  ^-  (quip card _state)
  ?+    -.upd  `state
      %sequencer-receipt
    :_  state  :_  ~
    %+  fact:io
      :-  %pongo-thread-update
      !>  ^-  thread-update
      [%finished +.+.upd]
    ~[/token-send-updates]
  ==
::
++  handle-address-share
  |=  share=share-address:uqbar
  ^-  (quip card _state)
  :_  state  :_  ~
  ?-    -.share
      %request  !!
      %deny
    ::  surface this
    %+  fact:io
      :-  %pongo-thread-update
      !>  ^-  thread-update
      [%denied src.bowl]
    ~[/token-send-updates]
  ::
      %share
    ::  what we really want
    %+  fact:io
      :-  %pongo-thread-update
      !>  ^-  thread-update
      [%shared src.bowl address.share]
    ~[/token-send-updates]
  ==
::
++  handle-scry
  |=  =path
  ^-  (unit (unit cage))
  ?+    path  ~|("unexpected scry into {<dap.bowl>} on path {<path>}" !!)
  ::
  ::  get all conversations and get unread count + most recent message
  ::
      [%x %conversations ~]
    ::  TODO
    !!
    ::  =-  ``pongo-update+!>(`pongo-update`[%conversations -])
    ::  ^-  (list conversation-info)
    ::  %+  turn
    ::    ::  only get undeleted conversations
    ::    =<  -
    ::    %+  ~(q db:nec database.state)  %pongo
    ::    [%select %conversations where=[%s %deleted %& %eq %.n]]
    ::  |=  =row:nec
    ::  =/  convo=conversation
    ::    !<(conversation [-:!>(*conversation) row])
    ::  =/  last-message=(unit message)
    ::    =-  ?~(-.- ~ `!<(message [-:!>(*message) (head -.-)]))
    ::    %+  ~(q db:nec database.state)  %pongo
    ::    [%select id.convo where=[%s %id %& %eq last-message.convo]]
    ::  :+  convo
    ::    last-message
    ::  ?~  last-message  0
    ::  ?:  (gth last-read.convo id.u.last-message)  0
    ::  (sub id.u.last-message last-read.convo)
  ::
  ::  get all messages from a particular conversation
  ::  warning: could be slow for long conversations!
  ::
      [%x %all-messages @ ~]
    ::  TODO
    !!
    ::  =-  ``pongo-update+!>(`pongo-update`[%message-list -])
    ::  ^-  (list message)
    ::  =/  convo-id  (slav %ux i.t.t.path)
    ::  ?~  convo=(fetch-conversation convo-id)
    ::    ~
    ::  %+  turn
    ::    -:(~(q db:nec database.state) %pongo [%select id.u.convo where=[%n ~]])
    ::  |=  =row:nec
    ::  !<(message [-:!>(*message) row])
  ::
  ::  /messages/[convo-id]/[msg-id]/[num-before]/[num-after]
  ::
      [%x %messages @ @ @ @ ~]
    ::  TODO
    !!
    ::  =/  convo-id    (slav %ux i.t.t.path)
    ::  =/  message-id  (slav %ud i.t.t.t.path)
    ::  =/  num-before  (slav %ud i.t.t.t.t.path)
    ::  =/  num-after   (slav %ud i.t.t.t.t.t.path)
    ::  =/  start=@ud
    ::    ?:  (gth num-before message-id)  0
    ::      (sub message-id num-before)
    ::  =/  end=@ud
    ::    (add message-id num-after)
    ::  =-  ``pongo-update+!>(`pongo-update`[%message-list -])
    ::  ^-  (list message)
    ::  ?~  convo=(fetch-conversation convo-id)  ~
    ::  %+  turn
    ::    =<  -
    ::    %+  ~(q db:nec database.state)  %pongo
    ::    :+  %select  id.u.convo
    ::    :+  %and
    ::      [%s %id %& %gte start]
    ::    [%s %id %& %lte end]
    ::  |=  =row:nec
    ::  !<(message [-:!>(*message) row])
  ::
  ::
  ::
      [%x %notification @ @ ~]
    ::  TODO
    !!
    ::  =/  convo-id    (slav %ux i.t.t.path)
    ::  =/  message-id  (slav %ud i.t.t.t.path)
    ::  ?~  convo=(fetch-conversation convo-id)  [~ ~]
    ::  =-  ``pongo-update+!>(`pongo-update`[%notification -])
    ::  :-  name.u.convo
    ::  =<  [author content]
    ::  !<  message
    ::  :-  -:!>(*message)
    ::  %-  head
    ::  =<  -
    ::  %+  ~(q db:nec database.state)  %pongo
    ::  :+  %select  id.u.convo
    ::  [%s %id %& %eq message-id]
  ::
  ::  get all sent and received invites
  ::
      [%x %invites ~]
    ``pongo-update+!>(`pongo-update`[%invites invites-sent.state invites.state])
  ::
  ::  get current notification level
  ::
      [%x %notif-settings ~]
    ``pongo-update+!>(`pongo-update`[%notif-settings notif-settings.state])
  ==
::
++  fetch-conversation
  |=  id=conversation-id
  ^-  (unit conversation)
  ::  TODO store this as a procedure w/ 1 param
  =/  jam
    %-  jam  ^-  query:nectar
    [%select %conversations where=[%s %id %& %eq id]]
  =/  rows=(list row:nectar)
    (nectar-scry %conversations jam [our now]:bowl)
  ?~(rows ~ `!<(conversation [-:!>(*conversation) (head rows)]))
::
++  get-total-unreads
  ^-  @ud
  ::  TODO store this as a procedure w/ no params
  =/  jam
    %-  jam  ^-  query:nectar
    [%select %conversations where=[%s %deleted %& %eq %.n]]
  =/  rows=(list row:nectar)
    (nectar-scry %conversations jam [our now]:bowl)
  ::  TODO see why this gives wrong answer sometimes
  %+  roll  rows
  |=  [=row:nectar i=@ud]
  =+  !<(conversation [-:!>(*conversation) row])
  ?:  (gte [last-read last-message]:-)  i
  (add i (sub [last-message last-read]:-))
::
++  give-update
  |=  upd=pongo-update
  ^-  card
  ::  ~&  >>  "giving fact to frontend: "
  ::  ~&  >>  (crip (en-json:html (update-to-json:parsing upd)))
  (fact:io pongo-update+!>(upd) ~[/updates])
--