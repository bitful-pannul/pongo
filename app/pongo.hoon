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
::  arbitrary limit for some measure of performance guarantees
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
      :_  this(state [%2 ['' '' %low] ~ ~])
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
      ::  check to make sure nectar has conversations table, add if not
      :_  this(state state-2.old, updates-sub updates-sub.old)
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
    ++  on-poke
      |=  [=mark =vase]
      ^-  (quip card _this)
      ?:  ?=(%sss-table-updates mark)
        =^  cards  updates-sub
          (apply:da-sub !<(into:da-sub (fled vase)))
        [cards this]
      =^  cards1  state
        ?+    mark  `state
            %ping
          (handle-ping:hc !<(ping vase))
            %entry
          (handle-entry:hc !<(entry vase))
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
            ?~  wave.msg  `state
            ::  if the message adjust conversation metadata, do that too
            (update-conversation:hc u.wave.msg)
          ==
        ==
      ::  TODO fix this disgusting mess
      =/  [cards2=(list card) ug=state-2 ly=_updates-sub]
        ?+    mark  `[state updates-sub]
            %pongo-action
          (handle-action:hc !<(action vase) updates-sub)
        ==
      [(weld cards1 cards2) this(state ug, updates-sub ly)]
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
+*  da-sub
  =/  da  (da table-updates ,[%updates @ @ ~])
  (da updates-sub bowl -:!>(*result:da) -:!>(*from:da) -:!>(*fail:da))
::
::  +update-conversation: upon receiving an update from %nectar that
::  our table has had a query applied to it, make any necessary changes
::  to our local conversation representation based on new messages/edits
::
++  update-conversation
  |=  =query:nectar
  ^-  (quip card _state)
  ?+    -.query  `state  ::  TODO the other queries we do!
      %insert
    ::  this is a new message
    ?~  rows.query  `state
    =/  =message  !<(message [-:!>(*message) i.rows.query])
    ?~  conv=(fetch-conversation table.query)
      `state
    =*  convo  u.conv
    ::  depending on message type, apply changes to conversation metadata
    =^  cards  convo
      ?+    kind.message
          ::  text, poll, send-tokens, app-link
          ?:  muted.convo  `convo
          =-  ?~  -  `convo  [u.-^~ convo]
          %:  give-push-notification
              get-total-unreads  ::  function
              convo  message
              notif-settings.state
              [our now]:bowl
          ==
      ::
          %member-add
        =.  members.p.meta.convo
          (~(put in members.p.meta.convo) (slav %p content.message))
        `convo
      ::
          %member-remove
        =+  them=(slav %p content.message)
        =.  members.p.meta.convo
          (~(del in members.p.meta.convo) them)
        ?:  =(our.bowl them)
          ::  we were removed, delete convo
          :_  convo
          :~  %+  ~(poke pass:io /leave-convo)
                [our.bowl %nectar]
              :-  %nectar-query
              !>  ^-  query-poke:nectar
              :-  %pongo
              :+  %delete  %conversations
              [%s %id %& %eq id.convo]
          ::
              %+  ~(poke pass:io /leave-convo)
                [our.bowl %nectar]
              :-  %nectar-track
              !>(`track:nectar`pongo+[%stop [router %pongo^id]:convo])
          ==
        `convo
      ::
          %change-name
        `convo(name content.message)
      ::
          %leader-add
        ?>  ?=(%managed -.p.meta.convo)
        =.  leaders.p.meta.convo
          (~(put in leaders.p.meta.convo) (slav %p content.message))
        `convo
      ::
          %leader-remove
        ?>  ?=(%managed -.p.meta.convo)
        =.  leaders.p.meta.convo
          (~(del in leaders.p.meta.convo) (slav %p content.message))
        `convo
      ::
          %change-router  !!  ::  TBD
      ==
    :_  state
    :_  cards
    %+  ~(poke pass:io /update-convo)
      [our.bowl %nectar]
    :-  %nectar-query
    !>  ^-  query-poke:nectar
    :-  %pongo
    =-  [%update-rows %conversations ~[-]]
    %=  convo
      last-active  now.bowl
      last-message  id.message
    ==
  ==
::
++  handle-ping
  |=  =ping
  ^-  (quip card _state)
  ?~  conv=(fetch-conversation -.+.ping)
    ~|("pongo: can't find conversation {<-.+.ping>}" !!)
  =*  convo  u.conv
  ?.  =(our.bowl router.convo)
    ::  we are not router; fail
    ~|("pongo: got message while not router" !!)
  ::  we are the router; we must integrate the message/edit/reaction
  ::  into our messages table, which will be synced to participants
  ?-    -.ping
      %message
    =*  message  message.ping
    ::  after validating message, insert it in our messages table
    =/  message-hash  (make-message-hash [content author timestamp]:message)
    ~|  "pongo: received invalid message, as router"
    ~|  message
    ?>  ?&  (validate:sig our.bowl p.signature.message message-hash now.bowl)
            (~(has in members.p.meta.convo) author.message)
            (lte (met 3 content.message) message-length-limit)
            (valid-message-contents message convo)
        ==
    :_  state  :_  ~
    %+  ~(poke pass:io /insert)
      [our.bowl %nectar]
    :-  %nectar-query
    !>  ^-  query-poke:nectar
    =-  pongo+[%insert id.convo ~[-]]
    ::  set proper timestamp and message ID
    ::  TODO use working autoincrement in nectar
    %=  message
      id  +(last-message.convo)
      timestamp  now.bowl
    ==
  ::
      %edit
    ::  apply an edit to a message
    :_  state  :_  ~
    %+  ~(poke pass:io /insert)
      [our.bowl %nectar]
    :-  %nectar-query
    !>  ^-  query-poke:nectar
    :-  %pongo
    :^  %update  id.convo
      :+  %and  [%s %id %& %eq on.ping]
      :+  %and  [%s %author %& %eq src.bowl]
      [%s %kind %& %eq %text]
    :~  [%content |=(value:nectar edit.ping)]
        [%edited |=(value:nectar %.y)]
    ==
  ::
      %react
    ::  apply a reaction to a message
    :_  state  :_  ~
    %+  ~(poke pass:io /insert)
      [our.bowl %nectar]
    :-  %nectar-query
    !>  ^-  query-poke:nectar
    :-  %pongo
    :^  %update  id.convo
      [%s %id %& %eq on.ping]
    :_  ~
    :-  %reactions
    |=  v=value:nectar
    ^-  value:nectar
    ?>  &(?=(^ v) ?=(%m -.v))
    m+(~(put by p.v) src.bowl reaction.ping)
  ==
::
++  handle-entry
  |=  =entry
  ^-  (quip card _state)
  ?:  ?=(%invite -.entry)
    ::  we've received an invite to a conversation
    =+  [src.bowl conversation.entry]
    :_  state(invites (~(put by invites.state) id.conversation.entry -))
    :~  (give-update [%invite conversation.entry])
        ::  remove this to turn off auto-accept
        %+  ~(poke pass:io /accept-invite)  [our.bowl %pongo]
        pongo-action+!>(`action`[%accept-invite id.conversation.entry])
    ==
  ?~  conv=(fetch-conversation conversation-id.entry)
    ::  we got pinged for a conversation we don't know about
    ::  TODO try to join it?
    !!
  =*  convo  u.conv
  ?-    -.entry
      %accept-invite
    ::  an invite we sent has been accepted
    ::  create a message in conversation with kind %member-add
    ?>  (~(has ju invites-sent.state) id.convo src.bowl)
    =/  hash  (make-message-hash (scot %p src.bowl) [our now]:bowl)
    :_  state(invites-sent (~(del ju invites-sent.state) id.convo src.bowl))
    :_  ~
    %+  ~(poke pass:io /route-member-add-message)
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
::  +valid-message-contents: as a router node, verify that messages which
::  adjust the "leadership structure" of the groupchat are performed by
::  those with the privilege to do so.
::
++  valid-message-contents
  |=  [=message convo=conversation]
  ^-  ?
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
  |=  [=action =_updates-sub]
  ^-  (quip card [_state _updates-sub])
  ::
  ::  we receive actions from our own client app
  ::
  ?>  =(our src):bowl
  ?-    -.action
      %make-conversation
    ::  create a new conversation and possibly send invites.
    ::  by default, the router is the ship that does this.
    ::  conversation IDs are meant to be *globally* unique
    ::  if a conversation is a DM (1:1 convo) we assign unique ID based
    ::  on the two ship names. DMs cannot be duplicated this way.
    ::
    =.  members.config.action
      (~(put in members.config.action) our.bowl)
    =/  member-count  ~(wyt in members.config.action)
    ::  generate unique ID
    =/  id=conversation-id
      ?:  ?=(%dm -.config.action)
        ::  enforce that we don't already have a DM of this nature
        ::  and that DMs have exactly 2 members
        ?.  =(member-count 2)
          ~|("pongo: error: tried to make multiparty DM" !!)
        `@ux`(sham (rap 3 ~(tap in members.config.action)))
      ::  enforce group chats have at least 3 members
      ?.  (gth member-count 2)
        ~|("pongo: error: tried to make group with <3 members" !!)
      `@ux`(sham (rap 3 ~[our.bowl now.bowl name.action]))
    =/  convo=conversation
      :*  `@ux`id
          name.action
          last-active=now.bowl
          last-message=0
          last-read=0
          router=our.bowl
          [%b config.action]
          muted=%.n
          ~
      ==
    ::  add this conversation to our table and create a messages table for it
    ::  set the table permissions to the members of the convo
    ::  start watching nectar publication for updates to messages table
    =^  cards  updates-sub
      (surf:da-sub our.bowl %nectar [%updates %pongo id.convo ~])
    =.  cards
      :^  %+  ~(poke pass:io /make-table)
            [our.bowl %nectar]
          :-  %nectar-query
          !>(`query-poke:nectar`pongo+[%update-rows %conversations ~[convo]])
      ::
          %+  ~(poke pass:io /make-table)
            [our.bowl %nectar]
          :-  %nectar-query
          !>  ^-  query-poke:nectar
          :-  %pongo
          :+  %add-table  id.convo
          :^    (make-schema:nectar messages-schema)
              primary-key=~[%id]
            (make-indices:nectar messages-indices)
          ~
      ::
          %+  ~(poke pass:io /make-table)
            [our.bowl %nectar]
          :-  %nectar-set-perms
          !>  ^-  set-perms:nectar
          [%pongo id.convo]^[%set members.config.action]
      cards
    ::  poke all indicated members in metadata with invites
    =/  mems  ~(tap in (~(del in members.config.action) our.bowl))
    =.  invites-sent.state
      |-  ?~  mems  invites-sent.state
      %=  $
        mems  t.mems
        invites-sent.state  (~(put ju invites-sent.state) id.convo i.mems)
      ==
    ~&  >>  "%pongo: made conversation id: {<id.convo>} and invited {<mems>}"
    :_  [state updates-sub]
    %+  welp  cards
    %+  turn  mems
    |=  to=@p
    %+  ~(poke pass:io /send-invite)  [to %pongo]
    entry+!>(`entry`[%invite convo])
  ::
      %leave-conversation
    ::  leave a conversation we're currently in
    ::  delete it from our conversations table, tell nectar to stop
    ::  tracking the router's table, and poke chat telling it we left.
    ::  TODO: if we leave *as router*, we should assign a new router!!
    ?~  convo=(fetch-conversation conversation-id.action)
      ~|("%pongo: couldn't find that conversation id" !!)
    ?<  =(our.bowl router.u.convo)
    :_  [state updates-sub]
    :~  %+  ~(poke pass:io /send-member-remove)
          [our.bowl %pongo]
        =-  pongo-action+!>(`^action`[%send-message -])
        ['' id.u.convo %member-remove (scot %p our.bowl) ~ ~]
    ::
        %+  ~(poke pass:io /leave-convo)
          [our.bowl %nectar]
        :-  %nectar-query
        !>  ^-  query-poke:nectar
        :-  %pongo
        :+  %delete  %conversations
        [%s %id %& %eq conversation-id.action]
    ::
        %+  ~(poke pass:io /leave-convo)
          [our.bowl %nectar]
        nectar-track+!>(`track:nectar`pongo+[%stop [router %pongo^id]:u.convo])
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
    :_  [state updates-sub]
    :~  (give-update [%sending id.u.convo identifier.action])
        %+  ~(poke pass:io /send-message)
          [router.u.convo %pongo]
        ping+!>(`ping`[%message conversation-id.action message])
    ==
  ::
      ?(%send-message-edit %send-reaction)
    ?~  convo=(fetch-conversation conversation-id.action)
      ~|("%pongo: couldn't find that conversation id" !!)
    :_  [state updates-sub]  :_  ~
    %+  ~(poke pass:io /send-edit)
      [router.u.convo %pongo]
    :-  %ping
    !>  ^-  ping
    ?-  -.action
      %send-message-edit  edit+[conversation-id on edit]:action
      %send-reaction      react+[conversation-id on reaction]:action
    ==
  ::
      %send-tokens
    =/  tid  `@ta`(cat 3 'token-send_' (scot %uv (sham eny.bowl)))
    =/  ta-now  `@ta`(scot %da now.bowl)
    =/  start-args
      :^  ~  `tid  byk.bowl(r da+now.bowl)
      token-send+!>(`^action`action)
    :_  [state updates-sub]  :_  ~
    %+  ~(poke pass:io /thread/[ta-now])
      [our.bowl %spider]
    spider-start+!>(start-args)
  ::
      %read-message
    ::  if read id is newer than current saved read id, replace in convo
    ?~  convo=(fetch-conversation conversation-id.action)
      ~|("%pongo: couldn't find that conversation id" !!)
    ?.  (gth message-id.action last-read.u.convo)  `[state updates-sub]
    :_  [state updates-sub]
    :~  ::  TODO: send out a new badge notif for app to update unread count
        %+  ~(poke pass:io /read-convo)
          [our.bowl %nectar]
        :-  %nectar-query
        !>  ^-  query-poke:nectar
        :-  %pongo
        :+  %update-rows  %conversations
        ~[u.convo(last-read message-id.action)]
    ==
  ::
      %make-invite
    ::  create an invite and send to someone
    ?~  convo=(fetch-conversation conversation-id.action)
      ~|("%pongo: couldn't find that conversation id" !!)
    :_  :_  updates-sub
        state(invites-sent (~(put ju invites-sent.state) id.u.convo to.action))
    :_  ~
    %+  ~(poke pass:io /send-invite)
      [to.action %pongo]
    entry+!>(`entry`[%invite u.convo])
  ::
      %accept-invite
    ::  accept an invite we've been sent
    ::  add this convo to our conversations table,
    ::  start tracking router's messages table
    =/  [from=@p convo=conversation]
      (~(got by invites.state) conversation-id.action)
    =.  members.p.meta.convo
      (~(put in members.p.meta.convo) our.bowl)
    =^  cards  updates-sub
      (surf:da-sub our.bowl %nectar [%updates %pongo id.convo ~])
    :_  [state(invites (~(del by invites.state) id.convo)) updates-sub]
    :^  %+  ~(poke pass:io /accept-invite)
            [from %pongo]
          entry+!>(`entry`[%accept-invite id.convo])
    ::
        %+  ~(poke pass:io /add-convo)
          [our.bowl %nectar]
        :-  %nectar-query
        !>  ^-  query-poke:nectar
        :-  %pongo
        :+  %insert  %conversations
        ~[convo(last-active now.bowl, last-read 0)]
    ::
        %+  ~(poke pass:io /nectar-track)
          [our.bowl %nectar]
        :-  %nectar-track
        !>  ^-  track:nectar
        pongo+[%start router.convo pongo+id.convo]
    cards
  ::
      %reject-invite
    ::  reject an invite we've been sent
    ?~  invite=(~(get by invites.state) conversation-id.action)
      `[state updates-sub]
    :_  :_  updates-sub
        state(invites (~(del by invites.state) conversation-id.action))
    :_  ~
    %+  ~(poke pass:io /accept-invite)
      [from.u.invite %pongo]
    entry+!>(`entry`[%reject-invite conversation-id.action])
  ::
      %make-invite-request
    ::  try to join a public conversation off id and existing member
    :_  [state updates-sub]  :_  ~
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
    :_  [state updates-sub]
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
    :_  [state updates-sub]  :_  ~
    %+  ~(poke pass:io /thread-stop/[ta-now])
      [our.bowl %spider]
    spider-stop+!>([tid %.y])
  ::
      %set-notifications
    `[state(notif-settings notif-settings.action) updates-sub]
  ::
      %set-notif-token
    :-  ~
    :_  updates-sub
    %=  state
      expo-token.notif-settings  expo-token.action
      ship-url.notif-settings    ship-url.action
    ==
  ::
      %set-notif-level
    `[state(level.notif-settings level.action) updates-sub]
  ::
      ?(%mute-conversation %unmute-conversation)
    :_  [state updates-sub]  :_  ~
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
    ::  TODO create an optimized join for this query
    =-  ``pongo-update+!>(`pongo-update`[%conversations -])
    ^-  (list conversation-info)
    %+  turn
      ::  grab all conversations
      =+  [%select %conversations where=[%n ~]]
      (nectar-scry %conversations - [our now]:bowl)
    |=  =row:nectar
    =/  convo  !<(conversation [-:!>(*conversation) row])
    ::  grab last message from each conversation, if any
    =/  last-message=(unit message)
      =-  ?~(- ~ `!<(message [-:!>(*message) (head -)]))
      =-  (nectar-scry id.convo - [our now]:bowl)
      [%select id.convo where=[%s %id %& %eq last-message.convo]]
    :+  convo
      last-message
    ?~  last-message  0
    ?:  (gth last-read.convo id.u.last-message)  0
    (sub id.u.last-message last-read.convo)
  ::
  ::  get all messages from a particular conversation
  ::  warning: could be slow for long conversations!
  ::
      [%x %all-messages @ ~]
    =-  ``pongo-update+!>(`pongo-update`[%message-list -])
    ^-  (list message)
    =/  convo-id  (slav %ux i.t.t.path)
    ?~  convo=(fetch-conversation convo-id)
      ~
    %+  turn
      =+  [%select id.u.convo where=[%n ~]]
      (nectar-scry id.u.convo - [our now]:bowl)
    |=  =row:nectar
    !<(message [-:!>(*message) row])
  ::
  ::  /messages/[convo-id]/[msg-id]/[num-before]/[num-after]
  ::
      [%x %messages @ @ @ @ ~]
    =/  convo-id    (slav %ux i.t.t.path)
    =/  message-id  (slav %ud i.t.t.t.path)
    =/  num-before  (slav %ud i.t.t.t.t.path)
    =/  num-after   (slav %ud i.t.t.t.t.t.path)
    =/  start=@ud
      ?:  (gth num-before message-id)  0
        (sub message-id num-before)
    =/  end=@ud
      (add message-id num-after)
    =-  ``pongo-update+!>(`pongo-update`[%message-list -])
    ^-  (list message)
    ?~  convo=(fetch-conversation convo-id)  ~
    %+  turn
      =-  (nectar-scry id.u.convo - [our now]:bowl)
      :+  %select  id.u.convo
      [%and [%s %id %& %gte start] [%s %id %& %lte end]]
    |=  =row:nectar
    !<(message [-:!>(*message) row])
  ::
  ::  /notification/[convo-id]/[message-id]
  ::
      [%x %notification @ @ ~]
    =/  convo-id    (slav %ux i.t.t.path)
    =/  message-id  (slav %ud i.t.t.t.path)
    ?~  convo=(fetch-conversation convo-id)  [~ ~]
    =-  ``pongo-update+!>(`pongo-update`[%notification -])
    :-  name.u.convo
    =<  [author content]
    !<  message
    :-  -:!>(*message)
    %-  head
    =-  (nectar-scry id.u.convo - [our now]:bowl)
    [%select id.u.convo [%s %id %& %eq message-id]]
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
  =/  rows=(list row:nectar)
    =-  (nectar-scry %conversations - [our now]:bowl)
    [%select %conversations where=[%s %id %& %eq id]]
  ?~(rows ~ `!<(conversation [-:!>(*conversation) (head rows)]))
::
++  get-total-unreads
  ^-  @ud
  ::  TODO store this as a procedure w/ no params
  =/  rows=(list row:nectar)
    =+  [%select %conversations where=[%n ~]]
    (nectar-scry %conversations - [our now]:bowl)
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