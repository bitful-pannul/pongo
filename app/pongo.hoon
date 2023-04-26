/-  *pongo, uqbar=zig-uqbar, wallet=zig-wallet,
    nectar, pongo-pings, orgs
/+  verb, dbug, default-agent, io=agentio,
    *pongo, *sss, sig, st=state-transition
|%
::  arbitrary limit for some measure of performance guarantees
++  message-length-limit  1.024
::
::  %pongo agent state
::
+$  state-2
  $:  %2
      ::  "deep state" = ELIMINATED
      =notif-settings  ::  "configuration state"
  ==
+$  card  card:agent:gall
--
::
^-  agent:gall
::  %+  verb  &
%-  agent:dbug
::  SSS declarations
=/  ping-sub  (mk-subs pongo-pings ,[%ping @ ~])
=/  ping-pub  (mk-pubs pongo-pings ,[%ping @ ~])
::
=|  state=state-2
=<
|_  =bowl:gall
+*  this  .
    hc    ~(. +> bowl)
    def   ~(. (default-agent this %|) bowl)
    da-ping
      =/  da  (da pongo-pings ,[%ping @ ~])
      (da ping-sub bowl -:!>(*result:da) -:!>(*from:da) -:!>(*fail:da))
    du-ping
      =/  du  (du pongo-pings ,[%ping @ ~])
      (du ping-pub bowl -:!>(*result:du))
::
++  on-leave  on-leave:def
++  on-fail   on-fail:def
++  on-peek   handle-scry:hc
::
++  on-init
  ::  nectar must be installed.
  ?.  %.  %nectar
      %~  has  in
      .^((set desk) %cd /(scot %p our.bowl)/base/(scot %da now.bowl))
    ~&  >>>  "pongo: error: you MUST have %nectar installed!"
    ~&  >>>  "installing nectar now. once it completes, please reboot pongo:"
    ~&  >>>  "|suspend %pongo"
    ~&  >>>  "|revive %pongo"
    :_  this(state [%2 ['' '' %low]])  :_  ~
    :*  %pass  /nectar-install  %agent  [our.bowl %hood]  %poke
        %kiln-install  !>([%nectar ~bacrys %nectar])
    ==
  ::  produce our conversations+inbox table
  ::  this will fail if it already exists, and that's okay!
  ::  TODO create our stored procedures here!
  :-  (init-tables [our now]:bowl)
  this(state [%2 ['' '' %low]])
::
++  on-save  !>([state ping-sub ping-pub])
::
++  on-load
  |=  =vase
  ^-  (quip card _this)
  ::  nectar must be installed.
  ?.  %.  %nectar
      %~  has  in
      .^((set desk) %cd /(scot %p our.bowl)/base/(scot %da now.bowl))
    ~&  >>>  "pongo: error: you MUST have %nectar installed!"
    ~&  >>>  "installing nectar now. once it completes, please reboot pongo:"
    ~&  >>>  "|suspend %pongo"
    ~&  >>>  "|revive %pongo"
    :_  this(state [%2 ['' '' %low]])  :_  ~
    :*  %pass  /nectar-install  %agent  [our.bowl %hood]  %poke
        %kiln-install  !>([%nectar ~bacrys %nectar])
    ==
  ?:  =(%0 -.q.vase)  on-init
  ?:  =(%1 -.q.vase)
    =/  [cards=(list card) new=state-2 pub=_ping-pub]
      (~(part-one transition:st bowl) !<(state-1:st vase))
    [cards this(state new, ping-pub pub)]
  =/  old  !<([state=state-2 sub=_ping-sub pub=_ping-pub] vase)
  :-  (~(poke pass:io /load-poke) [our.bowl %pongo] load+!>(~))^~
  this(state state.old, ping-sub sub.old, ping-pub pub.old)
::
++  on-arvo
  |=  [=wire sign=sign-arvo]
  ^-  (quip card _this)
  ?+  wire  `this
    [~ %sss %behn @ @ @ %ping @ ~]  [(behn:da-ping |3:wire) this]
  ==
::
++  on-watch  ::  FRONTEND ONLY
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
  ?:  =(~ -.wire)
    ?>  ?=(%poke-ack -.sign)
    ?~  p.sign   `this
    ?+    wire   `this
        [~ %sss %on-rock @ @ @ %ping @ ~]
      `this(ping-sub (chit:da-ping |3:wire sign))
    ::
        [~ %sss %scry-request @ @ @ %ping @ ~]
      =^  cards  ping-sub  (tell:da-ping |3:wire sign)
      [cards this]
    ==
  ::
  ?+    -.wire  (on-agent:def wire sign)
      %send-invite
    ?>  ?=(%poke-ack -.sign)
    ?~  p.sign  `this
    ?~  +.wire  `this
    ::  an invite we sent failed, if DM, try to join theirs instead
    ::  and delete our faultily-produced DM convo
    ::  (only DM invites have ID in wire)
    =/  =conversation-id  (slav %ux -.+.wire)
    :_  this
    :~  %+  ~(poke pass:io /leave-bad-dm)
          [our.bowl %pongo]
        pongo-action+!>(`action`[%leave-conversation conversation-id])
    ::
        %+  ~(poke pass:io /request-invite)
          [src.bowl %pongo]
        entry+!>(`entry`[%request-invite conversation-id])
    ==
  ::
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
          %pongo-update
        ::  forward updates along search results path
        =/  tid  -.+.+.wire
        ::  ~&  >  !<(pongo-update q.cage.sign)
        :_  this
        (fact:io cage.sign ~[/search-results/[tid]])^~
      ==
    ==
  ==
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  |^
  ?+    mark  `this
      %ping
    (handle-ping !<(ping vase))
      %pongo-action
    (handle-action !<(action vase))
      %catchup
    =^  cards  state
      (handle-catchup:hc !<(catchup vase))
    [cards this]
      %wallet-update
    =^  cards  state
      (handle-wallet-update:hc !<(wallet-update:wallet vase))
    [cards this]
      %uqbar-share-address
    =^  cards  state
      (handle-address-share:hc !<(share-address:uqbar vase))
    [cards this]
      %sss-pongo-pings
    =^  cards  ping-sub
      (apply:da-ping !<(into:da-ping (fled vase)))
    [cards this]
  ::
      %orgs-action
    ::  send an add or remove member message to an %org-type chat
    ::  based on an update from the %orgs middleware
    ::  currently ignoring any actions other than member add/del
    ::  TODO handle %replace-members, %delete-org
    =/  =action:con:orgs  !<(action:con:orgs vase)
    ?.  ?=(?(%add-member %del-member) -.action)
      `this
    :_  this  :_  ~
    %+  ~(poke pass:io /orgs-member-change)
      [our.bowl %pongo]
    :-  %pongo-action
    !>  ^-  ^action
    :*  %send-message
        ''
        org-id.action
        ?-  -.action  ::  lol
          %add-member  %member-add
          %del-member  %member-remove
        ==
        (scot %p ship.action)
        ~  ~
    ==
  ::
  ::  dumb indirection layer because on-load/on-init in gall are broken
  ::
      %load
    ::  check to make sure nectar has conversations table, add if not
    ~&  >>  "pongo: loaded"
    =/  check=?
      .^  ?  %gx
        (scot %p our.bowl)  %nectar  (scot %da now.bowl)
        /table-exists/pongo/conversations/noun
      ==
    ?.  check
      [(init-tables [our now]:bowl) this]
    ::  for every convo that we don't route for, re-sub to router
    =^  cards  ping-sub
      (~(part-two transition:st bowl) ping-sub)
    [cards this]
  ::
      %entry
    =/  =entry  !<(entry vase)
    :_  this
    ?-    -.entry
        %invite
      ::  we've received an invite to a conversation: automatically accept
      ::  if we *already have this conversation and are router*,
      ::  make sure to REJECT invite AND alert inviter of this
      =/  convo  (fetch-conversation:hc id.conversation.entry)
      ?:  &(?=(^ convo) !deleted.u.convo)
        !!
      ~&  >>  "joining convo"
      :~  (give-update [%invite conversation.entry])
          %+  ~(poke pass:io /accept-invite)  [our.bowl %pongo]
          pongo-action+!>(`action`[%accept-invite src.bowl conversation.entry])
      ==
    ::
        %request-invite
      ?~  convo=(fetch-conversation:hc conversation-id.entry)  !!
      ~&  >>  "giving invite to requester {<src.bowl>}"
      :_  ~
      %+  ~(poke pass:io /send-invite)
        [src.bowl %pongo]
      entry+!>(`^entry`[%invite u.convo])
    ==
  ::
      %sss-on-rock
    ::  here's where we get messages as a non-router
    =/  msg  !<(from:da-ping (fled vase))
    ?.  stale.msg
      ?~  wave.msg  `this  ::  ignore non-waves
      (handle-ping `ping`u.wave.msg)
    ::  we were removed from a convo, delete it and stuff
    =/  =conversation-id  -.+.-.msg
    :_  this
    :+  %+  ~(poke pass:io /leave-convo)
          [our.bowl %nectar]
        :-  %nectar-query
        !>  ^-  query-poke:nectar
        :-  %pongo
        :+  %delete  %conversations
        [%s %id %& %eq conversation-id]
      %+  ~(poke pass:io /drop-convo)
        [our.bowl %nectar]
      :-  %nectar-query
      !>  ^-  query-poke:nectar
      [%pongo %drop-table conversation-id]
    ~
  ::
      %sss-to-pub
    =/  msg  !<(into:du-ping (fled vase))
    =^  cards  ping-pub
      (apply:du-ping msg)
    [cards this]
  ==
  ::
  ::  helpers that manipulate SSS
  ::
  ++  handle-ping
    |=  =ping
    ^-  (quip card _this)
    ?~  conv=(fetch-conversation:hc -.+.ping)
      ~|("pongo: can't find conversation {<-.+.ping>}" !!)
    =*  convo  u.conv
    =?    ping
        &(?=(%message -.ping) =(our.bowl router.convo))
      ::  as router, set proper message ID
      ping(id.message +(last-message.convo))
    =^  router-cards  ping-pub
      ::  do not forward *inbox* messages to anyone
      ?:  |(=(%inbox id.convo) !=(our.bowl router.convo))  `ping-pub
      ::  we are the router; forward the ping to all participants
      ::  if a member is added or removed, add/remove them from
      ::  permissioned watchers on this chat's publication
      ?.  ?&  ?=(%message -.ping)
              ?=(?(%member-add %member-remove) kind.message.ping)
          ==
        (give:du-ping [%ping id.convo ~] ping)
      =.  ping-pub
        %+  perm:du-ping  [%ping id.convo ~]^~
        |=  s=(unit (set @p))
        :-  ~
        =+  (slav %p content.message.ping)
        ?-  kind.message.ping
          %member-add  (~(put in (fall s *(set @p))) -)
            %member-remove
          ?<  =(- router.convo)
          (~(del in (fall s *(set @p))) -)
        ==
      (give:du-ping [%ping id.convo ~] ping)
    ?-    -.ping
        %message
      =*  message  message.ping
      ::  after validating message, insert it in our messages table
      =/  message-hash  (make-message-hash [content author timestamp]:message)
      ~|  "pongo: ignoring invalid message"
      ~|  message
      ?>  ?&  (validate:sig our.bowl p.signature.message message-hash now.bowl)
              (~(has in members.p.meta.convo) author.message)
              (lte (met 3 content.message) message-length-limit)
              (valid-message-contents message convo)
          ==
      ::  set local timestamp (throwing away ability to re-verify sig btw!!!)
      =.  timestamp.message  now.bowl
      ::  depending on message type, apply changes to conversation metadata
      =^  cards  convo
        ?+    kind.message
            ::  text, poll, send-tokens, app-link
            ?:  muted.convo  `convo
            =-  :_  convo
                :-  (give-update [%message id.convo message])
                ?~  -  ~  u.-^~
            %:  give-push-notification
                get-total-unreads:hc  ::  function
                convo  message
                notif-settings.state
                [our now]:bowl
            ==
        ::
            %member-add
          =.  members.p.meta.convo
            (~(put in members.p.meta.convo) (slav %p content.message))
          ?.  =(our.bowl router.convo)  `convo
          =/  p=@p  (slav %p content.message.ping)
          :_  convo  :_  ~
          %+  ~(poke pass:io /send-invite)
            [p %pongo]
          entry+!>(`entry`[%invite convo])
        ::
            %member-remove
          =+  them=(slav %p content.message)
          ?:  =(them router.convo)  `convo
          =.  members.p.meta.convo
            (~(del in members.p.meta.convo) them)
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
      ::  if this message is for a DM that we've "deleted", set conversation
      ::  to undeleted and add a new messages table for it.
      :_  this
      %+  weld
        ?.  &(deleted.convo ?=(%dm -.p.meta.convo))
          cards
        (weld (make-messages-table:hc id.convo our.bowl) cards)
      ^-  (list card)
      :+  %+  ~(poke pass:io /insert)
            [our.bowl %nectar]
          :-  %nectar-query
          !>  ^-  query-poke:nectar
          pongo+[%insert id.convo ~[message]]
        %+  ~(poke pass:io /update-convo)
          [our.bowl %nectar]
        :-  %nectar-query
        !>  ^-  query-poke:nectar
        :-  %pongo
        =-  [%update-rows %conversations ~[-]]
        %=  convo
          deleted       %.n
          last-active   timestamp.message
          last-message  id.message
        ==
      ::  if this message indicates that we're missing messages (ID > last+1)
      ::  try to do a catch-up from router.
      ?:  (gth id.message +(last-message.convo))
        :-  %+  ~(poke pass:io /ask-for-catchup)
              [router.convo %pongo]
            catchup+!>(`catchup`[%request id.convo last-message.convo])
        router-cards
      router-cards
    ::
        ?(%edit %react)
      ::  apply an edit or reaction to a message
      =/  hash  (make-reaction-edit-hash [|4 -:|3]:ping)
      ~|  "pongo: received invalid signature on message edit/react"
      ?>  (validate:sig our.bowl sig.ping hash now.bowl)
      =/  m=(unit message)
        =-  ?~(- ~ `!<(message [-:!>(*message) i.-]))
        =-  (nectar-scry id.convo - [our now]:bowl)
        :+  %select  id.convo
        ?:  ?=(%react -.ping)
          [%s %id %& %eq on.ping]
        [%and [%s %id %& %eq on.ping] [%s %author %& %eq q.sig.ping]]
      ?~  m  `this
      =.  u.m
        ?:  ?=(%edit -.ping)
          u.m(content edit.ping, edited %.y)
        u.m(p.reactions (~(put by p.reactions.u.m) q.sig.ping reaction.ping))
      :_  this
      :+   (give-update [%message id.convo u.m])
        %+  ~(poke pass:io /insert)
          [our.bowl %nectar]
        :-  %nectar-query
        !>  ^-  query-poke:nectar
        pongo+[%update-rows id.convo ~[u.m]]
      router-cards
    ==
  ::
  ++  handle-action
    |=  =action
    ^-  (quip card _this)
    ::
    ::  we receive actions from our own client app
    ::
    ?>  =(our src):bowl
    ?-    -.action
        %make-conversation
      ::  create a new conversation and possibly send invites.
      ::  by default, the ship that does this becomes the router.
      ::  conversation IDs are meant to be *globally* unique
      ::  if a conversation is a DM (1:1 convo) we assign unique ID based
      ::  on the two ship names. DMs cannot be duplicated this way.
      ::
      =?    members.config.action
          ?=(%org -.config.action)
        .^  (set ship)  %gx
          (scot %p our.bowl)  %orgs  (scot %da now.bowl)
          %get-members  (scot %ux id.config.action)
          (snoc name.config.action %noun)
        ==
      =.  members.config.action
        (~(put in members.config.action) our.bowl)
      =/  member-count  ~(wyt in members.config.action)
      ::  generate unique ID
      =/  id=conversation-id
        ?+    -.config.action
            ::  default case
            `@ux`(sham (rap 3 ~[our.bowl now.bowl name.action]))
        ::
            %org  id.config.action
        ::
            %dm
          ::  enforce that we don't already have a DM of this nature
          ::  and that DMs have exactly 2 members
          ?.  =(member-count 2)
            ~|("pongo: error: tried to make multiparty DM" !!)
          `@ux`(sham (rap 3 ~(tap in members.config.action)))
        ==
      =/  convo=conversation
        :*  `@ux`id
            ?+  -.config.action  name.action
              %org  (spat name.config.action)
            ==
            last-active=now.bowl
            last-message=0
            last-read=0
            router=our.bowl
            [%b config.action]
            [%.n %.n ~]
        ==
      ::  add this conversation to our table and create a messages table for it,
      ::  poke all indicated members in metadata with invites
      =/  mems  ~(tap in (~(del in members.config.action) our.bowl))
      ~&  >>  "%pongo: made conversation id: {<id.convo>} and invited {<mems>}"
      ::  why? so that we can immediately set permissions on this path.
      =/  path  [%ping id.convo ~]
      =.  ping-pub  +:(give:du-ping path *ping)
      =.  ping-pub  (rule:du-ping path [0 1])  ::  just keep one wave
      =.  ping-pub
        %+  perm:du-ping  path^~
        |=((unit (set @p)) `members.config.action)
      :_  this
      %+  welp
        (make-messages-table:hc id.convo our.bowl)
      :-  %+  ~(poke pass:io /add-convo)
            [our.bowl %nectar]
          :-  %nectar-query
          !>  ^-  query-poke:nectar
          pongo+[%update-rows %conversations ~[convo]]
      =-  ::  if convo type is %org, start tracking %orgs
          ?.  ?=(%org -.config.action)  -
          :_  -
          %+  ~(poke pass:io /track-orgs)
            [our.bowl %orgs]
          tracker-request+!>(%pongo)
      %+  turn  mems
      |=  to=@p
      %+  ~(poke pass:io /send-invite/(scot %ux id.convo))
        [to %pongo]
      entry+!>(`entry`[%invite convo])
    ::
        %leave-conversation
      ::  leave a conversation we're currently in. poke chat telling it we left,
      ::  delete it from our conversations table, drop the messages table.
      ::
      ::  TODO: for groupchats, if we leave *as router*, assign a new router!!
      ::  for now, router cannot leave.
      ::
      ~&  >>  "leaving conversation"
      ?~  conv=(fetch-conversation:hc conversation-id.action)
        ~|("%pongo: couldn't find that conversation id" !!)
      =*  convo  u.conv
      =?    ping-sub
          !?=(%dm -.p.meta.convo)
        (quit:da-ping router.convo %pongo [%ping id.convo ~])
      :_  this
      :-  %+  ~(poke pass:io /drop-convo)
            [our.bowl %nectar]
          :-  %nectar-query
          !>  ^-  query-poke:nectar
          [%pongo %drop-table id.convo]
      ?:  ?=(%dm -.p.meta.convo)
        ::  for DMs, you don't leave, you just delete your local representation.
        ::  if you get a new message in that DM it will look like the first.
        :~  %+  ~(poke pass:io /leave-convo)
              [our.bowl %nectar]
            :-  %nectar-query
            !>  ^-  query-poke:nectar
            pongo+[%update-rows %conversations ~[convo(deleted %.y)]]
        ==
      ::  for groupchats, tell everyone you're leaving
      ::  TODO if you are the router, assign a new router as you leave.
      ?:  =(our.bowl router.convo)  !!
      :~  %+  ~(poke pass:io /send-member-remove)
            [our.bowl %pongo]
          =-  pongo-action+!>(`^action`[%send-message -])
          ['' id.convo %member-remove (scot %p our.bowl) ~ ~]
      ::
          %+  ~(poke pass:io /leave-convo)
            [our.bowl %nectar]
          :-  %nectar-query
          !>  ^-  query-poke:nectar
          :-  %pongo
          :+  %delete  %conversations
          [%s %id %& %eq conversation-id.action]
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
            timestamp=now.bowl
            message-kind.action
            content.action
            edited=%.n
            reference.action
            [%m ~]  [%s mentions.action]  ~
        ==
      ?~  convo=(fetch-conversation:hc conversation-id.action)
        ~|("%pongo: couldn't find that conversation id" !!)
      :_  this
      :~  (give-update [%sending id.u.convo identifier.action])
          %+  ~(poke pass:io /send-message)
            [router.u.convo %pongo]
          ping+!>(`ping`[%message conversation-id.action message])
      ==
    ::
        ?(%send-message-edit %send-reaction)
      ?~  convo=(fetch-conversation:hc conversation-id.action)
        ~|("%pongo: couldn't find that conversation id" !!)
      =/  hash       (make-reaction-edit-hash |3:action on.action)
      =/  =ship-sig  (sign:sig our.bowl now.bowl hash)
      :_  this  :_  ~
      %+  ~(poke pass:io /send-edit)
        [router.u.convo %pongo]
      :-  %ping
      !>  ^-  ping
      ?-  -.action
        %send-message-edit  edit+id.u.convo^ship-sig^|2:action
        %send-reaction      react+id.u.convo^ship-sig^|2:action
      ==
    ::
        %send-tokens
      =/  tid  `@ta`(cat 3 'token-send_' (scot %uv (sham eny.bowl)))
      =/  ta-now  `@ta`(scot %da now.bowl)
      =/  start-args
        :^  ~  `tid  byk.bowl(r da+now.bowl)
        token-send+!>(`^action`action)
      :_  this  :_  ~
      %+  ~(poke pass:io /thread/[ta-now])
        [our.bowl %spider]
      spider-start+!>(start-args)
    ::
        %read-message
      ::  if read id is newer than current saved read id, replace in convo
      ?~  convo=(fetch-conversation:hc conversation-id.action)
        ~|("%pongo: couldn't find that conversation id" !!)
      ?.  (gth message-id.action last-read.u.convo)  `this
      :_  this
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
      ::  make a %member-add message in the convo; router will make invite
      ?~  convo=(fetch-conversation:hc conversation-id.action)
        ~|("%pongo: couldn't find that conversation id" !!)
      ~&  >>  "inviting {<to.action>} to conversation"
      :_  this  :_  ~
      %+  ~(poke pass:io /send-member-add)
        [our.bowl %pongo]
      =-  pongo-action+!>(`^action`[%send-message -])
      ['' id.u.convo %member-add (scot %p to.action) ~ ~]
    ::
        %accept-invite
      ::  accept an invite we've been sent
      ::  add this convo to our conversations table,
      ::  make a messages table, start surfing the wave
      ~&  >>  "accepting invite"
      =*  convo  conversation.action
      =^  surf-cards  ping-sub
        (surf:da-ping router.convo %pongo [%ping id.convo ~])
      :_  this
      %+  welp  surf-cards
      :_  (make-messages-table:hc id.convo our.bowl)
      %+  ~(poke pass:io /add-convo)
        [our.bowl %nectar]
      :-  %nectar-query
      !>  ^-  query-poke:nectar
      :-  %pongo
      :+  %update-rows  %conversations
      ~[convo(last-active now.bowl, last-read 0)]
    ::
        %search
      ::  search in messages for a phrase. can filter by conversation
      ::  or author. to get results, first subscribe to /search-results
      ::  batch results in groupings of 1.000 messages in order of
      ::  recency to get fast initial returns
      ~&  >>  action
      =/  tid  `@ta`(cat 3 'search_' (scot %ux uid.action))
      =/  ta-now  `@ta`(scot %da now.bowl)
      =/  start-args
        :^  ~  `tid  byk.bowl(r da+now.bowl)
        search+!>(`search`+.+.action)
      :_  this
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
      :_  this  :_  ~
      %+  ~(poke pass:io /thread-stop/[ta-now])
        [our.bowl %spider]
      spider-stop+!>([tid %.y])
    ::
        %set-notifications
      `this(notif-settings.state notif-settings.action)
    ::
        %set-notif-token
      :-  ~
      %=  this
        expo-token.notif-settings.state  expo-token.action
        ship-url.notif-settings.state    ship-url.action
      ==
    ::
        %set-notif-level
      `this(level.notif-settings.state level.action)
    ::
        ?(%mute-conversation %unmute-conversation)
      :_  this  :_  ~
      %+  ~(poke pass:io /mute-unmute-conversation)
        [our.bowl %nectar]
      :-  %nectar-query
      !>  ^-  query-poke:nectar
      :-  %pongo
      :^  %update  %conversations
        [%s %id %& %eq conversation-id.action]
      ~[[%muted |=(v=value:nectar ?:(=(%mute-conversation -.action) %.y %.n))]]
    ==
  --
--
::
::  helper core for arms that want the bowl in subject
::
|_  bowl=bowl:gall
++  handle-catchup
  |=  =catchup
  ^-  (quip card _state)
  ?~  conv=(fetch-conversation conversation-id.catchup)
    ~|("pongo: can't find conversation {<conversation-id.catchup>}" !!)
  =*  convo  u.conv
  ?-    -.catchup
      %request
    ::  we are router, member wants some messages
    ::  make sure they're allowed to see, then give
    ?>  (~(has in members.p.meta.convo) src.bowl)
    ::  TODO enforce beginning of catchup based on member's join time
    =/  missed=(list message)
      %+  turn
        =-  (nectar-scry id.convo - [our now]:bowl)
        [%select id.convo where=[%s %id %& %gth from.catchup]]
      |=  =row:nectar
      !<(message [-:!>(*message) row])
    :_  state  :_  ~
    %+  ~(poke pass:io /give-catchup)  [src.bowl %pongo]
    catchup+!>(`^catchup`[%receive id.convo missed])
  ::
      %receive
    ::  list of missing messages returned by router
    ::  integrate them with our convo
    ?>  =(src.bowl router.convo)
    :_  state
    :+  (give-update [%message-list messages.catchup])
      %+  ~(poke pass:io /integrate-catchup)  [our.bowl %nectar]
      :-  %nectar-query
      !>  ^-  query-poke:nectar
      [%pongo %update-rows id.convo messages.catchup]
    ~
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
    ?:  deleted.convo  [convo ~ 0]
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
    ?~  convo=(fetch-conversation convo-id)  ~
    ?:  deleted.u.convo  ~
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
    ?:  deleted.u.convo  ~
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
    ?:  deleted.u.convo  [~ ~]
    =-  ``pongo-update+!>(`pongo-update`[%notification -])
    :-  name.u.convo
    =<  [author content]
    !<  message
    :-  -:!>(*message)
    %-  head
    =-  (nectar-scry id.u.convo - [our now]:bowl)
    [%select id.u.convo [%s %id %& %eq message-id]]
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
  %+  roll  rows
  |=  [=row:nectar i=@ud]
  =+  !<(conversation [-:!>(*conversation) row])
  ?:  (gte [last-read last-message]:-)  i
  (add i (sub [last-message last-read]:-))
--
