/-  *pongo, uqbar=zig-uqbar, wallet=zig-wallet,
    nectar, table-updates
/+  verb, dbug, default-agent, io=agentio,
    *pongo, *sss, sig
|%
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
      invites-sent=(jug conversation-id @p)
  ==
+$  card  card:agent:gall
--
::
^-  agent:gall
%+  verb  &
%-  agent:dbug
::
=|  state=state-2
=<  |_  =bowl:gall
    +*  this  .
        hc    ~(. +> bowl)
        def   ~(. (default-agent this %|) bowl)
    ::
    ++  on-init
      :_  this(state [%2 ['' '' %low] ~])
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
    ++  on-save  !>(state)
    ::
    ++  on-load
      |=  =vase
      ^-  (quip card _this)
      ?:  =(%0 -.q.vase)  on-init
      ?:  =(%1 -.q.vase)  on-init
      =/  old  !<(state-2 vase)
      ::  check to make sure nectar has conversations table, add if not
      :_  this(state old)
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
      =^  cards  state
        ?+    mark  `state
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
    ++  on-arvo   on-arvo:def
    ++  on-leave  on-leave:def
    ++  on-fail   on-fail:def
    --
::
|_  bowl=bowl:gall
++  handle-ping
  |=  =ping
  ^-  (quip card _state)
  ?~  conv=(fetch-conversation -.+.ping)
    ~|("pongo: can't find conversation {<-.+.ping>}" !!)
  =*  convo  u.conv
  =?    ping
      &(?=(%message -.ping) =(our.bowl router.convo))
    ::  as router, set proper message ID
    ping(id.message +(last-message.convo))
  =/  router-cards=(list card)
    ::  TODO replace this with SSS
    ?.  =(our.bowl router.convo)  ~
    ::  we are the router; forward the ping to all participants
    %+  turn  ~(tap in (~(del in members.p.meta.convo) our.bowl))
    |=  =ship
    (~(poke pass:io /routed-message) [ship %pongo] ping+!>(ping))
  ?-    -.ping
      %message
    =*  message  message.ping
    ::  after validating message, insert it in our messages table
    =/  message-hash  (make-message-hash [content author timestamp]:message)
    ~|  "pongo: received invalid message"
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
    ::  if this message is for a DM that we've "deleted", set conversation
    ::  to undeleted and add a new messages table for it.
    :_  state
    %+  weld
      ?.  &(deleted.convo ?=(%dm -.p.meta.convo))
        cards
      (weld (make-messages-table id.convo) cards)
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
    router-cards
  ::
      %edit
    ::  apply an edit to a message
    =/  edit-hash  (make-reaction-edit-hash [edit on]:ping)
    ~|  "pongo: received invalid signature on message edit"
    ?>  (validate:sig our.bowl sig.ping edit-hash now.bowl)
    :_  state
    :_  router-cards
    %+  ~(poke pass:io /insert)
      [our.bowl %nectar]
    :-  %nectar-query
    !>  ^-  query-poke:nectar
    :-  %pongo
    :^  %update  id.convo
      :+  %and  [%s %id %& %eq on.ping]
      :+  %and  [%s %author %& %eq q.sig.ping]
      [%s %kind %& %eq %text]
    :~  [%content |=(value:nectar edit.ping)]
        [%edited |=(value:nectar %.y)]
    ==
  ::
      %react
    ::  apply a reaction to a message
    =/  reaction-hash  (make-reaction-edit-hash [reaction on]:ping)
    ~|  "pongo: received invalid signature on message reaction"
    ?>  (validate:sig our.bowl sig.ping reaction-hash now.bowl)
    :_  state
    :_  router-cards
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
    :_  state
    :~  (give-update [%invite conversation.entry])
        %+  ~(poke pass:io /accept-invite)  [our.bowl %pongo]
        pongo-action+!>(`action`[%accept-invite src.bowl conversation.entry])
    ==
  =/  convo  (need (fetch-conversation conversation-id.entry))
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
::  +valid-message-contents: verify that messages which adjust the
::  "leadership structure" of the groupchat are performed by those
::  with the privilege to do so.
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
  |=  =action
  ^-  (quip card _state)
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
      `@ux`(sham (rap 3 ~[our.bowl now.bowl name.action]))
    =/  convo=conversation
      :*  `@ux`id
          name.action
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
    =.  invites-sent.state
      |-  ?~  mems  invites-sent.state
      %=  $
        mems  t.mems
        invites-sent.state  (~(put ju invites-sent.state) id.convo i.mems)
      ==
    ~&  >>  "%pongo: made conversation id: {<id.convo>} and invited {<mems>}"
    :_  state
    %+  welp  (make-messages-table id.convo)
    :-  %+  ~(poke pass:io /add-convo)
          [our.bowl %nectar]
        :-  %nectar-query
        !>  ^-  query-poke:nectar
        pongo+[%update-rows %conversations ~[convo]]
    %+  turn  mems
    |=  to=@p
    %+  ~(poke pass:io /send-invite)  [to %pongo]
    entry+!>(`entry`[%invite convo])
  ::
      %leave-conversation
    ::  leave a conversation we're currently in. poke chat telling it we left,
    ::  delete it from our conversations table, drop the messages table.
    ::
    ::  TODO: for groupchats, if we leave *as router*, assign a new router!!
    ::
    :_  state
    ?~  conv=(fetch-conversation conversation-id.action)
      ~|("%pongo: couldn't find that conversation id" !!)
    =*  convo  u.conv
    :-  %+  ~(poke pass:io /drop-convo)
          [our.bowl %nectar]
        :-  %nectar-query
        !>  ^-  query-poke:nectar
        [%pongo %drop-table id.convo]
    ?:  ?=(%dm -.p.meta.convo)
      ::  for DMs, you don't "leave", you just delete your local representation.
      ::  if you get a new message in that DM it will look like the first.
      :~  %+  ~(poke pass:io /leave-convo)
            [our.bowl %nectar]
          :-  %nectar-query
          !>  ^-  query-poke:nectar
          pongo+[%update-rows %conversations ~[convo(deleted %.y)]]
      ==
    ::  for groupchats, tell everyone you're leaving
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
    ?~  convo=(fetch-conversation conversation-id.action)
      ~|("%pongo: couldn't find that conversation id" !!)
    :_  state
    :~  (give-update [%sending id.u.convo identifier.action])
        %+  ~(poke pass:io /send-message)
          [router.u.convo %pongo]
        ping+!>(`ping`[%message conversation-id.action message])
    ==
  ::
      ?(%send-message-edit %send-reaction)
    ?~  convo=(fetch-conversation conversation-id.action)
      ~|("%pongo: couldn't find that conversation id" !!)
    =/  hash  (make-reaction-edit-hash |3:action on.action)
    =/  =ship-sig
      (sign:sig our.bowl now.bowl hash)
    :_  state  :_  ~
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
    :_  state  :_  ~
    %+  ~(poke pass:io /thread/[ta-now])
      [our.bowl %spider]
    spider-start+!>(start-args)
  ::
      %read-message
    ::  if read id is newer than current saved read id, replace in convo
    ?~  convo=(fetch-conversation conversation-id.action)
      ~|("%pongo: couldn't find that conversation id" !!)
    ?.  (gth message-id.action last-read.u.convo)  `state
    :_  state
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
    :_  state(invites-sent (~(put ju invites-sent.state) id.u.convo to.action))
    :_  ~
    %+  ~(poke pass:io /send-invite)
      [to.action %pongo]
    entry+!>(`entry`[%invite u.convo])
  ::
      %accept-invite
    ::  accept an invite we've been sent
    ::  add this convo to our conversations table, make a messages table
    =/  [from=@p convo=conversation]  +.action
    =.  members.p.meta.convo
      (~(put in members.p.meta.convo) our.bowl)
    :_  state
    :+  %+  ~(poke pass:io /accept-invite)
          [from %pongo]
        entry+!>(`entry`[%accept-invite id.convo])
      %+  ~(poke pass:io /add-convo)
        [our.bowl %nectar]
      :-  %nectar-query
      !>  ^-  query-poke:nectar
      :-  %pongo
      :+  %insert  %conversations
      ~[convo(last-active now.bowl, last-read 0)]
    (make-messages-table id.convo)
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
    ::  TODO create an optimized join for this query
    =-  ``pongo-update+!>(`pongo-update`[%conversations -])
    ^-  (list conversation-info)
    %+  turn
      ::  grab all conversations
      =+  [%select %conversations where=[%n ~]]
      (nectar-scry %conversations - [our now]:bowl)
    |=  =row:nectar
    =/  convo  !<(conversation [-:!>(*conversation) row])
    ?:  deleted.convo
      [convo ~ 0]
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
::
++  make-messages-table
  |=  id=@
  ^-  (list card)
  :+  %+  ~(poke pass:io /make-table)
        [our.bowl %nectar]
      :-  %nectar-query
      !>  ^-  query-poke:nectar
      :-  %pongo
      :+  %add-table  id
      :^    (make-schema:nectar messages-schema)
          primary-key=~[%id]
        (make-indices:nectar messages-indices)
      ~
    %+  ~(poke pass:io /make-private)
      [our.bowl %nectar]
    :-  %nectar-set-perms
    !>  ^-  set-perms:nectar
    [%pongo id]^[%private ~]
  ~
--