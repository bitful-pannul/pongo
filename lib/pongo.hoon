/-  *pongo, nectar
/+  sig, io=agentio
|%
++  nectar-scry
  |=  [table=@ =query:nectar our=@p now=@da]
  ^-  (list row:nectar)
  .^  (list row:nectar)  %gx
    (scot %p our)  %nectar  (scot %da now)
    /jammed-query/pongo/[table]/(jam query)/noun
  ==
::
++  give-update
  |=  upd=pongo-update
  ^-  card:agent:gall
  ::  ~&  >>  "giving fact to frontend: "
  ::  ~&  >>  (crip (en-json:html (update-to-json:parsing upd)))
  (fact:io pongo-update+!>(upd) ~[/updates])
::
++  init-tables
  |=  [our=@p now=@da]
  ^-  (list card:agent:gall)
  %+  welp  (make-messages-table %inbox our)
  :~  %+  ~(poke pass:io /make-table)  [our %nectar]
      :-  %nectar-query
      !>  ^-  query-poke:nectar
      :^  %pongo  %add-table  %conversations
      ^-  table:nectar
      :^    (make-schema:nectar conversations-schema)
          primary-key=~[%id]
        (make-indices:nectar conversations-indices)
      ~
  ::
      %+  ~(poke pass:io /make-private)
        [our %nectar]
      :-  %nectar-set-perms
      !>  ^-  set-perms:nectar
      [%pongo %conversations]^[%private ~]
  ::
      %+  ~(poke pass:io /make-inbox)  [our %nectar]
      :-  %nectar-query
      !>  ^-  query-poke:nectar
      :^  %pongo  %insert  %conversations
      :_  ~
      :*  `@ux`%inbox
          'Inbox'
          last-active=now
          last-message=0
          last-read=0
          router=our
          [%b [%inbox [our ~ ~] ~]]
          [%.n %.n ~]
      ==
  ::  welcome to pongo message!
      %+  ~(poke pass:io /make-welcome-message)
        [our %pongo]
      =-  pongo-action+!>(`action`[%send-message -])
      ['' `@ux`%inbox %text 'Welcome to Pongo!' ~ ~]
  ==
::
++  make-messages-table
  |=  [id=@ our=@p]
  ^-  (list card:agent:gall)
  :+  %+  ~(poke pass:io /make-table)
        [our %nectar]
      :-  %nectar-query
      !>  ^-  query-poke:nectar
      :-  %pongo
      :+  %add-table  id
      :^    (make-schema:nectar messages-schema)
          primary-key=~[%id]
        (make-indices:nectar messages-indices)
      ~
    %+  ~(poke pass:io /make-private)
      [our %nectar]
    :-  %nectar-set-perms
    !>  ^-  set-perms:nectar
    [%pongo id]^[%private ~]
  ~
::
::  +valid-message-contents: verify that messages which adjust the
::  "leadership structure" of the groupchat are performed by those
::  with the privilege to do so.
::
++  valid-message-contents
  |=  [=message convo=conversation]
  ^-  ?
  ?:  ?=(%inbox -.p.meta.convo)
    ::  these three message types are only ones allowed in inbox
    ?=(?(%text %send-tokens %app-link) kind.message)
  ?+    kind.message  %.y
      %member-remove
    ?.  (~(has in members.p.meta.convo) (slav %p content.message))
      %.n
    ?:  =(author.message (slav %p content.message))  %.y
    ?-  -.p.meta.convo
      ?(%open %dm)  %.n
      %managed      (~(has in leaders.p.meta.convo) author.message)
    ==
  ::
      ?(%member-add %change-name)
    ?:  ?&  ?=(%member-add kind.message)
            (~(has in members.p.meta.convo) (slav %p content.message))
        ==
      %.n
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
++  give-push-notification
  |=  [unreads=@ud =conversation =message =notif-settings our=ship now=@da]
  ^-  (unit card:agent:gall)
  ?:  ?|  ?=(%off level.notif-settings)
          =('' expo-token.notif-settings)
          =('' ship-url.notif-settings)
          =(our author.message)
      ==
    ~
  ::  send http request
  ::
  =|  =request:http
  =:  method.request       %'POST'
      url.request          'https://exp.host/--/api/v2/push/send'
      header-list.request  ~[['Content-Type' 'application/json']]
      body.request
    :-  ~
    %-  as-octt:mimes:html
    %-  en-json:html
    %-  pairs:enjs:format
    :~  to+s+expo-token.notif-settings
        :-  %badge
        (numb:enjs:format unreads)
    ::
        :-  %title
        ?-    level.notif-settings
            %high  s+''
            ?(%low %medium)
          :-  %s
          ?:  (gth ~(wyt in members.p.meta.conversation) 2)
            name.conversation
          (scot %p author.message)
        ==
    ::
        :-  %body
        ?-  level.notif-settings
          %high    s+''
            %medium
          ?.  (~(has in p.mentions.message) our)
            s+'New message'
          s+'Someone mentioned you'
            %low
          ?.  (~(has in p.mentions.message) our)
            s+(rap 3 ~[(scot %p author.message) ': ' content.message])
          s+(rap 3 ~[(scot %p author.message) ' mentioned you: ' content.message])
        ==
    ::
        :-  %data
        %-  pairs:enjs:format
        :~  ['ship' s+(scot %p our)]
            ['ship_url' s+ship-url.notif-settings]
            ['conversation_id' s+(scot %ux id.conversation)]
            ['message_id' s+(scot %ud id.message)]
        ==
    ==
  ==
  :-  ~
  :*  %pass  /push-notification/(scot %da now)
      %arvo  %i  %request
      request  *outbound-config:iris
  ==
::
::  search thread stuff
::
::  type used for search threads
+$  search
  $:  only-in=(unit conversation-id)
      only-author=(unit @p)
      phrase=@t
  ==
::
++  do-search
  |=  [search our=@p now=@da]
  ^-  (list [conversation-id message])
  |^
  ?^  only-in
    ::  search is limited to one conversation
    (single-convo u.only-in)
  ::  search is across *all* conversations
  %-  zing
  %+  turn
    =+  [%select %conversations where=[%s %deleted %& %eq %.n]]
    (nectar-scry %conversations - our now)
  |=  =row:nectar
  =+  convo=!<(conversation [-:!>(*conversation) row])
  (single-convo id.convo)
  ::
  ++  single-convo
    |=  id=conversation-id
    ^-  (list [conversation-id message])
    %+  turn
      =-  (nectar-scry id - our now)
      :+  %select  id
      =+  [%s %content %& %text-find (trip phrase)]
      ?~  only-author  -
      [%and [%s %author %& %eq u.only-author] -]
    |=  =row:nectar
    [id !<(message [-:!>(*message) row])]
  --
::
::  utils
::
++  make-message-hash
  |=  [content=@t src=@p now=@da]
  ^-  @
  %-  sham
  %+  rap  3
  :~  'signed pongo message by '
      (scot %p src)
      ' at '
      (scot %da now)
      ': '
      content
  ==
::
++  make-reaction-edit-hash
  |=  [=reaction on=message-id]
  ^-  @
  %-  sham
  %+  rap  3
  :~  'signed-pongo-react-edit: '
      reaction
      'on message '
      (scot %ud on)
  ==
::
::  json creation
::
++  parsing
  =,  enjs:format
  |%
  ++  message-to-json
    |=  [m=message c=(unit conversation-id)]
    ^-  json
    %-  pairs
    :*  ['id' s+(scot %ud id.m)]
        ['author' s+(scot %p author.m)]
        ::  don't share signatures
        ['timestamp' (sect timestamp.m)]
        ['kind' s+(scot %tas kind.m)]
        ['content' s+content.m]
        ['edited' b+edited.m]
        ['reference' ?~(reference.m ~ s+(scot %ud u.reference.m))]
        :-  'reactions'
        %-  pairs
        ::  transform (map @p reaction) into a (map reaction (list @p))
        =+  all=~(tap by p.reactions.m)
        =|  res=(jar @t @p)
        |-
        ?~  all
          %+  turn  ~(tap by res)
          |=  [t=@t ps=(list @p)]
          [t a+(turn ps ship)]
        $(all t.all, res (~(add ja res) q.i.all p.i.all))
        ::
        ['mentions' a+(turn ~(tap in p.mentions.m) ship)]
        ?~  c  ~
        ['conversation_id' s+(scot %ux u.c)]^~
    ==
  ::
  ++  conversation-to-json
    |=  c=conversation
    ^-  json
    %-  pairs
    :~  ['id' s+(scot %ux id.c)]
        ::  don't share messages table id
        ['name' s+name.c]
        ['last_active' (sect last-active.c)]
        ['last_read' s+(scot %ud last-read.c)]
        ::  don't share router node
        ['dm' b+?=(%dm -.p.meta.c)]
        ['members' a+(turn ~(tap in members.p.meta.c) ship)]
        :-  'leaders'
        ?+  -.p.meta.c  ~
          %managed      a+(turn ~(tap in leaders.p.meta.c) ship)
        ==
        ['muted' b+muted.c]
    ==
  ::
  ++  update-to-json
    |=  upd=pongo-update
    ^-  json
    ?-    -.upd
        %conversations
      %+  frond  'conversations'
      %-  pairs
      %+  turn  +.upd
      |=  ci=conversation-info
      :-  (scot %ux id.ci)
      %-  pairs
      :~  ['conversation' (conversation-to-json -.ci)]
          ['unreads' (numb unreads.ci)]
          :-  'last_message'
          ?~(last.ci ~ (message-to-json:parsing u.last.ci ~))
      ==
    ::
        %message-list
      %+  frond  'message_list'
      :-  %a
      %+  turn  +.upd
      |=  =message
      (message-to-json:parsing message ~)
    ::
        %message
      %+  frond  'message'
      %-  pairs
      :~  ['conversation_id' s+(scot %ux conversation-id.upd)]
          ['message' (message-to-json:parsing message.upd ~)]
      ==
    ::
        %invite
      %+  frond  'invite'
      (conversation-to-json:parsing +.upd)
    ::
        %sending
      %+  frond  'sending'
      %-  pairs
      :~  ['conversation_id' s+(scot %ux conversation-id.upd)]
          ['identifier' s+identifier.upd]
      ==
    ::
        %delivered
      %+  frond  'delivered'
      %-  pairs
      :~  ['conversation_id' s+(scot %ux conversation-id.upd)]
          ['identifier' s+identifier.upd]
          ['message_id' s+(scot %ud message-id.upd)]
      ==
    ::
        %search-result
      %+  frond  'search_result'
      :-  %a
      %+  turn  +.upd
      |=  [c=conversation-id =message]
      (message-to-json:parsing message `c)
    ::
        %notification
      %+  frond  'notification'
      %-  pairs
      :~  ['convo_name' s+convo-name.upd]
          ['author' (ship author.upd)]
          ['content' s+content.upd]
      ==
    ::
        %notif-settings
      %+  frond  'notif-settings'
      %-  pairs
      :~  ['expo_token' s+expo-token.upd]
          ['ship_url' s+ship-url.upd]
          ['level' s+(scot %tas level.upd)]
      ==
    ==
  --
::
::  NOT USING THESE ATM
::
++  print-message
  |=  =message
  ^-  @t
  ?+    kind.message
      %+  rap  3
      :~  'Message ('
          (scot %ud id.message)
          ') from '
          (scot %p author.message)
          ': '
          content.message
      ==
  ::
      %member-add
    %^  cat  3
      content.message
    ' joined the conversation.'
  ::
      %member-remove
    %^  cat  3
      content.message
    ' left the conversation.'
  ::
      %change-name
    %^  cat  3
      'Conversation name changed to '
    (cat 3 content.message '.')
  ::
      %leader-add
    %^  cat  3
      content.message
    ' is now managing the conversation.'
  ::
      %leader-remove
    %^  cat  3
      content.message
    ' is no longer managing the conversation.'
  ==
::
++  print-reaction
  |=  [src=ship =ping]
  ^-  @t
  ?>  ?=(%react -.ping)
  %-  crip
  "{<src>} reacted {<reaction.ping>} to message {<on.ping>}"
::
++  print-edit
  |=  [src=ship =ping]
  ^-  @t
  ?>  ?=(%edit -.ping)
  %-  crip
  "{<src>} edited message {<on.ping>} to {<edit.ping>}"
--