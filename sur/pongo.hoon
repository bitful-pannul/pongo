/-  uqbar=zig-uqbar
|%
+$  notif-settings
  $:  expo-token=@t
      ship-url=@t
      level=?(%off %low %medium %high)
  ==
+$  ship-sig  [p=@ux q=ship r=life]
::
::  message table schema: a table handles one conversation
::
++  messages-schema
  :~  [%id [0 | %ud]]           ::  ordering produced by message router
      [%author [1 | %p]]
      [%signature [2 | %blob]]
      [%timestamp [3 | %da]]    ::  time that router received message at
      [%kind [4 | %tas]]
      [%content [5 | %t]]
      [%edited [6 | %f]]
      [%reference [7 & %ud]]    ::  for replies
      [%reactions [8 | %map]]
      [%mentions [9 | %set]]
  ==
::
::  indices: columns in table we keep an index of
::  compute time to handle a message guarantees unique timestamps :)
::
++  messages-indices
  :~  [~[%id] primary=& autoincrement=~ unique=& clustered=&]
      [~[%timestamp] primary=| autoincrement=~ unique=& clustered=&]
      ::  can add an author index if we want to add search by author
  ==
::
::  message mold must match message schema
::
+$  message
  $:  id=message-id
      author=@p
      signature=[%b p=ship-sig]
      timestamp=@da
      kind=message-kind
      content=@t
      edited=?
      reference=(unit message-id)
      reactions=[%m p=(map @p reaction)]
      mentions=[%s p=(set @p)]
      ~
  ==
::
::  a message id is an ordered integer starting at 0
::
+$  message-id  @ud
::
::  a message can be one of these things -- messages that want to
::  be many things can be broken into multiple messages.
::
+$  message-kind
  $?  %text
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
+$  reaction  @t
::
::  conversations schema: we keep one table of all our conversations
::
++  conversations-schema
  :~  [%id [0 | %ux]]            ::  also the name of a convo's messages table
      [%name [1 | %t]]
      [%last-active [2 | %da]]
      [%last-message [3 | %ud]]
      [%last-read [4 | %ud]]     ::  id of message we last saw
      [%router [5 | %p]]
      [%members [6 | %blob]]     ::  members and type of convo
      [%deleted [7 | %f]]
      [%muted [8 | %f]]
  ==
::
++  conversations-indices
  :~  [~[%id] primary=& autoincrement=~ unique=& clustered=|]
      [~[%last-active] primary=| autoincrement=~ unique=| clustered=&]
  ==
::
::  a groupchat id must be globally unique, so that tables can
::  be synced by anyone. it is constructed by the +sham of
::  the creator @p, time at creation, and the conversation's name.
::  DM ids are made by +sham-ing the two @ps involved
::
+$  conversation-id  @ux
::
::  conversation mold must match conversations schema
::
+$  conversation
  $:  id=conversation-id
      name=@t
      last-active=@da
      last-message=message-id
      last-read=message-id
      router=@p
      meta=[%b p=conversation-metadata]
      deleted=?
      muted=?
      ~
  ==
::
+$  conversation-metadata
  $%  [%managed members=(set @p) leaders=(set @p)]
      [%open members=(set @p) ~]  ::  hate this ~
      [%dm members=(set @p) ~]
      [%inbox members=(set @p) ~]  ::  only ever contains us
  ==
::
::  all messaging is done through pings.
::  pings are all sent to router
::
+$  ping
  $%  [%message =conversation-id =message]
      [%edit =conversation-id sig=ship-sig on=message-id edit=@t]
      [%react =conversation-id sig=ship-sig on=message-id =reaction]
  ==
::
::  entry pokes handle creating and joining conversations.
::
+$  entry  [%invite =conversation]  ::  router sends this
::
::  pokes that our frontend performs:
::
+$  action
  $%  [%make-conversation name=@t config=conversation-metadata]
      [%leave-conversation =conversation-id]
      ::
      $:  %send-message
          identifier=@t
          =conversation-id
          =message-kind
          content=@t
          reference=(unit message-id)
          mentions=(set @p)
      ==
      [%send-message-edit =conversation-id on=message-id edit=@t]
      [%send-reaction =conversation-id on=message-id =reaction]
      ::
      $:  %send-tokens
          =conversation-id
          from=@ux
          contract=@ux
          town=@ux
          to=@p
          amount=@ud
          item=@ux
      ==
      ::  frontend telling us we've seen up to message-id in convo
      [%read-message =conversation-id =message-id]
      ::
      [%make-invite to=@p =conversation-id]
      [%accept-invite from=@p =conversation]
      ::
      $:  %search  uid=@ux
          only-in=(unit conversation-id)
          only-author=(unit @p)
          phrase=@t
      ==
      [%cancel-search uid=@ux]
      ::
      [%set-notifications =notif-settings]
      [%set-notif-token expo-token=@t ship-url=@t]
      [%set-notif-level level=?(%off %low %medium %high)]
      [%mute-conversation =conversation-id]
      [%unmute-conversation =conversation-id]
  ==
::
::  update types from scries and subscriptions, used for interacting with FE
::
+$  pongo-update
  $%  [%conversations (list conversation-info)]
      [%message-list (list message)]
      [%message =conversation-id =message]  ::  tell frontend about new message
      [%invite conversation]                                    ::  new invite
      [%sending =conversation-id identifier=@t]
      [%delivered =conversation-id identifier=@t =message-id]
      [%search-result (list [=conversation-id =message])]
      [%notification convo-name=@t author=@p content=@t]
      [%notif-settings notif-settings]
  ==
::  updates we sent to token-send thread
+$  thread-update
  $%  [%denied from=@p]
      [%shared from=@p address=@ux]
      [%finished hash=@ux sequencer-receipt:uqbar]
  ==
::
+$  conversation-info
  $:  conversation
      last=(unit message)
      unreads=@ud
  ==
--