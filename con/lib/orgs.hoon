::  lib/orgs.hoon  [UQ| DAO]
/+  *zig-sys-smart
=>  |%
    ::  sub-orgs are stacked to create tags
    ::  note: each org can have its own controller,
    ::  but every org "above" in its tag-path can
    ::  unilaterally modify.
    +$  org
      $:  name=@t
          parent-path=path
          desc=(unit @t)  ::  optional label or description
          controller=id   ::  contract that is permitted to edit org
          members=(pset ship)
          sub-orgs=(pmap @t org)
      ==
    --
|%
+$  tag  path
::
+$  action
  $%
  ::  create top level org––parent-path will be ignored
    [%create =org]
  ::
  ::  used to modify contents of an org. *cannot edit name-path*
  ::  tag selects which org/sub-org to edit
    [%edit-org org-id=id where=tag desc=(unit @t) controller=(unit id)]
  ::
  ::  must nest underneath existing org-structure
    [%add-sub-org org-id=id where=tag =org]
  ::
  ::  "it's over" –– calling on top-level org is an item burn
    [%delete-org org-id=id where=tag ~]
  ::
  ::  replace existing member-set of an org/sub-org
  ::  TODO remove this?
    [%replace-members org-id=id where=tag new=(pset ship)]
  ::
  ::  add address to member-set of an org/sub-org
    [%add-member org-id=id where=tag =ship]
  ::
  ::  remove address from member-set of an org/sub-org
    [%del-member org-id=id where=tag =ship]
  ==
::
::  matches %social-graph API, but always address->ships
::  nests under contract `event`
::  tags are always absolute, relative to a top-level org
::
+$  org-event
  $%  [%add-tag =tag from=[%address address] to=[%ship @p]]
      [%del-tag =tag from=[%address address] to=[%ship @p]]
      [%nuke-tag =tag]  ::  remove this tag from all edges
      [%nuke-top-level-tag =tag]  :: remove all tags with same first element
  ==
::
::  helpers for producing events
::
++  add-tag
  |=  [=tag =id =ship]
  ^-  (list org-event)
  [%add-tag tag [%address id] [%ship ship]]^~
::
++  del-tag
  |=  [=tag =id =ship]
  ^-  (list org-event)
  [%del-tag tag [%address id] [%ship ship]]^~
::
++  nuke-tag
  |=  =tag
  ^-  (list org-event)
  [%nuke-tag tag]^~
::
++  make-tag
  |=  [=tag =id members=(pset ship)]
  ^-  (list event)
  %+  turn  ~(tap pn members)
  |=  =ship
  [%add-tag tag [%address id] [%ship ship]]
::
::  make all the add-tag events for a new org
::
++  produce-org-events
  |=  [=id =org]
  ^-  (list org-event)
  =/  =tag
    ?~  parent-path.org  /[name.org]
    (snoc parent-path.org name.org)
  ?>  ?=(^ tag)
  %+  weld
    %+  turn  ~(tap pn members.org)
    |=  =ship
    [%add-tag tag [%address id] [%ship ship]]
  ^-  (list org-event)
  %-  zing
  %+  turn  ~(val py sub-orgs.org)
  |=  sub=^org
  (produce-org-events id sub)
::
::  cannot touch name.org
+$  org-mod  $-(org org)
::
::  given an org, modify either that org or sub-org within
::
++  modify-org
  |=  [=org at=tag =org-mod]
  ^+  org
  ?~  at  (org-mod org)
  ?>  =(i.at name.org)
  ?~  t.at  (org-mod org)
  %=    org
      sub-orgs
    %+  ~(put py sub-orgs.org)
      i.t.at
    $(at t.at, org (~(got py sub-orgs.org) i.t.at))
  ==
::
::  given a path, find allowed controllers for sub-org
::
++  valid-controller
  |=  [where=tag =org who=id]
  ^-  ?
  ?:  =(who controller.org)  %.y
  ?~  where  %.n
  ?>  =(i.where name.org)
  ?~  t.where  %.n
  $(where t.where, org (~(got py sub-orgs.org) i.t.where))
--
