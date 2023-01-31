/-  p=pongo
::
|_  =action:p
++  grab
  |%
  ++  noun  action:p
  ++  json
    |=  jon=^json
    =,  dejs:format
    |^
    %-  action:p
    =-  ?.  =(%'make-conversation' -.-)  -
        ?.  =(%'open' -.+.+.-)  -
        -(+ [-.+.- -.+.+.- +.+.+.- ~])
    %.  jon
    %-  of
    :~  [%make-conversation (ot ~[[%name so] [%config parse-config]])]
        [%make-conversation-from-posse (ot ~[[%name so] [%tag so]])]
        [%leave-conversation (ot ~[[%convo (se %ux)]])]
        ::
        :-  %send-message
        %-  ot
        :~  [%identifier so]
            [%convo (se %ux)]
            [%kind (se %tas)]
            [%content so]
            ::  doesn't want dots
            [%reference (su dem):dejs-soft:format]
        ==
        ::
        :-  %send-message-edit
        (ot ~[[%convo (se %ux)] [%on (se %ud)] [%edit so]])
        ::
        :-  %send-reaction
        (ot ~[[%convo (se %ux)] [%on (se %ud)] [%reaction so]])
        ::
        [%read-message (ot ~[[%convo (se %ux)] [%message (se %ud)]])]
        [%make-invite (ot ~[[%to (se %p)] [%id (se %ux)]])]
        [%accept-invite (ot ~[[%id (se %ux)]])]
        [%reject-invite (ot ~[[%id (se %ux)]])]
        [%make-invite-request (ot ~[[%to (se %p)] [%id (se %ux)]])]
        [%block (ot ~[[%who (se %p)]])]
        [%unblock (ot ~[[%who (se %p)]])]
        ::
        :-  %search
        %-  ot
        :~  [%uid (se %ux)]
            [%only-in (su hex):dejs-soft:format]  ::  doesn't want dots or 0x
            [%only-author (su fed:ag):dejs-soft:format]  ::  doesn't want ~
            [%phrase so]
        ==
        ::
        [%cancel-search (ot ~[[%uid (se %ux)]])]
        [%mute-conversation (ot ~[[%id (se %ux)]])]
        [%unmute-conversation (ot ~[[%id (se %ux)]])]
    ==
    ++  parse-config
      ::  %-  conversation-metadata:p
      %-  of
      :~  [%managed (ot ~[[%members (as (se %p))] [%leaders (as (se %p))]])]
          [%open (ot ~[[%members (as (se %p))]])]
      ==
    --
  --
::
++  grow
  |%
  ++  noun  action
  --
::
++  grad  %noun
--
