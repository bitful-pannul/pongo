::  simple nickname & pfp registry
::  contract agnostic
::
::
/+  smart=zig-sys-smart
|%
::
+$  profile  [name=@t uri=@t address=@ux item=@ux sig=(unit sig)]
::  
+$  niccbook  (map ship profile)
::
+$  action
  $%
    [%whodis =ship]
    [%disme item=@ux address=@ux =sig]
    [%mint name=@t uri=@t nft=@ux address=@ux ship=?]
    [%set-profile item=@ux address=@ux]
    [%sign-ship address=@ux]
    [%find-ships ships=(list ship)]
    [%tell-ships ships=(list ship)]
  ==
::
+$  update  :: scries, and sub updates
  $%
    [%ships ships=(list [ship (unit profile)])]
    [%ship =ship =profile]
    [%user address=@ux item=@ux ship=(unit @p) name=(unit @t) uri=(unit @t)]
    [%no-user ~]
  ==
+$  sig  [v=@ r=@ s=@]
::
+$  nft         :: from con/lib
    $:  id=@ud
        uri=@t
        metadata=id:smart
        allowances=(pset:smart address:smart)
        properties=(pmap:smart @tas @t)
        transferrable=?
    ==
::
++  nimi-domain   minter-contract                
++  nimi-type    (pairs:enjs:format ~[[%ship [%s '@p']] [%salt [%s '@ud']]])  :: everyone just needs to sign/check the same thing
::
++  nft-contract     0xc7ac.2b08.6748.221b.8628.3813.5875.3579.01d9.2bbe.e6e8.d385.f8c3.b801.84fc.00ae
++  zigs-contract    0x74.6361.7274.6e6f.632d.7367.697a
++  dec-18           1.000.000.000.000.000.000
::  
::  
++  minter-contract  0xd8bf.084a.6e9a.abc0.c0b1.7b54.a05f.7633.e8fa.ddaf.d1df.1cda.2cf8.5f7d.8861.ded7
++  uqnames          0x3ee1.a614.06c5.be2f.dfba.f017.39ff.4ecc.0b8f.5a94.b44c.83dc.c6cb.8fb5.3790.cede
::
+$  ship-sig      [p=@ux q=ship r=life]
-- 

