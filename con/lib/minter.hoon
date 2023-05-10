/+  *zig-sys-smart
|%
+$  action
  $%  
    [%mint nft=id name=@t uri=@t ship=(unit @p)]
    [%on-push from=id amount=@ud calldata=*]
  == 
::
++  nft-contract   0xc7ac.2b08.6748.221b.8628.3813.5875.3579.01d9.2bbe.e6e8.d385.f8c3.b801.84fc.00ae
::
++  zigs-contract  0x74.6361.7274.6e6f.632d.7367.697a
::
++  dec-18  1.000.000.000.000.000.000
::  mold from nft contract
+$  metadata
  $:  name=@t
      symbol=@t
      properties=(pset @tas)
      supply=@ud
      cap=(unit @ud)  
      mintable=?      
      minters=(pset address)
      deployer=id
      salt=@
  ==
::
--
