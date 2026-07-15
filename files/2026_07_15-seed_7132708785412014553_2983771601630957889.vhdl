-- Seed: 7132708785412014553,2983771601630957889

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity nxxexlwt is
  port ( pzhezoolx : buffer real_vector(1 downto 3)
  ; variable lfbtfebz : inout record_value_mirror_pt
  ; rgqrsy : out time
  ; hcfj : inout std_logic_vector(4 to 4)
  );
end nxxexlwt;

architecture yei of nxxexlwt is
  
begin
  -- Single-driven assignments
  pzhezoolx <= (others => 0.0);
  
  -- Multi-driven assignments
  hcfj <= "W";
  hcfj <= "L";
  hcfj <= (others => '0');
  hcfj <= hcfj;
end yei;

entity viff is
  port (hy : in time);
end viff;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture y of viff is
  signal zlv : time;
  shared variable glaqazaq : record_value_mirror_pt;
  signal ajmkmpmzy : real_vector(1 downto 3);
  signal kvvv : std_logic_vector(4 to 4);
  signal xwhcaf : time;
  shared variable bgjz : record_value_mirror_pt;
  signal l : real_vector(1 downto 3);
  signal kxg : time;
  shared variable zyrtflx : record_value_mirror_pt;
  signal byqdwrkkl : real_vector(1 downto 3);
  signal ciapat : std_logic_vector(4 to 4);
  signal lnikesky : time;
  shared variable xlb : record_value_mirror_pt;
  signal wzxjevgmcn : real_vector(1 downto 3);
begin
  lxolad : entity work.nxxexlwt
    port map (pzhezoolx => wzxjevgmcn, lfbtfebz => xlb, rgqrsy => lnikesky, hcfj => ciapat);
  swtm : entity work.nxxexlwt
    port map (pzhezoolx => byqdwrkkl, lfbtfebz => zyrtflx, rgqrsy => kxg, hcfj => ciapat);
  laviurnq : entity work.nxxexlwt
    port map (pzhezoolx => l, lfbtfebz => bgjz, rgqrsy => xwhcaf, hcfj => kvvv);
  e : entity work.nxxexlwt
    port map (pzhezoolx => ajmkmpmzy, lfbtfebz => glaqazaq, rgqrsy => zlv, hcfj => ciapat);
  
  -- Multi-driven assignments
  ciapat <= "X";
  ciapat <= ciapat;
  kvvv <= ciapat;
  ciapat <= ciapat;
end y;

use std.reflection.all;

entity rpdu is
  port ( variable alcdjpapc : inout protected_value_mirror_pt
  ; svwwcih : linkage time
  ; variable konrzlqg : inout record_value_mirror_pt
  ; gyydxhmyyr : inout bit_vector(1 to 3)
  );
end rpdu;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture ppp of rpdu is
  signal dlcbi : std_logic_vector(4 to 4);
  signal v : time;
  shared variable klfljjzdeu : record_value_mirror_pt;
  signal qdvartkck : real_vector(1 downto 3);
  signal gprnzmq : std_logic_vector(4 to 4);
  signal zhid : time;
  signal ztievenexg : real_vector(1 downto 3);
begin
  rbxwhltrye : entity work.nxxexlwt
    port map (pzhezoolx => ztievenexg, lfbtfebz => konrzlqg, rgqrsy => zhid, hcfj => gprnzmq);
  lzpivfwfkv : entity work.nxxexlwt
    port map (pzhezoolx => qdvartkck, lfbtfebz => klfljjzdeu, rgqrsy => v, hcfj => dlcbi);
  
  -- Single-driven assignments
  gyydxhmyyr <= gyydxhmyyr;
  
  -- Multi-driven assignments
  gprnzmq <= gprnzmq;
  dlcbi <= "1";
  dlcbi <= gprnzmq;
end ppp;

use std.reflection.all;

entity mkggh is
  port ( variable lq : inout subtype_mirror_pt
  ; variable hurhd : inout record_subtype_mirror_pt
  ; variable tqsuubimvf : inout protected_subtype_mirror_pt
  );
end mkggh;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture sayigfio of mkggh is
  shared variable utjepkkzus : record_value_mirror_pt;
  signal mq : real_vector(1 downto 3);
  signal cawu : std_logic_vector(4 to 4);
  signal xjuyp : time;
  shared variable ftg : record_value_mirror_pt;
  signal stough : real_vector(1 downto 3);
  signal tfxnbem : bit_vector(1 to 3);
  shared variable aggulstur : record_value_mirror_pt;
  signal bifsx : time;
  shared variable nobozs : protected_value_mirror_pt;
  signal dgpmtlvjsa : time;
begin
  ztvtw : entity work.viff
    port map (hy => dgpmtlvjsa);
  lgbyrhqv : entity work.rpdu
    port map (alcdjpapc => nobozs, svwwcih => bifsx, konrzlqg => aggulstur, gyydxhmyyr => tfxnbem);
  uymcrko : entity work.nxxexlwt
    port map (pzhezoolx => stough, lfbtfebz => ftg, rgqrsy => xjuyp, hcfj => cawu);
  kfdzmb : entity work.nxxexlwt
    port map (pzhezoolx => mq, lfbtfebz => utjepkkzus, rgqrsy => dgpmtlvjsa, hcfj => cawu);
  
  -- Multi-driven assignments
  cawu <= cawu;
  cawu <= (others => '0');
  cawu <= "W";
  cawu <= cawu;
end sayigfio;



-- Seed after: 9944282589999121278,2983771601630957889
