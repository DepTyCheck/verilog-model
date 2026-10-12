-- Seed: 1375493773183407375,3426943171337919827

library ieee;
use ieee.std_logic_1164.all;

entity vzzuak is
  port (xoy : in std_logic; mpve : buffer character; bnih : linkage bit);
end vzzuak;

architecture kagvjo of vzzuak is
  
begin
  
end kagvjo;

library ieee;
use ieee.std_logic_1164.all;

entity bhd is
  port (zfkrpfbng : inout std_logic; weaebhw : in real);
end bhd;

library ieee;
use ieee.std_logic_1164.all;

architecture kiyu of bhd is
  signal tbvshzix : bit;
  signal hjrwpwelh : character;
  signal misban : std_logic;
begin
  bka : entity work.vzzuak
    port map (xoy => misban, mpve => hjrwpwelh, bnih => tbvshzix);
end kiyu;

library ieee;
use ieee.std_logic_1164.all;

entity jsrln is
  port (ajw : out std_logic; isgdhj : inout severity_level);
end jsrln;

library ieee;
use ieee.std_logic_1164.all;

architecture a of jsrln is
  signal qtypnn : bit;
  signal qnrtszw : character;
  signal mhiqupp : std_logic;
  signal u : bit;
  signal iut : character;
  signal zvxucgwjh : std_logic;
  signal rksnw : bit;
  signal ysfkdukcoy : character;
  signal wdpzzdgmk : real;
  signal gbyxnncph : std_logic;
begin
  chvtrdrxz : entity work.bhd
    port map (zfkrpfbng => gbyxnncph, weaebhw => wdpzzdgmk);
  jxnnuhmy : entity work.vzzuak
    port map (xoy => ajw, mpve => ysfkdukcoy, bnih => rksnw);
  sdq : entity work.vzzuak
    port map (xoy => zvxucgwjh, mpve => iut, bnih => u);
  jzfpsovoxr : entity work.vzzuak
    port map (xoy => mhiqupp, mpve => qnrtszw, bnih => qtypnn);
  
  -- Single-driven assignments
  isgdhj <= isgdhj;
  wdpzzdgmk <= wdpzzdgmk;
  
  -- Multi-driven assignments
  mhiqupp <= '0';
end a;



-- Seed after: 1142228174935366367,3426943171337919827
