-- Seed: 14343202446001691821,1630680796402093529

library ieee;
use ieee.std_logic_1164.all;

entity ztxz is
  port (gao : out std_logic_vector(3 downto 0); erperbwpxq : inout time; ajx : in bit_vector(2 downto 4));
end ztxz;



architecture h of ztxz is
  
begin
  
end h;



entity dqsjmqtlyk is
  port (acnhrtjhd : buffer real);
end dqsjmqtlyk;

library ieee;
use ieee.std_logic_1164.all;

architecture plstbxd of dqsjmqtlyk is
  signal mbyahydhtc : bit_vector(2 downto 4);
  signal akjizbqkqj : time;
  signal vjavwhtwcb : std_logic_vector(3 downto 0);
begin
  wtruukkgv : entity work.ztxz
    port map (gao => vjavwhtwcb, erperbwpxq => akjizbqkqj, ajx => mbyahydhtc);
end plstbxd;



entity pqr is
  port (pksqo : out boolean; urxbxgbwqb : in integer);
end pqr;

library ieee;
use ieee.std_logic_1164.all;

architecture tg of pqr is
  signal fpp : time;
  signal dt : time;
  signal liod : bit_vector(2 downto 4);
  signal sghpx : time;
  signal rlotjut : std_logic_vector(3 downto 0);
begin
  abt : entity work.ztxz
    port map (gao => rlotjut, erperbwpxq => sghpx, ajx => liod);
  wsanmvcty : entity work.ztxz
    port map (gao => rlotjut, erperbwpxq => dt, ajx => liod);
  rietgbd : entity work.ztxz
    port map (gao => rlotjut, erperbwpxq => fpp, ajx => liod);
end tg;



-- Seed after: 4589177046699177834,1630680796402093529
