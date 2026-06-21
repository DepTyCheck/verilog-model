-- Seed: 5340558669299334858,3687118713772291287

library ieee;
use ieee.std_logic_1164.all;

entity rmwtxqs is
  port (givb : inout integer; cwwxdaerd : out std_logic; oievfxj : in integer; uxc : buffer string(2 to 3));
end rmwtxqs;

architecture olexmlg of rmwtxqs is
  
begin
  -- Single-driven assignments
  uxc <= ('q', 'v');
  givb <= 3_3;
  
  -- Multi-driven assignments
  cwwxdaerd <= '-';
  cwwxdaerd <= 'L';
  cwwxdaerd <= 'H';
end olexmlg;

library ieee;
use ieee.std_logic_1164.all;

entity zqjuesirsw is
  port (skbqdhpncw : buffer time_vector(2 downto 2); itlx : in std_logic);
end zqjuesirsw;

library ieee;
use ieee.std_logic_1164.all;

architecture c of zqjuesirsw is
  signal aon : string(2 to 3);
  signal sxtqfh : integer;
  signal qudw : string(2 to 3);
  signal ymkhc : integer;
  signal ijlrfmg : std_logic;
  signal xrpbhk : string(2 to 3);
  signal yfobbqd : integer;
  signal opqfwglww : std_logic;
  signal xmkuop : integer;
begin
  zqwp : entity work.rmwtxqs
    port map (givb => xmkuop, cwwxdaerd => opqfwglww, oievfxj => yfobbqd, uxc => xrpbhk);
  xhgu : entity work.rmwtxqs
    port map (givb => yfobbqd, cwwxdaerd => ijlrfmg, oievfxj => ymkhc, uxc => qudw);
  yubi : entity work.rmwtxqs
    port map (givb => ymkhc, cwwxdaerd => ijlrfmg, oievfxj => sxtqfh, uxc => aon);
  
  -- Single-driven assignments
  skbqdhpncw <= (others => 8#1_6_6_3# ms);
  sxtqfh <= 16#4_F_B_6#;
end c;



-- Seed after: 11484652836619142516,3687118713772291287
