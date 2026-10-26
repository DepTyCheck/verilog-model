-- Seed: 2653545535483949506,6562834012285671529

library ieee;
use ieee.std_logic_1164.all;

entity sik is
  port (roas : out string(1 to 4); z : buffer std_logic; smbdpa : inout std_logic);
end sik;

architecture tbrvxpemmq of sik is
  
begin
  -- Single-driven assignments
  roas <= "ynny";
  
  -- Multi-driven assignments
  smbdpa <= z;
  smbdpa <= z;
end tbrvxpemmq;

entity i is
  port (qb : linkage integer; xhhfd : inout string(4 to 2); hfrmuhthfk : in time; fyaknkhu : inout time);
end i;

library ieee;
use ieee.std_logic_1164.all;

architecture kcqwmsu of i is
  signal r : string(1 to 4);
  signal esduu : std_logic;
  signal sqnl : std_logic;
  signal veu : string(1 to 4);
  signal lfxnfvgj : std_logic;
  signal kzvxzwzks : std_logic;
  signal bvxh : string(1 to 4);
begin
  klkhgpg : entity work.sik
    port map (roas => bvxh, z => kzvxzwzks, smbdpa => lfxnfvgj);
  yj : entity work.sik
    port map (roas => veu, z => sqnl, smbdpa => esduu);
  tflocrsypy : entity work.sik
    port map (roas => r, z => kzvxzwzks, smbdpa => esduu);
  
  -- Multi-driven assignments
  esduu <= '-';
  kzvxzwzks <= 'W';
end kcqwmsu;



-- Seed after: 9369974637967528980,6562834012285671529
