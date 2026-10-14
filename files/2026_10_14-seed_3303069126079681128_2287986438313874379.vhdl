-- Seed: 3303069126079681128,2287986438313874379

entity ylwyh is
  port (ctipmufsoc : in integer; t : buffer time_vector(4 downto 3));
end ylwyh;

architecture f of ylwyh is
  
begin
  -- Single-driven assignments
  t <= (8#7_4_6_6.72267# us, 3_0_2_1_0 ps);
end f;

library ieee;
use ieee.std_logic_1164.all;

entity jfc is
  port (asjwrkpsj : in std_logic; zjc : buffer time; zaxek : in std_logic_vector(0 downto 1); fcmjpw : in time);
end jfc;

architecture fcvjtii of jfc is
  signal zon : time_vector(4 downto 3);
  signal nvjkvgrrxk : integer;
  signal calvpxsve : time_vector(4 downto 3);
  signal jvncmap : integer;
  signal soqodaehed : time_vector(4 downto 3);
  signal yhqbug : integer;
  signal b : time_vector(4 downto 3);
  signal gdhnhewpue : integer;
begin
  ngnfusxu : entity work.ylwyh
    port map (ctipmufsoc => gdhnhewpue, t => b);
  tbtmedfq : entity work.ylwyh
    port map (ctipmufsoc => yhqbug, t => soqodaehed);
  ydwncnk : entity work.ylwyh
    port map (ctipmufsoc => jvncmap, t => calvpxsve);
  gxqikpnfeq : entity work.ylwyh
    port map (ctipmufsoc => nvjkvgrrxk, t => zon);
  
  -- Single-driven assignments
  yhqbug <= 1011;
  zjc <= 4 min;
  gdhnhewpue <= 2#1#;
end fcvjtii;



-- Seed after: 17408162052701832935,2287986438313874379
