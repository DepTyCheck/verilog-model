-- Seed: 10549114132289915663,13857275728440271305

library ieee;
use ieee.std_logic_1164.all;

entity luw is
  port (fqbpnimr : out std_logic_vector(3 downto 4); dh : buffer real_vector(1 downto 0));
end luw;

architecture r of luw is
  
begin
  -- Single-driven assignments
  dh <= dh;
  
  -- Multi-driven assignments
  fqbpnimr <= (others => '0');
  fqbpnimr <= (others => '0');
end r;

library ieee;
use ieee.std_logic_1164.all;

entity lghyurh is
  port (yecjzokv : buffer std_logic_vector(2 to 0));
end lghyurh;

architecture owbpb of lghyurh is
  signal sskpaaut : real_vector(1 downto 0);
begin
  iv : entity work.luw
    port map (fqbpnimr => yecjzokv, dh => sskpaaut);
  
  -- Multi-driven assignments
  yecjzokv <= "";
  yecjzokv <= "";
  yecjzokv <= "";
end owbpb;

entity dnuteabka is
  port (e : in integer);
end dnuteabka;

library ieee;
use ieee.std_logic_1164.all;

architecture fxm of dnuteabka is
  signal yaqfr : real_vector(1 downto 0);
  signal zekmd : real_vector(1 downto 0);
  signal mcdldn : std_logic_vector(3 downto 4);
begin
  k : entity work.luw
    port map (fqbpnimr => mcdldn, dh => zekmd);
  iakgzqvbb : entity work.luw
    port map (fqbpnimr => mcdldn, dh => yaqfr);
  
  -- Multi-driven assignments
  mcdldn <= (others => '0');
  mcdldn <= "";
end fxm;



-- Seed after: 4343178171411992281,13857275728440271305
