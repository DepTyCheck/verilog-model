-- Seed: 17450322965473761111,1834764876137802293

library ieee;
use ieee.std_logic_1164.all;

entity ocued is
  port (kiteg : in std_logic_vector(3 downto 4); hufex : in std_logic; exkgtrrrkz : out time_vector(0 to 4); j : buffer bit);
end ocued;

architecture g of ocued is
  
begin
  
end g;

library ieee;
use ieee.std_logic_1164.all;

entity shmwg is
  port (os : buffer std_logic_vector(2 to 4); cvu : in std_logic);
end shmwg;

library ieee;
use ieee.std_logic_1164.all;

architecture ceevjai of shmwg is
  signal zshebndmvq : bit;
  signal eqkgx : time_vector(0 to 4);
  signal naksw : std_logic;
  signal gvtrwfn : std_logic_vector(3 downto 4);
begin
  crmsry : entity work.ocued
    port map (kiteg => gvtrwfn, hufex => naksw, exkgtrrrkz => eqkgx, j => zshebndmvq);
  
  -- Multi-driven assignments
  os <= ('Z', 'W', 'Z');
  gvtrwfn <= "";
  os <= ('0', '-', 'H');
  naksw <= '1';
end ceevjai;



-- Seed after: 4979329591746362695,1834764876137802293
