-- Seed: 14159612394287756274,7808623373429384027

library ieee;
use ieee.std_logic_1164.all;

entity e is
  port (bgnjv : in std_logic; h : out std_logic; khz : in std_logic_vector(3 to 0));
end e;

architecture oz of e is
  
begin
  -- Multi-driven assignments
  h <= 'X';
  h <= h;
  h <= '1';
  h <= '-';
end oz;

entity zfwpa is
  port (wnbyiia : out bit_vector(4 downto 2));
end zfwpa;

library ieee;
use ieee.std_logic_1164.all;

architecture rkyzohj of zfwpa is
  signal px : std_logic_vector(3 to 0);
  signal qci : std_logic;
begin
  kgsc : entity work.e
    port map (bgnjv => qci, h => qci, khz => px);
  
  -- Single-driven assignments
  wnbyiia <= wnbyiia;
  
  -- Multi-driven assignments
  qci <= qci;
end rkyzohj;



-- Seed after: 3595054568481293124,7808623373429384027
