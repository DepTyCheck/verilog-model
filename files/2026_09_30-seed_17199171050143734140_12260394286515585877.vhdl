-- Seed: 17199171050143734140,12260394286515585877

library ieee;
use ieee.std_logic_1164.all;

entity svjeytvc is
  port (cnmvuq : out severity_level; ludrsabgdz : out time; pzwikqmmz : in real; x : out std_logic);
end svjeytvc;

architecture fxlncfko of svjeytvc is
  
begin
  -- Single-driven assignments
  ludrsabgdz <= 2#1_0_0# ns;
  cnmvuq <= WARNING;
  
  -- Multi-driven assignments
  x <= 'X';
  x <= x;
  x <= x;
end fxlncfko;

entity k is
  port (g : buffer time; n : inout bit);
end k;

library ieee;
use ieee.std_logic_1164.all;

architecture xhumruq of k is
  signal e : std_logic;
  signal daiaigxrap : real;
  signal ddzh : severity_level;
begin
  a : entity work.svjeytvc
    port map (cnmvuq => ddzh, ludrsabgdz => g, pzwikqmmz => daiaigxrap, x => e);
  
  -- Single-driven assignments
  n <= n;
  daiaigxrap <= daiaigxrap;
  
  -- Multi-driven assignments
  e <= 'H';
  e <= e;
  e <= '0';
  e <= e;
end xhumruq;



-- Seed after: 9949524272223205102,12260394286515585877
