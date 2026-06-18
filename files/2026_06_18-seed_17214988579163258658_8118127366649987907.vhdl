-- Seed: 17214988579163258658,8118127366649987907

library ieee;
use ieee.std_logic_1164.all;

entity pvmuf is
  port (q : out std_logic);
end pvmuf;

architecture lqncmn of pvmuf is
  
begin
  -- Multi-driven assignments
  q <= 'H';
  q <= 'Z';
  q <= 'L';
end lqncmn;

entity hasqigqko is
  port (qugflj : out integer);
end hasqigqko;

library ieee;
use ieee.std_logic_1164.all;

architecture lazjqbggq of hasqigqko is
  signal t : std_logic;
begin
  w : entity work.pvmuf
    port map (q => t);
  
  -- Single-driven assignments
  qugflj <= 1;
  
  -- Multi-driven assignments
  t <= '0';
end lazjqbggq;



-- Seed after: 7934670630583377883,8118127366649987907
