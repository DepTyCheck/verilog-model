-- Seed: 4078019820334140936,3108530264173481209

entity fjyurs is
  port (aco : out integer; ww : buffer real_vector(0 to 2));
end fjyurs;

architecture j of fjyurs is
  
begin
  -- Single-driven assignments
  ww <= (2#0_1_1.11#, 4.0332, 8#4071.6_1_5_2#);
end j;

library ieee;
use ieee.std_logic_1164.all;

entity nsla is
  port (gh : in std_logic_vector(4 to 2));
end nsla;

architecture s of nsla is
  signal rjdspj : real_vector(0 to 2);
  signal xetpck : integer;
begin
  pcayfsxjnp : entity work.fjyurs
    port map (aco => xetpck, ww => rjdspj);
end s;



-- Seed after: 12233918434843976228,3108530264173481209
