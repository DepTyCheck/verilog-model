-- Seed: 2842544337401663475,2338584220606314193

entity ald is
  port (bfk : linkage integer);
end ald;

architecture d of ald is
  
begin
  
end d;

library ieee;
use ieee.std_logic_1164.all;

entity qxbktomioe is
  port (l : out std_logic_vector(0 downto 4); dgr : buffer real);
end qxbktomioe;

architecture pg of qxbktomioe is
  signal spgmgctwka : integer;
  signal cdndy : integer;
begin
  ltgktgdh : entity work.ald
    port map (bfk => cdndy);
  csn : entity work.ald
    port map (bfk => spgmgctwka);
  
  -- Single-driven assignments
  dgr <= 23.4_0_2;
  
  -- Multi-driven assignments
  l <= l;
  l <= (others => '0');
end pg;



-- Seed after: 14353829268600856638,2338584220606314193
