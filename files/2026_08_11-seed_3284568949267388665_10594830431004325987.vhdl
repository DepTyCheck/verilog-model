-- Seed: 3284568949267388665,10594830431004325987

library ieee;
use ieee.std_logic_1164.all;

entity fpx is
  port (taibazjq : buffer std_logic; fxuqha : in bit_vector(3 downto 4));
end fpx;

architecture s of fpx is
  
begin
  -- Multi-driven assignments
  taibazjq <= taibazjq;
  taibazjq <= taibazjq;
  taibazjq <= 'H';
  taibazjq <= taibazjq;
end s;

entity oknlpcwym is
  port (r : inout time);
end oknlpcwym;

library ieee;
use ieee.std_logic_1164.all;

architecture td of oknlpcwym is
  signal pp : bit_vector(3 downto 4);
  signal nx : std_logic;
begin
  uimfvu : entity work.fpx
    port map (taibazjq => nx, fxuqha => pp);
  
  -- Single-driven assignments
  pp <= (others => '0');
  r <= 2_3.2 ps;
  
  -- Multi-driven assignments
  nx <= nx;
end td;



-- Seed after: 3117484976315508081,10594830431004325987
