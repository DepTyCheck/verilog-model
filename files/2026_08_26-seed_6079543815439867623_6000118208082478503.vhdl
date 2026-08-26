-- Seed: 6079543815439867623,6000118208082478503

library ieee;
use ieee.std_logic_1164.all;

entity tjez is
  port (qmfgwtjgm : out std_logic_vector(3 to 2); u : linkage integer; ihjx : inout time; eftdrk : in std_logic_vector(3 to 2));
end tjez;

architecture r of tjez is
  
begin
  -- Single-driven assignments
  ihjx <= 1 hr;
  
  -- Multi-driven assignments
  qmfgwtjgm <= (others => '0');
  qmfgwtjgm <= eftdrk;
  qmfgwtjgm <= eftdrk;
end r;



-- Seed after: 17384181697111325905,6000118208082478503
