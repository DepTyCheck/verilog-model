-- Seed: 11488234068969280796,7198033922882419595

library ieee;
use ieee.std_logic_1164.all;

entity rowtk is
  port (q : in integer; c : out std_logic);
end rowtk;

architecture ac of rowtk is
  
begin
  -- Multi-driven assignments
  c <= c;
  c <= c;
  c <= c;
end ac;



-- Seed after: 13190500748601155949,7198033922882419595
