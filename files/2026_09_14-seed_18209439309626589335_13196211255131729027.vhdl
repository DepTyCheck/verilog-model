-- Seed: 18209439309626589335,13196211255131729027

library ieee;
use ieee.std_logic_1164.all;

entity pffu is
  port (r : out time; ovdtynn : in time; h : out severity_level; nud : buffer std_logic);
end pffu;

architecture tf of pffu is
  
begin
  -- Single-driven assignments
  h <= h;
  r <= 3 ms;
  
  -- Multi-driven assignments
  nud <= 'W';
  nud <= nud;
  nud <= 'W';
  nud <= nud;
end tf;



-- Seed after: 17413358532652051003,13196211255131729027
