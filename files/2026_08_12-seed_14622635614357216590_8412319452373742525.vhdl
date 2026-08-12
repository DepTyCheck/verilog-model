-- Seed: 14622635614357216590,8412319452373742525

library ieee;
use ieee.std_logic_1164.all;

entity ajdusp is
  port (ggr : buffer std_logic_vector(4 to 3));
end ajdusp;

architecture ty of ajdusp is
  
begin
  -- Multi-driven assignments
  ggr <= ggr;
  ggr <= "";
end ty;



-- Seed after: 2740115335299389108,8412319452373742525
