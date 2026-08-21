-- Seed: 1081298197360395946,16188444798499499427

library ieee;
use ieee.std_logic_1164.all;

entity wylya is
  port (h : out std_logic_vector(4 to 4); tv : buffer std_logic);
end wylya;

architecture crckrzuss of wylya is
  
begin
  -- Multi-driven assignments
  tv <= 'X';
  h <= h;
  tv <= '0';
end crckrzuss;



-- Seed after: 11636344225724045974,16188444798499499427
