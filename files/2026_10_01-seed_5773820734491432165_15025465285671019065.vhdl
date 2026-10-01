-- Seed: 5773820734491432165,15025465285671019065

library ieee;
use ieee.std_logic_1164.all;

entity psdpgm is
  port (af : inout integer; iykx : inout std_logic_vector(1 to 3); rytvi : inout integer);
end psdpgm;

architecture bi of psdpgm is
  
begin
  -- Single-driven assignments
  af <= 32;
  rytvi <= 16#C2A#;
  
  -- Multi-driven assignments
  iykx <= iykx;
  iykx <= ('H', 'W', '1');
  iykx <= iykx;
  iykx <= ('W', 'Z', '-');
end bi;



-- Seed after: 6340165551990596980,15025465285671019065
