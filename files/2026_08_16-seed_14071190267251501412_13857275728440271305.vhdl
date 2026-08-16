-- Seed: 14071190267251501412,13857275728440271305

library ieee;
use ieee.std_logic_1164.all;

entity jhaapic is
  port (txre : buffer std_logic_vector(2 to 2));
end jhaapic;

architecture xu of jhaapic is
  
begin
  -- Multi-driven assignments
  txre <= txre;
  txre <= txre;
end xu;



-- Seed after: 7649152927272751526,13857275728440271305
