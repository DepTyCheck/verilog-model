-- Seed: 13500627873719571687,4860866131898729603

library ieee;
use ieee.std_logic_1164.all;

entity winkec is
  port (vdfsb : buffer std_logic_vector(2 to 3); z : inout time);
end winkec;

architecture xaiuuhoa of winkec is
  
begin
  -- Single-driven assignments
  z <= 2#1_0_0_1_0# ps;
  
  -- Multi-driven assignments
  vdfsb <= ('0', 'U');
  vdfsb <= "0-";
end xaiuuhoa;



-- Seed after: 142815424375445279,4860866131898729603
