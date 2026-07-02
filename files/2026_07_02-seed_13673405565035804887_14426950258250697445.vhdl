-- Seed: 13673405565035804887,14426950258250697445

library ieee;
use ieee.std_logic_1164.all;

entity xf is
  port (yxcbzor : in std_logic_vector(4 downto 1); mfslyd : inout time; py : buffer std_logic_vector(0 downto 0));
end xf;

architecture dhytoy of xf is
  
begin
  -- Single-driven assignments
  mfslyd <= 0_1.1 ms;
  
  -- Multi-driven assignments
  py <= "W";
  py <= (others => 'Z');
  py <= py;
  py <= "H";
end dhytoy;



-- Seed after: 13225230044539143364,14426950258250697445
