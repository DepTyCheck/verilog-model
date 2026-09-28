-- Seed: 5423053086478540040,7311216359267151659

library ieee;
use ieee.std_logic_1164.all;

entity mchsnu is
  port (qr : out severity_level; uzy : out boolean; fyobphutj : out std_logic);
end mchsnu;

architecture c of mchsnu is
  
begin
  -- Single-driven assignments
  uzy <= FALSE;
  qr <= FAILURE;
  
  -- Multi-driven assignments
  fyobphutj <= 'U';
  fyobphutj <= 'X';
  fyobphutj <= '1';
end c;



-- Seed after: 713899844307932390,7311216359267151659
