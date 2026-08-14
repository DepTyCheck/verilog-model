-- Seed: 15052678335300407310,8437298063418820479

library ieee;
use ieee.std_logic_1164.all;

entity shfo is
  port (pu : inout integer; sovoll : inout boolean; gzpnmywjxu : out time_vector(2 downto 3); ckbyloaztf : inout std_logic_vector(4 downto 1));
end shfo;

architecture zuq of shfo is
  
begin
  -- Single-driven assignments
  sovoll <= sovoll;
  gzpnmywjxu <= (others => 0 ns);
  
  -- Multi-driven assignments
  ckbyloaztf <= ('U', '1', '1', 'H');
  ckbyloaztf <= ('-', '-', 'U', 'Z');
end zuq;



-- Seed after: 16371753248315181687,8437298063418820479
