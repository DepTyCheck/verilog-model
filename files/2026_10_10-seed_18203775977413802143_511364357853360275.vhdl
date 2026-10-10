-- Seed: 18203775977413802143,511364357853360275

library ieee;
use ieee.std_logic_1164.all;

entity w is
  port (ftp : out severity_level; fjyml : out std_logic_vector(2 to 4));
end w;

architecture xuuljrnqo of w is
  
begin
  -- Single-driven assignments
  ftp <= WARNING;
  
  -- Multi-driven assignments
  fjyml <= ('X', 'X', 'W');
  fjyml <= fjyml;
  fjyml <= "W-X";
  fjyml <= "1XW";
end xuuljrnqo;



-- Seed after: 899436751391730443,511364357853360275
