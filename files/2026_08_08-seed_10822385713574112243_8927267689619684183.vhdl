-- Seed: 10822385713574112243,8927267689619684183

library ieee;
use ieee.std_logic_1164.all;

entity lw is
  port (ombh : inout std_logic_vector(2 downto 0); sxqfu : out boolean_vector(1 downto 3));
end lw;

architecture hlqe of lw is
  
begin
  -- Single-driven assignments
  sxqfu <= (others => TRUE);
  
  -- Multi-driven assignments
  ombh <= ('Z', '0', '-');
  ombh <= ('H', 'X', '-');
  ombh <= ('0', 'X', '0');
end hlqe;



-- Seed after: 10873811956507937867,8927267689619684183
