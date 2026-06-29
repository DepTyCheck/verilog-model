-- Seed: 6667310697527872415,17047277710231705797

entity cfkwhnq is
  port (oifteh : buffer integer_vector(2 downto 1));
end cfkwhnq;

architecture jeckqgoa of cfkwhnq is
  
begin
  -- Single-driven assignments
  oifteh <= (16#EB90E#, 16#5_B_E_D_F#);
end jeckqgoa;

library ieee;
use ieee.std_logic_1164.all;

entity vnrbsze is
  port (ehmynwxrj : inout std_logic);
end vnrbsze;

architecture cma of vnrbsze is
  signal vfgqk : integer_vector(2 downto 1);
begin
  qpprxmugnb : entity work.cfkwhnq
    port map (oifteh => vfgqk);
  
  -- Multi-driven assignments
  ehmynwxrj <= '0';
  ehmynwxrj <= 'H';
  ehmynwxrj <= '-';
  ehmynwxrj <= '0';
end cma;



-- Seed after: 1877714519293739829,17047277710231705797
