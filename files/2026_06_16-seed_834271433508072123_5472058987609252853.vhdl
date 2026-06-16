-- Seed: 834271433508072123,5472058987609252853

entity lcfb is
  port (u : inout real; hu : out real);
end lcfb;

architecture sbouire of lcfb is
  
begin
  -- Single-driven assignments
  u <= 8#0.32547#;
  hu <= 8#2_7_0_2.5566#;
end sbouire;

library ieee;
use ieee.std_logic_1164.all;

entity feffyhp is
  port (ckswkv : out time_vector(4 downto 4); vyvlctnw : buffer bit; qkykga : inout std_logic_vector(2 to 2); wiyrplcjjh : buffer real);
end feffyhp;

architecture cdexigc of feffyhp is
  signal exjonliydx : real;
begin
  srm : entity work.lcfb
    port map (u => wiyrplcjjh, hu => exjonliydx);
  
  -- Single-driven assignments
  ckswkv <= (others => 1_4_4 ps);
  vyvlctnw <= '0';
end cdexigc;



-- Seed after: 10152372760811608307,5472058987609252853
