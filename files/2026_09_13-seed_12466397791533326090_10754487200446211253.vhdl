-- Seed: 12466397791533326090,10754487200446211253

entity trlhdcl is
  port (lw : buffer integer; xw : out real);
end trlhdcl;

architecture rbr of trlhdcl is
  
begin
  -- Single-driven assignments
  xw <= xw;
  lw <= 8#6_1_3_3_2#;
end rbr;

library ieee;
use ieee.std_logic_1164.all;

entity suedw is
  port (l : out string(1 downto 3); ri : in std_logic; wp : out std_logic_vector(4 downto 3));
end suedw;

architecture iq of suedw is
  
begin
  -- Multi-driven assignments
  wp <= wp;
  wp <= "-U";
  wp <= wp;
  wp <= ('Z', 'X');
end iq;



-- Seed after: 2470172429722119852,10754487200446211253
