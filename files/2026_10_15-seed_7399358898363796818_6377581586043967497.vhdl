-- Seed: 7399358898363796818,6377581586043967497

library ieee;
use ieee.std_logic_1164.all;

entity xv is
  port (g : buffer std_logic_vector(4 downto 0));
end xv;

architecture fnslv of xv is
  
begin
  
end fnslv;

entity pv is
  port (qtbcasqe : out time);
end pv;

architecture lblzuhozjf of pv is
  
begin
  -- Single-driven assignments
  qtbcasqe <= 0_2.034 ps;
end lblzuhozjf;

library ieee;
use ieee.std_logic_1164.all;

entity eubrnbx is
  port (rgczmawhw : out std_logic_vector(1 downto 1); ocnrrwq : buffer real_vector(3 downto 2); ywdwxreztb : inout std_logic);
end eubrnbx;

library ieee;
use ieee.std_logic_1164.all;

architecture gfimatcpq of eubrnbx is
  signal zadrbsrjvt : std_logic_vector(4 downto 0);
begin
  pexmhb : entity work.xv
    port map (g => zadrbsrjvt);
  rid : entity work.xv
    port map (g => zadrbsrjvt);
  ssq : entity work.xv
    port map (g => zadrbsrjvt);
  
  -- Single-driven assignments
  ocnrrwq <= (16#3_9_2_F_8.4_B_9_7_E#, 2#0110.0_0#);
  
  -- Multi-driven assignments
  zadrbsrjvt <= ('Z', '0', 'H', 'L', 'W');
end gfimatcpq;



-- Seed after: 10691025373850057649,6377581586043967497
