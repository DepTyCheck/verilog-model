-- Seed: 7053372799207538572,3400751927341804175

library ieee;
use ieee.std_logic_1164.all;

entity zcrmwt is
  port (nif : buffer bit_vector(2 to 2); yyummryx : inout std_logic_vector(1 to 3); gy : in bit; we : inout std_logic_vector(4 to 4));
end zcrmwt;

architecture nhldwb of zcrmwt is
  
begin
  -- Single-driven assignments
  nif <= (others => '1');
  
  -- Multi-driven assignments
  we <= "L";
  yyummryx <= "W-Z";
end nhldwb;

entity oes is
  port (jwto : inout character);
end oes;

library ieee;
use ieee.std_logic_1164.all;

architecture v of oes is
  signal atrsvnar : std_logic_vector(4 to 4);
  signal bc : bit_vector(2 to 2);
  signal hwjyd : std_logic_vector(4 to 4);
  signal fhldkruc : bit;
  signal m : std_logic_vector(1 to 3);
  signal awbw : bit_vector(2 to 2);
begin
  bkcodymp : entity work.zcrmwt
    port map (nif => awbw, yyummryx => m, gy => fhldkruc, we => hwjyd);
  ve : entity work.zcrmwt
    port map (nif => bc, yyummryx => m, gy => fhldkruc, we => atrsvnar);
  
  -- Multi-driven assignments
  m <= m;
  hwjyd <= "L";
end v;



-- Seed after: 4835051821199447740,3400751927341804175
