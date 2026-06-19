-- Seed: 1707071900373813591,3108530264173481209

library ieee;
use ieee.std_logic_1164.all;

entity wumaldre is
  port (dhuqdxjk : buffer std_logic_vector(0 to 2); xhhcyvedac : out severity_level; leqchorwfg : inout integer);
end wumaldre;

architecture k of wumaldre is
  
begin
  -- Single-driven assignments
  leqchorwfg <= 2#01111#;
  xhhcyvedac <= WARNING;
  
  -- Multi-driven assignments
  dhuqdxjk <= ('1', 'W', '0');
end k;

entity dn is
  port (mqzawyxloa : in integer);
end dn;

library ieee;
use ieee.std_logic_1164.all;

architecture nfsfxnno of dn is
  signal owworjh : integer;
  signal t : severity_level;
  signal fj : integer;
  signal uqtsmhmd : severity_level;
  signal noa : std_logic_vector(0 to 2);
begin
  vapw : entity work.wumaldre
    port map (dhuqdxjk => noa, xhhcyvedac => uqtsmhmd, leqchorwfg => fj);
  ylovlkab : entity work.wumaldre
    port map (dhuqdxjk => noa, xhhcyvedac => t, leqchorwfg => owworjh);
  
  -- Multi-driven assignments
  noa <= "-XX";
  noa <= "U0-";
  noa <= ('W', 'U', '1');
end nfsfxnno;



-- Seed after: 7519421782460268473,3108530264173481209
