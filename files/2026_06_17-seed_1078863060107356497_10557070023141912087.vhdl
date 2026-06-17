-- Seed: 1078863060107356497,10557070023141912087

library ieee;
use ieee.std_logic_1164.all;

entity zacncdyf is
  port (yt : out real; thnoinnj : out bit_vector(3 to 1); lqayfu : inout std_logic_vector(4 to 4));
end zacncdyf;

architecture dgsjv of zacncdyf is
  
begin
  -- Single-driven assignments
  thnoinnj <= (others => '0');
  yt <= 2#0_1_1.0_0_1#;
end dgsjv;

library ieee;
use ieee.std_logic_1164.all;

entity dn is
  port (clljpnhq : in integer; yabaszu : out time; cvwfmnnef : out std_logic_vector(1 downto 0));
end dn;

library ieee;
use ieee.std_logic_1164.all;

architecture yyiqnjx of dn is
  signal qyzjyeedh : bit_vector(3 to 1);
  signal aeknu : real;
  signal xqeyytqxsi : std_logic_vector(4 to 4);
  signal a : bit_vector(3 to 1);
  signal nzzottmc : real;
  signal wynzfxrqva : bit_vector(3 to 1);
  signal uwfdbbt : real;
  signal szbs : std_logic_vector(4 to 4);
  signal zlhrjj : bit_vector(3 to 1);
  signal vikg : real;
begin
  bds : entity work.zacncdyf
    port map (yt => vikg, thnoinnj => zlhrjj, lqayfu => szbs);
  evsy : entity work.zacncdyf
    port map (yt => uwfdbbt, thnoinnj => wynzfxrqva, lqayfu => szbs);
  e : entity work.zacncdyf
    port map (yt => nzzottmc, thnoinnj => a, lqayfu => xqeyytqxsi);
  jh : entity work.zacncdyf
    port map (yt => aeknu, thnoinnj => qyzjyeedh, lqayfu => szbs);
  
  -- Single-driven assignments
  yabaszu <= 201 us;
  
  -- Multi-driven assignments
  cvwfmnnef <= ('X', 'X');
end yyiqnjx;



-- Seed after: 4800237885481722559,10557070023141912087
