-- Seed: 10570008347927791438,3181554006726329157

library ieee;
use ieee.std_logic_1164.all;

entity koxhxoslkt is
  port (khrcrpi : buffer character; ktjwklqp : in time; smmw : out std_logic_vector(2 downto 4));
end koxhxoslkt;

architecture uxqnxi of koxhxoslkt is
  
begin
  -- Single-driven assignments
  khrcrpi <= 'h';
  
  -- Multi-driven assignments
  smmw <= (others => '0');
  smmw <= smmw;
  smmw <= smmw;
end uxqnxi;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity wwtpd is
  port (vy : buffer std_logic; fijhvm : inout value_mirror);
end wwtpd;

architecture fdkxi of wwtpd is
  
begin
  -- Multi-driven assignments
  vy <= '1';
end fdkxi;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity dazqplibpu is
  port (jvstomqhn : out std_logic_vector(2 downto 2); vnermjbkc : inout real; g : inout file_subtype_mirror);
end dazqplibpu;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture p of dazqplibpu is
  signal hlwinxgs : std_logic_vector(2 downto 4);
  signal zdpdzma : time;
  signal ayse : character;
  signal d : character;
  shared variable ifscnrzp : value_mirror;
  signal lsaiv : std_logic;
  signal hzsq : std_logic_vector(2 downto 4);
  signal jlipcjjvt : time;
  signal ay : character;
begin
  thbywm : entity work.koxhxoslkt
    port map (khrcrpi => ay, ktjwklqp => jlipcjjvt, smmw => hzsq);
  xoxtnii : entity work.wwtpd
    port map (vy => lsaiv, fijhvm => ifscnrzp);
  wqjz : entity work.koxhxoslkt
    port map (khrcrpi => d, ktjwklqp => jlipcjjvt, smmw => hzsq);
  cjbg : entity work.koxhxoslkt
    port map (khrcrpi => ayse, ktjwklqp => zdpdzma, smmw => hlwinxgs);
  
  -- Single-driven assignments
  vnermjbkc <= 4.2_1_4_1;
  zdpdzma <= jlipcjjvt;
  jlipcjjvt <= jlipcjjvt;
  
  -- Multi-driven assignments
  jvstomqhn <= jvstomqhn;
  jvstomqhn <= "-";
end p;



-- Seed after: 1432936841706239821,3181554006726329157
