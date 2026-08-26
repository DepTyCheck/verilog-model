-- Seed: 10459562413500637372,6000118208082478503

library ieee;
use ieee.std_logic_1164.all;

entity snf is
  port (gvkghxq : buffer severity_level; jkbiybbl : out std_logic_vector(3 to 3); sivg : out std_logic; lkszhb : in time);
end snf;

architecture j of snf is
  
begin
  -- Single-driven assignments
  gvkghxq <= WARNING;
  
  -- Multi-driven assignments
  sivg <= sivg;
  jkbiybbl <= jkbiybbl;
end j;

entity ixgxu is
  port (wkh : out boolean_vector(3 downto 2));
end ixgxu;

library ieee;
use ieee.std_logic_1164.all;

architecture kneoasd of ixgxu is
  signal yjm : time;
  signal jayzobgqt : std_logic;
  signal zxgxupp : std_logic_vector(3 to 3);
  signal sjiw : severity_level;
  signal slsj : time;
  signal xcnsj : std_logic;
  signal cbrdoujk : std_logic_vector(3 to 3);
  signal dlknh : severity_level;
begin
  frskhw : entity work.snf
    port map (gvkghxq => dlknh, jkbiybbl => cbrdoujk, sivg => xcnsj, lkszhb => slsj);
  jbsssxs : entity work.snf
    port map (gvkghxq => sjiw, jkbiybbl => zxgxupp, sivg => jayzobgqt, lkszhb => yjm);
  
  -- Single-driven assignments
  yjm <= 4.3 fs;
end kneoasd;



-- Seed after: 9920737537265790108,6000118208082478503
