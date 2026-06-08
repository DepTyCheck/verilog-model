-- Seed: 2264824717597693369,7142793346053417159

library ieee;
use ieee.std_logic_1164.all;

entity zttnvccp is
  port (wbt : in std_logic_vector(2 to 0); moz : in real; drptb : inout real; rkqhgv : out boolean);
end zttnvccp;



architecture iftjnxhbfo of zttnvccp is
  
begin
  
end iftjnxhbfo;

library ieee;
use ieee.std_logic_1164.all;

entity zqvgv is
  port (rrsztf : out std_logic; bo : buffer bit_vector(3 downto 0); zpgw : out std_logic_vector(4 downto 0); iilp : in std_logic);
end zqvgv;

library ieee;
use ieee.std_logic_1164.all;

architecture np of zqvgv is
  signal evnzc : boolean;
  signal fpg : real;
  signal rb : std_logic_vector(2 to 0);
  signal fepxysox : boolean;
  signal mtdaqe : real;
  signal ex : real;
  signal srsrmiycq : boolean;
  signal kqyjhgh : real;
  signal kw : boolean;
  signal vtmyn : real;
  signal flz : std_logic_vector(2 to 0);
begin
  tt : entity work.zttnvccp
    port map (wbt => flz, moz => vtmyn, drptb => vtmyn, rkqhgv => kw);
  vhej : entity work.zttnvccp
    port map (wbt => flz, moz => vtmyn, drptb => kqyjhgh, rkqhgv => srsrmiycq);
  qxu : entity work.zttnvccp
    port map (wbt => flz, moz => ex, drptb => mtdaqe, rkqhgv => fepxysox);
  whz : entity work.zttnvccp
    port map (wbt => rb, moz => vtmyn, drptb => fpg, rkqhgv => evnzc);
end np;



entity y is
  port (xdchk : in time; gob : inout integer);
end y;

library ieee;
use ieee.std_logic_1164.all;

architecture alzyckabiz of y is
  signal bqh : bit_vector(3 downto 0);
  signal dxdwl : std_logic;
  signal fwrqj : boolean;
  signal p : real;
  signal affwnihu : real;
  signal nkaeniroje : std_logic_vector(2 to 0);
  signal vofajv : std_logic;
  signal ntsmbd : bit_vector(3 downto 0);
  signal mcjiipx : std_logic;
  signal eqtkn : std_logic_vector(4 downto 0);
  signal zawzfzohq : bit_vector(3 downto 0);
  signal ouxntjz : std_logic;
begin
  cmrifus : entity work.zqvgv
    port map (rrsztf => ouxntjz, bo => zawzfzohq, zpgw => eqtkn, iilp => ouxntjz);
  ldypq : entity work.zqvgv
    port map (rrsztf => mcjiipx, bo => ntsmbd, zpgw => eqtkn, iilp => vofajv);
  wgtcvh : entity work.zttnvccp
    port map (wbt => nkaeniroje, moz => affwnihu, drptb => p, rkqhgv => fwrqj);
  axbg : entity work.zqvgv
    port map (rrsztf => dxdwl, bo => bqh, zpgw => eqtkn, iilp => dxdwl);
end alzyckabiz;



-- Seed after: 7053652078346346935,7142793346053417159
