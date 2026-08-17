-- Seed: 11853189965597441301,13843488114570579517

library ieee;
use ieee.std_logic_1164.all;

entity wlhqc is
  port (wcexsncubi : out std_logic; bbujjlwv : out integer; yww : in integer);
end wlhqc;

architecture cxigi of wlhqc is
  
begin
  -- Single-driven assignments
  bbujjlwv <= 2#10011#;
end cxigi;

entity ctlml is
  port (cdgkb : linkage integer; qwyeoeqvnt : buffer boolean_vector(4 downto 3); hioyiux : out real);
end ctlml;

library ieee;
use ieee.std_logic_1164.all;

architecture clqsusyw of ctlml is
  signal vpcf : integer;
  signal rbskno : integer;
  signal mmgvmfbdhh : integer;
  signal jxfoizkg : integer;
  signal qxly : std_logic;
begin
  lqnzog : entity work.wlhqc
    port map (wcexsncubi => qxly, bbujjlwv => jxfoizkg, yww => mmgvmfbdhh);
  pwtpbw : entity work.wlhqc
    port map (wcexsncubi => qxly, bbujjlwv => rbskno, yww => vpcf);
  
  -- Single-driven assignments
  hioyiux <= hioyiux;
  mmgvmfbdhh <= jxfoizkg;
  qwyeoeqvnt <= (FALSE, FALSE);
  vpcf <= 2#1_0#;
end clqsusyw;

library ieee;
use ieee.std_logic_1164.all;

entity s is
  port (wq : in bit_vector(1 to 1); gxfsqrd : out std_logic; rfshueyo : linkage real_vector(3 downto 2));
end s;

library ieee;
use ieee.std_logic_1164.all;

architecture nydltnxvit of s is
  signal iuqeizbk : integer;
  signal vuou : integer;
  signal ramtwoi : integer;
  signal ij : std_logic;
  signal ikngwoku : real;
  signal ticwndrt : boolean_vector(4 downto 3);
  signal fyu : integer;
  signal jkwgmsvk : integer;
  signal hb : integer;
  signal pgnwxzoyby : std_logic;
begin
  xtytjifa : entity work.wlhqc
    port map (wcexsncubi => pgnwxzoyby, bbujjlwv => hb, yww => jkwgmsvk);
  rridlwhbkk : entity work.ctlml
    port map (cdgkb => fyu, qwyeoeqvnt => ticwndrt, hioyiux => ikngwoku);
  xuj : entity work.wlhqc
    port map (wcexsncubi => ij, bbujjlwv => jkwgmsvk, yww => ramtwoi);
  sykwotsg : entity work.wlhqc
    port map (wcexsncubi => gxfsqrd, bbujjlwv => vuou, yww => iuqeizbk);
  
  -- Single-driven assignments
  iuqeizbk <= 21;
  ramtwoi <= 33;
  
  -- Multi-driven assignments
  gxfsqrd <= '1';
  ij <= '0';
  gxfsqrd <= '1';
end nydltnxvit;

entity irvo is
  port (rfplxxzqz : inout boolean; abavwuse : out integer; peotvacw : linkage severity_level);
end irvo;

library ieee;
use ieee.std_logic_1164.all;

architecture naeg of irvo is
  signal e : std_logic;
  signal fohpires : integer;
  signal x : integer;
  signal aharbrc : real_vector(3 downto 2);
  signal mnk : std_logic;
  signal aqyx : bit_vector(1 to 1);
begin
  iccyir : entity work.s
    port map (wq => aqyx, gxfsqrd => mnk, rfshueyo => aharbrc);
  jm : entity work.wlhqc
    port map (wcexsncubi => mnk, bbujjlwv => x, yww => fohpires);
  xostwqn : entity work.wlhqc
    port map (wcexsncubi => e, bbujjlwv => abavwuse, yww => x);
  
  -- Single-driven assignments
  rfplxxzqz <= rfplxxzqz;
  fohpires <= abavwuse;
  aqyx <= aqyx;
  
  -- Multi-driven assignments
  mnk <= mnk;
  e <= mnk;
  mnk <= '0';
  e <= mnk;
end naeg;



-- Seed after: 75068270362241003,13843488114570579517
