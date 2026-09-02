-- Seed: 4072470527675366621,3400751927341804175

library ieee;
use ieee.std_logic_1164.all;

entity zl is
  port (rsqabx : in integer; ctsowkg : in std_logic_vector(4 to 2); h : linkage integer);
end zl;

architecture wxaifsis of zl is
  
begin
  
end wxaifsis;

library ieee;
use ieee.std_logic_1164.all;

entity ljp is
  port (bhnsr : buffer integer; ddswthw : inout real; eknjnza : out std_logic);
end ljp;

library ieee;
use ieee.std_logic_1164.all;

architecture lbai of ljp is
  signal nckokert : integer;
  signal okcggb : integer;
  signal cuuoyqu : integer;
  signal qy : integer;
  signal t : std_logic_vector(4 to 2);
  signal piwn : integer;
begin
  gdoi : entity work.zl
    port map (rsqabx => piwn, ctsowkg => t, h => bhnsr);
  jysfzyyk : entity work.zl
    port map (rsqabx => piwn, ctsowkg => t, h => qy);
  dzxeuqbzh : entity work.zl
    port map (rsqabx => cuuoyqu, ctsowkg => t, h => piwn);
  qudfhbo : entity work.zl
    port map (rsqabx => okcggb, ctsowkg => t, h => nckokert);
  
  -- Single-driven assignments
  ddswthw <= ddswthw;
  
  -- Multi-driven assignments
  eknjnza <= eknjnza;
end lbai;

library ieee;
use ieee.std_logic_1164.all;

entity mektei is
  port (fe : inout integer; kza : inout std_logic);
end mektei;

library ieee;
use ieee.std_logic_1164.all;

architecture gjbzvyzyq of mektei is
  signal ifmxlg : std_logic;
  signal xvongykehg : real;
  signal hjjzl : real;
  signal lvpojtdl : integer;
  signal xglbdfc : std_logic_vector(4 to 2);
  signal updxwz : integer;
  signal s : real;
  signal q : integer;
begin
  tieqqejla : entity work.ljp
    port map (bhnsr => q, ddswthw => s, eknjnza => kza);
  v : entity work.zl
    port map (rsqabx => updxwz, ctsowkg => xglbdfc, h => lvpojtdl);
  jchdh : entity work.ljp
    port map (bhnsr => fe, ddswthw => hjjzl, eknjnza => kza);
  zvedvsvkr : entity work.ljp
    port map (bhnsr => updxwz, ddswthw => xvongykehg, eknjnza => ifmxlg);
  
  -- Multi-driven assignments
  kza <= 'W';
  xglbdfc <= xglbdfc;
  xglbdfc <= (others => '0');
  kza <= kza;
end gjbzvyzyq;



-- Seed after: 5788874439293062463,3400751927341804175
