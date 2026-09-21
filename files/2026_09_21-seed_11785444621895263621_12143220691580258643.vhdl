-- Seed: 11785444621895263621,12143220691580258643

library ieee;
use ieee.std_logic_1164.all;

entity te is
  port (mnrrtddq : buffer real; rewzmoblg : buffer std_logic; k : out integer; gbptmp : in time);
end te;

architecture i of te is
  
begin
  -- Single-driven assignments
  k <= k;
  mnrrtddq <= 2#00.00010#;
  
  -- Multi-driven assignments
  rewzmoblg <= rewzmoblg;
  rewzmoblg <= '0';
end i;

library ieee;
use ieee.std_logic_1164.all;

entity fmo is
  port (lofzkhjw : out real; qqr : linkage integer; xxdvnhj : inout std_logic);
end fmo;

library ieee;
use ieee.std_logic_1164.all;

architecture mxejk of fmo is
  signal rtxdq : time;
  signal wpmvccz : integer;
  signal zus : std_logic;
  signal nk : time;
  signal n : integer;
  signal zr : std_logic;
  signal qkzyol : real;
  signal wd : time;
  signal tppiihkgr : integer;
  signal yaeqbu : real;
begin
  xcblc : entity work.te
    port map (mnrrtddq => yaeqbu, rewzmoblg => xxdvnhj, k => tppiihkgr, gbptmp => wd);
  czgmwfv : entity work.te
    port map (mnrrtddq => qkzyol, rewzmoblg => zr, k => n, gbptmp => nk);
  lu : entity work.te
    port map (mnrrtddq => lofzkhjw, rewzmoblg => zus, k => wpmvccz, gbptmp => rtxdq);
  
  -- Single-driven assignments
  wd <= wd;
  rtxdq <= wd;
  nk <= 8#4_0# ns;
  
  -- Multi-driven assignments
  xxdvnhj <= 'X';
end mxejk;

library ieee;
use ieee.std_logic_1164.all;

entity hf is
  port (pd : inout std_logic; xhsyz : out std_logic; uvzzayvsmb : out integer);
end hf;

architecture m of hf is
  signal jbziuk : time;
  signal yeqw : real;
begin
  qdpl : entity work.te
    port map (mnrrtddq => yeqw, rewzmoblg => pd, k => uvzzayvsmb, gbptmp => jbziuk);
  
  -- Single-driven assignments
  jbziuk <= 03321.4144 ps;
  
  -- Multi-driven assignments
  pd <= xhsyz;
  xhsyz <= 'L';
  pd <= xhsyz;
  xhsyz <= xhsyz;
end m;



-- Seed after: 12091121269346704892,12143220691580258643
