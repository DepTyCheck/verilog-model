-- Seed: 12336951579970116148,13501862637168280927

entity tcgh is
  port (d : linkage severity_level; wcy : in bit);
end tcgh;

architecture bpkirpimtt of tcgh is
  
begin
  
end bpkirpimtt;

library ieee;
use ieee.std_logic_1164.all;

entity irp is
  port (wqeocce : out time; ipqkgkicn : buffer bit; ipdin : out std_logic; docyblvrhi : buffer time);
end irp;

architecture dunkj of irp is
  signal ssyapj : bit;
  signal htr : severity_level;
  signal jlwde : severity_level;
begin
  rrw : entity work.tcgh
    port map (d => jlwde, wcy => ipqkgkicn);
  odkzxvyuea : entity work.tcgh
    port map (d => htr, wcy => ssyapj);
  
  -- Single-driven assignments
  wqeocce <= docyblvrhi;
  ssyapj <= '1';
  ipqkgkicn <= ipqkgkicn;
  docyblvrhi <= 16#B.2# ms;
end dunkj;

entity dnco is
  port (zozeiwkla : in real; kotxcjm : out time_vector(2 to 1));
end dnco;

library ieee;
use ieee.std_logic_1164.all;

architecture txfab of dnco is
  signal eyflderx : severity_level;
  signal d : bit;
  signal tog : severity_level;
  signal gbzt : time;
  signal jdbg : std_logic;
  signal cxbk : time;
  signal iaaygmfhcv : bit;
  signal xrulb : severity_level;
begin
  wiqcssww : entity work.tcgh
    port map (d => xrulb, wcy => iaaygmfhcv);
  bqbzuzlg : entity work.irp
    port map (wqeocce => cxbk, ipqkgkicn => iaaygmfhcv, ipdin => jdbg, docyblvrhi => gbzt);
  eu : entity work.tcgh
    port map (d => tog, wcy => d);
  hylmuz : entity work.tcgh
    port map (d => eyflderx, wcy => iaaygmfhcv);
  
  -- Single-driven assignments
  kotxcjm <= kotxcjm;
  
  -- Multi-driven assignments
  jdbg <= jdbg;
  jdbg <= 'X';
end txfab;

library ieee;
use ieee.std_logic_1164.all;

entity zoxsje is
  port (aumwh : inout std_logic_vector(0 to 0); rkyyesgpa : buffer bit; hhgbe : buffer std_logic; i : linkage severity_level);
end zoxsje;

architecture d of zoxsje is
  signal owz : time_vector(2 to 1);
  signal p : real;
  signal ib : time;
  signal rml : time;
begin
  ppobc : entity work.irp
    port map (wqeocce => rml, ipqkgkicn => rkyyesgpa, ipdin => hhgbe, docyblvrhi => ib);
  gmgmlck : entity work.dnco
    port map (zozeiwkla => p, kotxcjm => owz);
  tscwtk : entity work.tcgh
    port map (d => i, wcy => rkyyesgpa);
  
  -- Single-driven assignments
  p <= p;
  
  -- Multi-driven assignments
  aumwh <= (others => 'L');
  hhgbe <= hhgbe;
  aumwh <= aumwh;
  aumwh <= aumwh;
end d;



-- Seed after: 12224148312869320734,13501862637168280927
