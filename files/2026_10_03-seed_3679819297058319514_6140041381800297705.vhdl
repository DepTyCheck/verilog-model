-- Seed: 3679819297058319514,6140041381800297705

library ieee;
use ieee.std_logic_1164.all;

entity qx is
  port (impvbm : inout std_logic_vector(4 to 4); qiticw : in std_logic_vector(3 downto 1));
end qx;

architecture navd of qx is
  
begin
  -- Multi-driven assignments
  impvbm <= impvbm;
end navd;

entity fhnl is
  port (ebciq : out integer; hu : inout time);
end fhnl;

architecture zrfpj of fhnl is
  
begin
  -- Single-driven assignments
  ebciq <= ebciq;
  hu <= hu;
end zrfpj;

library ieee;
use ieee.std_logic_1164.all;

entity ngjrhhrgj is
  port (qwxxqnm : linkage std_logic_vector(2 to 2); y : buffer boolean; lvybzvo : inout time; qgv : inout integer);
end ngjrhhrgj;

architecture gbajledno of ngjrhhrgj is
  
begin
  -- Single-driven assignments
  lvybzvo <= 2#1# ns;
  y <= y;
end gbajledno;

library ieee;
use ieee.std_logic_1164.all;

entity o is
  port (i : buffer integer; tusl : in time; sludxsdclv : buffer std_logic_vector(0 downto 4));
end o;

library ieee;
use ieee.std_logic_1164.all;

architecture niibtnm of o is
  signal a : time;
  signal yrbat : boolean;
  signal vpavihsi : std_logic_vector(3 downto 1);
  signal uiod : std_logic_vector(2 to 2);
  signal lfbsmtqddu : time;
  signal sl : integer;
begin
  dsxhktzfj : entity work.fhnl
    port map (ebciq => sl, hu => lfbsmtqddu);
  gkntbxs : entity work.qx
    port map (impvbm => uiod, qiticw => vpavihsi);
  msa : entity work.ngjrhhrgj
    port map (qwxxqnm => uiod, y => yrbat, lvybzvo => a, qgv => i);
  
  -- Multi-driven assignments
  vpavihsi <= ('Z', '1', 'W');
  sludxsdclv <= sludxsdclv;
end niibtnm;



-- Seed after: 10499351780620763911,6140041381800297705
