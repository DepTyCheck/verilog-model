-- Seed: 10843229390134881905,15025465285671019065

library ieee;
use ieee.std_logic_1164.all;

entity ezyuodho is
  port (vbiubtr : buffer std_logic; ddamr : in std_logic; egkrqmt : inout boolean);
end ezyuodho;

architecture gwsbms of ezyuodho is
  
begin
  -- Single-driven assignments
  egkrqmt <= FALSE;
  
  -- Multi-driven assignments
  vbiubtr <= ddamr;
  vbiubtr <= 'U';
  vbiubtr <= 'W';
  vbiubtr <= vbiubtr;
end gwsbms;

library ieee;
use ieee.std_logic_1164.all;

entity qjvjstunzb is
  port (hunviakvos : inout time; h : linkage time; aoh : inout std_logic);
end qjvjstunzb;

library ieee;
use ieee.std_logic_1164.all;

architecture hkxim of qjvjstunzb is
  signal tlblgtm : boolean;
  signal nhpufshqv : std_logic;
  signal qqfrj : std_logic;
  signal ubvujbuq : boolean;
  signal ilqkcywdax : std_logic;
begin
  vhwlqjd : entity work.ezyuodho
    port map (vbiubtr => ilqkcywdax, ddamr => ilqkcywdax, egkrqmt => ubvujbuq);
  kqdfwghwwk : entity work.ezyuodho
    port map (vbiubtr => qqfrj, ddamr => nhpufshqv, egkrqmt => tlblgtm);
  
  -- Single-driven assignments
  hunviakvos <= 2#1# ps;
  
  -- Multi-driven assignments
  aoh <= qqfrj;
end hkxim;

library ieee;
use ieee.std_logic_1164.all;

entity jnz is
  port (wyo : out std_logic; vsxye : linkage std_logic_vector(4 downto 0); vixitkit : buffer time; uahliliy : inout time);
end jnz;

library ieee;
use ieee.std_logic_1164.all;

architecture i of jnz is
  signal ma : boolean;
  signal plisurxlr : std_logic;
  signal jaokgrqsj : boolean;
begin
  jtnpk : entity work.ezyuodho
    port map (vbiubtr => wyo, ddamr => wyo, egkrqmt => jaokgrqsj);
  cc : entity work.ezyuodho
    port map (vbiubtr => wyo, ddamr => plisurxlr, egkrqmt => ma);
  
  -- Single-driven assignments
  uahliliy <= 2#01000.0100# fs;
  
  -- Multi-driven assignments
  wyo <= '-';
end i;



-- Seed after: 13588357725256450575,15025465285671019065
