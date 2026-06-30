-- Seed: 13970779280731174269,14629254427735353553

library ieee;
use ieee.std_logic_1164.all;

entity eo is
  port (eknfzpas : inout std_logic; okuqynkddi : out time_vector(4 downto 1); vvia : linkage severity_level; bso : in boolean);
end eo;

architecture bx of eo is
  
begin
  -- Single-driven assignments
  okuqynkddi <= (3 us, 2#11100.1# fs, 1 fs, 1 hr);
  
  -- Multi-driven assignments
  eknfzpas <= 'X';
  eknfzpas <= 'Z';
  eknfzpas <= 'W';
end bx;

entity k is
  port (ttnjclwxv : linkage time);
end k;

library ieee;
use ieee.std_logic_1164.all;

architecture pjq of k is
  signal apckedhutf : boolean;
  signal usu : severity_level;
  signal ypzsyvdwco : time_vector(4 downto 1);
  signal iwowqq : std_logic;
  signal mbfhamf : boolean;
  signal fquxfdmivh : severity_level;
  signal jqis : time_vector(4 downto 1);
  signal qmxkpx : severity_level;
  signal ebgke : time_vector(4 downto 1);
  signal ptfdsubd : std_logic;
  signal mbgmye : boolean;
  signal fptbc : severity_level;
  signal egah : time_vector(4 downto 1);
  signal yu : std_logic;
begin
  yfkqbh : entity work.eo
    port map (eknfzpas => yu, okuqynkddi => egah, vvia => fptbc, bso => mbgmye);
  eeekx : entity work.eo
    port map (eknfzpas => ptfdsubd, okuqynkddi => ebgke, vvia => qmxkpx, bso => mbgmye);
  i : entity work.eo
    port map (eknfzpas => yu, okuqynkddi => jqis, vvia => fquxfdmivh, bso => mbfhamf);
  dsyafalcu : entity work.eo
    port map (eknfzpas => iwowqq, okuqynkddi => ypzsyvdwco, vvia => usu, bso => apckedhutf);
  
  -- Single-driven assignments
  mbgmye <= TRUE;
  apckedhutf <= FALSE;
  mbfhamf <= TRUE;
  
  -- Multi-driven assignments
  ptfdsubd <= '0';
  yu <= '0';
end pjq;



-- Seed after: 14142841074412526066,14629254427735353553
