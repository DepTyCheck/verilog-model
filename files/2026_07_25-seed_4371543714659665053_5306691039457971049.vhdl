-- Seed: 4371543714659665053,5306691039457971049

library ieee;
use ieee.std_logic_1164.all;

entity pmmfaudx is
  port (whlhpn : in std_logic; kooxqirhxp : in time_vector(3 to 1); ccmnn : out integer);
end pmmfaudx;

architecture khjwn of pmmfaudx is
  
begin
  
end khjwn;

library ieee;
use ieee.std_logic_1164.all;

entity vwgqglxkrj is
  port (dcpoteoorp : out boolean; oxvqznb : out time; wdeqgwp : in std_logic_vector(4 downto 4));
end vwgqglxkrj;

library ieee;
use ieee.std_logic_1164.all;

architecture vgdyatzixu of vwgqglxkrj is
  signal qlgtqn : integer;
  signal azdvluyty : time_vector(3 to 1);
  signal r : integer;
  signal fmfnsn : time_vector(3 to 1);
  signal eeozjdbfai : std_logic;
begin
  xiapy : entity work.pmmfaudx
    port map (whlhpn => eeozjdbfai, kooxqirhxp => fmfnsn, ccmnn => r);
  xh : entity work.pmmfaudx
    port map (whlhpn => eeozjdbfai, kooxqirhxp => azdvluyty, ccmnn => qlgtqn);
  
  -- Single-driven assignments
  oxvqznb <= 2_1_4_1_1.1_0_2_0_4 fs;
  azdvluyty <= fmfnsn;
  
  -- Multi-driven assignments
  eeozjdbfai <= '1';
  eeozjdbfai <= 'L';
end vgdyatzixu;

entity fhzfgm is
  port (shn : in time; kbp : linkage boolean);
end fhzfgm;

library ieee;
use ieee.std_logic_1164.all;

architecture fef of fhzfgm is
  signal kpmvufyax : integer;
  signal lsh : time_vector(3 to 1);
  signal tuhuj : time;
  signal ep : boolean;
  signal pjqtmei : std_logic_vector(4 downto 4);
  signal ys : time;
  signal rboz : boolean;
  signal mokdhemzn : integer;
  signal kowzgwpr : time_vector(3 to 1);
  signal hqfcq : std_logic;
begin
  xb : entity work.pmmfaudx
    port map (whlhpn => hqfcq, kooxqirhxp => kowzgwpr, ccmnn => mokdhemzn);
  kahigfw : entity work.vwgqglxkrj
    port map (dcpoteoorp => rboz, oxvqznb => ys, wdeqgwp => pjqtmei);
  lgv : entity work.vwgqglxkrj
    port map (dcpoteoorp => ep, oxvqznb => tuhuj, wdeqgwp => pjqtmei);
  qv : entity work.pmmfaudx
    port map (whlhpn => hqfcq, kooxqirhxp => lsh, ccmnn => kpmvufyax);
  
  -- Single-driven assignments
  kowzgwpr <= (others => 0 ns);
  lsh <= kowzgwpr;
  
  -- Multi-driven assignments
  pjqtmei <= (others => '1');
  pjqtmei <= (others => 'U');
  hqfcq <= hqfcq;
end fef;

library ieee;
use ieee.std_logic_1164.all;

entity pybzwbjxtm is
  port (lyu : out std_logic; rwbsk : out std_logic_vector(3 downto 4); fr : out integer; eguwi : linkage time);
end pybzwbjxtm;

architecture lap of pybzwbjxtm is
  
begin
  -- Single-driven assignments
  fr <= 8#5#;
  
  -- Multi-driven assignments
  rwbsk <= "";
end lap;



-- Seed after: 8622761975286375301,5306691039457971049
