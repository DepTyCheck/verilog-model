-- Seed: 9022700355515650618,10557070023141912087

library ieee;
use ieee.std_logic_1164.all;

entity manha is
  port (ppqedwto : linkage real_vector(0 downto 0); agjnsvrt : out std_logic; v : inout std_logic_vector(0 downto 3));
end manha;

architecture suewq of manha is
  
begin
  -- Multi-driven assignments
  v <= "";
end suewq;

library ieee;
use ieee.std_logic_1164.all;

entity nat is
  port (nyjvlya : in bit; nwu : out std_logic; ikyfauh : out real; rbe : out bit_vector(1 downto 1));
end nat;

architecture mkysamh of nat is
  
begin
  -- Single-driven assignments
  rbe <= (others => '1');
  ikyfauh <= 30.203;
  
  -- Multi-driven assignments
  nwu <= 'H';
  nwu <= '-';
  nwu <= 'Z';
end mkysamh;

library ieee;
use ieee.std_logic_1164.all;

entity v is
  port (pi : in std_logic);
end v;

library ieee;
use ieee.std_logic_1164.all;

architecture s of v is
  signal pwndpzgqeu : std_logic_vector(0 downto 3);
  signal cciyum : std_logic;
  signal okhpchiz : real_vector(0 downto 0);
begin
  arq : entity work.manha
    port map (ppqedwto => okhpchiz, agjnsvrt => cciyum, v => pwndpzgqeu);
end s;

entity yaol is
  port (zksladsqdm : buffer integer);
end yaol;

library ieee;
use ieee.std_logic_1164.all;

architecture vmucss of yaol is
  signal tdcdbwmw : std_logic_vector(0 downto 3);
  signal ckl : real_vector(0 downto 0);
  signal mpty : std_logic;
  signal qzbwnmko : bit_vector(1 downto 1);
  signal xvzvu : real;
  signal izw : std_logic;
  signal wvlt : bit;
  signal nectimavor : bit_vector(1 downto 1);
  signal xf : real;
  signal pucuakqj : std_logic;
  signal zjxsbw : bit;
begin
  kdampw : entity work.nat
    port map (nyjvlya => zjxsbw, nwu => pucuakqj, ikyfauh => xf, rbe => nectimavor);
  zvjmjbfg : entity work.nat
    port map (nyjvlya => wvlt, nwu => izw, ikyfauh => xvzvu, rbe => qzbwnmko);
  vwcppkt : entity work.v
    port map (pi => mpty);
  hlodzffk : entity work.manha
    port map (ppqedwto => ckl, agjnsvrt => pucuakqj, v => tdcdbwmw);
  
  -- Single-driven assignments
  zksladsqdm <= 323;
  wvlt <= '1';
  zjxsbw <= '1';
  
  -- Multi-driven assignments
  pucuakqj <= '0';
  mpty <= '-';
end vmucss;



-- Seed after: 13622309558460767246,10557070023141912087
