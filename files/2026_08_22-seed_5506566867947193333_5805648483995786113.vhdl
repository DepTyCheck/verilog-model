-- Seed: 5506566867947193333,5805648483995786113

library ieee;
use ieee.std_logic_1164.all;

entity vf is
  port (huqqi : buffer boolean; qnrbavj : buffer std_logic_vector(3 to 4); umwimkkiu : in time; uy : out std_logic);
end vf;

architecture lhbcvp of vf is
  
begin
  -- Single-driven assignments
  huqqi <= FALSE;
  
  -- Multi-driven assignments
  uy <= '1';
end lhbcvp;

entity go is
  port (zc : buffer severity_level);
end go;

library ieee;
use ieee.std_logic_1164.all;

architecture pdfech of go is
  signal jkuzmlf : time;
  signal zskbp : boolean;
  signal pnun : time;
  signal pnnp : std_logic_vector(3 to 4);
  signal comwz : boolean;
  signal ynge : time;
  signal uvfnvxepp : std_logic_vector(3 to 4);
  signal im : boolean;
  signal cuzbqtgnry : std_logic;
  signal leqoyfdx : time;
  signal cuvh : std_logic_vector(3 to 4);
  signal zsexf : boolean;
begin
  xddvrmv : entity work.vf
    port map (huqqi => zsexf, qnrbavj => cuvh, umwimkkiu => leqoyfdx, uy => cuzbqtgnry);
  bnuodrgiby : entity work.vf
    port map (huqqi => im, qnrbavj => uvfnvxepp, umwimkkiu => ynge, uy => cuzbqtgnry);
  idrbmbtc : entity work.vf
    port map (huqqi => comwz, qnrbavj => pnnp, umwimkkiu => pnun, uy => cuzbqtgnry);
  xekp : entity work.vf
    port map (huqqi => zskbp, qnrbavj => pnnp, umwimkkiu => jkuzmlf, uy => cuzbqtgnry);
  
  -- Single-driven assignments
  zc <= FAILURE;
  
  -- Multi-driven assignments
  uvfnvxepp <= cuvh;
  cuvh <= cuvh;
  pnnp <= "H0";
  cuvh <= cuvh;
end pdfech;

library ieee;
use ieee.std_logic_1164.all;

entity zvoz is
  port (inut : inout std_logic_vector(1 downto 4));
end zvoz;

library ieee;
use ieee.std_logic_1164.all;

architecture kslvmyntnf of zvoz is
  signal vpnpioybp : severity_level;
  signal qnysueubo : std_logic;
  signal y : time;
  signal qijuhal : std_logic_vector(3 to 4);
  signal er : boolean;
begin
  nbgbezxwd : entity work.vf
    port map (huqqi => er, qnrbavj => qijuhal, umwimkkiu => y, uy => qnysueubo);
  zzi : entity work.go
    port map (zc => vpnpioybp);
  
  -- Single-driven assignments
  y <= 3_3.1 us;
  
  -- Multi-driven assignments
  qijuhal <= "Z1";
  inut <= inut;
  qijuhal <= qijuhal;
  qijuhal <= "LU";
end kslvmyntnf;



-- Seed after: 4867638321574044007,5805648483995786113
