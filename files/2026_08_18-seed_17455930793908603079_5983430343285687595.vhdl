-- Seed: 17455930793908603079,5983430343285687595

library ieee;
use ieee.std_logic_1164.all;

entity ebsy is
  port (kwhjrpuw : in boolean; fbzhszmlrt : out std_logic; rtuknroz : linkage std_logic; mcepiswt : inout std_logic_vector(4 downto 0));
end ebsy;

architecture edbpgo of ebsy is
  
begin
  -- Multi-driven assignments
  mcepiswt <= ('H', 'W', 'X', 'X', 'U');
  mcepiswt <= mcepiswt;
end edbpgo;

entity twkldvwtqt is
  port (s : buffer integer);
end twkldvwtqt;

library ieee;
use ieee.std_logic_1164.all;

architecture cxyjc of twkldvwtqt is
  signal sgmaxiwz : std_logic_vector(4 downto 0);
  signal epavqggz : std_logic;
  signal b : boolean;
begin
  r : entity work.ebsy
    port map (kwhjrpuw => b, fbzhszmlrt => epavqggz, rtuknroz => epavqggz, mcepiswt => sgmaxiwz);
  d : entity work.ebsy
    port map (kwhjrpuw => b, fbzhszmlrt => epavqggz, rtuknroz => epavqggz, mcepiswt => sgmaxiwz);
  
  -- Single-driven assignments
  b <= TRUE;
  s <= s;
  
  -- Multi-driven assignments
  epavqggz <= '0';
  epavqggz <= 'X';
end cxyjc;

library ieee;
use ieee.std_logic_1164.all;

entity grzhav is
  port (czwlnjw : buffer bit_vector(2 downto 4); jcnc : inout std_logic; becysc : in std_logic; twxrewmgbk : in std_logic);
end grzhav;

library ieee;
use ieee.std_logic_1164.all;

architecture sh of grzhav is
  signal qnfvdzvs : integer;
  signal ngj : std_logic_vector(4 downto 0);
  signal xi : std_logic_vector(4 downto 0);
  signal kyetuzg : boolean;
  signal yvlio : integer;
begin
  yycuhnk : entity work.twkldvwtqt
    port map (s => yvlio);
  jw : entity work.ebsy
    port map (kwhjrpuw => kyetuzg, fbzhszmlrt => jcnc, rtuknroz => twxrewmgbk, mcepiswt => xi);
  saf : entity work.ebsy
    port map (kwhjrpuw => kyetuzg, fbzhszmlrt => jcnc, rtuknroz => becysc, mcepiswt => ngj);
  vauwd : entity work.twkldvwtqt
    port map (s => qnfvdzvs);
  
  -- Multi-driven assignments
  xi <= ('X', '1', '-', '0', 'X');
  jcnc <= '0';
  jcnc <= 'H';
  ngj <= ('-', '0', 'L', '0', 'W');
end sh;



-- Seed after: 8496641326436902175,5983430343285687595
