-- Seed: 8525502188284748360,7311216359267151659

library ieee;
use ieee.std_logic_1164.all;

entity mnrdtx is
  port (dmxwiiv : out std_logic);
end mnrdtx;

architecture widfnsy of mnrdtx is
  
begin
  -- Multi-driven assignments
  dmxwiiv <= dmxwiiv;
  dmxwiiv <= '-';
end widfnsy;

library ieee;
use ieee.std_logic_1164.all;

entity hool is
  port (mkem : out integer_vector(2 downto 2); lbhvgmnwj : buffer std_logic_vector(1 downto 4); afakxxvqid : buffer integer);
end hool;

library ieee;
use ieee.std_logic_1164.all;

architecture ygiuxqg of hool is
  signal dihlcxa : std_logic;
begin
  sbgheqg : entity work.mnrdtx
    port map (dmxwiiv => dihlcxa);
end ygiuxqg;

library ieee;
use ieee.std_logic_1164.all;

entity jsbcixni is
  port (khxew : out time; uwm : buffer boolean; etlumn : out std_logic_vector(2 to 4); mdbyfvfu : buffer real);
end jsbcixni;

library ieee;
use ieee.std_logic_1164.all;

architecture emoi of jsbcixni is
  signal iinjjpsgfa : integer;
  signal khuylanv : std_logic_vector(1 downto 4);
  signal glwh : integer_vector(2 downto 2);
begin
  htneokiuk : entity work.hool
    port map (mkem => glwh, lbhvgmnwj => khuylanv, afakxxvqid => iinjjpsgfa);
  
  -- Single-driven assignments
  mdbyfvfu <= mdbyfvfu;
  uwm <= TRUE;
  
  -- Multi-driven assignments
  etlumn <= etlumn;
  etlumn <= etlumn;
  etlumn <= ('U', 'U', 'H');
end emoi;

entity g is
  port (dzwh : out real; vkuzbzlejp : linkage time; xc : out integer);
end g;

library ieee;
use ieee.std_logic_1164.all;

architecture zybbddrz of g is
  signal gbazyfv : std_logic;
  signal afqmlmut : std_logic;
  signal rnuadavknl : real;
  signal zvvakgtop : boolean;
  signal gn : time;
  signal drcxueyl : std_logic_vector(2 to 4);
  signal xofzffnwb : boolean;
  signal y : time;
begin
  qm : entity work.jsbcixni
    port map (khxew => y, uwm => xofzffnwb, etlumn => drcxueyl, mdbyfvfu => dzwh);
  ztsf : entity work.jsbcixni
    port map (khxew => gn, uwm => zvvakgtop, etlumn => drcxueyl, mdbyfvfu => rnuadavknl);
  oszw : entity work.mnrdtx
    port map (dmxwiiv => afqmlmut);
  peozp : entity work.mnrdtx
    port map (dmxwiiv => gbazyfv);
  
  -- Single-driven assignments
  xc <= xc;
  
  -- Multi-driven assignments
  afqmlmut <= 'U';
  drcxueyl <= ('L', 'W', '0');
  drcxueyl <= "ZLL";
end zybbddrz;



-- Seed after: 179793706562487815,7311216359267151659
