-- Seed: 4836096679956751516,8927267689619684183

library ieee;
use ieee.std_logic_1164.all;

entity cvatp is
  port (kyqdbnl : out std_logic; d : buffer integer; xetrf : buffer std_logic_vector(0 to 3); vpewqlm : out integer);
end cvatp;

architecture wcozej of cvatp is
  
begin
  -- Multi-driven assignments
  xetrf <= xetrf;
end wcozej;

entity wv is
  port (ukpgnms : inout time; tndhtag : in time);
end wv;

architecture gzsppjvf of wv is
  
begin
  
end gzsppjvf;

entity sl is
  port (dhohgymal : in integer; uliscjuc : linkage string(3 to 2));
end sl;

library ieee;
use ieee.std_logic_1164.all;

architecture ryqasru of sl is
  signal jiqe : integer;
  signal pdl : std_logic_vector(0 to 3);
  signal eifjpu : integer;
  signal jnoa : std_logic;
begin
  ky : entity work.cvatp
    port map (kyqdbnl => jnoa, d => eifjpu, xetrf => pdl, vpewqlm => jiqe);
  
  -- Multi-driven assignments
  pdl <= ('U', 'Z', 'X', 'H');
end ryqasru;

library ieee;
use ieee.std_logic_1164.all;

entity s is
  port (vmeeeujav : out std_logic; nburs : in std_logic_vector(2 downto 3); bwkakxraln : out boolean; ray : in time);
end s;

library ieee;
use ieee.std_logic_1164.all;

architecture qdcq of s is
  signal hpphjmdqf : integer;
  signal suihgzksnk : std_logic_vector(0 to 3);
  signal sqslaxkq : integer;
  signal qmtcbge : time;
  signal hxaluea : time;
  signal plzdcofvut : integer;
  signal vvi : std_logic_vector(0 to 3);
  signal gvr : integer;
begin
  ufprfgxyne : entity work.cvatp
    port map (kyqdbnl => vmeeeujav, d => gvr, xetrf => vvi, vpewqlm => plzdcofvut);
  gduucnqr : entity work.wv
    port map (ukpgnms => hxaluea, tndhtag => qmtcbge);
  eufjpxosb : entity work.cvatp
    port map (kyqdbnl => vmeeeujav, d => sqslaxkq, xetrf => suihgzksnk, vpewqlm => hpphjmdqf);
  
  -- Single-driven assignments
  bwkakxraln <= TRUE;
  qmtcbge <= 1342.2_1_3 fs;
  
  -- Multi-driven assignments
  suihgzksnk <= "11UX";
  suihgzksnk <= vvi;
  vvi <= "W-1X";
end qdcq;



-- Seed after: 700964098667412622,8927267689619684183
