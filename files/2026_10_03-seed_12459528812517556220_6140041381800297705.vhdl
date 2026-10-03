-- Seed: 12459528812517556220,6140041381800297705

library ieee;
use ieee.std_logic_1164.all;

entity fqhvon is
  port (ipijn : out std_logic; kqacynfd : inout boolean; d : inout std_logic_vector(3 downto 3));
end fqhvon;

architecture bwpvrg of fqhvon is
  
begin
  -- Single-driven assignments
  kqacynfd <= FALSE;
  
  -- Multi-driven assignments
  d <= "L";
  d <= (others => 'Z');
  d <= d;
end bwpvrg;

entity jqnh is
  port (jsirxlyq : out time);
end jqnh;

library ieee;
use ieee.std_logic_1164.all;

architecture xsvkyfsnn of jqnh is
  signal kjf : boolean;
  signal khplegbz : std_logic;
  signal ibw : std_logic_vector(3 downto 3);
  signal egdyzvkayz : boolean;
  signal sdrpbz : std_logic_vector(3 downto 3);
  signal wghd : boolean;
  signal n : std_logic;
begin
  sahrsy : entity work.fqhvon
    port map (ipijn => n, kqacynfd => wghd, d => sdrpbz);
  i : entity work.fqhvon
    port map (ipijn => n, kqacynfd => egdyzvkayz, d => ibw);
  kdqjmoes : entity work.fqhvon
    port map (ipijn => khplegbz, kqacynfd => kjf, d => sdrpbz);
  
  -- Single-driven assignments
  jsirxlyq <= jsirxlyq;
  
  -- Multi-driven assignments
  sdrpbz <= sdrpbz;
  n <= 'W';
  khplegbz <= 'X';
end xsvkyfsnn;

entity t is
  port (cl : buffer integer);
end t;

library ieee;
use ieee.std_logic_1164.all;

architecture bcycerpldn of t is
  signal baybhu : boolean;
  signal eeskcevs : std_logic;
  signal jmatsry : boolean;
  signal n : std_logic;
  signal by : boolean;
  signal trbnzuefsb : std_logic;
  signal maqhidu : std_logic_vector(3 downto 3);
  signal gjpg : boolean;
  signal qba : std_logic;
begin
  xcxcng : entity work.fqhvon
    port map (ipijn => qba, kqacynfd => gjpg, d => maqhidu);
  dkot : entity work.fqhvon
    port map (ipijn => trbnzuefsb, kqacynfd => by, d => maqhidu);
  sjzaqrhul : entity work.fqhvon
    port map (ipijn => n, kqacynfd => jmatsry, d => maqhidu);
  rnzvz : entity work.fqhvon
    port map (ipijn => eeskcevs, kqacynfd => baybhu, d => maqhidu);
end bcycerpldn;



-- Seed after: 12048469097815493948,6140041381800297705
