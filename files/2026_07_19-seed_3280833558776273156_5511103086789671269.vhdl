-- Seed: 3280833558776273156,5511103086789671269

library ieee;
use ieee.std_logic_1164.all;

entity zxtzjzhi is
  port (egxu : inout std_logic; qglceutyk : in integer_vector(0 downto 1); ktkiw : inout character);
end zxtzjzhi;

architecture qajugxfle of zxtzjzhi is
  
begin
  -- Single-driven assignments
  ktkiw <= 'r';
  
  -- Multi-driven assignments
  egxu <= 'X';
  egxu <= 'H';
  egxu <= '-';
end qajugxfle;

library ieee;
use ieee.std_logic_1164.all;

entity twdqzcyfvu is
  port (sjnxz : out std_logic; ca : in std_logic_vector(2 downto 4); sdijws : out character; xnk : linkage bit);
end twdqzcyfvu;

library ieee;
use ieee.std_logic_1164.all;

architecture ogshp of twdqzcyfvu is
  signal esbsslgg : character;
  signal e : integer_vector(0 downto 1);
  signal zuyvik : std_logic;
  signal yutrrom : character;
  signal umogfjyqki : integer_vector(0 downto 1);
  signal wmpjmnds : character;
  signal vz : integer_vector(0 downto 1);
  signal omp : std_logic;
begin
  cqy : entity work.zxtzjzhi
    port map (egxu => omp, qglceutyk => vz, ktkiw => wmpjmnds);
  zjzmnsskzj : entity work.zxtzjzhi
    port map (egxu => omp, qglceutyk => umogfjyqki, ktkiw => yutrrom);
  xvlgpkqa : entity work.zxtzjzhi
    port map (egxu => zuyvik, qglceutyk => e, ktkiw => sdijws);
  dxpnqdxtcv : entity work.zxtzjzhi
    port map (egxu => zuyvik, qglceutyk => e, ktkiw => esbsslgg);
  
  -- Single-driven assignments
  vz <= umogfjyqki;
  
  -- Multi-driven assignments
  zuyvik <= sjnxz;
  sjnxz <= 'Z';
end ogshp;



-- Seed after: 15425257329744573866,5511103086789671269
