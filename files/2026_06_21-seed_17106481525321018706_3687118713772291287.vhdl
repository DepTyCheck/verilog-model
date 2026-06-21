-- Seed: 17106481525321018706,3687118713772291287

library ieee;
use ieee.std_logic_1164.all;

entity rcckyyv is
  port (fqmblt : inout std_logic_vector(0 to 4); okk : out std_logic_vector(2 downto 3));
end rcckyyv;

architecture grww of rcckyyv is
  
begin
  -- Multi-driven assignments
  okk <= (others => '0');
  okk <= "";
  okk <= (others => '0');
end grww;

entity tgh is
  port (rrvfqstpo : linkage time);
end tgh;

architecture acaop of tgh is
  
begin
  
end acaop;

library ieee;
use ieee.std_logic_1164.all;

entity rnisquet is
  port (obien : out std_logic_vector(1 downto 4); juohvnrz : out std_logic_vector(1 to 3));
end rnisquet;

library ieee;
use ieee.std_logic_1164.all;

architecture yi of rnisquet is
  signal givhxntji : std_logic_vector(0 to 4);
begin
  vtuiggt : entity work.rcckyyv
    port map (fqmblt => givhxntji, okk => obien);
end yi;

library ieee;
use ieee.std_logic_1164.all;

entity xisgibwr is
  port (mfjifb : linkage integer; fwz : inout std_logic_vector(2 to 2); pgl : in std_logic);
end xisgibwr;

library ieee;
use ieee.std_logic_1164.all;

architecture h of xisgibwr is
  signal jcjxwniv : std_logic_vector(2 downto 3);
  signal gaibr : std_logic_vector(0 to 4);
  signal e : std_logic_vector(1 downto 4);
  signal mfvc : std_logic_vector(1 to 3);
  signal sgjwvharu : std_logic_vector(2 downto 3);
begin
  gzqr : entity work.rnisquet
    port map (obien => sgjwvharu, juohvnrz => mfvc);
  mcrfelp : entity work.rnisquet
    port map (obien => e, juohvnrz => mfvc);
  jaglmg : entity work.rcckyyv
    port map (fqmblt => gaibr, okk => sgjwvharu);
  r : entity work.rcckyyv
    port map (fqmblt => gaibr, okk => jcjxwniv);
  
  -- Multi-driven assignments
  gaibr <= "Z---0";
  e <= "";
  e <= (others => '0');
  gaibr <= ('L', 'H', '1', '1', 'X');
end h;



-- Seed after: 5216344966304342136,3687118713772291287
