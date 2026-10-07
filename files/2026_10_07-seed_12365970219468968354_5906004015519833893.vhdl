-- Seed: 12365970219468968354,5906004015519833893

library ieee;
use ieee.std_logic_1164.all;

entity npu is
  port (rvyo : inout character; scizncd : inout std_logic; bas : out integer; ulbx : in time);
end npu;

architecture tzyl of npu is
  
begin
  
end tzyl;

library ieee;
use ieee.std_logic_1164.all;

entity ihx is
  port (zxamogknp : buffer integer; difxqnhbo : linkage std_logic_vector(2 to 2));
end ihx;

library ieee;
use ieee.std_logic_1164.all;

architecture juromgnjpa of ihx is
  signal smbi : std_logic;
  signal riqsogdglq : character;
  signal pk : time;
  signal owhev : integer;
  signal ccxahuiphy : std_logic;
  signal mv : character;
begin
  fzregoj : entity work.npu
    port map (rvyo => mv, scizncd => ccxahuiphy, bas => owhev, ulbx => pk);
  pdbzmdebnx : entity work.npu
    port map (rvyo => riqsogdglq, scizncd => smbi, bas => zxamogknp, ulbx => pk);
  
  -- Multi-driven assignments
  ccxahuiphy <= ccxahuiphy;
  ccxahuiphy <= smbi;
  ccxahuiphy <= ccxahuiphy;
  smbi <= ccxahuiphy;
end juromgnjpa;

library ieee;
use ieee.std_logic_1164.all;

entity ur is
  port (hdbawqyljo : buffer boolean; oesm : linkage std_logic_vector(1 downto 4));
end ur;

library ieee;
use ieee.std_logic_1164.all;

architecture ecnw of ur is
  signal wdqbnn : std_logic_vector(2 to 2);
  signal iswunyw : integer;
  signal iqjgza : time;
  signal jjd : integer;
  signal loxcfortb : std_logic;
  signal fgkvwr : character;
  signal uuaohujrp : integer;
  signal gi : std_logic_vector(2 to 2);
  signal ymokj : integer;
begin
  v : entity work.ihx
    port map (zxamogknp => ymokj, difxqnhbo => gi);
  vyaogn : entity work.ihx
    port map (zxamogknp => uuaohujrp, difxqnhbo => gi);
  tmwmcnbr : entity work.npu
    port map (rvyo => fgkvwr, scizncd => loxcfortb, bas => jjd, ulbx => iqjgza);
  boid : entity work.ihx
    port map (zxamogknp => iswunyw, difxqnhbo => wdqbnn);
  
  -- Single-driven assignments
  hdbawqyljo <= TRUE;
  
  -- Multi-driven assignments
  gi <= gi;
  wdqbnn <= gi;
  gi <= gi;
  wdqbnn <= gi;
end ecnw;



-- Seed after: 14544623604095291994,5906004015519833893
