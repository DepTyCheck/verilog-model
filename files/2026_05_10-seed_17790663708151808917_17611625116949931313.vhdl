-- Seed: 17790663708151808917,17611625116949931313

library ieee;
use ieee.std_logic_1164.all;

entity e is
  port (yxkwz : buffer std_logic; ubzjtdz : out time);
end e;



architecture yfabzji of e is
  
begin
  
end yfabzji;

library ieee;
use ieee.std_logic_1164.all;

entity qzwr is
  port (k : out character; ec : out std_logic; ljzgbj : inout boolean; etamy : buffer character);
end qzwr;

library ieee;
use ieee.std_logic_1164.all;

architecture i of qzwr is
  signal goposdpzhn : time;
  signal bqdzz : time;
  signal vlvqsv : time;
  signal rh : std_logic;
begin
  lse : entity work.e
    port map (yxkwz => rh, ubzjtdz => vlvqsv);
  jjnbvhbm : entity work.e
    port map (yxkwz => ec, ubzjtdz => bqdzz);
  ujwoszv : entity work.e
    port map (yxkwz => rh, ubzjtdz => goposdpzhn);
end i;



entity jrd is
  port (svipypnax : out integer; gadogd : buffer integer; przpoppyjf : out integer);
end jrd;

library ieee;
use ieee.std_logic_1164.all;

architecture zflocdyx of jrd is
  signal apyriop : time;
  signal pokjraiixm : character;
  signal nwmzokia : boolean;
  signal pc : std_logic;
  signal cy : character;
begin
  txllabplon : entity work.qzwr
    port map (k => cy, ec => pc, ljzgbj => nwmzokia, etamy => pokjraiixm);
  lxect : entity work.e
    port map (yxkwz => pc, ubzjtdz => apyriop);
end zflocdyx;



-- Seed after: 4321998333322301890,17611625116949931313
