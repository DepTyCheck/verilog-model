-- Seed: 12407854363136587356,13857275728440271305

entity eya is
  port (gllrczo : linkage integer);
end eya;

architecture jw of eya is
  
begin
  
end jw;

library ieee;
use ieee.std_logic_1164.all;

entity jvfbr is
  port (dtebre : linkage std_logic_vector(4 to 2));
end jvfbr;

architecture vemhkicsi of jvfbr is
  signal hsuzy : integer;
  signal th : integer;
  signal curdpjd : integer;
  signal d : integer;
begin
  kbgqwhfn : entity work.eya
    port map (gllrczo => d);
  axakqfude : entity work.eya
    port map (gllrczo => curdpjd);
  nkuqcuibe : entity work.eya
    port map (gllrczo => th);
  vaid : entity work.eya
    port map (gllrczo => hsuzy);
end vemhkicsi;

library ieee;
use ieee.std_logic_1164.all;

entity fp is
  port (uaynkbe : inout std_logic; mpzav : in std_logic_vector(1 downto 1); st : buffer time; xplusfbujm : in time);
end fp;

library ieee;
use ieee.std_logic_1164.all;

architecture emo of fp is
  signal ahay : integer;
  signal vexsuj : integer;
  signal flwugbmamx : std_logic_vector(4 to 2);
begin
  jvppyet : entity work.jvfbr
    port map (dtebre => flwugbmamx);
  ijiusmat : entity work.eya
    port map (gllrczo => vexsuj);
  dxfav : entity work.eya
    port map (gllrczo => ahay);
  
  -- Single-driven assignments
  st <= xplusfbujm;
end emo;

entity bvrfa is
  port (er : linkage bit);
end bvrfa;

library ieee;
use ieee.std_logic_1164.all;

architecture ezkiw of bvrfa is
  signal xa : integer;
  signal ln : std_logic_vector(4 to 2);
  signal qkyhdpbvru : time;
  signal u : std_logic_vector(1 downto 1);
  signal vmosknze : std_logic;
begin
  vev : entity work.fp
    port map (uaynkbe => vmosknze, mpzav => u, st => qkyhdpbvru, xplusfbujm => qkyhdpbvru);
  sjxujh : entity work.jvfbr
    port map (dtebre => ln);
  z : entity work.eya
    port map (gllrczo => xa);
end ezkiw;



-- Seed after: 7363789728699125537,13857275728440271305
