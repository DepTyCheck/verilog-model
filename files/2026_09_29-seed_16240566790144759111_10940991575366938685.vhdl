-- Seed: 16240566790144759111,10940991575366938685

library ieee;
use ieee.std_logic_1164.all;

entity xbiv is
  port (gzgdqkmt : in std_logic; lsimrnzji : inout std_logic; qng : linkage real; vwfqt : buffer std_logic);
end xbiv;

architecture btjkbbpk of xbiv is
  
begin
  -- Multi-driven assignments
  vwfqt <= 'X';
  lsimrnzji <= vwfqt;
  lsimrnzji <= vwfqt;
end btjkbbpk;

library ieee;
use ieee.std_logic_1164.all;

entity ayl is
  port (czzimx : buffer integer_vector(4 to 3); fkljejx : linkage std_logic_vector(4 to 2); shka : linkage real);
end ayl;

library ieee;
use ieee.std_logic_1164.all;

architecture ehcfhbbzv of ayl is
  signal eozscbtqv : std_logic;
  signal kqny : std_logic;
  signal w : std_logic;
begin
  n : entity work.xbiv
    port map (gzgdqkmt => w, lsimrnzji => kqny, qng => shka, vwfqt => eozscbtqv);
  
  -- Multi-driven assignments
  w <= 'W';
  eozscbtqv <= 'L';
  w <= w;
end ehcfhbbzv;

library ieee;
use ieee.std_logic_1164.all;

entity xdwjyzukmm is
  port (tjbr : inout std_logic);
end xdwjyzukmm;

library ieee;
use ieee.std_logic_1164.all;

architecture jpn of xdwjyzukmm is
  signal f : std_logic;
  signal bwoaz : real;
  signal nrjt : std_logic;
  signal pkmnrxhle : std_logic;
  signal hpgiltep : real;
  signal wbotehzi : real;
  signal znjcykwgke : std_logic;
  signal y : std_logic;
  signal nkoj : real;
  signal tjo : std_logic_vector(4 to 2);
  signal ex : integer_vector(4 to 3);
begin
  polwqn : entity work.ayl
    port map (czzimx => ex, fkljejx => tjo, shka => nkoj);
  eoyc : entity work.xbiv
    port map (gzgdqkmt => y, lsimrnzji => znjcykwgke, qng => wbotehzi, vwfqt => y);
  eo : entity work.xbiv
    port map (gzgdqkmt => znjcykwgke, lsimrnzji => znjcykwgke, qng => hpgiltep, vwfqt => pkmnrxhle);
  ttjs : entity work.xbiv
    port map (gzgdqkmt => nrjt, lsimrnzji => pkmnrxhle, qng => bwoaz, vwfqt => f);
  
  -- Multi-driven assignments
  tjbr <= 'U';
  tjo <= tjo;
  f <= tjbr;
end jpn;



-- Seed after: 4050038717266311580,10940991575366938685
