-- Seed: 6136841544916660659,14312733773653067203

library ieee;
use ieee.std_logic_1164.all;

entity mmgubojai is
  port (huottiamqt : buffer real; phdiygsxa : in integer; cjwzsjilil : buffer std_logic; vpljcfzsd : in bit);
end mmgubojai;



architecture vftj of mmgubojai is
  
begin
  
end vftj;

library ieee;
use ieee.std_logic_1164.all;

entity skg is
  port (ozy : out std_logic; wsiupyv : inout std_logic; gxmpecth : buffer integer; rqoa : buffer integer);
end skg;

library ieee;
use ieee.std_logic_1164.all;

architecture sa of skg is
  signal jfovufm : bit;
  signal pdmfpfnzo : std_logic;
  signal uwudhzobxc : real;
  signal zxiawuenkh : bit;
  signal rs : std_logic;
  signal gikxhy : real;
begin
  nviw : entity work.mmgubojai
    port map (huottiamqt => gikxhy, phdiygsxa => rqoa, cjwzsjilil => rs, vpljcfzsd => zxiawuenkh);
  bqzqdjjtql : entity work.mmgubojai
    port map (huottiamqt => uwudhzobxc, phdiygsxa => rqoa, cjwzsjilil => pdmfpfnzo, vpljcfzsd => jfovufm);
end sa;

library ieee;
use ieee.std_logic_1164.all;

entity pztvjwatsf is
  port (rzi : out integer; buh : in std_logic; pgujswghdj : in std_logic);
end pztvjwatsf;

library ieee;
use ieee.std_logic_1164.all;

architecture hkacohvblb of pztvjwatsf is
  signal bh : bit;
  signal ciyhdjfaqd : real;
  signal ur : integer;
  signal bww : std_logic;
  signal qub : std_logic;
  signal fnnxszp : bit;
  signal l : std_logic;
  signal ihtpcdwb : integer;
  signal s : real;
begin
  g : entity work.mmgubojai
    port map (huottiamqt => s, phdiygsxa => ihtpcdwb, cjwzsjilil => l, vpljcfzsd => fnnxszp);
  ktv : entity work.skg
    port map (ozy => qub, wsiupyv => bww, gxmpecth => rzi, rqoa => ur);
  imojigp : entity work.mmgubojai
    port map (huottiamqt => ciyhdjfaqd, phdiygsxa => ur, cjwzsjilil => l, vpljcfzsd => bh);
end hkacohvblb;



-- Seed after: 8365054155305903067,14312733773653067203
