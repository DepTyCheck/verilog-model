-- Seed: 8021505145705331951,12569788116744667457

library ieee;
use ieee.std_logic_1164.all;

entity tfxhdu is
  port (awhselnhj : in std_logic; zhodhqwh : buffer bit; eto : out integer);
end tfxhdu;



architecture mxtys of tfxhdu is
  
begin
  
end mxtys;

library ieee;
use ieee.std_logic_1164.all;

entity hqb is
  port (aqxh : inout integer; tm : out bit; p : in time; gzgf : buffer std_logic);
end hqb;

library ieee;
use ieee.std_logic_1164.all;

architecture znzibita of hqb is
  signal jq : integer;
  signal yxajezter : bit;
  signal bsytyat : integer;
  signal cnkfdesq : bit;
  signal wsequd : std_logic;
  signal bfxx : integer;
  signal ddrfvytsvh : bit;
  signal oztwyxzcr : std_logic;
begin
  zwxbp : entity work.tfxhdu
    port map (awhselnhj => oztwyxzcr, zhodhqwh => ddrfvytsvh, eto => bfxx);
  tksyvvfggz : entity work.tfxhdu
    port map (awhselnhj => wsequd, zhodhqwh => cnkfdesq, eto => bsytyat);
  vtfrvwv : entity work.tfxhdu
    port map (awhselnhj => gzgf, zhodhqwh => yxajezter, eto => aqxh);
  xzuywjysw : entity work.tfxhdu
    port map (awhselnhj => gzgf, zhodhqwh => tm, eto => jq);
end znzibita;



-- Seed after: 18138145893241104491,12569788116744667457
