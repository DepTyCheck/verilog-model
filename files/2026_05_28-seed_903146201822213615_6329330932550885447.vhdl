-- Seed: 903146201822213615,6329330932550885447



entity psp is
  port (datnd : out real_vector(4 downto 1));
end psp;



architecture genileoh of psp is
  
begin
  
end genileoh;

library ieee;
use ieee.std_logic_1164.all;

entity dhidv is
  port (uahdianduw : out std_logic_vector(3 downto 0));
end dhidv;



architecture etlrxrbek of dhidv is
  signal mnzyr : real_vector(4 downto 1);
  signal flxl : real_vector(4 downto 1);
  signal hgpt : real_vector(4 downto 1);
begin
  qhav : entity work.psp
    port map (datnd => hgpt);
  yvkeaifdet : entity work.psp
    port map (datnd => flxl);
  pduzasaf : entity work.psp
    port map (datnd => mnzyr);
end etlrxrbek;

library ieee;
use ieee.std_logic_1164.all;

entity gzfztt is
  port (ywzeskbky : in std_logic_vector(1 downto 3); dvirclia : buffer time; ew : in bit);
end gzfztt;



architecture byvive of gzfztt is
  signal v : real_vector(4 downto 1);
  signal gi : real_vector(4 downto 1);
begin
  iqbvjtpy : entity work.psp
    port map (datnd => gi);
  zcocwcbgom : entity work.psp
    port map (datnd => v);
end byvive;

library ieee;
use ieee.std_logic_1164.all;

entity gqrxnrvdsp is
  port (osebv : inout real; dnrzccvro : inout real_vector(2 to 3); eyrm : linkage std_logic_vector(3 to 4));
end gqrxnrvdsp;

library ieee;
use ieee.std_logic_1164.all;

architecture je of gqrxnrvdsp is
  signal cyw : bit;
  signal arybe : time;
  signal vtthkcu : std_logic_vector(1 downto 3);
  signal ozzxw : bit;
  signal mhaktsu : time;
  signal jaqpk : std_logic_vector(1 downto 3);
  signal xoygkgebl : real_vector(4 downto 1);
  signal kayllpr : real_vector(4 downto 1);
begin
  xxo : entity work.psp
    port map (datnd => kayllpr);
  xeqhtpjz : entity work.psp
    port map (datnd => xoygkgebl);
  deqjntew : entity work.gzfztt
    port map (ywzeskbky => jaqpk, dvirclia => mhaktsu, ew => ozzxw);
  qzarptoiwq : entity work.gzfztt
    port map (ywzeskbky => vtthkcu, dvirclia => arybe, ew => cyw);
end je;



-- Seed after: 13709178235503245239,6329330932550885447
