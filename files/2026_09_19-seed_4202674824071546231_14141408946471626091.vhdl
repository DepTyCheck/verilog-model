-- Seed: 4202674824071546231,14141408946471626091

library ieee;
use ieee.std_logic_1164.all;

entity qgnwcjmji is
  port (oojnkxp : linkage std_logic_vector(3 downto 3); fozynsbaze : in std_logic_vector(0 to 2));
end qgnwcjmji;

architecture cax of qgnwcjmji is
  
begin
  
end cax;

library ieee;
use ieee.std_logic_1164.all;

entity cshjadhm is
  port (qnh : out std_logic);
end cshjadhm;

library ieee;
use ieee.std_logic_1164.all;

architecture ksfae of cshjadhm is
  signal jxa : std_logic_vector(0 to 2);
  signal slrqbhsman : std_logic_vector(3 downto 3);
begin
  vruprywsmr : entity work.qgnwcjmji
    port map (oojnkxp => slrqbhsman, fozynsbaze => jxa);
  uakbnnnuk : entity work.qgnwcjmji
    port map (oojnkxp => slrqbhsman, fozynsbaze => jxa);
  kdqcvce : entity work.qgnwcjmji
    port map (oojnkxp => slrqbhsman, fozynsbaze => jxa);
  
  -- Multi-driven assignments
  qnh <= qnh;
  slrqbhsman <= "L";
  qnh <= qnh;
  jxa <= jxa;
end ksfae;

entity jtyhuozmob is
  port (wficydf : in integer; tuecfquc : buffer real);
end jtyhuozmob;

library ieee;
use ieee.std_logic_1164.all;

architecture ijtqlquge of jtyhuozmob is
  signal fq : std_logic_vector(3 downto 3);
  signal qqmqi : std_logic_vector(0 to 2);
  signal soucf : std_logic_vector(0 to 2);
  signal xirlqgqu : std_logic_vector(3 downto 3);
  signal l : std_logic;
begin
  jypox : entity work.cshjadhm
    port map (qnh => l);
  p : entity work.qgnwcjmji
    port map (oojnkxp => xirlqgqu, fozynsbaze => soucf);
  bxmqmgau : entity work.qgnwcjmji
    port map (oojnkxp => xirlqgqu, fozynsbaze => qqmqi);
  xzbmnstgza : entity work.qgnwcjmji
    port map (oojnkxp => fq, fozynsbaze => soucf);
  
  -- Single-driven assignments
  tuecfquc <= 16#BDB.DA37#;
  
  -- Multi-driven assignments
  fq <= "W";
  l <= 'L';
  soucf <= soucf;
end ijtqlquge;



-- Seed after: 8952790672532251562,14141408946471626091
