-- Seed: 7320307726921854109,662889661651915549

library ieee;
use ieee.std_logic_1164.all;

entity whargcp is
  port (jodeegc : in std_logic; oemi : out time; ek : inout std_logic_vector(2 to 4); tjivhd : inout std_logic_vector(4 downto 1));
end whargcp;

architecture idqesfzqf of whargcp is
  
begin
  -- Single-driven assignments
  oemi <= 1441.33 ns;
  
  -- Multi-driven assignments
  tjivhd <= ('W', '0', 'U', 'U');
end idqesfzqf;

library ieee;
use ieee.std_logic_1164.all;

entity yj is
  port (uhdjuqtov : out time; kxjheobgo : inout time; tflwqhdf : inout std_logic_vector(1 downto 4));
end yj;

library ieee;
use ieee.std_logic_1164.all;

architecture vg of yj is
  signal tmp : std_logic_vector(2 to 4);
  signal lkarsa : std_logic_vector(4 downto 1);
  signal srsvrwpouz : time;
  signal oczsmkrj : std_logic_vector(4 downto 1);
  signal voxyxcz : std_logic_vector(2 to 4);
  signal fskmtewa : time;
  signal pfsuh : std_logic;
  signal abwj : std_logic_vector(4 downto 1);
  signal vudexqzumv : std_logic_vector(2 to 4);
  signal nynw : std_logic;
begin
  fxvpru : entity work.whargcp
    port map (jodeegc => nynw, oemi => kxjheobgo, ek => vudexqzumv, tjivhd => abwj);
  g : entity work.whargcp
    port map (jodeegc => pfsuh, oemi => fskmtewa, ek => voxyxcz, tjivhd => oczsmkrj);
  k : entity work.whargcp
    port map (jodeegc => nynw, oemi => srsvrwpouz, ek => vudexqzumv, tjivhd => lkarsa);
  lavqonni : entity work.whargcp
    port map (jodeegc => nynw, oemi => uhdjuqtov, ek => tmp, tjivhd => abwj);
  
  -- Multi-driven assignments
  tflwqhdf <= "";
  oczsmkrj <= abwj;
  tmp <= ('L', '0', 'H');
end vg;



-- Seed after: 11570578655872382950,662889661651915549
