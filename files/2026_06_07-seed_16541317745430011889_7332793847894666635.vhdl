-- Seed: 16541317745430011889,7332793847894666635

library ieee;
use ieee.std_logic_1164.all;

entity zvbex is
  port (sfcdllnwyj : in time_vector(4 downto 4); g : buffer time; iu : linkage std_logic);
end zvbex;



architecture wpsi of zvbex is
  
begin
  
end wpsi;

library ieee;
use ieee.std_logic_1164.all;

entity cuk is
  port (gjvffjuw : in time; clkm : out std_logic);
end cuk;



architecture jhqtirz of cuk is
  signal sifqk : time;
  signal bytvysg : time;
  signal mbwfdo : time_vector(4 downto 4);
  signal czeszt : time;
  signal bbo : time_vector(4 downto 4);
begin
  hkht : entity work.zvbex
    port map (sfcdllnwyj => bbo, g => czeszt, iu => clkm);
  tw : entity work.zvbex
    port map (sfcdllnwyj => mbwfdo, g => bytvysg, iu => clkm);
  wgno : entity work.zvbex
    port map (sfcdllnwyj => bbo, g => sifqk, iu => clkm);
end jhqtirz;



entity oji is
  port (eez : out real; lgao : inout boolean; lijknrze : linkage character; videwoie : buffer severity_level);
end oji;

library ieee;
use ieee.std_logic_1164.all;

architecture tscwj of oji is
  signal spkjqu : time;
  signal leg : time_vector(4 downto 4);
  signal af : std_logic;
  signal an : std_logic;
  signal gz : time;
  signal gaicvg : std_logic;
  signal kals : time;
  signal dfg : time_vector(4 downto 4);
begin
  yaomcybteh : entity work.zvbex
    port map (sfcdllnwyj => dfg, g => kals, iu => gaicvg);
  ghfwm : entity work.zvbex
    port map (sfcdllnwyj => dfg, g => gz, iu => an);
  w : entity work.cuk
    port map (gjvffjuw => kals, clkm => af);
  ebchacmats : entity work.zvbex
    port map (sfcdllnwyj => leg, g => spkjqu, iu => an);
end tscwj;

library ieee;
use ieee.std_logic_1164.all;

entity lwpi is
  port (chzogsgj : in std_logic_vector(1 downto 2));
end lwpi;

library ieee;
use ieee.std_logic_1164.all;

architecture dqe of lwpi is
  signal olz : std_logic;
  signal sawaemdhi : time;
  signal p : time_vector(4 downto 4);
begin
  r : entity work.zvbex
    port map (sfcdllnwyj => p, g => sawaemdhi, iu => olz);
end dqe;



-- Seed after: 18040050011183133330,7332793847894666635
