-- Seed: 2747909255176225416,1834764876137802293

library ieee;
use ieee.std_logic_1164.all;

entity dqoyviz is
  port (bgpxfddff : inout std_logic; l : buffer time);
end dqoyviz;

architecture emn of dqoyviz is
  
begin
  -- Single-driven assignments
  l <= 2#0_0_0# ms;
end emn;

library ieee;
use ieee.std_logic_1164.all;

entity xcpozyegu is
  port (dolbc : in std_logic_vector(1 to 2); qulnj : out integer_vector(3 downto 3); cnt : buffer real; tqffzzaxb : out real);
end xcpozyegu;

library ieee;
use ieee.std_logic_1164.all;

architecture cvagg of xcpozyegu is
  signal akuiox : time;
  signal paupzpdqel : time;
  signal oviekhnuuh : std_logic;
  signal cdlsbp : time;
  signal thwtbcdck : std_logic;
begin
  nlbhqp : entity work.dqoyviz
    port map (bgpxfddff => thwtbcdck, l => cdlsbp);
  qrjlmuzl : entity work.dqoyviz
    port map (bgpxfddff => oviekhnuuh, l => paupzpdqel);
  chvrgto : entity work.dqoyviz
    port map (bgpxfddff => thwtbcdck, l => akuiox);
  
  -- Single-driven assignments
  qulnj <= (others => 1_1);
  tqffzzaxb <= 0.34;
  cnt <= 8#4.3602#;
  
  -- Multi-driven assignments
  thwtbcdck <= '0';
end cvagg;



-- Seed after: 16663297725716846312,1834764876137802293
