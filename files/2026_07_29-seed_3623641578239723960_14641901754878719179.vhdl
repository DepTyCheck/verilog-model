-- Seed: 3623641578239723960,14641901754878719179

library ieee;
use ieee.std_logic_1164.all;

entity cpkkw is
  port (sdhhdnrnm : buffer std_logic_vector(0 to 1));
end cpkkw;

architecture bmsuboxrl of cpkkw is
  
begin
  -- Multi-driven assignments
  sdhhdnrnm <= ('L', 'W');
  sdhhdnrnm <= ('L', '0');
end bmsuboxrl;

library ieee;
use ieee.std_logic_1164.all;

entity lqjddju is
  port (vkkpz : in time; ymcs : out std_logic);
end lqjddju;

library ieee;
use ieee.std_logic_1164.all;

architecture rpnyx of lqjddju is
  signal ezjvyt : std_logic_vector(0 to 1);
  signal mcobdwit : std_logic_vector(0 to 1);
  signal kdruyfooxk : std_logic_vector(0 to 1);
begin
  ejhvmcg : entity work.cpkkw
    port map (sdhhdnrnm => kdruyfooxk);
  m : entity work.cpkkw
    port map (sdhhdnrnm => mcobdwit);
  mvhytpa : entity work.cpkkw
    port map (sdhhdnrnm => ezjvyt);
  
  -- Multi-driven assignments
  ymcs <= ymcs;
end rpnyx;



-- Seed after: 15818418870061138707,14641901754878719179
