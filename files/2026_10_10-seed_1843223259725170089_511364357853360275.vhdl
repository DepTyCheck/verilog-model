-- Seed: 1843223259725170089,511364357853360275

library ieee;
use ieee.std_logic_1164.all;

entity rungutpgo is
  port (g : inout integer; aod : inout std_logic_vector(1 to 0));
end rungutpgo;

architecture uckzace of rungutpgo is
  
begin
  -- Single-driven assignments
  g <= 8#442#;
  
  -- Multi-driven assignments
  aod <= aod;
  aod <= (others => '0');
  aod <= (others => '0');
end uckzace;

entity efp is
  port (iq : inout time);
end efp;

library ieee;
use ieee.std_logic_1164.all;

architecture ym of efp is
  signal lahawgaj : std_logic_vector(1 to 0);
  signal qh : integer;
begin
  qhaebnid : entity work.rungutpgo
    port map (g => qh, aod => lahawgaj);
end ym;

library ieee;
use ieee.std_logic_1164.all;

entity inyzg is
  port (slzltedg : buffer std_logic);
end inyzg;

library ieee;
use ieee.std_logic_1164.all;

architecture e of inyzg is
  signal qnxzqnjaim : std_logic_vector(1 to 0);
  signal fafyh : integer;
  signal idou : time;
  signal luuimughr : integer;
  signal sqdm : std_logic_vector(1 to 0);
  signal yaquyw : integer;
begin
  gjskbirpo : entity work.rungutpgo
    port map (g => yaquyw, aod => sqdm);
  ejtsu : entity work.rungutpgo
    port map (g => luuimughr, aod => sqdm);
  sz : entity work.efp
    port map (iq => idou);
  vnorhila : entity work.rungutpgo
    port map (g => fafyh, aod => qnxzqnjaim);
  
  -- Multi-driven assignments
  slzltedg <= 'W';
  qnxzqnjaim <= sqdm;
  qnxzqnjaim <= sqdm;
end e;



-- Seed after: 18344601089289738562,511364357853360275
