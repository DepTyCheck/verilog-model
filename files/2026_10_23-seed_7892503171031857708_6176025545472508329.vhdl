-- Seed: 7892503171031857708,6176025545472508329

library ieee;
use ieee.std_logic_1164.all;

entity boad is
  port (wm : linkage std_logic; xxfbfif : out std_logic_vector(2 to 3));
end boad;

architecture eyil of boad is
  
begin
  -- Multi-driven assignments
  xxfbfif <= ('L', '1');
  xxfbfif <= ('X', '-');
  xxfbfif <= xxfbfif;
end eyil;

library ieee;
use ieee.std_logic_1164.all;

entity xmeextoat is
  port (rejwufosp : out std_logic; n : in std_logic; pa : out time);
end xmeextoat;

library ieee;
use ieee.std_logic_1164.all;

architecture am of xmeextoat is
  signal fpxvhetzob : std_logic_vector(2 to 3);
begin
  ylxbxx : entity work.boad
    port map (wm => n, xxfbfif => fpxvhetzob);
  
  -- Single-driven assignments
  pa <= 3 min;
  
  -- Multi-driven assignments
  fpxvhetzob <= "U-";
  rejwufosp <= n;
end am;

library ieee;
use ieee.std_logic_1164.all;

entity gywti is
  port (h : inout time; fsuxlgyb : in std_logic);
end gywti;

architecture nhsbph of gywti is
  
begin
  
end nhsbph;

library ieee;
use ieee.std_logic_1164.all;

entity prol is
  port (csypwr : in integer; dev : linkage std_logic_vector(0 to 3); qduazx : out time_vector(1 to 0); hiic : buffer boolean_vector(2 downto 0));
end prol;

library ieee;
use ieee.std_logic_1164.all;

architecture scrlmsntou of prol is
  signal c : time;
  signal s : std_logic_vector(2 to 3);
  signal ljeay : std_logic;
  signal xhcs : std_logic;
  signal mkxd : time;
begin
  y : entity work.gywti
    port map (h => mkxd, fsuxlgyb => xhcs);
  d : entity work.boad
    port map (wm => ljeay, xxfbfif => s);
  huxabq : entity work.boad
    port map (wm => xhcs, xxfbfif => s);
  vdbgkr : entity work.xmeextoat
    port map (rejwufosp => xhcs, n => xhcs, pa => c);
end scrlmsntou;



-- Seed after: 567384011902343586,6176025545472508329
