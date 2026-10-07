-- Seed: 6293384326237157999,5906004015519833893

library ieee;
use ieee.std_logic_1164.all;

entity yuatu is
  port (j : in real; ug : out std_logic; iqqhz : buffer time_vector(3 to 4));
end yuatu;

architecture zguz of yuatu is
  
begin
  -- Multi-driven assignments
  ug <= '0';
  ug <= 'U';
end zguz;

entity enynxezh is
  port (cjpam : out real);
end enynxezh;

architecture sjf of enynxezh is
  
begin
  -- Single-driven assignments
  cjpam <= 2_3.3;
end sjf;

library ieee;
use ieee.std_logic_1164.all;

entity k is
  port (xqbllimfcs : in std_logic_vector(3 to 3); zre : inout real; m : inout string(4 to 2); qschhiacjp : buffer std_logic);
end k;

library ieee;
use ieee.std_logic_1164.all;

architecture zwp of k is
  signal qmxcolkaig : time_vector(3 to 4);
  signal ejuxxvqd : std_logic;
  signal plcdhc : time_vector(3 to 4);
  signal djwfqbv : real;
begin
  fun : entity work.enynxezh
    port map (cjpam => djwfqbv);
  djjjwvphg : entity work.yuatu
    port map (j => zre, ug => qschhiacjp, iqqhz => plcdhc);
  vhahkm : entity work.yuatu
    port map (j => zre, ug => ejuxxvqd, iqqhz => qmxcolkaig);
  
  -- Single-driven assignments
  m <= "";
  zre <= 8#14651.4_0_0#;
  
  -- Multi-driven assignments
  ejuxxvqd <= 'H';
end zwp;

entity wufzfezna is
  port (avvglcb : buffer time; vkgtifutc : linkage real);
end wufzfezna;

architecture izfsctz of wufzfezna is
  
begin
  -- Single-driven assignments
  avvglcb <= avvglcb;
end izfsctz;



-- Seed after: 17295304727038780400,5906004015519833893
