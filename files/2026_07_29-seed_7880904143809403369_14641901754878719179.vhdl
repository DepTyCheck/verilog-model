-- Seed: 7880904143809403369,14641901754878719179

library ieee;
use ieee.std_logic_1164.all;

entity wdzahkw is
  port (r : linkage std_logic);
end wdzahkw;

architecture qclng of wdzahkw is
  
begin
  
end qclng;

entity pov is
  port (vy : linkage real; dpgxehsmo : out integer_vector(0 to 4));
end pov;

library ieee;
use ieee.std_logic_1164.all;

architecture fththduspc of pov is
  signal cnbuixblb : std_logic;
begin
  s : entity work.wdzahkw
    port map (r => cnbuixblb);
end fththduspc;

entity r is
  port (nlycs : in character; ov : out time);
end r;

library ieee;
use ieee.std_logic_1164.all;

architecture rjequoqy of r is
  signal nokaih : std_logic;
  signal qtxzwqcrao : integer_vector(0 to 4);
  signal kqzrlis : real;
begin
  zfjtkegxxb : entity work.pov
    port map (vy => kqzrlis, dpgxehsmo => qtxzwqcrao);
  wt : entity work.wdzahkw
    port map (r => nokaih);
  
  -- Single-driven assignments
  ov <= 2#100.110# ps;
  
  -- Multi-driven assignments
  nokaih <= 'Z';
  nokaih <= nokaih;
end rjequoqy;

library ieee;
use ieee.std_logic_1164.all;

entity gx is
  port (t : in real; wr : inout time; bkcxy : inout std_logic_vector(2 downto 0));
end gx;

library ieee;
use ieee.std_logic_1164.all;

architecture sib of gx is
  signal ona : std_logic;
  signal vqee : std_logic;
  signal oo : std_logic;
  signal eknyt : integer_vector(0 to 4);
  signal aax : real;
begin
  wtybkqtdr : entity work.pov
    port map (vy => aax, dpgxehsmo => eknyt);
  kupyp : entity work.wdzahkw
    port map (r => oo);
  g : entity work.wdzahkw
    port map (r => vqee);
  bun : entity work.wdzahkw
    port map (r => ona);
  
  -- Single-driven assignments
  wr <= 143.120 ps;
  
  -- Multi-driven assignments
  oo <= '1';
end sib;



-- Seed after: 13956107177263247229,14641901754878719179
