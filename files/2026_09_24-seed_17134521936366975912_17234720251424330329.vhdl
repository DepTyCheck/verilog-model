-- Seed: 17134521936366975912,17234720251424330329

library ieee;
use ieee.std_logic_1164.all;

entity encfbvc is
  port (m : out boolean; fnghujpi : inout std_logic; mxjynlzhsr : buffer real; ganiv : buffer std_logic_vector(2 to 2));
end encfbvc;

architecture jlerfo of encfbvc is
  
begin
  
end jlerfo;

library ieee;
use ieee.std_logic_1164.all;

entity uufzyfut is
  port (ra : buffer severity_level; xjdukodpj : inout std_logic_vector(0 to 4); em : in boolean; srnfcdm : buffer time_vector(3 to 2));
end uufzyfut;

architecture ygg of uufzyfut is
  
begin
  -- Multi-driven assignments
  xjdukodpj <= "HWLXX";
  xjdukodpj <= xjdukodpj;
  xjdukodpj <= "XZ1U1";
  xjdukodpj <= xjdukodpj;
end ygg;

entity bxhlouuvhq is
  port (hsjsnzqxyu : in bit_vector(4 downto 3));
end bxhlouuvhq;

library ieee;
use ieee.std_logic_1164.all;

architecture xd of bxhlouuvhq is
  signal hgaz : time_vector(3 to 2);
  signal kscwukimsa : severity_level;
  signal a : std_logic_vector(2 to 2);
  signal c : real;
  signal mx : std_logic;
  signal qb : boolean;
  signal xkezapukyz : time_vector(3 to 2);
  signal gwq : severity_level;
  signal xbihasawbc : time_vector(3 to 2);
  signal zvahq : boolean;
  signal k : std_logic_vector(0 to 4);
  signal bm : severity_level;
begin
  amjogzvww : entity work.uufzyfut
    port map (ra => bm, xjdukodpj => k, em => zvahq, srnfcdm => xbihasawbc);
  vh : entity work.uufzyfut
    port map (ra => gwq, xjdukodpj => k, em => zvahq, srnfcdm => xkezapukyz);
  aqeyzi : entity work.encfbvc
    port map (m => qb, fnghujpi => mx, mxjynlzhsr => c, ganiv => a);
  jf : entity work.uufzyfut
    port map (ra => kscwukimsa, xjdukodpj => k, em => zvahq, srnfcdm => hgaz);
  
  -- Single-driven assignments
  zvahq <= TRUE;
  
  -- Multi-driven assignments
  k <= k;
  k <= k;
  k <= "WL0ZX";
  k <= "XH0HU";
end xd;

library ieee;
use ieee.std_logic_1164.all;

entity wl is
  port (nvztcr : inout std_logic_vector(4 to 2); zgd : linkage bit_vector(1 downto 1); zsvkheh : in real);
end wl;

library ieee;
use ieee.std_logic_1164.all;

architecture pzfw of wl is
  signal pn : std_logic_vector(2 to 2);
  signal ubdg : real;
  signal lva : boolean;
  signal vhg : std_logic_vector(2 to 2);
  signal d : real;
  signal nosswahv : std_logic;
  signal llkqijn : boolean;
begin
  vbzfllu : entity work.encfbvc
    port map (m => llkqijn, fnghujpi => nosswahv, mxjynlzhsr => d, ganiv => vhg);
  rx : entity work.encfbvc
    port map (m => lva, fnghujpi => nosswahv, mxjynlzhsr => ubdg, ganiv => pn);
  
  -- Multi-driven assignments
  vhg <= vhg;
  nvztcr <= nvztcr;
  nosswahv <= '1';
  vhg <= (others => '-');
end pzfw;



-- Seed after: 3367847195312667553,17234720251424330329
