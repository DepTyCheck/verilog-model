-- Seed: 3680746611022357189,6176025545472508329

library ieee;
use ieee.std_logic_1164.all;

entity drxwurf is
  port (j : linkage std_logic_vector(0 to 1); bh : linkage std_logic; xrcxz : in integer; oalq : buffer boolean);
end drxwurf;

architecture bzznfudrop of drxwurf is
  
begin
  -- Single-driven assignments
  oalq <= FALSE;
end bzznfudrop;

library ieee;
use ieee.std_logic_1164.all;

entity lfooadsl is
  port (od : inout std_logic; akkqfexuw : inout integer; k : inout integer);
end lfooadsl;

library ieee;
use ieee.std_logic_1164.all;

architecture bmyej of lfooadsl is
  signal agmacb : boolean;
  signal jfvvvwunf : integer;
  signal xl : std_logic_vector(0 to 1);
  signal yetgrg : boolean;
  signal uunvuo : std_logic;
  signal xbml : boolean;
  signal r : integer;
  signal vnbwkl : std_logic;
  signal orgcusyy : boolean;
  signal niyco : std_logic;
  signal l : std_logic_vector(0 to 1);
begin
  dgezza : entity work.drxwurf
    port map (j => l, bh => niyco, xrcxz => k, oalq => orgcusyy);
  sydbord : entity work.drxwurf
    port map (j => l, bh => vnbwkl, xrcxz => r, oalq => xbml);
  ndrrfvh : entity work.drxwurf
    port map (j => l, bh => uunvuo, xrcxz => k, oalq => yetgrg);
  b : entity work.drxwurf
    port map (j => xl, bh => od, xrcxz => jfvvvwunf, oalq => agmacb);
  
  -- Single-driven assignments
  k <= 2;
  jfvvvwunf <= akkqfexuw;
  r <= k;
  akkqfexuw <= 2#0_1_1#;
  
  -- Multi-driven assignments
  uunvuo <= niyco;
  od <= 'Z';
  od <= '0';
  xl <= l;
end bmyej;



-- Seed after: 15914781301846682542,6176025545472508329
