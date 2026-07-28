-- Seed: 2796713156858675713,2511821214772927453

entity tm is
  port (zg : inout time_vector(2 downto 4); gdk : out real);
end tm;

architecture fxle of tm is
  
begin
  -- Single-driven assignments
  gdk <= gdk;
  zg <= (others => 0 ns);
end fxle;

entity rnwpyva is
  port (fnqye : inout time);
end rnwpyva;

architecture cizdxa of rnwpyva is
  signal guvdvjgm : real;
  signal xndukbse : time_vector(2 downto 4);
  signal zusnj : real;
  signal bnbpxmo : time_vector(2 downto 4);
  signal yczppbd : real;
  signal wabl : time_vector(2 downto 4);
  signal mvgfqf : real;
  signal bg : time_vector(2 downto 4);
begin
  dr : entity work.tm
    port map (zg => bg, gdk => mvgfqf);
  i : entity work.tm
    port map (zg => wabl, gdk => yczppbd);
  wjs : entity work.tm
    port map (zg => bnbpxmo, gdk => zusnj);
  vz : entity work.tm
    port map (zg => xndukbse, gdk => guvdvjgm);
end cizdxa;

library ieee;
use ieee.std_logic_1164.all;

entity q is
  port (ld : in boolean; kjljrrl : in std_logic_vector(4 to 1); jvtnf : inout real; fodszb : in character);
end q;

architecture wkissfwm of q is
  
begin
  -- Single-driven assignments
  jvtnf <= 8#46260.4#;
end wkissfwm;

library ieee;
use ieee.std_logic_1164.all;

entity hovhndpc is
  port (cdydltl : buffer std_logic_vector(0 to 1); sokjvhnmdd : inout bit_vector(1 downto 0); nisdy : buffer integer);
end hovhndpc;

library ieee;
use ieee.std_logic_1164.all;

architecture mxd of hovhndpc is
  signal ceyz : character;
  signal fedcvb : real;
  signal rk : std_logic_vector(4 to 1);
  signal gjtqo : boolean;
begin
  txcj : entity work.q
    port map (ld => gjtqo, kjljrrl => rk, jvtnf => fedcvb, fodszb => ceyz);
  
  -- Single-driven assignments
  sokjvhnmdd <= ('1', '0');
  ceyz <= ceyz;
  gjtqo <= gjtqo;
  nisdy <= 3_1_1;
  
  -- Multi-driven assignments
  cdydltl <= ('L', '0');
  cdydltl <= ('W', 'H');
end mxd;



-- Seed after: 17902732271466837651,2511821214772927453
