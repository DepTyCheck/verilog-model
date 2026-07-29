-- Seed: 109711738808428373,14641901754878719179

entity idyytg is
  port (rpqdknt : linkage string(4 downto 2); jryni : inout bit);
end idyytg;

architecture sjh of idyytg is
  
begin
  -- Single-driven assignments
  jryni <= '0';
end sjh;

library ieee;
use ieee.std_logic_1164.all;

entity eoqffoovy is
  port (sxkkgffa : buffer std_logic_vector(2 to 3); iugrbk : in std_logic; ilgvze : in bit; onzrecjl : in std_logic_vector(0 to 1));
end eoqffoovy;

architecture rfpiuhf of eoqffoovy is
  
begin
  -- Multi-driven assignments
  sxkkgffa <= onzrecjl;
end rfpiuhf;

library ieee;
use ieee.std_logic_1164.all;

entity kikbabqqhi is
  port (kasq : in std_logic_vector(1 downto 3); t : in std_logic_vector(3 downto 2));
end kikbabqqhi;

architecture zlgdoufqg of kikbabqqhi is
  
begin
  
end zlgdoufqg;

library ieee;
use ieee.std_logic_1164.all;

entity bshsdr is
  port (i : buffer real; h : buffer std_logic_vector(4 downto 3); atqnma : linkage std_logic_vector(4 to 3));
end bshsdr;

library ieee;
use ieee.std_logic_1164.all;

architecture cuaq of bshsdr is
  signal zuypdtvd : std_logic_vector(1 downto 3);
  signal v : std_logic_vector(1 downto 3);
  signal ngtjuopxh : bit;
  signal krl : string(4 downto 2);
  signal ez : std_logic_vector(3 downto 2);
  signal otcdamsvp : std_logic_vector(1 downto 3);
begin
  hofokzj : entity work.kikbabqqhi
    port map (kasq => otcdamsvp, t => ez);
  ydzg : entity work.idyytg
    port map (rpqdknt => krl, jryni => ngtjuopxh);
  ij : entity work.kikbabqqhi
    port map (kasq => v, t => h);
  djgbl : entity work.kikbabqqhi
    port map (kasq => zuypdtvd, t => ez);
  
  -- Multi-driven assignments
  h <= h;
  h <= "W0";
  v <= "";
  ez <= h;
end cuaq;



-- Seed after: 14685844676341497808,14641901754878719179
