-- Seed: 13923797119978116149,15300320181035395489

library ieee;
use ieee.std_logic_1164.all;

entity uteziavz is
  port (f : buffer std_logic_vector(0 to 4); d : in time; biembrkwf : linkage std_logic_vector(1 to 0));
end uteziavz;

architecture xfoo of uteziavz is
  
begin
  -- Multi-driven assignments
  f <= "H-H0L";
end xfoo;

entity eigo is
  port (phgfrexv : buffer character; krptrlqw : inout boolean_vector(4 to 2));
end eigo;

library ieee;
use ieee.std_logic_1164.all;

architecture pxfeyzn of eigo is
  signal az : std_logic_vector(1 to 0);
  signal ezampnzphk : time;
  signal zss : std_logic_vector(0 to 4);
  signal qkqltnpwpa : std_logic_vector(1 to 0);
  signal p : time;
  signal vur : std_logic_vector(0 to 4);
begin
  zmlu : entity work.uteziavz
    port map (f => vur, d => p, biembrkwf => qkqltnpwpa);
  ottggcgfdn : entity work.uteziavz
    port map (f => zss, d => ezampnzphk, biembrkwf => az);
  
  -- Multi-driven assignments
  zss <= "H-1Z-";
  vur <= ('H', '1', 'U', 'W', 'X');
  zss <= ('W', 'Z', '0', 'H', 'H');
  qkqltnpwpa <= "";
end pxfeyzn;

library ieee;
use ieee.std_logic_1164.all;

entity f is
  port (upu : buffer integer; w : in time; p : buffer character; oqdwk : inout std_logic);
end f;

library ieee;
use ieee.std_logic_1164.all;

architecture fmnrqmb of f is
  signal r : time;
  signal kvzuouxth : std_logic_vector(0 to 4);
  signal dv : std_logic_vector(1 to 0);
  signal galuh : time;
  signal jgjs : std_logic_vector(0 to 4);
begin
  hkytw : entity work.uteziavz
    port map (f => jgjs, d => galuh, biembrkwf => dv);
  pisqcp : entity work.uteziavz
    port map (f => kvzuouxth, d => galuh, biembrkwf => dv);
  hzbpby : entity work.uteziavz
    port map (f => jgjs, d => r, biembrkwf => dv);
  
  -- Single-driven assignments
  p <= 'z';
  upu <= 8#7#;
  r <= 2#0.0_1_1_0# ps;
  
  -- Multi-driven assignments
  oqdwk <= 'U';
end fmnrqmb;



-- Seed after: 11186344759316509554,15300320181035395489
