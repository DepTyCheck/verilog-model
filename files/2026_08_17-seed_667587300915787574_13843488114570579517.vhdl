-- Seed: 667587300915787574,13843488114570579517

entity gdhl is
  port (rwpri : linkage time);
end gdhl;

architecture ydagyesf of gdhl is
  
begin
  
end ydagyesf;

entity ll is
  port (csjwufsaa : linkage time; vepcwtzpkx : out integer; ntkdx : in boolean);
end ll;

architecture qsidg of ll is
  
begin
  
end qsidg;

library ieee;
use ieee.std_logic_1164.all;

entity vlahc is
  port (q : linkage std_logic_vector(0 to 0); vbqjppcgnz : buffer std_logic);
end vlahc;

architecture maqgfwplg of vlahc is
  signal zowi : time;
  signal sawaee : time;
  signal ytamkxcwll : boolean;
  signal bdvknmncf : integer;
  signal uosx : time;
  signal wgjilh : time;
begin
  zdwebe : entity work.gdhl
    port map (rwpri => wgjilh);
  ayqkwg : entity work.ll
    port map (csjwufsaa => uosx, vepcwtzpkx => bdvknmncf, ntkdx => ytamkxcwll);
  hooxckxd : entity work.gdhl
    port map (rwpri => sawaee);
  uakx : entity work.gdhl
    port map (rwpri => zowi);
  
  -- Single-driven assignments
  ytamkxcwll <= TRUE;
  
  -- Multi-driven assignments
  vbqjppcgnz <= '0';
  vbqjppcgnz <= 'Z';
end maqgfwplg;



-- Seed after: 10814717158184761380,13843488114570579517
