-- Seed: 1004552726085805022,3687118713772291287

library ieee;
use ieee.std_logic_1164.all;

entity b is
  port (uybty : in integer; rhybe : inout real_vector(4 to 4); linc : buffer std_logic_vector(1 to 2));
end b;

architecture tix of b is
  
begin
  -- Multi-driven assignments
  linc <= "0-";
  linc <= ('0', 'H');
  linc <= ('-', 'X');
  linc <= ('U', 'L');
end tix;

entity pgitllutr is
  port (q : inout real);
end pgitllutr;

library ieee;
use ieee.std_logic_1164.all;

architecture fcoieyjvz of pgitllutr is
  signal zy : real_vector(4 to 4);
  signal mlkmps : std_logic_vector(1 to 2);
  signal o : real_vector(4 to 4);
  signal uep : integer;
begin
  pjmswsk : entity work.b
    port map (uybty => uep, rhybe => o, linc => mlkmps);
  jkqwn : entity work.b
    port map (uybty => uep, rhybe => zy, linc => mlkmps);
  
  -- Multi-driven assignments
  mlkmps <= "UX";
  mlkmps <= "LZ";
end fcoieyjvz;



-- Seed after: 5810475647621282342,3687118713772291287
