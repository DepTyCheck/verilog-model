-- Seed: 8754673790816692989,13843488114570579517

library ieee;
use ieee.std_logic_1164.all;

entity tjmblkejyw is
  port (zitylfhut : buffer std_logic_vector(4 downto 4));
end tjmblkejyw;

architecture bhnehv of tjmblkejyw is
  
begin
  
end bhnehv;

entity txevbrxeyw is
  port (ridj : buffer bit; l : out time);
end txevbrxeyw;

library ieee;
use ieee.std_logic_1164.all;

architecture n of txevbrxeyw is
  signal t : std_logic_vector(4 downto 4);
  signal qxudvh : std_logic_vector(4 downto 4);
  signal crdltqqn : std_logic_vector(4 downto 4);
begin
  vhtfy : entity work.tjmblkejyw
    port map (zitylfhut => crdltqqn);
  d : entity work.tjmblkejyw
    port map (zitylfhut => qxudvh);
  bts : entity work.tjmblkejyw
    port map (zitylfhut => t);
  ygqc : entity work.tjmblkejyw
    port map (zitylfhut => crdltqqn);
  
  -- Single-driven assignments
  l <= 8#3_2.56# fs;
  ridj <= ridj;
  
  -- Multi-driven assignments
  crdltqqn <= t;
  t <= "0";
  t <= qxudvh;
end n;

library ieee;
use ieee.std_logic_1164.all;

entity qobqbyzkzn is
  port (voewthnerq : linkage severity_level; lgc : in std_logic);
end qobqbyzkzn;

library ieee;
use ieee.std_logic_1164.all;

architecture gxg of qobqbyzkzn is
  signal zqprzfn : std_logic_vector(4 downto 4);
  signal zqcbvhls : time;
  signal sm : bit;
begin
  tszqv : entity work.txevbrxeyw
    port map (ridj => sm, l => zqcbvhls);
  otohencvtz : entity work.tjmblkejyw
    port map (zitylfhut => zqprzfn);
  
  -- Multi-driven assignments
  zqprzfn <= (others => 'U');
  zqprzfn <= "X";
  zqprzfn <= (others => 'H');
end gxg;



-- Seed after: 4332886847047385479,13843488114570579517
