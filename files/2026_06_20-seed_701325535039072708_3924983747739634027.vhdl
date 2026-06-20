-- Seed: 701325535039072708,3924983747739634027

library ieee;
use ieee.std_logic_1164.all;

entity ahxkmxlm is
  port (lxet : linkage std_logic_vector(3 to 4); obak : inout integer; fvjylq : in std_logic);
end ahxkmxlm;

architecture lqayo of ahxkmxlm is
  
begin
  -- Single-driven assignments
  obak <= 2123;
end lqayo;

entity avqxjod is
  port (xozvqlgzp : out boolean; zakbo : in real_vector(3 downto 3));
end avqxjod;

architecture brzvyld of avqxjod is
  
begin
  -- Single-driven assignments
  xozvqlgzp <= TRUE;
end brzvyld;

entity x is
  port (dneo : linkage string(2 downto 3));
end x;

library ieee;
use ieee.std_logic_1164.all;

architecture qoy of x is
  signal sstwwolka : integer;
  signal gwrqs : std_logic_vector(3 to 4);
  signal icrtffj : std_logic;
  signal mbre : integer;
  signal mdytvq : std_logic;
  signal qxusvmjym : integer;
  signal klu : std_logic_vector(3 to 4);
begin
  d : entity work.ahxkmxlm
    port map (lxet => klu, obak => qxusvmjym, fvjylq => mdytvq);
  srvyymcqky : entity work.ahxkmxlm
    port map (lxet => klu, obak => mbre, fvjylq => icrtffj);
  fgb : entity work.ahxkmxlm
    port map (lxet => gwrqs, obak => sstwwolka, fvjylq => icrtffj);
  
  -- Multi-driven assignments
  gwrqs <= ('1', 'X');
  klu <= ('L', '0');
end qoy;



-- Seed after: 4878291590658182363,3924983747739634027
