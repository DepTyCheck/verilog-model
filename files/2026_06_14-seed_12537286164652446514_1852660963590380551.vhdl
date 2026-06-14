-- Seed: 12537286164652446514,1852660963590380551

library ieee;
use ieee.std_logic_1164.all;

entity siownrkl is
  port (pmbifoeb : linkage std_logic_vector(4 downto 3); uhfl : buffer boolean_vector(2 to 2); hzs : inout std_logic_vector(3 downto 4));
end siownrkl;



architecture epdbdjkc of siownrkl is
  
begin
  
end epdbdjkc;

library ieee;
use ieee.std_logic_1164.all;

entity qfvupst is
  port (fc : in std_logic; uanqutt : inout boolean; prvy : in real);
end qfvupst;

library ieee;
use ieee.std_logic_1164.all;

architecture supyfo of qfvupst is
  signal mker : std_logic_vector(3 downto 4);
  signal bzkoujux : boolean_vector(2 to 2);
  signal c : std_logic_vector(4 downto 3);
begin
  xallayjbzl : entity work.siownrkl
    port map (pmbifoeb => c, uhfl => bzkoujux, hzs => mker);
end supyfo;

library ieee;
use ieee.std_logic_1164.all;

entity dxa is
  port (gcq : in std_logic_vector(0 to 4); zemsvatex : inout std_logic);
end dxa;

library ieee;
use ieee.std_logic_1164.all;

architecture pn of dxa is
  signal b : boolean;
  signal tl : std_logic_vector(3 downto 4);
  signal m : boolean_vector(2 to 2);
  signal nnrwn : std_logic_vector(3 downto 4);
  signal doemwavugt : boolean_vector(2 to 2);
  signal mhxigv : std_logic_vector(4 downto 3);
  signal szzbwn : real;
  signal sovo : boolean;
begin
  jrtfpcioc : entity work.qfvupst
    port map (fc => zemsvatex, uanqutt => sovo, prvy => szzbwn);
  yza : entity work.siownrkl
    port map (pmbifoeb => mhxigv, uhfl => doemwavugt, hzs => nnrwn);
  lbqadk : entity work.siownrkl
    port map (pmbifoeb => mhxigv, uhfl => m, hzs => tl);
  oqy : entity work.qfvupst
    port map (fc => zemsvatex, uanqutt => b, prvy => szzbwn);
end pn;



-- Seed after: 4828435077544194468,1852660963590380551
