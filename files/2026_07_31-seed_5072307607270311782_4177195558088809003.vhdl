-- Seed: 5072307607270311782,4177195558088809003

library ieee;
use ieee.std_logic_1164.all;

entity klaiaqe is
  port (tqay : buffer std_logic; uwad : linkage std_logic_vector(0 to 4); ecjwoy : buffer bit; nktl : in time);
end klaiaqe;

architecture kysjvv of klaiaqe is
  
begin
  -- Single-driven assignments
  ecjwoy <= '0';
  
  -- Multi-driven assignments
  tqay <= 'L';
  tqay <= 'W';
  tqay <= tqay;
  tqay <= tqay;
end kysjvv;

entity sgfrni is
  port (uuilz : linkage time_vector(1 downto 1));
end sgfrni;

library ieee;
use ieee.std_logic_1164.all;

architecture lik of sgfrni is
  signal ngiamydx : bit;
  signal sopbsjq : std_logic;
  signal d : time;
  signal gtoq : bit;
  signal pubpxwl : time;
  signal nbyktbzj : bit;
  signal iudeqbaafo : std_logic_vector(0 to 4);
  signal uzzdqd : std_logic;
begin
  ngey : entity work.klaiaqe
    port map (tqay => uzzdqd, uwad => iudeqbaafo, ecjwoy => nbyktbzj, nktl => pubpxwl);
  hcuozsy : entity work.klaiaqe
    port map (tqay => uzzdqd, uwad => iudeqbaafo, ecjwoy => gtoq, nktl => d);
  gatzc : entity work.klaiaqe
    port map (tqay => sopbsjq, uwad => iudeqbaafo, ecjwoy => ngiamydx, nktl => d);
  
  -- Single-driven assignments
  pubpxwl <= 3_2 ms;
  
  -- Multi-driven assignments
  uzzdqd <= 'W';
end lik;

library ieee;
use ieee.std_logic_1164.all;

entity f is
  port (fcpk : inout bit_vector(4 to 0); lkfik : in severity_level; wrpwhbf : buffer std_logic_vector(2 downto 3); xdqdjnnyfn : inout time);
end f;

library ieee;
use ieee.std_logic_1164.all;

architecture znbbhdnza of f is
  signal bpchw : time;
  signal n : bit;
  signal bzsdbpsas : std_logic_vector(0 to 4);
  signal thptofrxw : std_logic;
begin
  oxogq : entity work.klaiaqe
    port map (tqay => thptofrxw, uwad => bzsdbpsas, ecjwoy => n, nktl => bpchw);
  
  -- Single-driven assignments
  fcpk <= (others => '0');
  bpchw <= xdqdjnnyfn;
  xdqdjnnyfn <= xdqdjnnyfn;
end znbbhdnza;



-- Seed after: 9055630819581331797,4177195558088809003
