-- Seed: 7273647181558729828,8067602802092121131

library ieee;
use ieee.std_logic_1164.all;

entity vty is
  port (iwrynsima : out std_logic; ul : out real_vector(1 to 0));
end vty;

architecture ahuo of vty is
  
begin
  -- Single-driven assignments
  ul <= (others => 0.0);
end ahuo;

entity jfhndme is
  port (zjl : out real; xmnswsbyc : buffer real);
end jfhndme;

library ieee;
use ieee.std_logic_1164.all;

architecture y of jfhndme is
  signal buuzdxtse : real_vector(1 to 0);
  signal ec : std_logic;
  signal wk : real_vector(1 to 0);
  signal jcjorzvpu : real_vector(1 to 0);
  signal d : std_logic;
  signal ldijjnw : real_vector(1 to 0);
  signal udt : std_logic;
begin
  qaay : entity work.vty
    port map (iwrynsima => udt, ul => ldijjnw);
  tanqque : entity work.vty
    port map (iwrynsima => d, ul => jcjorzvpu);
  xh : entity work.vty
    port map (iwrynsima => udt, ul => wk);
  bvyhrfe : entity work.vty
    port map (iwrynsima => ec, ul => buuzdxtse);
  
  -- Multi-driven assignments
  udt <= 'H';
  d <= 'L';
end y;

library ieee;
use ieee.std_logic_1164.all;

entity nhq is
  port (ikykeexpr : buffer string(3 to 5); k : inout time; zzzrtzpzst : in std_logic_vector(3 to 3));
end nhq;

library ieee;
use ieee.std_logic_1164.all;

architecture lh of nhq is
  signal srf : real_vector(1 to 0);
  signal vs : std_logic;
  signal x : real;
  signal ce : real;
begin
  gvj : entity work.jfhndme
    port map (zjl => ce, xmnswsbyc => x);
  nyhufe : entity work.vty
    port map (iwrynsima => vs, ul => srf);
  
  -- Single-driven assignments
  k <= 4_1_1.1 ms;
  ikykeexpr <= ikykeexpr;
  
  -- Multi-driven assignments
  vs <= '0';
  vs <= 'W';
  vs <= vs;
  vs <= vs;
end lh;



-- Seed after: 9435842598874031339,8067602802092121131
