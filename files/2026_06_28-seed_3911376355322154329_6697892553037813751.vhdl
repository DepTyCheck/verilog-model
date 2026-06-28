-- Seed: 3911376355322154329,6697892553037813751

library ieee;
use ieee.std_logic_1164.all;

entity m is
  port (otllpqteb : in integer; vipbbbrxxk : in std_logic_vector(3 downto 4); uzbz : linkage bit; twewlizh : inout integer);
end m;

architecture h of m is
  
begin
  -- Single-driven assignments
  twewlizh <= 8#7_3_3_4#;
end h;

library ieee;
use ieee.std_logic_1164.all;

entity yhgn is
  port (zpavt : in std_logic_vector(4 to 3); t : out integer; gpxs : inout std_logic; agoltrnjd : inout std_logic);
end yhgn;

library ieee;
use ieee.std_logic_1164.all;

architecture pxvorf of yhgn is
  signal kxnghee : integer;
  signal faqnpxlgwe : bit;
  signal iimf : bit;
  signal kmcbfxvnhm : std_logic_vector(3 downto 4);
  signal xgrqnor : bit;
  signal xnqxs : std_logic_vector(3 downto 4);
  signal eowd : integer;
begin
  bjs : entity work.m
    port map (otllpqteb => eowd, vipbbbrxxk => xnqxs, uzbz => xgrqnor, twewlizh => t);
  smuljgmyy : entity work.m
    port map (otllpqteb => eowd, vipbbbrxxk => kmcbfxvnhm, uzbz => iimf, twewlizh => eowd);
  gpj : entity work.m
    port map (otllpqteb => eowd, vipbbbrxxk => zpavt, uzbz => faqnpxlgwe, twewlizh => kxnghee);
  
  -- Multi-driven assignments
  kmcbfxvnhm <= "";
  agoltrnjd <= 'U';
  agoltrnjd <= 'X';
end pxvorf;

library ieee;
use ieee.std_logic_1164.all;

entity gcuox is
  port (xugl : inout boolean; rjuanuv : linkage boolean; anfjsrjgu : buffer integer; ulx : linkage std_logic);
end gcuox;

library ieee;
use ieee.std_logic_1164.all;

architecture rvpudhky of gcuox is
  signal wwojc : bit;
  signal snxaewi : std_logic_vector(3 downto 4);
  signal oxkydq : integer;
begin
  xwvadze : entity work.m
    port map (otllpqteb => oxkydq, vipbbbrxxk => snxaewi, uzbz => wwojc, twewlizh => anfjsrjgu);
  
  -- Single-driven assignments
  oxkydq <= 3;
  xugl <= TRUE;
  
  -- Multi-driven assignments
  snxaewi <= (others => '0');
end rvpudhky;



-- Seed after: 5236007878036190851,6697892553037813751
