-- Seed: 762988878608926211,10975587770517081373

library ieee;
use ieee.std_logic_1164.all;

entity rw is
  port (h : buffer time_vector(1 downto 3); iedjczlj : out std_logic);
end rw;

architecture rbp of rw is
  
begin
  -- Single-driven assignments
  h <= (others => 0 ns);
  
  -- Multi-driven assignments
  iedjczlj <= 'W';
  iedjczlj <= iedjczlj;
  iedjczlj <= iedjczlj;
end rbp;

library ieee;
use ieee.std_logic_1164.all;

entity sucfg is
  port (xaldc : out std_logic_vector(1 downto 2));
end sucfg;

library ieee;
use ieee.std_logic_1164.all;

architecture zcmevtgplc of sucfg is
  signal hnmtzff : time_vector(1 downto 3);
  signal fbnk : time_vector(1 downto 3);
  signal khwgappv : std_logic;
  signal vzrdarm : time_vector(1 downto 3);
begin
  q : entity work.rw
    port map (h => vzrdarm, iedjczlj => khwgappv);
  fvzp : entity work.rw
    port map (h => fbnk, iedjczlj => khwgappv);
  duomuad : entity work.rw
    port map (h => hnmtzff, iedjczlj => khwgappv);
  
  -- Multi-driven assignments
  khwgappv <= khwgappv;
  xaldc <= xaldc;
end zcmevtgplc;

library ieee;
use ieee.std_logic_1164.all;

entity uplifyn is
  port (b : in std_logic_vector(2 to 3));
end uplifyn;

library ieee;
use ieee.std_logic_1164.all;

architecture p of uplifyn is
  signal gaxojdqdut : std_logic_vector(1 downto 2);
begin
  wg : entity work.sucfg
    port map (xaldc => gaxojdqdut);
  uhckf : entity work.sucfg
    port map (xaldc => gaxojdqdut);
  
  -- Multi-driven assignments
  gaxojdqdut <= (others => '0');
  gaxojdqdut <= gaxojdqdut;
end p;



-- Seed after: 1861273805375569093,10975587770517081373
