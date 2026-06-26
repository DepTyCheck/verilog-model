-- Seed: 16346737892040375872,12011142928354116943

library ieee;
use ieee.std_logic_1164.all;

entity lee is
  port (tfhlcqczzk : in std_logic; vhnw : in std_logic; ytgdx : out real; ntqqaq : in std_logic_vector(4 downto 3));
end lee;

architecture lzk of lee is
  
begin
  
end lzk;

entity ulp is
  port (jqgmtvnj : in real; xtzfyvypi : inout time);
end ulp;

library ieee;
use ieee.std_logic_1164.all;

architecture xf of ulp is
  signal qycjdfa : std_logic_vector(4 downto 3);
  signal dn : real;
  signal m : real;
  signal kaxmfep : std_logic_vector(4 downto 3);
  signal lojlknm : real;
  signal ghbhkrnmxe : std_logic;
  signal obl : std_logic;
begin
  agqmpvrq : entity work.lee
    port map (tfhlcqczzk => obl, vhnw => ghbhkrnmxe, ytgdx => lojlknm, ntqqaq => kaxmfep);
  frrvyudceu : entity work.lee
    port map (tfhlcqczzk => obl, vhnw => ghbhkrnmxe, ytgdx => m, ntqqaq => kaxmfep);
  skyxkzibb : entity work.lee
    port map (tfhlcqczzk => obl, vhnw => obl, ytgdx => dn, ntqqaq => qycjdfa);
  
  -- Multi-driven assignments
  obl <= 'U';
  kaxmfep <= "Z0";
  obl <= 'W';
end xf;



-- Seed after: 15631980340132045426,12011142928354116943
