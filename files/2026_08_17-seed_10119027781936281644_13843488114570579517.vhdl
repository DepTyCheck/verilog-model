-- Seed: 10119027781936281644,13843488114570579517

library ieee;
use ieee.std_logic_1164.all;

entity ut is
  port (fxwybyfrrh : in real; phe : out std_logic);
end ut;

architecture uhyltqyygn of ut is
  
begin
  -- Multi-driven assignments
  phe <= '0';
end uhyltqyygn;

library ieee;
use ieee.std_logic_1164.all;

entity m is
  port (sjjwcdh : inout character; grvpvw : in std_logic; acjdguruft : inout time; jymi : inout std_logic_vector(3 to 3));
end m;

library ieee;
use ieee.std_logic_1164.all;

architecture tc of m is
  signal pvcr : std_logic;
  signal yvdn : real;
  signal hkbsyy : std_logic;
  signal ivgho : std_logic;
  signal ywfwylvah : real;
begin
  ipszojspnl : entity work.ut
    port map (fxwybyfrrh => ywfwylvah, phe => ivgho);
  sfmr : entity work.ut
    port map (fxwybyfrrh => ywfwylvah, phe => hkbsyy);
  ltldxm : entity work.ut
    port map (fxwybyfrrh => yvdn, phe => hkbsyy);
  qijp : entity work.ut
    port map (fxwybyfrrh => ywfwylvah, phe => pvcr);
  
  -- Single-driven assignments
  yvdn <= ywfwylvah;
  
  -- Multi-driven assignments
  hkbsyy <= ivgho;
  jymi <= (others => 'H');
end tc;



-- Seed after: 2828359154121945020,13843488114570579517
