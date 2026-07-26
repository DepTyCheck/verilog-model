-- Seed: 1027470993529155756,7808623373429384027

library ieee;
use ieee.std_logic_1164.all;

entity req is
  port (xppey : linkage real; gtwirn : linkage std_logic);
end req;

architecture mmqy of req is
  
begin
  
end mmqy;

entity dt is
  port (bkwke : out integer);
end dt;

library ieee;
use ieee.std_logic_1164.all;

architecture paywsvitk of dt is
  signal uvvdk : std_logic;
  signal drtkg : real;
  signal xrksn : std_logic;
  signal vyjg : real;
  signal bzvv : std_logic;
  signal bqxbeacg : real;
begin
  vhgf : entity work.req
    port map (xppey => bqxbeacg, gtwirn => bzvv);
  zdbuvpud : entity work.req
    port map (xppey => vyjg, gtwirn => xrksn);
  fy : entity work.req
    port map (xppey => drtkg, gtwirn => uvvdk);
  
  -- Single-driven assignments
  bkwke <= 2#01#;
  
  -- Multi-driven assignments
  bzvv <= 'W';
  bzvv <= uvvdk;
end paywsvitk;

entity gferjvv is
  port (czknyag : in boolean);
end gferjvv;

library ieee;
use ieee.std_logic_1164.all;

architecture gtchxmiklm of gferjvv is
  signal ubvivsq : integer;
  signal fbnrljxwl : std_logic;
  signal lvspggc : real;
begin
  w : entity work.req
    port map (xppey => lvspggc, gtwirn => fbnrljxwl);
  npj : entity work.dt
    port map (bkwke => ubvivsq);
  
  -- Multi-driven assignments
  fbnrljxwl <= fbnrljxwl;
  fbnrljxwl <= fbnrljxwl;
  fbnrljxwl <= fbnrljxwl;
end gtchxmiklm;



-- Seed after: 7914089162797835915,7808623373429384027
