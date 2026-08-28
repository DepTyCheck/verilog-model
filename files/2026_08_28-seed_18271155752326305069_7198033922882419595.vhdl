-- Seed: 18271155752326305069,7198033922882419595

entity ujqkhqsq is
  port (btgbvb : inout time);
end ujqkhqsq;

architecture thylp of ujqkhqsq is
  
begin
  -- Single-driven assignments
  btgbvb <= btgbvb;
end thylp;

library ieee;
use ieee.std_logic_1164.all;

entity qpr is
  port (ghepepbn : inout std_logic);
end qpr;

architecture bahdvka of qpr is
  
begin
  -- Multi-driven assignments
  ghepepbn <= '0';
end bahdvka;

library ieee;
use ieee.std_logic_1164.all;

entity lqxw is
  port (pclhqi : in std_logic; kz : out real; plzljzbrh : inout time);
end lqxw;

library ieee;
use ieee.std_logic_1164.all;

architecture kuydp of lqxw is
  signal qspvbvygk : std_logic;
  signal tkdx : std_logic;
begin
  wmlqls : entity work.qpr
    port map (ghepepbn => tkdx);
  cmis : entity work.qpr
    port map (ghepepbn => tkdx);
  pqmvoqed : entity work.ujqkhqsq
    port map (btgbvb => plzljzbrh);
  qnnr : entity work.qpr
    port map (ghepepbn => qspvbvygk);
  
  -- Multi-driven assignments
  qspvbvygk <= 'W';
end kuydp;

entity lxsefs is
  port (xdc : out time; aqvoe : in time; utomqk : out real; pzxbi : out character);
end lxsefs;

library ieee;
use ieee.std_logic_1164.all;

architecture rvaiexsih of lxsefs is
  signal db : time;
  signal sqlclrrp : time;
  signal sebnle : real;
  signal q : std_logic;
begin
  wboadum : entity work.lqxw
    port map (pclhqi => q, kz => sebnle, plzljzbrh => sqlclrrp);
  usuoexurnp : entity work.qpr
    port map (ghepepbn => q);
  rya : entity work.ujqkhqsq
    port map (btgbvb => xdc);
  zlckazasyx : entity work.ujqkhqsq
    port map (btgbvb => db);
  
  -- Single-driven assignments
  pzxbi <= pzxbi;
  utomqk <= utomqk;
  
  -- Multi-driven assignments
  q <= '1';
end rvaiexsih;



-- Seed after: 14308208922956223978,7198033922882419595
