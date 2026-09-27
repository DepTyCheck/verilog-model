-- Seed: 8916164276569534467,6379010654866854599

library ieee;
use ieee.std_logic_1164.all;

entity jvg is
  port (gnpmd : in std_logic_vector(3 to 0); gi : inout integer; w : inout time; sto : in std_logic_vector(4 downto 3));
end jvg;

architecture fllmym of jvg is
  
begin
  -- Single-driven assignments
  w <= 3.3_1_1 ms;
  gi <= 8#7#;
end fllmym;

entity ye is
  port (swcd : in string(3 to 3); mu : in integer_vector(0 downto 1); jqj : in real; rjwaoaco : buffer real);
end ye;

library ieee;
use ieee.std_logic_1164.all;

architecture i of ye is
  signal cbu : time;
  signal szeoddt : integer;
  signal f : std_logic_vector(4 downto 3);
  signal sivxqcu : time;
  signal btm : integer;
  signal yzrlh : std_logic_vector(3 to 0);
begin
  fu : entity work.jvg
    port map (gnpmd => yzrlh, gi => btm, w => sivxqcu, sto => f);
  mrs : entity work.jvg
    port map (gnpmd => yzrlh, gi => szeoddt, w => cbu, sto => f);
  
  -- Multi-driven assignments
  f <= ('H', '-');
  f <= f;
  yzrlh <= yzrlh;
  f <= f;
end i;

entity ssougj is
  port (f : buffer bit_vector(2 to 1));
end ssougj;

library ieee;
use ieee.std_logic_1164.all;

architecture gk of ssougj is
  signal xucsqrpjhm : std_logic_vector(4 downto 3);
  signal czrsjg : time;
  signal undxtjta : integer;
  signal n : std_logic_vector(3 to 0);
  signal uxbahwu : real;
  signal wxwuzm : integer_vector(0 downto 1);
  signal hkeiidsyg : string(3 to 3);
begin
  zzjpuz : entity work.ye
    port map (swcd => hkeiidsyg, mu => wxwuzm, jqj => uxbahwu, rjwaoaco => uxbahwu);
  faviospga : entity work.jvg
    port map (gnpmd => n, gi => undxtjta, w => czrsjg, sto => xucsqrpjhm);
  
  -- Single-driven assignments
  f <= f;
  wxwuzm <= (others => 0);
  hkeiidsyg <= hkeiidsyg;
  
  -- Multi-driven assignments
  n <= n;
end gk;



-- Seed after: 4777830153095727012,6379010654866854599
