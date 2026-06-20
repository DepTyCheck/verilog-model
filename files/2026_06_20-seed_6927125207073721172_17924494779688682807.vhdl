-- Seed: 6927125207073721172,17924494779688682807

entity aoqb is
  port (ev : buffer time; ar : out real; oqwmto : in integer_vector(0 downto 0));
end aoqb;

architecture vdedmmnk of aoqb is
  
begin
  -- Single-driven assignments
  ev <= 4.0_3_0_1_0 us;
  ar <= 16#E88.E#;
end vdedmmnk;

library ieee;
use ieee.std_logic_1164.all;

entity hxgq is
  port (rpokclgigo : linkage std_logic_vector(0 to 0); cjp : out integer);
end hxgq;

architecture kjxeanqx of hxgq is
  signal s : integer_vector(0 downto 0);
  signal lpl : real;
  signal shejd : time;
begin
  vafgawxmik : entity work.aoqb
    port map (ev => shejd, ar => lpl, oqwmto => s);
end kjxeanqx;

library ieee;
use ieee.std_logic_1164.all;

entity ejrmz is
  port (q : in string(3 to 5); ssrzgluis : buffer time; nkeft : in std_logic_vector(4 to 4); nhwv : linkage severity_level);
end ejrmz;

architecture muarki of ejrmz is
  signal fz : integer;
  signal murapgyc : real;
  signal vlg : time;
  signal rofwu : integer_vector(0 downto 0);
  signal t : real;
begin
  dfzl : entity work.aoqb
    port map (ev => ssrzgluis, ar => t, oqwmto => rofwu);
  qflcokvrz : entity work.aoqb
    port map (ev => vlg, ar => murapgyc, oqwmto => rofwu);
  efnca : entity work.hxgq
    port map (rpokclgigo => nkeft, cjp => fz);
  
  -- Single-driven assignments
  rofwu <= (others => 03143);
end muarki;



-- Seed after: 218156883421902255,17924494779688682807
