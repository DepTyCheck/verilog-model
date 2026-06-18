-- Seed: 6842858017040857134,8118127366649987907

library ieee;
use ieee.std_logic_1164.all;

entity s is
  port (vdgsw : buffer std_logic_vector(1 downto 4); v : buffer real; iwlfud : linkage time);
end s;

architecture mrvp of s is
  
begin
  -- Multi-driven assignments
  vdgsw <= "";
  vdgsw <= "";
  vdgsw <= "";
end mrvp;

entity loflsnqyd is
  port (vld : buffer integer; aiim : inout integer; o : out boolean_vector(0 downto 0));
end loflsnqyd;

library ieee;
use ieee.std_logic_1164.all;

architecture r of loflsnqyd is
  signal bf : time;
  signal olvryfrkab : real;
  signal drgaaxn : std_logic_vector(1 downto 4);
  signal cilmrzwksg : time;
  signal c : real;
  signal jx : time;
  signal xrwfbr : real;
  signal qjdzc : std_logic_vector(1 downto 4);
begin
  xyqymzdqq : entity work.s
    port map (vdgsw => qjdzc, v => xrwfbr, iwlfud => jx);
  ltszd : entity work.s
    port map (vdgsw => qjdzc, v => c, iwlfud => cilmrzwksg);
  vfeuzg : entity work.s
    port map (vdgsw => drgaaxn, v => olvryfrkab, iwlfud => bf);
  
  -- Single-driven assignments
  aiim <= 1;
  vld <= 0;
  o <= (others => FALSE);
  
  -- Multi-driven assignments
  drgaaxn <= (others => '0');
  qjdzc <= (others => '0');
  qjdzc <= "";
  qjdzc <= (others => '0');
end r;

entity jalhitdb is
  port (gcislgkkgg : in time);
end jalhitdb;

library ieee;
use ieee.std_logic_1164.all;

architecture jct of jalhitdb is
  signal rbkzmbffu : boolean_vector(0 downto 0);
  signal giidtt : integer;
  signal xm : integer;
  signal cqmimf : time;
  signal pklukurya : real;
  signal jz : std_logic_vector(1 downto 4);
  signal obmfy : boolean_vector(0 downto 0);
  signal mgqou : integer;
  signal yxtlef : integer;
begin
  lucoxy : entity work.loflsnqyd
    port map (vld => yxtlef, aiim => mgqou, o => obmfy);
  zzjfu : entity work.s
    port map (vdgsw => jz, v => pklukurya, iwlfud => cqmimf);
  orveblko : entity work.loflsnqyd
    port map (vld => xm, aiim => giidtt, o => rbkzmbffu);
end jct;



-- Seed after: 11784830885816561053,8118127366649987907
