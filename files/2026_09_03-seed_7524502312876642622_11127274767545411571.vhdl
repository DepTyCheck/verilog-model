-- Seed: 7524502312876642622,11127274767545411571

entity z is
  port (ppcjxc : buffer time; klncvzmss : linkage real; eeaytexxn : buffer integer; gcezwdlj : in time);
end z;

architecture bue of z is
  
begin
  -- Single-driven assignments
  eeaytexxn <= eeaytexxn;
  ppcjxc <= 2#1.10001# us;
end bue;

library ieee;
use ieee.std_logic_1164.all;

entity goalg is
  port (bjuuph : inout std_logic_vector(4 downto 4); lyyyjawz : out time_vector(1 to 2); tmphacbta : in std_logic; klal : out real);
end goalg;

architecture fveozpwgz of goalg is
  signal hiqlr : time;
  signal nz : integer;
  signal cfekz : time;
begin
  cna : entity work.z
    port map (ppcjxc => cfekz, klncvzmss => klal, eeaytexxn => nz, gcezwdlj => hiqlr);
  
  -- Single-driven assignments
  hiqlr <= hiqlr;
  lyyyjawz <= lyyyjawz;
  
  -- Multi-driven assignments
  bjuuph <= bjuuph;
  bjuuph <= (others => '-');
  bjuuph <= bjuuph;
end fveozpwgz;

library ieee;
use ieee.std_logic_1164.all;

entity tafi is
  port (ncvk : in std_logic_vector(0 to 2); r : out real; htjhycl : in severity_level; bk : linkage std_logic);
end tafi;

library ieee;
use ieee.std_logic_1164.all;

architecture hxbne of tafi is
  signal qslu : std_logic;
  signal cvr : time_vector(1 to 2);
  signal igcemgabst : std_logic_vector(4 downto 4);
  signal em : time;
  signal dqqliqhqz : integer;
  signal c : real;
  signal lfc : time;
  signal ohduk : integer;
  signal jjook : real;
  signal gsv : time;
begin
  okd : entity work.z
    port map (ppcjxc => gsv, klncvzmss => jjook, eeaytexxn => ohduk, gcezwdlj => lfc);
  fxdgdbrerb : entity work.z
    port map (ppcjxc => lfc, klncvzmss => c, eeaytexxn => dqqliqhqz, gcezwdlj => em);
  exjfurhjrv : entity work.goalg
    port map (bjuuph => igcemgabst, lyyyjawz => cvr, tmphacbta => qslu, klal => r);
  
  -- Single-driven assignments
  em <= 20.34410 ms;
  
  -- Multi-driven assignments
  qslu <= qslu;
  igcemgabst <= (others => '-');
end hxbne;



-- Seed after: 14005548633414167840,11127274767545411571
