-- Seed: 7338719818384459139,8118127366649987907

library ieee;
use ieee.std_logic_1164.all;

entity jc is
  port (vwmd : out std_logic; vd : inout std_logic; zforfh : buffer time; vg : inout std_logic);
end jc;

architecture hxy of jc is
  
begin
  -- Single-driven assignments
  zforfh <= 1 hr;
  
  -- Multi-driven assignments
  vg <= 'Z';
  vwmd <= 'H';
  vg <= 'X';
  vg <= 'H';
end hxy;

entity swnxijvp is
  port (vafbzph : inout real; jgibip : out time_vector(2 downto 1); onkbgrt : inout time);
end swnxijvp;

library ieee;
use ieee.std_logic_1164.all;

architecture yth of swnxijvp is
  signal siltrqbjnh : std_logic;
  signal kgea : time;
  signal qd : std_logic;
  signal zw : std_logic;
  signal uqlo : time;
  signal qnwmyfko : std_logic;
  signal stolcd : time;
  signal ynzkahq : std_logic;
begin
  fjk : entity work.jc
    port map (vwmd => ynzkahq, vd => ynzkahq, zforfh => stolcd, vg => qnwmyfko);
  cfdglw : entity work.jc
    port map (vwmd => ynzkahq, vd => qnwmyfko, zforfh => uqlo, vg => zw);
  hcxmje : entity work.jc
    port map (vwmd => ynzkahq, vd => qd, zforfh => kgea, vg => siltrqbjnh);
  hcen : entity work.jc
    port map (vwmd => zw, vd => qd, zforfh => onkbgrt, vg => siltrqbjnh);
  
  -- Single-driven assignments
  jgibip <= (8#21# us, 2#1_1_0_0_1.1000# fs);
  vafbzph <= 0_4_3_3.44;
end yth;

library ieee;
use ieee.std_logic_1164.all;

entity tbzzszd is
  port (qxge : linkage std_logic_vector(2 to 3); uozdnymrtp : inout integer);
end tbzzszd;

architecture mbq of tbzzszd is
  signal ajdzpj : time;
  signal evvldipycu : time_vector(2 downto 1);
  signal jkrlgnoxb : real;
  signal eivffvr : time;
  signal cleplijqdu : time_vector(2 downto 1);
  signal tvmccxn : real;
begin
  qxszvck : entity work.swnxijvp
    port map (vafbzph => tvmccxn, jgibip => cleplijqdu, onkbgrt => eivffvr);
  bmodgptyb : entity work.swnxijvp
    port map (vafbzph => jkrlgnoxb, jgibip => evvldipycu, onkbgrt => ajdzpj);
end mbq;



-- Seed after: 10134150542063874037,8118127366649987907
