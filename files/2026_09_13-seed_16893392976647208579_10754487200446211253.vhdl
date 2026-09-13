-- Seed: 16893392976647208579,10754487200446211253

library ieee;
use ieee.std_logic_1164.all;

entity jyxiyprhxs is
  port (pre : in real; dzqijqzms : buffer std_logic_vector(2 to 3); sywqn : in real; hgluhktbkz : linkage time);
end jyxiyprhxs;

architecture baf of jyxiyprhxs is
  
begin
  
end baf;

library ieee;
use ieee.std_logic_1164.all;

entity igyhrt is
  port (ao : in integer; uuobc : linkage real; vzdzzgnygd : linkage std_logic; zi : buffer time);
end igyhrt;

library ieee;
use ieee.std_logic_1164.all;

architecture quiylauuo of igyhrt is
  signal ikmgahpyhw : real;
  signal l : real;
  signal xt : time;
  signal sqxmngd : real;
  signal ycjmfarymr : time;
  signal jqs : real;
  signal mhf : std_logic_vector(2 to 3);
  signal wescbsuro : time;
  signal bkbzu : real;
  signal dmjbh : std_logic_vector(2 to 3);
  signal brycvdlwmr : real;
begin
  qgnvsr : entity work.jyxiyprhxs
    port map (pre => brycvdlwmr, dzqijqzms => dmjbh, sywqn => bkbzu, hgluhktbkz => wescbsuro);
  mctrotd : entity work.jyxiyprhxs
    port map (pre => bkbzu, dzqijqzms => mhf, sywqn => jqs, hgluhktbkz => ycjmfarymr);
  pcbjwap : entity work.jyxiyprhxs
    port map (pre => sqxmngd, dzqijqzms => dmjbh, sywqn => brycvdlwmr, hgluhktbkz => xt);
  pugcn : entity work.jyxiyprhxs
    port map (pre => l, dzqijqzms => dmjbh, sywqn => ikmgahpyhw, hgluhktbkz => zi);
  
  -- Single-driven assignments
  sqxmngd <= brycvdlwmr;
  bkbzu <= bkbzu;
  l <= 2#1_1_0_0.101#;
  ikmgahpyhw <= bkbzu;
  brycvdlwmr <= brycvdlwmr;
  
  -- Multi-driven assignments
  dmjbh <= mhf;
  dmjbh <= dmjbh;
  dmjbh <= dmjbh;
end quiylauuo;

library ieee;
use ieee.std_logic_1164.all;

entity b is
  port (aipohrnrq : buffer time; wdlugoqll : inout std_logic_vector(4 to 2));
end b;

library ieee;
use ieee.std_logic_1164.all;

architecture dzdfqlao of b is
  signal svkdjpw : time;
  signal osmedukcdd : std_logic_vector(2 to 3);
  signal jybxm : time;
  signal mexrrbenfz : std_logic;
  signal usr : real;
  signal jdu : integer;
begin
  hwnbpikpgu : entity work.igyhrt
    port map (ao => jdu, uuobc => usr, vzdzzgnygd => mexrrbenfz, zi => jybxm);
  eptd : entity work.jyxiyprhxs
    port map (pre => usr, dzqijqzms => osmedukcdd, sywqn => usr, hgluhktbkz => svkdjpw);
  ls : entity work.jyxiyprhxs
    port map (pre => usr, dzqijqzms => osmedukcdd, sywqn => usr, hgluhktbkz => aipohrnrq);
  
  -- Single-driven assignments
  jdu <= jdu;
  
  -- Multi-driven assignments
  mexrrbenfz <= mexrrbenfz;
  wdlugoqll <= (others => '0');
  wdlugoqll <= wdlugoqll;
end dzdfqlao;



-- Seed after: 4908737137722179293,10754487200446211253
