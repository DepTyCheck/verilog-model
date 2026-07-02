-- Seed: 16626745316878091284,13694093582652240945

library ieee;
use ieee.std_logic_1164.all;

entity nswuqzn is
  port (jzcaixs : inout std_logic; cnyyrcihww : out integer; qkyrokex : linkage std_logic_vector(2 to 3));
end nswuqzn;

architecture ketpuoxhmv of nswuqzn is
  
begin
  -- Multi-driven assignments
  jzcaixs <= '0';
  jzcaixs <= 'L';
  jzcaixs <= 'H';
end ketpuoxhmv;

library ieee;
use ieee.std_logic_1164.all;

entity hjwjyph is
  port (muyunvuun : linkage std_logic_vector(2 to 0); oovpktdjv : inout real);
end hjwjyph;

library ieee;
use ieee.std_logic_1164.all;

architecture pmvdnrqut of hjwjyph is
  signal t : integer;
  signal s : std_logic_vector(2 to 3);
  signal oumupoii : integer;
  signal dfthyrjaju : std_logic_vector(2 to 3);
  signal f : integer;
  signal rplbzjqj : std_logic;
begin
  qfwogezqmk : entity work.nswuqzn
    port map (jzcaixs => rplbzjqj, cnyyrcihww => f, qkyrokex => dfthyrjaju);
  qknuqwqla : entity work.nswuqzn
    port map (jzcaixs => rplbzjqj, cnyyrcihww => oumupoii, qkyrokex => s);
  foyzchqz : entity work.nswuqzn
    port map (jzcaixs => rplbzjqj, cnyyrcihww => t, qkyrokex => dfthyrjaju);
  
  -- Single-driven assignments
  oovpktdjv <= 2_1.0312;
  
  -- Multi-driven assignments
  rplbzjqj <= '0';
  rplbzjqj <= '-';
end pmvdnrqut;

entity wmh is
  port (qno : buffer time; bdtj : buffer integer_vector(2 to 4); irudb : inout time; zswmjjy : linkage real);
end wmh;

library ieee;
use ieee.std_logic_1164.all;

architecture vbtpupay of wmh is
  signal tbkvzv : integer;
  signal rmmuwsdrsy : std_logic;
  signal p : real;
  signal sdluhzba : std_logic_vector(2 to 0);
  signal ts : integer;
  signal gnfez : std_logic_vector(2 to 3);
  signal xjgi : integer;
  signal xiomti : std_logic;
begin
  xurlvesc : entity work.nswuqzn
    port map (jzcaixs => xiomti, cnyyrcihww => xjgi, qkyrokex => gnfez);
  sktdvj : entity work.nswuqzn
    port map (jzcaixs => xiomti, cnyyrcihww => ts, qkyrokex => gnfez);
  itahfsyq : entity work.hjwjyph
    port map (muyunvuun => sdluhzba, oovpktdjv => p);
  yjhkdcqb : entity work.nswuqzn
    port map (jzcaixs => rmmuwsdrsy, cnyyrcihww => tbkvzv, qkyrokex => gnfez);
  
  -- Single-driven assignments
  irudb <= 030.3_0_4_3_3 fs;
  qno <= 2#0011# ns;
  bdtj <= (0_0, 0423, 0_4_1_1_2);
end vbtpupay;

entity pxzttps is
  port (ihrxs : inout bit; pmv : buffer integer; srm : buffer boolean_vector(4 to 1); bgkbupbtht : buffer integer);
end pxzttps;

library ieee;
use ieee.std_logic_1164.all;

architecture fdkvum of pxzttps is
  signal tfy : real;
  signal prwspwwn : std_logic_vector(2 to 0);
begin
  f : entity work.hjwjyph
    port map (muyunvuun => prwspwwn, oovpktdjv => tfy);
  
  -- Single-driven assignments
  bgkbupbtht <= 200;
  pmv <= 16#E8F#;
  srm <= (others => TRUE);
  ihrxs <= '0';
  
  -- Multi-driven assignments
  prwspwwn <= "";
  prwspwwn <= (others => '0');
  prwspwwn <= (others => '0');
end fdkvum;



-- Seed after: 10062284400615726236,13694093582652240945
