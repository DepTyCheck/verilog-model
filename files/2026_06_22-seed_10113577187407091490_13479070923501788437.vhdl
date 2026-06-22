-- Seed: 10113577187407091490,13479070923501788437

library ieee;
use ieee.std_logic_1164.all;

entity fodvbxily is
  port (ihztpsue : in time; rmp : out integer; odclchjkzi : inout std_logic);
end fodvbxily;

architecture ysugodrj of fodvbxily is
  
begin
  -- Multi-driven assignments
  odclchjkzi <= 'H';
  odclchjkzi <= '-';
  odclchjkzi <= 'H';
  odclchjkzi <= 'X';
end ysugodrj;

library ieee;
use ieee.std_logic_1164.all;

entity bxdirizfnh is
  port (anhftote : in integer; nloss : inout std_logic);
end bxdirizfnh;

library ieee;
use ieee.std_logic_1164.all;

architecture bwu of bxdirizfnh is
  signal yldeajqu : integer;
  signal xjpeyqbl : time;
  signal ipmal : integer;
  signal mmkictw : std_logic;
  signal kyjc : integer;
  signal w : time;
  signal azrhienm : std_logic;
  signal qmz : integer;
  signal pjd : time;
begin
  wjjgwibb : entity work.fodvbxily
    port map (ihztpsue => pjd, rmp => qmz, odclchjkzi => azrhienm);
  jqll : entity work.fodvbxily
    port map (ihztpsue => w, rmp => kyjc, odclchjkzi => mmkictw);
  m : entity work.fodvbxily
    port map (ihztpsue => pjd, rmp => ipmal, odclchjkzi => nloss);
  skfbuqtr : entity work.fodvbxily
    port map (ihztpsue => xjpeyqbl, rmp => yldeajqu, odclchjkzi => azrhienm);
  
  -- Single-driven assignments
  pjd <= 2 sec;
  w <= 2#1_1# fs;
  xjpeyqbl <= 2_0_0_4_1.13033 us;
  
  -- Multi-driven assignments
  nloss <= '0';
  mmkictw <= 'U';
end bwu;



-- Seed after: 17428220081995046272,13479070923501788437
