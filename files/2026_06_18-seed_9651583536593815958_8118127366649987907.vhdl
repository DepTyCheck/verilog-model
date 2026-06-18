-- Seed: 9651583536593815958,8118127366649987907

library ieee;
use ieee.std_logic_1164.all;

entity z is
  port (pmyl : in std_logic_vector(4 downto 2); rlvyguvs : out std_logic_vector(4 downto 0); vofuvdyxs : buffer std_logic; dmeqj : inout character);
end z;

architecture cceebbh of z is
  
begin
  -- Single-driven assignments
  dmeqj <= 'q';
end cceebbh;

library ieee;
use ieee.std_logic_1164.all;

entity mlkhnmyhvw is
  port (hdfiqj : in std_logic; yn : in time; srfidaortf : out bit_vector(1 downto 0); aansaue : linkage std_logic_vector(3 to 3));
end mlkhnmyhvw;

library ieee;
use ieee.std_logic_1164.all;

architecture bte of mlkhnmyhvw is
  signal ueraknco : character;
  signal us : std_logic;
  signal yjrqsogx : std_logic_vector(4 downto 0);
  signal xpswlemt : character;
  signal siypabzjup : std_logic;
  signal siwget : std_logic_vector(4 downto 0);
  signal e : std_logic_vector(4 downto 2);
begin
  ucomxl : entity work.z
    port map (pmyl => e, rlvyguvs => siwget, vofuvdyxs => siypabzjup, dmeqj => xpswlemt);
  wifcmqhbkt : entity work.z
    port map (pmyl => e, rlvyguvs => yjrqsogx, vofuvdyxs => us, dmeqj => ueraknco);
  
  -- Single-driven assignments
  srfidaortf <= ('1', '0');
  
  -- Multi-driven assignments
  e <= "XLX";
end bte;

entity ihnmbp is
  port (crv : buffer severity_level; ilhlgq : out time; npth : inout time; udw : buffer boolean);
end ihnmbp;

architecture dzwsj of ihnmbp is
  
begin
  -- Single-driven assignments
  npth <= 2 sec;
  crv <= WARNING;
  ilhlgq <= 1 min;
  udw <= FALSE;
end dzwsj;



-- Seed after: 57693148815251415,8118127366649987907
