-- Seed: 6793899459268828881,3924983747739634027

library ieee;
use ieee.std_logic_1164.all;

entity ldau is
  port (jzgxeirdkp : inout integer; fmalfjmp : in std_logic; t : buffer integer);
end ldau;

architecture edbhw of ldau is
  
begin
  -- Single-driven assignments
  jzgxeirdkp <= 16#E8#;
  t <= 1304;
end edbhw;

library ieee;
use ieee.std_logic_1164.all;

entity yt is
  port (xmae : inout time; ehot : inout integer; hurlro : inout real; hi : inout std_logic);
end yt;

library ieee;
use ieee.std_logic_1164.all;

architecture rjkbfc of yt is
  signal xwv : std_logic;
  signal zcoya : integer;
  signal gjarpf : integer;
  signal a : integer;
  signal lwychroqcw : integer;
  signal hyvyno : integer;
begin
  sxhwrqt : entity work.ldau
    port map (jzgxeirdkp => hyvyno, fmalfjmp => hi, t => lwychroqcw);
  dtt : entity work.ldau
    port map (jzgxeirdkp => a, fmalfjmp => hi, t => gjarpf);
  m : entity work.ldau
    port map (jzgxeirdkp => zcoya, fmalfjmp => xwv, t => ehot);
  
  -- Multi-driven assignments
  hi <= '1';
end rjkbfc;

entity g is
  port (zelzfv : linkage integer);
end g;

library ieee;
use ieee.std_logic_1164.all;

architecture vobiwgdu of g is
  signal tdvsspxz : real;
  signal wnb : integer;
  signal ppo : time;
  signal hczrlhvgpg : integer;
  signal cfwkysals : std_logic;
  signal la : integer;
  signal odzgpdtfns : integer;
  signal bxkvn : std_logic;
  signal mftrjmlxd : integer;
begin
  t : entity work.ldau
    port map (jzgxeirdkp => mftrjmlxd, fmalfjmp => bxkvn, t => odzgpdtfns);
  bigiojkxle : entity work.ldau
    port map (jzgxeirdkp => la, fmalfjmp => cfwkysals, t => hczrlhvgpg);
  dzwi : entity work.yt
    port map (xmae => ppo, ehot => wnb, hurlro => tdvsspxz, hi => bxkvn);
  
  -- Multi-driven assignments
  bxkvn <= '-';
end vobiwgdu;



-- Seed after: 11792557460439194130,3924983747739634027
