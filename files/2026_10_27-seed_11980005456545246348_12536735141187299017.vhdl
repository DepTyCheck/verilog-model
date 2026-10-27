-- Seed: 11980005456545246348,12536735141187299017

library ieee;
use ieee.std_logic_1164.all;

entity fna is
  port (khccgi : inout std_logic; rykt : in std_logic);
end fna;

architecture iotcspllw of fna is
  
begin
  -- Multi-driven assignments
  khccgi <= rykt;
  khccgi <= '1';
  khccgi <= khccgi;
  khccgi <= '-';
end iotcspllw;

entity juvsabnake is
  port (otjiqw : in real; hudoc : out string(2 to 3));
end juvsabnake;

library ieee;
use ieee.std_logic_1164.all;

architecture sk of juvsabnake is
  signal uydnbyrpe : std_logic;
  signal qri : std_logic;
  signal aitrsuqd : std_logic;
begin
  fup : entity work.fna
    port map (khccgi => aitrsuqd, rykt => aitrsuqd);
  qn : entity work.fna
    port map (khccgi => qri, rykt => uydnbyrpe);
  
  -- Single-driven assignments
  hudoc <= ('n', 'y');
  
  -- Multi-driven assignments
  uydnbyrpe <= aitrsuqd;
  aitrsuqd <= '1';
  aitrsuqd <= aitrsuqd;
end sk;



-- Seed after: 9956076852939789250,12536735141187299017
