-- Seed: 9919868731489579779,8118127366649987907

library ieee;
use ieee.std_logic_1164.all;

entity qeebaijoee is
  port (gp : out integer; gjourno : in time; boxkybozjr : inout std_logic_vector(1 to 1));
end qeebaijoee;

architecture aahvij of qeebaijoee is
  
begin
  -- Single-driven assignments
  gp <= 2;
  
  -- Multi-driven assignments
  boxkybozjr <= (others => 'X');
end aahvij;

entity mhajgqt is
  port (wudsvat : inout bit; rtnoxc : out integer; ntphl : buffer time; felljbdth : inout time);
end mhajgqt;

library ieee;
use ieee.std_logic_1164.all;

architecture ev of mhajgqt is
  signal chhefzraw : std_logic_vector(1 to 1);
begin
  raudc : entity work.qeebaijoee
    port map (gp => rtnoxc, gjourno => felljbdth, boxkybozjr => chhefzraw);
  
  -- Multi-driven assignments
  chhefzraw <= "L";
  chhefzraw <= (others => 'U');
  chhefzraw <= "Z";
  chhefzraw <= "H";
end ev;



-- Seed after: 11158480428667087869,8118127366649987907
