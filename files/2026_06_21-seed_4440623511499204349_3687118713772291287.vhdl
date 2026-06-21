-- Seed: 4440623511499204349,3687118713772291287

library ieee;
use ieee.std_logic_1164.all;

entity cmrjtatwha is
  port (dvarfizhf : inout real_vector(4 to 2); qk : buffer time; bjamay : inout std_logic);
end cmrjtatwha;

architecture mpnwykw of cmrjtatwha is
  
begin
  -- Single-driven assignments
  qk <= 23134 ms;
  
  -- Multi-driven assignments
  bjamay <= '1';
  bjamay <= 'U';
  bjamay <= 'W';
  bjamay <= 'W';
end mpnwykw;

library ieee;
use ieee.std_logic_1164.all;

entity ejuhsttjvf is
  port (uvammv : in std_logic; p : inout bit; uaofaxypt : linkage std_logic_vector(0 downto 0));
end ejuhsttjvf;

architecture av of ejuhsttjvf is
  
begin
  -- Single-driven assignments
  p <= '0';
end av;

library ieee;
use ieee.std_logic_1164.all;

entity qqqgcvtow is
  port (j : buffer time_vector(3 to 0); oho : linkage bit; oqssn : inout std_logic_vector(4 downto 3));
end qqqgcvtow;

library ieee;
use ieee.std_logic_1164.all;

architecture xzv of qqqgcvtow is
  signal awyan : std_logic_vector(0 downto 0);
  signal ycgzrmryl : bit;
  signal gsqld : time;
  signal txwa : real_vector(4 to 2);
  signal nbup : std_logic;
  signal hajgtza : time;
  signal rt : real_vector(4 to 2);
begin
  csohgk : entity work.cmrjtatwha
    port map (dvarfizhf => rt, qk => hajgtza, bjamay => nbup);
  v : entity work.cmrjtatwha
    port map (dvarfizhf => txwa, qk => gsqld, bjamay => nbup);
  cmpeyaesyr : entity work.ejuhsttjvf
    port map (uvammv => nbup, p => ycgzrmryl, uaofaxypt => awyan);
  
  -- Single-driven assignments
  j <= (others => 0 ns);
  
  -- Multi-driven assignments
  awyan <= "U";
  awyan <= (others => 'L');
end xzv;



-- Seed after: 3470584921721377911,3687118713772291287
