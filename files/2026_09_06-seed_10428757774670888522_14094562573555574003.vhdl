-- Seed: 10428757774670888522,14094562573555574003

library ieee;
use ieee.std_logic_1164.all;

entity yruxultiju is
  port (da : out real; cyv : buffer time; jfbxkdosw : linkage std_logic);
end yruxultiju;

architecture bhr of yruxultiju is
  
begin
  
end bhr;

entity lcqqmbqacw is
  port (vigeik : linkage real);
end lcqqmbqacw;

library ieee;
use ieee.std_logic_1164.all;

architecture oxzeazkmyk of lcqqmbqacw is
  signal mgvfgwu : std_logic;
  signal jskkrbqla : time;
  signal qsyplelqmu : real;
begin
  xxi : entity work.yruxultiju
    port map (da => qsyplelqmu, cyv => jskkrbqla, jfbxkdosw => mgvfgwu);
end oxzeazkmyk;

entity qpx is
  port (sslepsqwyp : in real_vector(2 downto 1));
end qpx;

library ieee;
use ieee.std_logic_1164.all;

architecture qyjxx of qpx is
  signal ujh : std_logic;
  signal gwemxibge : time;
  signal maak : real;
  signal vwpw : time;
  signal jolth : real;
  signal nblmstmz : std_logic;
  signal nxs : time;
  signal izvgufh : real;
begin
  lvnxqcuj : entity work.yruxultiju
    port map (da => izvgufh, cyv => nxs, jfbxkdosw => nblmstmz);
  u : entity work.yruxultiju
    port map (da => jolth, cyv => vwpw, jfbxkdosw => nblmstmz);
  itxawsh : entity work.yruxultiju
    port map (da => maak, cyv => gwemxibge, jfbxkdosw => ujh);
  
  -- Multi-driven assignments
  nblmstmz <= '0';
end qyjxx;



-- Seed after: 15834379431002350932,14094562573555574003
