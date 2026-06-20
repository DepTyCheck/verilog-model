-- Seed: 14082056345354319820,17924494779688682807

library ieee;
use ieee.std_logic_1164.all;

entity btfjn is
  port (ltxtvusda : in real; hwwj : inout std_logic_vector(4 downto 4));
end btfjn;

architecture ii of btfjn is
  
begin
  -- Multi-driven assignments
  hwwj <= "-";
  hwwj <= "L";
end ii;

entity tmj is
  port (scviawsoaz : buffer time);
end tmj;

library ieee;
use ieee.std_logic_1164.all;

architecture jec of tmj is
  signal pmevmw : std_logic_vector(4 downto 4);
  signal lwdvni : real;
begin
  sjxdy : entity work.btfjn
    port map (ltxtvusda => lwdvni, hwwj => pmevmw);
  
  -- Multi-driven assignments
  pmevmw <= (others => '-');
  pmevmw <= (others => 'L');
end jec;

entity esw is
  port (k : linkage time; qjziznyy : inout time_vector(1 downto 0); qatynn : inout time);
end esw;

architecture emjmrxp of esw is
  
begin
  -- Single-driven assignments
  qatynn <= 8#3_4_6_3_0# us;
  qjziznyy <= (8#4.6_4_1_4_2# ns, 8#01# ms);
end emjmrxp;

library ieee;
use ieee.std_logic_1164.all;

entity rglib is
  port (vhewx : inout integer; wbu : out time; w : out std_logic);
end rglib;

architecture nvgkjq of rglib is
  signal glcdtdifn : time;
  signal f : time_vector(1 downto 0);
  signal wxwvlbnaqr : time;
begin
  bvnulaude : entity work.esw
    port map (k => wxwvlbnaqr, qjziznyy => f, qatynn => glcdtdifn);
  
  -- Single-driven assignments
  wbu <= 1 sec;
  vhewx <= 2_3_3_3;
  
  -- Multi-driven assignments
  w <= 'H';
  w <= '0';
  w <= '-';
end nvgkjq;



-- Seed after: 6923610508015387718,17924494779688682807
