-- Seed: 14812617507175642552,7304262412290825129

library ieee;
use ieee.std_logic_1164.all;

entity hwxkozlbw is
  port (a : out time; mub : buffer std_logic_vector(3 to 3); xhxun : inout bit);
end hwxkozlbw;

architecture npjpapqst of hwxkozlbw is
  
begin
  -- Single-driven assignments
  a <= a;
end npjpapqst;

entity ckcdfrd is
  port (brruiepmp : inout severity_level);
end ckcdfrd;

library ieee;
use ieee.std_logic_1164.all;

architecture lpudc of ckcdfrd is
  signal iuxegihwat : bit;
  signal iwbp : time;
  signal dayprb : bit;
  signal v : std_logic_vector(3 to 3);
  signal raeha : time;
  signal qpofb : bit;
  signal a : std_logic_vector(3 to 3);
  signal uryo : time;
  signal mlnt : bit;
  signal beetg : std_logic_vector(3 to 3);
  signal i : time;
begin
  ya : entity work.hwxkozlbw
    port map (a => i, mub => beetg, xhxun => mlnt);
  pnqzsy : entity work.hwxkozlbw
    port map (a => uryo, mub => a, xhxun => qpofb);
  pjmsgvu : entity work.hwxkozlbw
    port map (a => raeha, mub => v, xhxun => dayprb);
  cqwaxoybl : entity work.hwxkozlbw
    port map (a => iwbp, mub => v, xhxun => iuxegihwat);
  
  -- Single-driven assignments
  brruiepmp <= FAILURE;
  
  -- Multi-driven assignments
  beetg <= "U";
  v <= beetg;
  beetg <= v;
  beetg <= (others => 'H');
end lpudc;



-- Seed after: 13222177584788325954,7304262412290825129
