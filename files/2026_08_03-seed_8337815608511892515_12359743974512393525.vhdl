-- Seed: 8337815608511892515,12359743974512393525

library ieee;
use ieee.std_logic_1164.all;

entity uiyj is
  port (cwbsbx : linkage time; i : out std_logic_vector(4 downto 3));
end uiyj;

architecture bvqskwkie of uiyj is
  
begin
  
end bvqskwkie;

entity ttom is
  port (rdwo : buffer time);
end ttom;

library ieee;
use ieee.std_logic_1164.all;

architecture gorbq of ttom is
  signal q : time;
  signal ddeiolwen : time;
  signal fagdb : time;
  signal jqce : std_logic_vector(4 downto 3);
  signal bfijwxh : time;
begin
  ccyuyo : entity work.uiyj
    port map (cwbsbx => bfijwxh, i => jqce);
  xqyblkrpv : entity work.uiyj
    port map (cwbsbx => fagdb, i => jqce);
  voblabwh : entity work.uiyj
    port map (cwbsbx => ddeiolwen, i => jqce);
  rutddofg : entity work.uiyj
    port map (cwbsbx => q, i => jqce);
  
  -- Single-driven assignments
  rdwo <= bfijwxh;
  
  -- Multi-driven assignments
  jqce <= ('U', 'Z');
  jqce <= "1X";
  jqce <= ('Z', 'L');
  jqce <= jqce;
end gorbq;



-- Seed after: 8813065647001515324,12359743974512393525
