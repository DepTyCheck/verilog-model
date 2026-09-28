-- Seed: 15419744691567180183,7311216359267151659

entity mrgbgs is
  port (qksrsqdfl : in integer; jdfzhxeio : in real);
end mrgbgs;

architecture dckhbnkk of mrgbgs is
  
begin
  
end dckhbnkk;

entity w is
  port (xd : in real);
end w;

architecture bcjsgar of w is
  signal jqf : integer;
begin
  mldhcitad : entity work.mrgbgs
    port map (qksrsqdfl => jqf, jdfzhxeio => xd);
  
  -- Single-driven assignments
  jqf <= 16#2D5A#;
end bcjsgar;

entity aiypsby is
  port (rakzbgi : inout integer_vector(4 downto 0); jtxufmww : buffer bit_vector(1 to 3));
end aiypsby;

architecture uncjgalnsa of aiypsby is
  
begin
  -- Single-driven assignments
  jtxufmww <= ('0', '0', '1');
  rakzbgi <= (4, 0_4_4_1, 2#0_0_1_0_0#, 16#198#, 2#10000#);
end uncjgalnsa;

library ieee;
use ieee.std_logic_1164.all;

entity iax is
  port (e : out time; eymoncasg : buffer std_logic);
end iax;

architecture vyxoqmvdrc of iax is
  signal bjll : integer;
  signal jvnfo : bit_vector(1 to 3);
  signal fwvcroh : integer_vector(4 downto 0);
  signal sffxhutd : real;
begin
  hhshtzqb : entity work.w
    port map (xd => sffxhutd);
  o : entity work.aiypsby
    port map (rakzbgi => fwvcroh, jtxufmww => jvnfo);
  wgjtanuofh : entity work.mrgbgs
    port map (qksrsqdfl => bjll, jdfzhxeio => sffxhutd);
  
  -- Single-driven assignments
  sffxhutd <= 1.3_1_1_1;
  
  -- Multi-driven assignments
  eymoncasg <= '-';
  eymoncasg <= eymoncasg;
end vyxoqmvdrc;



-- Seed after: 4618358446767340700,7311216359267151659
