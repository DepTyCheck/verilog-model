-- Seed: 17932834364705047307,1112937151005418631

entity rwvgwuhuyv is
  port (dyrllni : out integer; hyix : buffer integer_vector(0 to 4); jwzsd : out time);
end rwvgwuhuyv;

architecture ijcaoks of rwvgwuhuyv is
  
begin
  -- Single-driven assignments
  jwzsd <= 2200.01 ps;
  dyrllni <= 16#0_E#;
  hyix <= (1_1_0, 4, 23103, 8#5#, 16#4_9_8_7_6#);
end ijcaoks;

entity blmhxupun is
  port (ycpwiw : in integer);
end blmhxupun;

architecture q of blmhxupun is
  signal mv : time;
  signal efhtzqan : integer_vector(0 to 4);
  signal zl : integer;
  signal xfmhnsg : time;
  signal yjnmf : integer_vector(0 to 4);
  signal vbqzmjntgf : integer;
  signal vay : time;
  signal mpqxdnnytm : integer_vector(0 to 4);
  signal eopcaflxix : integer;
begin
  scjrlnyikv : entity work.rwvgwuhuyv
    port map (dyrllni => eopcaflxix, hyix => mpqxdnnytm, jwzsd => vay);
  ufsjgbake : entity work.rwvgwuhuyv
    port map (dyrllni => vbqzmjntgf, hyix => yjnmf, jwzsd => xfmhnsg);
  rf : entity work.rwvgwuhuyv
    port map (dyrllni => zl, hyix => efhtzqan, jwzsd => mv);
end q;

library ieee;
use ieee.std_logic_1164.all;

entity ytpiry is
  port (ldiysrqgfe : out std_logic; bnck : buffer time; mgmple : inout std_logic; dsx : out time);
end ytpiry;

architecture sff of ytpiry is
  signal mv : time;
  signal uxicukydoq : integer_vector(0 to 4);
  signal yxpojxev : integer;
  signal goqlzppaa : time;
  signal euz : integer_vector(0 to 4);
  signal bnmzp : integer;
begin
  nhqeqqnpoc : entity work.rwvgwuhuyv
    port map (dyrllni => bnmzp, hyix => euz, jwzsd => goqlzppaa);
  bwoftd : entity work.rwvgwuhuyv
    port map (dyrllni => yxpojxev, hyix => uxicukydoq, jwzsd => mv);
  
  -- Single-driven assignments
  dsx <= 1 sec;
  bnck <= dsx;
end sff;

library ieee;
use ieee.std_logic_1164.all;

entity f is
  port (wukzh : in std_logic_vector(4 downto 3));
end f;

library ieee;
use ieee.std_logic_1164.all;

architecture tybnxm of f is
  signal hx : time;
  signal xslqfxh : std_logic;
  signal bzshlyhkmy : time;
  signal bypufem : std_logic;
  signal q : time;
  signal hywmnnr : integer_vector(0 to 4);
  signal fidleq : integer;
begin
  vmfyqplek : entity work.blmhxupun
    port map (ycpwiw => fidleq);
  vhqgjo : entity work.rwvgwuhuyv
    port map (dyrllni => fidleq, hyix => hywmnnr, jwzsd => q);
  y : entity work.ytpiry
    port map (ldiysrqgfe => bypufem, bnck => bzshlyhkmy, mgmple => xslqfxh, dsx => hx);
  
  -- Multi-driven assignments
  bypufem <= 'H';
end tybnxm;



-- Seed after: 6129583565869660925,1112937151005418631
