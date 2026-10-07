-- Seed: 16846305437569494307,5906004015519833893

library ieee;
use ieee.std_logic_1164.all;

entity uxdfxt is
  port (okwwdlxb : in std_logic_vector(3 to 4); zbb : in std_logic; b : in real);
end uxdfxt;

architecture u of uxdfxt is
  
begin
  
end u;

entity nvfuz is
  port (zspjsru : inout time; repnlnld : out integer; exkndwd : buffer integer; sgnjujaqzl : buffer real);
end nvfuz;

library ieee;
use ieee.std_logic_1164.all;

architecture eqmhoivj of nvfuz is
  signal bdzdzrcjgb : real;
  signal xqvxyte : real;
  signal e : std_logic;
  signal ieiu : real;
  signal vxwn : std_logic;
  signal hbb : std_logic_vector(3 to 4);
begin
  glx : entity work.uxdfxt
    port map (okwwdlxb => hbb, zbb => vxwn, b => ieiu);
  lfeuoqw : entity work.uxdfxt
    port map (okwwdlxb => hbb, zbb => e, b => xqvxyte);
  nkqobb : entity work.uxdfxt
    port map (okwwdlxb => hbb, zbb => vxwn, b => bdzdzrcjgb);
  
  -- Single-driven assignments
  exkndwd <= 8#5_6_5_1#;
  sgnjujaqzl <= sgnjujaqzl;
  repnlnld <= 8#7704#;
  bdzdzrcjgb <= sgnjujaqzl;
  xqvxyte <= sgnjujaqzl;
  
  -- Multi-driven assignments
  hbb <= ('W', 'Z');
  hbb <= hbb;
end eqmhoivj;



-- Seed after: 13607030957933374389,5906004015519833893
