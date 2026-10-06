-- Seed: 10427537046415599197,3042374792655995433

entity xsuj is
  port (hlxdxu : in real; wn : in integer; udkehouujj : linkage bit_vector(2 to 3); k : in time);
end xsuj;

architecture hyvlkxf of xsuj is
  
begin
  
end hyvlkxf;

library ieee;
use ieee.std_logic_1164.all;

entity rimbjh is
  port (jlnn : in integer; jokngtnrwj : in integer; s : inout std_logic);
end rimbjh;

architecture iooxy of rimbjh is
  signal zemzfxlw : bit_vector(2 to 3);
  signal ku : integer;
  signal ffmyyzuceb : real;
  signal vuu : bit_vector(2 to 3);
  signal hrmopgxpo : time;
  signal uhf : bit_vector(2 to 3);
  signal zjc : real;
  signal dkw : time;
  signal bcd : bit_vector(2 to 3);
  signal ivlgdyqse : real;
begin
  ccrkwt : entity work.xsuj
    port map (hlxdxu => ivlgdyqse, wn => jlnn, udkehouujj => bcd, k => dkw);
  bjdcepo : entity work.xsuj
    port map (hlxdxu => zjc, wn => jlnn, udkehouujj => uhf, k => hrmopgxpo);
  oonw : entity work.xsuj
    port map (hlxdxu => zjc, wn => jokngtnrwj, udkehouujj => vuu, k => dkw);
  thave : entity work.xsuj
    port map (hlxdxu => ffmyyzuceb, wn => ku, udkehouujj => zemzfxlw, k => hrmopgxpo);
  
  -- Single-driven assignments
  dkw <= 0_3.3_4 us;
  ffmyyzuceb <= 16#3D.391F6#;
  hrmopgxpo <= 1 hr;
  
  -- Multi-driven assignments
  s <= 'X';
end iooxy;



-- Seed after: 4766530133221862489,3042374792655995433
