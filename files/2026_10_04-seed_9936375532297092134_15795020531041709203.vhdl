-- Seed: 9936375532297092134,15795020531041709203

library ieee;
use ieee.std_logic_1164.all;

entity zw is
  port (v : linkage real; jehfn : linkage std_logic);
end zw;

architecture dx of zw is
  
begin
  
end dx;

library ieee;
use ieee.std_logic_1164.all;

entity feauualh is
  port (nurdg : inout bit; oyc : inout std_logic_vector(2 to 2));
end feauualh;

library ieee;
use ieee.std_logic_1164.all;

architecture q of feauualh is
  signal ymhw : real;
  signal guvhck : std_logic;
  signal yfscu : real;
  signal ahieo : real;
  signal usqvcrj : std_logic;
  signal ejne : real;
begin
  jxiriich : entity work.zw
    port map (v => ejne, jehfn => usqvcrj);
  xctayfzo : entity work.zw
    port map (v => ahieo, jehfn => usqvcrj);
  xt : entity work.zw
    port map (v => yfscu, jehfn => guvhck);
  se : entity work.zw
    port map (v => ymhw, jehfn => usqvcrj);
  
  -- Single-driven assignments
  nurdg <= '1';
end q;

entity qtvnjxagc is
  port (vftry : inout integer; xmsz : buffer real; sai : out integer; cxkssikn : inout time);
end qtvnjxagc;

library ieee;
use ieee.std_logic_1164.all;

architecture oyymp of qtvnjxagc is
  signal npgbuh : bit;
  signal bfmkikfiz : std_logic;
  signal nnicfhjh : real;
  signal stna : std_logic_vector(2 to 2);
  signal ahmtafsc : bit;
begin
  xkhl : entity work.feauualh
    port map (nurdg => ahmtafsc, oyc => stna);
  rjkbxlspd : entity work.zw
    port map (v => nnicfhjh, jehfn => bfmkikfiz);
  ylxwra : entity work.feauualh
    port map (nurdg => npgbuh, oyc => stna);
  rmnxpik : entity work.zw
    port map (v => xmsz, jehfn => bfmkikfiz);
  
  -- Single-driven assignments
  cxkssikn <= cxkssikn;
  vftry <= sai;
  sai <= sai;
  
  -- Multi-driven assignments
  bfmkikfiz <= 'L';
end oyymp;



-- Seed after: 10946177400975946393,15795020531041709203
