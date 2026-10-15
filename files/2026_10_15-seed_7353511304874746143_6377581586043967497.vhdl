-- Seed: 7353511304874746143,6377581586043967497

library ieee;
use ieee.std_logic_1164.all;

entity qiyvfc is
  port (n : in boolean_vector(1 to 2); wdgaagzz : in time; tvusqz : in std_logic_vector(4 downto 4));
end qiyvfc;

architecture jkgyh of qiyvfc is
  
begin
  
end jkgyh;

entity eih is
  port (iukc : buffer integer; kdblsj : linkage boolean);
end eih;

library ieee;
use ieee.std_logic_1164.all;

architecture zbvfj of eih is
  signal vltbnd : std_logic_vector(4 downto 4);
  signal hvk : time;
  signal tetbw : std_logic_vector(4 downto 4);
  signal isbpoct : time;
  signal bb : boolean_vector(1 to 2);
begin
  qis : entity work.qiyvfc
    port map (n => bb, wdgaagzz => isbpoct, tvusqz => tetbw);
  gua : entity work.qiyvfc
    port map (n => bb, wdgaagzz => hvk, tvusqz => vltbnd);
  sxaflefui : entity work.qiyvfc
    port map (n => bb, wdgaagzz => isbpoct, tvusqz => tetbw);
  vbbqx : entity work.qiyvfc
    port map (n => bb, wdgaagzz => isbpoct, tvusqz => tetbw);
  
  -- Single-driven assignments
  bb <= (FALSE, TRUE);
  iukc <= 2_2;
  hvk <= isbpoct;
  
  -- Multi-driven assignments
  tetbw <= tetbw;
  vltbnd <= tetbw;
  tetbw <= "L";
  vltbnd <= "Z";
end zbvfj;



-- Seed after: 1944461066775158832,6377581586043967497
