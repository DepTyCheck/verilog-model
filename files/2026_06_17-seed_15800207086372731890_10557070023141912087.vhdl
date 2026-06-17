-- Seed: 15800207086372731890,10557070023141912087

library ieee;
use ieee.std_logic_1164.all;

entity ehpfbqs is
  port (jsmsodw : linkage real_vector(2 downto 3); zghbnc : inout time; po : inout std_logic_vector(0 to 1));
end ehpfbqs;

architecture ugrcz of ehpfbqs is
  
begin
  -- Single-driven assignments
  zghbnc <= 2#0_1_1_0.0_0_0_0_1# ns;
  
  -- Multi-driven assignments
  po <= "LL";
  po <= "-H";
  po <= ('0', 'W');
end ugrcz;

entity ummpqmji is
  port (v : buffer time; nhmqzb : linkage real; qtz : buffer integer; ru : buffer character);
end ummpqmji;

architecture kfmkh of ummpqmji is
  
begin
  
end kfmkh;

entity pxlsbympz is
  port (ghksb : inout bit);
end pxlsbympz;

library ieee;
use ieee.std_logic_1164.all;

architecture bcedus of pxlsbympz is
  signal clngnuig : std_logic_vector(0 to 1);
  signal qoz : time;
  signal vfigaa : real_vector(2 downto 3);
begin
  wucqp : entity work.ehpfbqs
    port map (jsmsodw => vfigaa, zghbnc => qoz, po => clngnuig);
  
  -- Single-driven assignments
  ghksb <= '1';
  
  -- Multi-driven assignments
  clngnuig <= "0-";
  clngnuig <= "Z1";
  clngnuig <= "X1";
end bcedus;

library ieee;
use ieee.std_logic_1164.all;

entity euhvfvzf is
  port (oru : linkage real; r : out character; pgprbmgtf : in std_logic);
end euhvfvzf;

architecture s of euhvfvzf is
  signal nuftb : bit;
  signal iwij : bit;
  signal yhjy : character;
  signal daohas : integer;
  signal kcjsc : real;
  signal xzhtvabmz : time;
begin
  y : entity work.ummpqmji
    port map (v => xzhtvabmz, nhmqzb => kcjsc, qtz => daohas, ru => yhjy);
  x : entity work.pxlsbympz
    port map (ghksb => iwij);
  mzik : entity work.pxlsbympz
    port map (ghksb => nuftb);
  
  -- Single-driven assignments
  r <= 'w';
end s;



-- Seed after: 11332833238603591074,10557070023141912087
